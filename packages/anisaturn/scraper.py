import logging
from argparse import ArgumentParser, Namespace
from asyncio import (Event, Semaphore, TaskGroup, TimeoutError, run, sleep,
                     wait_for)
from dataclasses import dataclass
from sys import exit

from argcomplete import autocomplete
from playwright.async_api import (Browser, BrowserContext, Page,
                                  async_playwright)

DEFAULT_STEALTH_SCRIPT = """
Object.defineProperty(navigator, 'webdriver', { get: () => undefined });
window.chrome = { runtime: {} };
Object.defineProperty(navigator, 'languages', { get: () => ['it-IT', 'it'] });
Object.defineProperty(navigator, 'plugins', { get: () => [1, 2, 3, 4, 5] });
"""

DEFAULT_USER_AGENT = (
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
    "(KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36"
)


@dataclass
class ScraperConfig:
    """Scraper configuration, with sensible default values."""

    url: str | None = None
    episodes: int = 12
    max_concurrency: int = 3
    retries: int = 1
    max_wait_seconds: float = 12
    pause_between_requests: float = 5
    stagger_start: float = 2
    click_timeout: int = 15000
    button_timeout: int = 6000
    valid_extensions: tuple[str, ...] = (".mp4", ".m3u8")
    user_agent: str = DEFAULT_USER_AGENT
    stealth_script: str = DEFAULT_STEALTH_SCRIPT

    @property
    def paths(self) -> list[str]:
        return [f"ep-{i}" for i in range(1, self.episodes + 1)]

    @staticmethod
    def add_arguments(parser: ArgumentParser) -> None:
        defaults = ScraperConfig()
        group = parser.add_argument_group("scraper")
        group.add_argument(
            "--url",
            default=defaults.url,
            help="Anime page url used to derive episode urls. Required, no default value.",
        )
        group.add_argument(
            "--episodes",
            type=int,
            default=defaults.episodes,
            help="Number of episodes to attempt, generates ep-1..ep-N (default: %(default)s)",
        )
        group.add_argument(
            "--max-concurrency",
            type=int,
            default=defaults.max_concurrency,
            help="Maximum number of pages opened in parallel (default: %(default)s)",
        )
        group.add_argument(
            "--retries",
            type=int,
            default=defaults.retries,
            help="Number of retries per episode on failure (default: %(default)s)",
        )
        group.add_argument(
            "--max-wait-seconds",
            type=float,
            default=defaults.max_wait_seconds,
            help="Maximum seconds to wait for a valid url (default: %(default)s)",
        )
        group.add_argument(
            "--pause-between-requests",
            type=float,
            default=defaults.pause_between_requests,
            help="Seconds to pause between retries (default: %(default)s)",
        )
        group.add_argument(
            "--stagger-start",
            type=float,
            default=defaults.stagger_start,
            help="Seconds of stagger between starting one episode and the next (default: %(default)s)",
        )
        group.add_argument(
            "--click-timeout",
            type=int,
            default=defaults.click_timeout,
            help="Timeout (ms) for scrolling/clicking the play button (default: %(default)s)",
        )
        group.add_argument(
            "--button-timeout",
            type=int,
            default=defaults.button_timeout,
            help="Timeout (ms) to check for the play button's presence (default: %(default)s)",
        )

    @classmethod
    def from_namespace(cls, ns: Namespace) -> "ScraperConfig":
        return cls(
            url=ns.url,
            episodes=ns.episodes,
            max_concurrency=ns.max_concurrency,
            retries=ns.retries,
            max_wait_seconds=ns.max_wait_seconds,
            pause_between_requests=ns.pause_between_requests,
            stagger_start=ns.stagger_start,
            click_timeout=ns.click_timeout,
            button_timeout=ns.button_timeout,
        )


@dataclass
class LoggingConfig:
    """Logging configuration, with sensible default values."""

    debug: bool = False
    fmt: str = "%(asctime)s [%(levelname)s] %(message)s"
    datefmt: str = "%H:%M:%S"

    @property
    def level(self) -> int:
        return logging.DEBUG if self.debug else logging.INFO

    @staticmethod
    def add_arguments(parser: ArgumentParser) -> None:
        defaults = LoggingConfig()
        group = parser.add_argument_group("logging")
        group.add_argument(
            "--debug",
            action="store_true",
            default=defaults.debug,
            help=(
                "Enable debug mode: verbose logging plus debug artifacts "
                "such as screenshots on missing mp4/m3u8 urls (default: %(default)s)"
            ),
        )

    @classmethod
    def from_namespace(cls, ns: Namespace) -> "LoggingConfig":
        return cls(debug=ns.debug)

    def configure(self) -> logging.Logger:
        logging.basicConfig(
            level=self.level,
            format=self.fmt,
            datefmt=self.datefmt,
        )
        return logging.getLogger("scraper")


def parse_args(argv: list[str] | None = None) -> Namespace:
    parser = ArgumentParser(
        prog="anisaturn", description="AnimeSaturn series urls extractor"
    )
    ScraperConfig.add_arguments(parser)
    LoggingConfig.add_arguments(parser)
    autocomplete(parser)
    return parser.parse_args(argv)


def build_configs(
    argv: list[str] | None = None,
) -> tuple[ScraperConfig, LoggingConfig]:
    ns = parse_args(argv)
    return ScraperConfig.from_namespace(ns), LoggingConfig.from_namespace(ns)


logger = logging.getLogger("scraper")


async def _attempt_single(
    browser: Browser,
    path: str,
    semaphore: Semaphore,
    cfg: ScraperConfig,
) -> tuple[str, set[str]]:
    url = f"{cfg.url}/{path}"
    mp4_urls: set[str] = set()
    found_event = Event()

    async with semaphore:

        def _track_url(resource_url: str) -> None:
            """
            Track any valid url given from the request/response if it matches
            cfg.valid_extensions.
            """
            if any(ext in resource_url.lower() for ext in cfg.valid_extensions):
                mp4_urls.add(resource_url)
                found_event.set()

        # A fresh, isolated context per attempt: no cookies, cache, localStorage,
        # or service workers carried over from a previous episode or retry.
        context: BrowserContext = await browser.new_context(user_agent=cfg.user_agent)
        page: Page = await context.new_page()
        await page.add_init_script(cfg.stealth_script)
        await page.set_extra_http_headers({"Referer": "https://www.animesaturn.net/"})
        page.on("request", lambda r: _track_url(r.url))
        page.on("response", lambda r: _track_url(r.url))
        # Diagnostics: surface JS console messages and uncaught page errors so we
        # can see *why* the player fails to load, instead of just observing that
        # no mp4/m3u8 request ever showed up.
        page.on(
            "console",
            lambda msg: logger.debug("%s - console[%s]: %s", path, msg.type, msg.text),
        )
        page.on(
            "pageerror",
            lambda exc: logger.debug("%s - pageerror: %s", path, exc),
        )
        try:
            logger.debug("Visiting: %s", url)
            # This site loads the player via JS after DOMContentLoaded: with
            # "domcontentloaded" the script would move on before the video
            # request even starts. "networkidle" is needed here, even if slower.
            await page.goto(url, wait_until="networkidle", timeout=20000)

            # if the "Watch streaming" button is present, click it; otherwise continue anyway
            locator = page.locator("a.ept-btn--play")
            button_present = False
            try:
                await locator.wait_for(state="visible", timeout=cfg.button_timeout)
                button_present = True
            except Exception:
                button_present = False

            logger.debug("%s - button_present=%s", path, button_present)

            if button_present:
                await locator.scroll_into_view_if_needed(timeout=cfg.click_timeout)
                await locator.click(timeout=cfg.click_timeout)
                await page.wait_for_load_state("networkidle", timeout=15000)
                logger.debug(
                    "%s - Button found, player page reached: %s", path, page.url
                )
            else:
                logger.debug(
                    "%s - No button found, trying directly on the current page", path
                )

            await page.wait_for_timeout(2000)

            player_error = await page.locator("text=failed to load").count()
            if player_error > 0:
                logger.warning(
                    "%s - Player shows a loading error (count=%s)", path, player_error
                )
                # The player is already in an error state: no point waiting up to
                # max_wait_seconds, no mp4/m3u8 will ever show up.
                return url, mp4_urls

            try:
                await wait_for(found_event.wait(), timeout=cfg.max_wait_seconds)
            except TimeoutError:
                pass

            logger.debug("%s - wait done, found=%s", path, len(mp4_urls))

            # Screenshots are only worth the disk/time cost when we're actually
            # debugging: gate them on the effective log level instead of a
            # separate flag, so "--log-level DEBUG" is the single switch for
            # both verbose logs and debug artifacts.
            if not mp4_urls and logger.isEnabledFor(logging.DEBUG):
                await page.screenshot(path=f"debug_{path}.png")
                logger.debug("%s - Screenshot saved: debug_%s.png", path, path)

        except Exception as e:
            logger.error("%s: %s", url, e)
        finally:
            await page.close()
            await context.close()

    return url, mp4_urls


async def find_mp4(
    browser: Browser,
    path: str,
    semaphore: Semaphore,
    config: ScraperConfig,
    start_delay: float = 0,
) -> tuple[str, set[str]]:
    if start_delay:
        await sleep(start_delay)

    url = f"{config.url}/{path}"
    mp4s: set[str] = set()

    for attempt in range(config.retries + 1):
        url, mp4s = await _attempt_single(browser, path, semaphore, config)
        if mp4s or attempt == config.retries:
            return url, mp4s
        logger.warning("%s - attempt %s failed, retrying...", url, attempt)
        await sleep(config.pause_between_requests)

    return url, mp4s


async def main() -> None:
    scraper_config, logging_config = build_configs()
    global logger
    logger = logging_config.configure()

    if not scraper_config.url:
        logger.error(
            "No --url provided. This is required: pass the anime page url, "
            "e.g. --url https://www.animesaturn.net/anime/some-anime-slug"
        )
        exit(1)

    semaphore = Semaphore(scraper_config.max_concurrency)
    results: list[tuple[str, set[str]]] = []

    async with async_playwright() as p:
        browser = await p.chromium.launch(
            headless=True,
            args=["--disable-blink-features=AutomationControlled"],
        )

        try:
            # TaskGroup (3.11+): if a task raises an unhandled exception, the
            # others get cancelled cleanly instead of running blind, unlike
            # asyncio.gather without return_exceptions=True.
            async with TaskGroup() as tg:
                tasks = [
                    tg.create_task(
                        find_mp4(
                            browser,
                            path,
                            semaphore,
                            scraper_config,
                            start_delay=i * scraper_config.stagger_start,
                        )
                    )
                    for i, path in enumerate(scraper_config.paths)
                ]
        except* Exception as eg:
            for exc in eg.exceptions:
                logger.error("Task failed: Cause: %s", exc)
            logger.error("One or more episodes failed; results may be incomplete.")
        else:
            results = [t.result() for t in tasks]

        await browser.close()

    for url, mp4s in results:
        for m in mp4s:
            print(m)


if __name__ == "__main__":
    run(main())
