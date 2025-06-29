{
  lib,
  rustPlatform,
  fetchFromGitHub,
  fetchNpmDeps,
  cargo-tauri,
  nodejs,
  npmHooks,
  pkg-config,
  wrapGAppsHook3,
  glib-networking,
  udev,
  webkitgtk_4_1,
}:

rustPlatform.buildRustPackage (finalAttrs: {
  pname = "zmk-studio";
  version = "0.3.1";

  src = fetchFromGitHub {
    owner = "zmkfirmware";
    repo = "zmk-studio";
    tag = "v${finalAttrs.version}";
    hash = "sha256-g7JqALJLB9hT35ZLGKp7SAH+nj8BhJgTattEwSh8Ss8=";
  };

  patches = [ ./lockfile-integrity.patch ];

  # building the download page requires the app to fetch the github API at build-time
  # this page would only ever be visited if it wasn't built with tauri, so we disable it instead
  postPatch = ''
    # turn `npm run generate-data` into a NOOP
    substituteInPlace package.json \
      --replace-fail '"generate-data": "' '"generate-data": "true || '

    # remove download page
    rm download.html src/download.tsx src/DownloadPage.tsx
    substituteInPlace vite.config.ts \
      --replace-fail 'download: "./download.html",' ""
  '';

  cargoRoot = "src-tauri";
  buildAndTestSubdir = "src-tauri";

  cargoHash = "sha256-BNX2vhsHaSk3eiadVPH6To0CgbOEGJ1JVKkW3Hw7QH0=";

  npmDeps = fetchNpmDeps {
    name = "zmk-studio-${finalAttrs.version}-npm-deps";
    inherit (finalAttrs) src patches;
    hash = "sha256-xvVsLqIWMNMnpZZ3vkgmQq/zvaWr+uh3a4rP2cSC3Yw=";
  };

  nativeBuildInputs = [
    cargo-tauri.hook
    nodejs
    npmHooks.npmConfigHook
    pkg-config
    wrapGAppsHook3
  ];

  buildInputs = [
    glib-networking # for image loading
    udev
    webkitgtk_4_1
  ];

  meta = {
    description = "Tool for runtime keymap updates on ZMK-powered devices without reflashing firmware";
    homepage = "https://github.com/zmkfirmware/zmk-studio";
    changelog = "https://github.com/zmkfirmware/zmk-studio/blob/${finalAttrs.src.rev}/CHANGELOG.md";
    license = lib.licenses.asl20;
    mainProgram = "zmk-studio";
    maintainers = [ lib.maintainers.luftmensch-luftmensch ];
    platforms = lib.platforms.linux;
  };
})
