{ lib, config, ... }:
{

  withCode = lib.optionalAttrs config.valentino.modules.editors.vscode.enable;
  withChromium = lib.optionalAttrs config.valentino.modules.browsers.chromium.enable;
  withFirefox = lib.optionalAttrs config.valentino.modules.browsers.firefox.enable;
  withIntellij = lib.optionalAttrs config.valentino.modules.editors.intellij.enable;
}
