{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.editors.vscode;
  theme = config.valentino.modules.themes;
in {
  options.valentino.modules.editors.vscode = with types; {
    enable = mkEnableOption "vscode";
    windowZoomLevel = mkOption {
      type = int;
      default = 0;
    };
  };

  config = mkIf cfg.enable {
    programs.vscode = {
      enable = true;
      package = pkgs.vscodium;
      extensions = with pkgs.vscode-extensions;
        [
          mkhl.direnv
          bbenoist.nix
          vscodevim.vim
          gruntfuggly.todo-tree
          dart-code.dart-code
          dart-code.flutter
        ]
        ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
          {
            name = "better-comments";
            publisher = "aaron-bond";
            version = "3.0.2";
            sha256 = "sha256-hQmA8PWjf2Nd60v5EAuqqD8LIEu7slrNs8luc3ePgZc=";
          }
        ];

      keybindings = [
        {
          key = "ctrl+shift+e";
          command = "workbench.action.toggleSidebarVisibility";
        }

        {
          key = "ctrl+a";
          command = "cursorLineStart";
          when = "editorTextFocus";
        }

        {
          key = "ctrl+e";
          command = "cursorLineEnd";
          when = "editorTextFocus";
        }

        {
          key = "alt+t";
          command = "workbench.action.terminal.toggleTerminal";
          when = "terminal.active";
        }

        {
          key = "alt+p";
          command = "workbench.action.quickOpen";
        }

        {
          key = "alt+e";
          command = "workbench.view.explorer";
        }
      ];
      userSettings = {
        "window.menuBarVisibility" = "toggle";
        "window.zoomLevel" = cfg.windowZoomLevel;
        "window.title" = "VSCode \${separator} \${activeEditorShort}";
        "workbench.colorCustomizations" = {
          "statusBar.background" = "#005f5f";
          "statusBar.noFolderBackground" = "#005f5f";
          "statusBar.debuggingBackground" = "#cc241d";
        };

        "editor.cursorBlinking" = "smooth";
        "editor.renderWhitespace" = "boundary";
        "editor.bracketPairColorization.enabled" = true;
        "editor.fontLigatures" = true;
        "editor.formatOnPaste" = false;
        "editor.formatOnSave" = false;
        "editor.formatOnType" = false;
        "editor.lineNumbers" = "relative";
        "editor.linkedEditing" = true;
        "editor.mouseWheelZoom" = true;
        "editor.smoothScrolling" = true;
        "editor.fontSize" = theme.font.regular.size + 6;
        "editor.fontFamily" = "'${theme.font.regular.family}', 'monospace', monospace";
        "editor.renderFinalNewline" = "off";
        "workbench.startupEditor" = "none";
        "workbench.list.smoothScrolling" = true;
        "workbench.editor.highlightModifiedTabs" = true;
        "workbench.editor.showTabs" = "multiple";
        "workbench.editor.labelFormat" = "short";
        "workbench.editor.enablePreview" = false;
        "security.workspace.trust.untrustedFiles" = "open";
        "update.mode" = "none";
        "editor.minimap.enabled" = false;
        "telemetry.telemetryLevel" = "off";

        # VIM
        "vim.hlsearch" = true;
        "vim.ignorecase" = true;
        "vim.incsearch" = true;
        "vim.smartcase" = true;
        "vim.camelCaseMotion.enable" = true;
        "vim.useSystemClipboard" = true;
        "vim.leader" = " ";
        "vim.handleKeys" = {
          "<C-p>" = false;
        };

        "vim.normalModeKeyBindingsNonRecursive" = [
          # Goto definition
          {
            "before" = [
              "<leader>"
              "g"
              "d"
            ];
            "commands" = [
              "editor.action.revealDefinition"
            ];
          }

          # Rename symbol
          {
            "before" = [
              "<leader>"
              "g"
              "r"
            ];
            "commands" = [
              "editor.action.rename"
            ];
          }

          # Rename symbol
          {
            "before" = [
              "<leader>"
              "g"
              "R"
            ];
            "commands" = [
              "editor.action.goToReferences"
            ];
          }

          # Toggle sidebar
          {
            "before" = [
              "<leader>"
              "d"
            ];
            "commands" = [
              "workbench.action.toggleSidebarVisibility"
            ];
          }

          # Split vertical
          {
            "before" = [
              "<leader>"
              "v"
            ];
            "commands" = [
              "workbench.action.splitEditor"
            ];
          }

          # Split horizontal
          {
            "before" = [
              "<leader>"
              "h"
            ];
            "commands" = [
              "workbench.action.splitEditorDown"
            ];
          }

          # Unsplit
          {
            "before" = [
              "<leader>"
              "x"
            ];
            "commands" = [
              "workbench.action.editorLayoutSingle"
            ];
          }

          # Goto Next problem (Error/Warning/Info)
          {
            "before" = [
              "<leader>"
              "g"
              "n"
            ];
            "commands" = [
              "editor.action.marker.next"
            ];
          }

          # Goto Previous problem (Error/Warning/Info)
          {
            "before" = [
              "<leader>"
              "g"
              "p"
            ];
            "commands" = [
              "editor.action.marker.previous"
            ];
          }
        ];
        # Indenting in visual mode
        "vim.visualModeKeyBindings" = [
          {
            "before" = [
              ">"
            ];
            "commands" = [
              "editor.action.indentLines"
            ];
          }
          {
            "before" = [
              "<"
            ];
            "commands" = [
              "editor.action.outdentLines"
            ];
          }
        ];
        # Dart
        "dart.checkForSdkUpdates" = false;
        "dart.lineLength" = 130;
        "[dart]" = {
          "editor.formatOnSave" = true;
          "editor.formatOnType" = true;
          "editor.rulers" = [
            130
          ];
          "editor.selectionHighlight" = false;
          "editor.suggestSelection" = "first";
          "editor.tabCompletion" = "onlySnippets";
          "editor.wordBasedSuggestions" = false;
        };
        "dart.showTodos" = false;

        "git.openRepositoryInParentFolders" = "never";
        # Better comments
        "better-comments.tags" = [
          {
            "tag" = "!";
            "color" = "#FF2D00";
            "strikethrough" = false;
            "underline" = false;
            "backgroundColor" = "transparent";
            "bold" = false;
            "italic" = false;
          }

          {
            "tag" = "?";
            "color" = "#3498DB";
            "strikethrough" = false;
            "underline" = false;
            "backgroundColor" = "transparent";
            "bold" = false;
            "italic" = false;
          }

          {
            "tag" = "//";
            "color" = "#474747";
            "strikethrough" = true;
            "underline" = false;
            "backgroundColor" = "transparent";
            "bold" = false;
            "italic" = false;
          }

          {
            "tag" = "todo";
            "color" = "#FF8C00";
            "strikethrough" = false;
            "underline" = false;
            "backgroundColor" = "transparent";
            "bold" = false;
            "italic" = false;
          }

          {
            "tag" = "*";
            "color" = "#98C379";
            "strikethrough" = false;
            "underline" = false;
            "backgroundColor" = "transparent";
            "bold" = false;
            "italic" = false;
          }
        ];
      };
    };
  };
}
