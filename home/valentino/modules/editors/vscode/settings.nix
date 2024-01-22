{
  theme,
  zoomLevel ? 0,
}: {
  keybindings = [
    # Show explorer
    {
      key = "alt+e";
      command = "workbench.view.explorer";
    }

    {
      key = "ctrl+shift+e";
      command = "workbench.action.toggleSidebarVisibility";
    }

    # Quick Open
    {
      key = "alt+p";
      command = "workbench.action.quickOpen";
    }

    {
      key = "ctrl+e";
      command = "-actions.findWithSelection";
    }

    {
      key = "r";
      command = "renameFile";
      when = "explorerViewletVisible && filesExplorerFocus && !explorerResourceIsRoot && !explorerResourceReadonly && !inputFocus";
    }

    {
      key = "enter";
      command = "-renameFile";
      when = "explorerViewletVisible && filesExplorerFocus && !explorerResourceIsRoot && !explorerResourceReadonly && !inputFocus";
    }

    {
      key = "j";
      command = "list.focusDown";
      when = "explorerViewletVisible && filesExplorerFocus && !explorerResourceIsRoot && !explorerResourceReadonly && !inputFocus";
    }

    {
      key = "k";
      command = "list.focusUp";
      when = "explorerViewletVisible && filesExplorerFocus && !explorerResourceIsRoot && !explorerResourceReadonly && !inputFocus";
    }

    # Code Suggestion
    {
      key = "ctrl+j";
      command = "selectNextSuggestion";
      when = "editorTextFocus && suggestWidgetMultipleSuggestions && suggestWidgetVisible";
    }

    {
      key = "ctrl+k";
      command = "selectPrevSuggestion";
      when = "editorTextFocus && suggestWidgetMultipleSuggestions && suggestWidgetVisible";
    }

    {
      key = "tab";
      command = "selectNextSuggestion";
      when = "editorTextFocus && suggestWidgetMultipleSuggestions && suggestWidgetVisible";
    }

    {
      key = "shift+tab";
      command = "selectPrevSuggestion";
      when = "editorTextFocus && suggestWidgetMultipleSuggestions && suggestWidgetVisible";
    }

    # Quick Open Navigation
    {
      key = "ctrl+j";
      command = "workbench.action.quickOpenNavigateNext";
      when = "inQuickOpen";
    }

    {
      key = "tab";
      command = "workbench.action.quickOpenNavigateNext";
      when = "inQuickOpen";
    }

    {
      key = "shift+tab";
      command = "workbench.action.quickOpenNavigatePrevious";
      when = "inQuickOpen";
    }

    {
      key = "ctrl+k";
      command = "workbench.action.quickOpenNavigatePrevious";
      when = "inQuickOpen";
    }

    # File tree navigation & actions
    {
      key = "enter";
      command = "list.select";
      when = "explorerViewletVisible && filesExplorerFocus";
    }

    {
      key = "l";
      command = "list.select";
      when = "explorerViewletVisible && filesExplorerFocus && !inputFocus";
    }

    {
      key = "o";
      command = "list.toggleExpand";
      when = "explorerViewletVisible && filesExplorerFocus && !inputFocus";
    }

    {
      key = "h";
      command = "list.collapse";
      when = "explorerViewletVisible && filesExplorerFocus && !inputFocus";
    }

    {
      key = "a";
      command = "explorer.newFile";
      when = "filesExplorerFocus && !inputFocus";
    }

    {
      key = "shift+a";
      command = "explorer.newFolder";
      when = "filesExplorerFocus && !inputFocus";
    }

    {
      key = "shift+;";
      command = "insertPrevSuggestion";
      when = "hasOtherSuggestions && textInputFocus && textInputFocus && !inSnippetMode && !suggestWidgetVisible && config.editor.tabCompletion == 'on'";
    }

    # Switch between sidebard/terminal and active editor
    {
      key = "ctrl+l";
      command = "workbench.action.focusActiveEditorGroup";
      when = "sideBarFocus";
    }

    {
      key = "ctrl+k";
      command = "workbench.action.focusActiveEditorGroup";
      when = "terminalFocus";
    }

    {
      key = "ctrl+shift+t";
      command = "workbench.action.terminal.focus";
      when = "!terminalFocus";
    }

    {
      key = "ctrl+j";
      command = "-editor.action.insertLineAfter";
      when = "editorTextFocus && neovim.ctrlKeysInsert && !neovim.recording && neovim.mode == 'insert'";
    }

    {
      key = "alt+j";
      command = "workbench.action.terminal.focus";
      when = "!terminalFocus";
    }

    {
      key = "ctrl+shift+t";
      command = "workbench.action.togglePanel";
    }

    {
      key = "ctrl+j";
      command = "-workbench.action.togglePanel";
    }

    {
      key = "shift+k";
      command = "editor.action.showHover";
      when = "editorTextFocus && neovim.mode != 'insert'";
    }

    {
      key = "ctrl+k ctrl+i";
      command = "-editor.action.showHover";
      when = "editorTextFocus";
    }

    {
      key = "shift+tab";
      command = "-acceptAlternativeSelectedSuggestion";
      when = "suggestWidgetVisible && textInputFocus && textInputFocus";
    }

    {
      key = "ctrl+f";
      command = "-vscode-neovim.ctrl-f";
      when = "editorTextFocus && neovim.ctrlKeysNormal && neovim.init && neovim.mode != 'insert'";
    }

    {
      key = "space";
      command = "whichkey.show";
      when = "neovim.mode != 'insert' && !inputFocus";
    }
  ];

  userSettings = {
    "window.menuBarVisibility" = "toggle";
    "window.zoomLevel" = zoomLevel;
    "window.title" = "VSCode \${separator} \${activeEditorShort}";
    "window.commandCenter" = false;
    "workbench.iconTheme" = "vscode-icons";
    "workbench.colorCustomizations" = {
      "statusBar.background" = "#005f5f";
      "statusBar.noFolderBackground" = "#005f5f";
      "statusBar.debuggingBackground" = "#cc241d";
    };

    "editor.cursorBlinking" = "smooth";
    "editor.renderWhitespace" = "boundary";
    "editor.bracketPairColorization.enabled" = true;
    "editor.guides.bracketPairs" = true;
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
    "terminal.integrated.fontFamily" = "'${theme.font.regular.family}', 'monospace', monospace";

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
    "files.trimTrailingWhitespace" = true;

    # Neovim support
    "extensions.experimental.affinity" = {
      "asvetliakov.vscode-neovim" = 1;
    };
    "vscode-neovim.neovimInitVimPaths.linux" = "$HOME/.config/nvim-vscode/init.vim";

    # Whichkey configuration
    "whichkey.sortOrder" = "alphabetically";
    "whichkey.delay" = 0;
    "whichkey.bindings" = [
      # Show command  -> Quick Open View commands
      {
        key = ";";
        name = "commands";
        type = "command";
        command = "workbench.action.showCommands";
      }

      {
        key = "q";
        name = "Close file";
        type = "command";
        command = "workbench.action.closeActiveEditor";
      }

      {
        key = "z";
        name = "Close file";
        type = "command";
        command = "workbench.action.closeActiveEditor";
      }

      # Comment line
      {
        key = "/";
        name = "comment";
        type = "command";
        command = "vscode-neovim.send";
        args = "<C-/>";
      }

      {
        key = "?";
        name = "View All References";
        type = "command";
        command = "references-view.find";
        when = "editorHasReferenceProvider";
      }

      # Buffers
      {
        key = "b";
        name = "Buffers/Editors...";
        type = "bindings";
        bindings = [
          {
            key = "b";
            name = "Show all buffers/editors";
            type = "command";
            command = "workbench.action.showAllEditors";
          }
          {
            key = "d";
            name = "Close active editor";
            type = "command";
            command = "workbench.action.closeActiveEditor";
          }
          {
            key = "h";
            name = "Move editor into left group";
            type = "command";
            command = "workbench.action.moveEditorToLeftGroup";
          }
          {
            key = "j";
            name = "Move editor into below group";
            type = "command";
            command = "workbench.action.moveEditorToBelowGroup";
          }
          {
            key = "k";
            name = "Move editor into above group";
            type = "command";
            command = "workbench.action.moveEditorToAboveGroup";
          }
          {
            key = "l";
            name = "Move editor into right group";
            type = "command";
            command = "workbench.action.moveEditorToRightGroup";
          }
          {
            key = "m";
            name = "Close other editors";
            type = "command";
            command = "workbench.action.closeOtherEditors";
          }
          {
            key = "n";
            name = "Next editor";
            type = "command";
            command = "workbench.action.nextEditor";
          }
          {
            key = "p";
            name = "Previous editor";
            type = "command";
            command = "workbench.action.previousEditor";
          }
          {
            key = "N";
            name = "New untitled editor";
            type = "command";
            command = "workbench.action.files.newUntitledFile";
          }
          {
            key = "u";
            name = "Reopen closed editor";
            type = "command";
            command = "workbench.action.reopenClosedEditor";
          }
          {
            key = "y";
            name = "Copy buffer to clipboard";
            type = "commands";
            commands = [
              "editor.action.selectAll"
              "editor.action.clipboardCopyAction"
              "cancelSelection"
            ];
          }
        ];
      }

      # Debugging
      # {
      #   key = "d";
      #   name = "Debug...";
      #   type = "bindings";
      #   bindings = [
      #     {
      #       key = "d";
      #       name = "Start debug";
      #       type = "command";
      #       command = "workbench.action.debug.start";
      #     }
      #     {
      #       key = "S";
      #       name = "Stop debug";
      #       type = "command";
      #       command = "workbench.action.debug.stop";
      #     }
      #     {
      #       key = "c";
      #       name = "Continue debug";
      #       type = "command";
      #       command = "workbench.action.debug.continue";
      #     }
      #     {
      #       key = "p";
      #       name = "Pause debug";
      #       type = "command";
      #       command = "workbench.action.debug.pause";
      #     }
      #     {
      #       key = "r";
      #       name = "Run without debugging";
      #       type = "command";
      #       command = "workbench.action.debug.run";
      #     }
      #     {
      #       key = "R";
      #       name = "Restart ebug";
      #       type = "command";
      #       command = "workbench.action.debug.restart";
      #     }
      #     {
      #       key = "i";
      #       name = "Step into";
      #       type = "command";
      #       command = "workbench.action.debug.stepInto";
      #     }
      #     {
      #       key = "s";
      #       name = "Step over";
      #       type = "command";
      #       command = "workbench.action.debug.stepOver";
      #     }
      #     {
      #       key = "o";
      #       name = "Step out";
      #       type = "command";
      #       command = "workbench.action.debug.stepOut";
      #     }
      #     {
      #       key = "b";
      #       name = "Toggle breakpoint";
      #       type = "command";
      #       command = "editor.debug.action.toggleBreakpoint";
      #     }
      #     {
      #       key = "B";
      #       name = "Toggle inline breakpoint";
      #       type = "command";
      #       command = "editor.debug.action.toggleInlineBreakpoint";
      #     }
      #     {
      #       key = "j";
      #       name = "Jump to cursor";
      #       type = "command";
      #       command = "debug.jumpToCursor";
      #     }
      #     {
      #       key = "v";
      #       name = "REPL";
      #       type = "command";
      #       command = "workbench.debug.action.toggleRepl";
      #     }
      #     {
      #       key = "w";
      #       name = "Focus on watch window";
      #       type = "command";
      #       command = "workbench.debug.action.focusWatchView";
      #     }
      #     {
      #       key = "W";
      #       name = "Add to watch";
      #       type = "command";
      #       command = "editor.debug.action.selectionToWatch";
      #     }
      #   ];
      # }

      {
        key = "e";
        name = "Toggle Explorer";
        type = "command";
        command = "workbench.action.toggleSidebarVisibility";
      }

      # Find
      {
        key = "f";
        name = "Find & Replace...";
        type = "bindings";
        bindings = [
          {
            key = "f";
            name = "File";
            type = "command";
            command = "editor.action.startFindReplaceAction";
          }
          {
            key = "s";
            name = "Symbol";
            type = "command";
            command = "editor.action.rename";
            when = "editorHasRenameProvider && editorTextFocus && !editorReadonly";
          }
          {
            key = "p";
            name = "Project";
            type = "command";
            command = "workbench.action.replaceInFiles";
          }
        ];
      }

      # Git support
      {
        key = "G";
        name = "Git...";
        type = "bindings";
        bindings = [
          {
            key = "/";
            name = "Search Commits";
            command = "gitlens.showCommitSearch";
            type = "command";
            when = "gitlens:enabled && config.gitlens.keymap == 'alternate'";
          }
          {
            key = "a";
            name = "Stage";
            type = "command";
            command = "git.stage";
          }
          {
            key = "b";
            name = "Checkout";
            type = "command";
            command = "git.checkout";
          }
          {
            key = "B";
            name = "Browse";
            type = "command";
            command = "gitlens.openFileInRemote";
          }
          {
            key = "c";
            name = "Commit";
            type = "command";
            command = "git.commit";
          }
          {
            key = "C";
            name = "Cherry Pick";
            type = "command";
            command = "gitlens.views.cherryPick";
          }
          {
            key = "d";
            name = "Delete Branch";
            type = "command";
            command = "git.deleteBranch";
          }
          {
            key = "f";
            name = "Fetch";
            type = "command";
            command = "git.fetch";
          }
          {
            key = "F";
            name = "Pull From";
            type = "command";
            command = "git.pullFrom";
          }
          {
            key = "g";
            name = "Graph";
            type = "command";
            command = "git-graph.view";
          }
          {
            key = "h";
            name = "Heatmap";
            type = "command";
            command = "gitlens.toggleFileHeatmap";
          }
          {
            key = "H";
            name = "History";
            type = "command";
            command = "git.viewFileHistory";
          }
          {
            key = "i";
            name = "Init";
            type = "command";
            command = "git.init";
          }
          {
            key = "j";
            name = "Next Change";
            type = "command";
            command = "workbench.action.editor.nextChange";
          }
          {
            key = "k";
            name = "Previous Change";
            type = "command";
            command = "workbench.action.editor.previousChange";
          }
          {
            key = "l";
            name = "Toggle Line Blame";
            type = "command";
            command = "gitlens.toggleLineBlame";
            when = "editorTextFocus && gitlens:canToggleCodeLens && gitlens:enabled && config.gitlens.keymap == 'alternate'";
          }
          {
            key = "L";
            name = "Toggle GitLens";
            type = "command";
            command = "gitlens.toggleCodeLens";
            when = "editorTextFocus && gitlens:canToggleCodeLens && gitlens:enabled && config.gitlens.keymap == 'alternate'";
          }
          {
            key = "m";
            name = "Merge";
            type = "command";
            command = "git.merge";
          }
          {
            key = "p";
            name = "Push";
            type = "command";
            command = "git.push";
          }
          {
            key = "P";
            name = "Pull";
            type = "command";
            command = "git.pull";
          }
          {
            key = "s";
            name = "Stash";
            type = "command";
            command = "workbench.view.scm";
          }
          {
            key = "S";
            name = "Status";
            type = "command";
            command = "gitlens.showQuickRepoStatus";
            when = "gitlens:enabled && config.gitlens.keymap == 'alternate'";
          }
          {
            key = "t";
            name = "Create Tag";
            type = "command";
            command = "git.createTag";
          }
          {
            key = "T";
            name = "Delete Tag";
            type = "command";
            command = "git.deleteTag";
          }
          {
            key = "U";
            name = "Unstage";
            type = "command";
            command = "git.unstage";
          }
        ];
      }

      # Splitting
      {
        key = "h";
        name = "Split Horizontal";
        type = "command";
        command = "workbench.action.splitEditorDown";
      }
      {
        key = "v";
        name = "Split Vertical";
        type = "command";
        command = "workbench.action.splitEditor";
      }

      {
        key = "i";
        name = "Insert...";
        type = "bindings";
        bindings = [
          {
            key = "j";
            name = "Insert line below";
            type = "command";
            command = "editor.action.insertLineAfter";
          }
          {
            key = "k";
            name = "Insert line above";
            type = "command";
            command = "editor.action.insertLineBefore";
          }
          {
            key = "s";
            name = "Insert snippet";
            type = "command";
            command = "editor.action.insertSnippet";
          }
        ];
      }

      # LSP support
      {
        key = "g";
        name = "LSP...";
        type = "bindings";
        bindings = [
          {
            key = ";";
            name = "Refactor";
            type = "command";
            command = "editor.action.refactor";
            when = "editorHasCodeActionsProvider && editorTextFocus && !editorReadonly";
          }
          {
            key = "a";
            name = "Auto Fix";
            type = "command";
            command = "editor.action.autoFix";
            when = "editorTextFocus && !editorReadonly && supportedCodeAction =~ /(\\s|^)quickfix\\b/";
          }
          {
            key = "d";
            name = "Definition";
            type = "command";
            command = "editor.action.revealDefinition";
            when = "editorHasDefinitionProvider && editorTextFocus && !isInEmbeddedEditor";
          }
          {
            key = "D";
            name = "Declaration";
            type = "command";
            command = "editor.action.revealDeclaration";
          }
          {
            key = "e";
            name = "Errors";
            type = "command";
            command = "workbench.actions.view.problems";
          }
          {
            key = "f";
            name = "Format";
            type = "command";
            command = "editor.action.formatDocument";
            when = "editorHasDocumentFormattingProvider && editorHasDocumentFormattingProvider && editorTextFocus && !editorReadonly && !inCompositeEditor";
          }
          {
            key = "i";
            name = "Implementation";
            type = "command";
            command = "editor.action.goToImplementation";
            when = "editorHasImplementationProvider && editorTextFocus && !isInEmbeddedEditor";
          }
          {
            key = "l";
            name = "Code Lens";
            type = "command";
            command = "codelens.showLensesInCurrentLine";
          }
          {
            key = "n";
            name = "Next Problem";
            type = "command";
            command = "editor.action.marker.next";
            when = "editorFocus";
          }
          {
            key = "N";
            name = "Next Problem (Proj)";
            type = "command";
            command = "editor.action.marker.nextInFiles";
            when = "editorFocus";
          }
          {
            key = "o";
            name = "Outline";
            type = "command";
            command = "outline.focus";
          }
          {
            key = "p";
            name = "Prev Problem";
            type = "command";
            command = "editor.action.marker.prevInFiles";
            when = "editorFocus";
          }
          {
            key = "P";
            name = "Prev Problem (Proj)";
            type = "command";
            command = "editor.action.marker.prev";
            when = "editorFocus";
          }
          {
            key = "q";
            name = "Quick Fix";
            type = "command";
            command = "editor.action.quickFix";
            when = "editorHasCodeActionsProvider && editorTextFocus && !editorReadonly";
          }
          {
            key = "r";
            name = "References";
            type = "command";
            command = "editor.action.goToReferences";
            when = "editorHasReferenceProvider && editorTextFocus && !inReferenceSearchEditor && !isInEmbeddedEditor";
          }
          {
            key = "R";
            name = "Rename";
            type = "command";
            command = "editor.action.rename";
            when = "editorHasRenameProvider && editorTextFocus && !editorReadonly";
          }
          {
            key = "v";
            name = "View All References";
            type = "command";
            command = "references-view.find";
            when = "editorHasReferenceProvider";
          }
          {
            key = "s";
            name = "Go To Symbol";
            type = "command";
            command = "workbench.action.gotoSymbol";
          }
          {
            key = "S";
            name = "Show All Symbols";
            type = "command";
            command = "workbench.action.showAllSymbols";
          }
        ];
      }

      {
        key = "m";
        name = "Mark...";
        type = "bindings";
        bindings = [
          {
            key = "c";
            name = "Clear Bookmarks";
            type = "command";
            command = "bookmarks.clear";
          }
          {
            key = "j";
            name = "Next Bookmark";
            type = "command";
            command = "bookmarks.jumpToNext";
            when = "editorTextFocus";
          }
          {
            key = "k";
            name = "Previous Bookmark";
            type = "command";
            command = "bookmarks.jumpToPrevious";
            when = "editorTextFocus";
          }
          {
            key = "l";
            name = "List Bookmarks";
            type = "command";
            command = "bookmarks.listFromAllFiles";
            when = "editorTextFocus";
          }
          {
            key = "r";
            name = "Refresh Bookmarks";
            type = "command";
            command = "bookmarks.refresh";
          }
          {
            key = "t";
            name = "Toggle Bookmark";
            type = "command";
            command = "bookmarks.toggle";
            when = "editorTextFocus";
          }
          {
            key = "s";
            name = "Show Bookmarks";
            type = "command";
            command = "workbench.view.extension.bookmarks";
          }
        ];
      }

      {
        key = "n";
        name = "No Highlight";
        type = "command";
        command = "vscode-neovim.send";
        args = ":noh<CR>";
      }

      # Open files & directories
      {
        key = "o";
        name = "Open...";
        type = "bindings";
        bindings = [
          {
            key = "d";
            name = "Directory";
            type = "command";
            command = "workbench.action.files.openFolder";
          }
          {
            key = "r";
            name = "Recent";
            type = "command";
            command = "workbench.action.openRecent";
          }
          {
            key = "f";
            name = "File";
            type = "command";
            command = "workbench.action.files.openFile";
          }
        ];
      }

      {
        key = "p";
        name = "Peek...";
        type = "bindings";
        bindings = [
          {
            key = "d";
            name = "Definition";
            type = "command";
            command = "editor.action.peekDefinition";
            when = "editorHasDefinitionProvider && editorTextFocus && !inReferenceSearchEditor && !isInEmbeddedEditor";
          }
          {
            key = "D";
            name = "Declaration";
            type = "command";
            command = "editor.action.peekDeclaration";
          }
          {
            key = "i";
            name = "Implementation";
            type = "command";
            command = "editor.action.peekImplementation";
            when = "editorHasImplementationProvider && editorTextFocus && !inReferenceSearchEditor && !isInEmbeddedEditor";
          }
          {
            key = "p";
            name = "Toggle Focus";
            type = "command";
            command = "togglePeekWidgetFocus";
            when = "inReferenceSearchEditor || referenceSearchVisible";
          }
          {
            key = "r";
            name = "References";
            type = "command";
            command = "editor.action.referenceSearch.trigger";
          }
          {
            key = "t";
            name = "Type Definition";
            type = "command";
            command = "editor.action.peekTypeDefinition";
          }
        ];
      }

      {
        key = "s";
        name = "Search...";
        type = "bindings";
        bindings = [
          {
            key = "f";
            name = "Files";
            type = "command";
            command = "workbench.action.quickOpen";
          }
          {
            key = "t";
            name = "Text";
            type = "command";
            command = "workbench.action.findInFiles";
          }
        ];
      }

      {
        key = "S";
        name = "Show...";
        type = "bindings";
        bindings = [
          {
            key = "e";
            name = "Show explorer";
            type = "command";
            command = "workbench.view.explorer";
          }
          {
            key = "s";
            name = "Show search";
            type = "command";
            command = "workbench.view.search";
          }
          {
            key = "g";
            name = "Show source control";
            type = "command";
            command = "workbench.view.scm";
          }
          {
            key = "t";
            name = "Show test";
            type = "command";
            command = "workbench.view.extension.test";
          }
          {
            key = "r";
            name = "Show remote explorer";
            type = "command";
            command = "workbench.view.remote";
          }
          {
            key = "x";
            name = "Show extensions";
            type = "command";
            command = "workbench.view.extensions";
          }
          {
            key = "p";
            name = "Show problem";
            type = "command";
            command = "workbench.actions.view.problems";
          }
          {
            key = "o";
            name = "Show output";
            type = "command";
            command = "workbench.action.output.toggleOutput";
          }
          {
            key = "d";
            name = "Show debug console";
            type = "command";
            command = "workbench.debug.action.toggleRepl";
          }
        ];
      }

      # Terminal
      {
        key = "t";
        name = "Terminal...";
        type = "bindings";
        bindings = [
          {
            key = "t";
            name = "Toggle Terminal";
            type = "command";
            command = "workbench.action.togglePanel";
          }
          {
            key = "T";
            name = "Focus Terminal";
            type = "command";
            command = "workbench.action.terminal.toggleTerminal";
            when = "!terminalFocus";
          }
        ];
      }

      {
        key = "u";
        name = "UI toggles...";
        type = "bindings";
        bindings = [
          {
            key = "b";
            name = "Toggle side bar visibility";
            type = "command";
            command = "workbench.action.toggleSidebarVisibility";
          }
          {
            key = "j";
            name = "Toggle panel visibility";
            type = "command";
            command = "workbench.action.togglePanel";
          }
          {
            key = "f";
            name = "Toggle full screen";
            type = "command";
            command = "workbench.action.toggleFullScreen";
          }
          {
            key = "s";
            name = "Select theme";
            type = "command";
            command = "workbench.action.selectTheme";
          }
          {
            key = "m";
            name = "Toggle maximized panel";
            type = "command";
            command = "workbench.action.toggleMaximizedPanel";
          }
        ];
      }

      # Windows
      {
        key = "w";
        name = "Window...";
        type = "bindings";
        bindings = [
          {
            key = "W";
            name = "Focus previous editor group";
            type = "command";
            command = "workbench.action.focusPreviousGroup";
          }
          {
            key = "h";
            name = "Move editor group left";
            type = "command";
            command = "workbench.action.moveActiveEditorGroupLeft";
          }
          {
            key = "j";
            name = "Move editor group down";
            type = "command";
            command = "workbench.action.moveActiveEditorGroupDown";
          }
          {
            key = "k";
            name = "Move editor group up";
            type = "command";
            command = "workbench.action.moveActiveEditorGroupUp";
          }
          {
            key = "l";
            name = "Move editor group right";
            type = "command";
            command = "workbench.action.moveActiveEditorGroupRight";
          }
          {
            key = "t";
            name = "Toggle editor group sizes";
            type = "command";
            command = "workbench.action.toggleEditorWidths";
          }
          {
            key = "m";
            name = "Maximize editor group";
            type = "command";
            command = "workbench.action.minimizeOtherEditors";
          }
          {
            key = "M";
            name = "Maximize editor group and hide side bar";
            type = "command";
            command = "workbench.action.maximizeEditor";
          }
          {
            key = "=";
            name = "Reset editor group sizes";
            type = "command";
            command = "workbench.action.evenEditorWidths";
          }
          {
            key = "z";
            name = "Combine all editors";
            type = "command";
            command = "workbench.action.joinAllGroups";
          }
          {
            key = "d";
            name = "Close editor group";
            type = "command";
            command = "workbench.action.closeEditorsInGroup";
          }
          {
            key = "x";
            name = "Close all editor groups";
            type = "command";
            command = "workbench.action.closeAllGroups";
          }
        ];
      }
    ];

    # Dart
    "dart.checkForSdkUpdates" = false;
    "dart.lineLength" = 150;
    "[dart]" = {
      "editor.formatOnSave" = true;
      "editor.formatOnType" = true;
      "editor.rulers" = [
        150
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
}
