[
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
]
