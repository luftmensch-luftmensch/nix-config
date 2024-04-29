{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.editors.vscode;
  inherit (config.valentino.modules) themes wayland;
  zoomLevel =
    if wayland.enable
    then 1.5
    else 0;
in {
  options.valentino.modules.editors.vscode.enable = mkEnableOption "vscode";

  config = mkIf cfg.enable {
    programs.vscode = {
      enable = true;
      package = pkgs.vscodium;
      extensions = with pkgs.vscode-extensions;
        [
          # Direnv support
          mkhl.direnv

          # Neovim + whichkey overlay
          asvetliakov.vscode-neovim
          vspacecode.whichkey

          # Todo tree view
          gruntfuggly.todo-tree

          # Language support
          dart-code.dart-code
          dart-code.flutter
          bbenoist.nix

          # Theming
          vscode-icons-team.vscode-icons
          viktorqvarfordt.vscode-pitch-black-theme
          bierner.docs-view
        ]
        ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
          {
            name = "better-comments";
            publisher = "aaron-bond";
            version = "3.0.2";
            sha256 = "sha256-hQmA8PWjf2Nd60v5EAuqqD8LIEu7slrNs8luc3ePgZc=";
          }

          {
            name = "toggle-excluded-files";
            publisher = "amodio";
            version = "2.0.0";
            sha256 = "sha256-eoab2rZSrIYAHFVIwojcnqZ59NbldJty8FqdkxzNlDc=";
          }
        ];

      keybindings = import ./keybindings.nix;
      userSettings = import ./settings.nix {
        inherit themes zoomLevel;
      };
    };

    home.file.".config/nvim-vscode/init.vim".text = ''
      " packadd quickscope

      " execute 'luafile ' . stdpath('config') . '/lua/settings.lua'
      function! s:manageEditorSize(...)
          let count = a:1
          let to = a:2
          for i in range(1, count ? count : 1)
              call VSCodeNotify(to == 'increase' ? 'workbench.action.increaseViewSize' : 'workbench.action.decreaseViewSize')
          endfor
      endfunction

      function! s:vscodeCommentary(...) abort
          if !a:0
              let &operatorfunc = matchstr(expand('<sfile>'), '[^. ]*$')
              return 'g@'
          elseif a:0 > 1
              let [line1, line2] = [a:1, a:2]
          else
              let [line1, line2] = [line("'["), line("']")]
          endif

          call VSCodeCallRange("editor.action.commentLine", line1, line2, 0)
      endfunction

      function! s:openVSCodeCommandsInVisualMode()
          normal! gv
          let visualmode = visualmode()
          if visualmode == "V"
              let startLine = line("v")
              let endLine = line(".")
              call VSCodeNotifyRange("workbench.action.showCommands", startLine, endLine, 1)
          else
              let startPos = getpos("v")
              let endPos = getpos(".")
              call VSCodeNotifyRangePos("workbench.action.showCommands", startPos[1], endPos[1], startPos[2], endPos[2], 1)
          endif
      endfunction

      function! s:openWhichKeyInVisualMode()
          normal! gv
          let visualmode = visualmode()
          if visualmode == "V"
              let startLine = line("v")
              let endLine = line(".")
              call VSCodeNotifyRange("whichkey.show", startLine, endLine, 1)
          else
              let startPos = getpos("v")
              let endPos = getpos(".")
              call VSCodeNotifyRangePos("whichkey.show", startPos[1], endPos[1], startPos[2], endPos[2], 1)
          endif
      endfunction

      " Better Navigation
      nnoremap <silent> <C-j> :call VSCodeNotify('workbench.action.navigateDown')<CR>
      xnoremap <silent> <C-j> :call VSCodeNotify('workbench.action.navigateDown')<CR>
      nnoremap <silent> <C-k> :call VSCodeNotify('workbench.action.navigateUp')<CR>
      xnoremap <silent> <C-k> :call VSCodeNotify('workbench.action.navigateUp')<CR>
      nnoremap <silent> <C-h> :call VSCodeNotify('workbench.action.navigateLeft')<CR>
      xnoremap <silent> <C-h> :call VSCodeNotify('workbench.action.navigateLeft')<CR>
      nnoremap <silent> <C-l> :call VSCodeNotify('workbench.action.navigateRight')<CR>
      xnoremap <silent> <C-l> :call VSCodeNotify('workbench.action.navigateRight')<CR>

      nnoremap gr <Cmd>call VSCodeNotify('editor.action.goToReferences')<CR>

      " Bind C-/ to vscode commentary since calling from vscode produces double comments due to multiple cursors
      xnoremap <expr> <C-/> <SID>vscodeCommentary()
      nnoremap <expr> <C-/> <SID>vscodeCommentary() . '_'

      nnoremap <silent> <C-w>_ :<C-u>call VSCodeNotify('workbench.action.toggleEditorWidths')<CR>

      nnoremap <silent> <Space> :call VSCodeNotify('whichkey.show')<CR>
      xnoremap <silent> <Space> :<C-u>call <SID>openWhichKeyInVisualMode()<CR>

      xnoremap <silent> <C-P> :<C-u>call <SID>openVSCodeCommandsInVisualMode()<CR>

      xmap gc  <Plug>VSCodeCommentary
      nmap gc  <Plug>VSCodeCommentary
      omap gc  <Plug>VSCodeCommentary
      nmap gcc <Plug>VSCodeCommentaryLine

      " Simulate same TAB behavior in VSCode
      nmap <Tab> :Tabnext<CR>
      nmap <S-Tab> :Tabprev<CR>

      set clipboard=unnamedplus
    '';
  };
}
