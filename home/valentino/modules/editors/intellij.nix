{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.editors.intellij;
in {
  options.valentino.modules.editors.intellij = {
    enable = mkEnableOption "Intellij";
  };

  config = mkIf cfg.enable {
    home = {
      packages = with pkgs.jetbrains; [
        idea-community
      ];

      # https://github.com/JetBrains/ideavim
      file.".config/ideavim/ideavimrc" = {
        enable = true;
        text = ''
          let mapleader = " "

          set clipboard+=unnamed

          nnoremap QQ :q!<CR>

          set incsearch
          set hlsearch
          set relativenumber

          imap <C-e> <esc>$i<right>
          imap <C-a> <esc>0i

          nnoremap <leader>, :bNext<CR>
          nnoremap <leader>v :vsplit<CR>
          nnoremap <leader>h :split<CR>
          nnoremap <leader><leader> :ls<CR>
          nnoremap <leader>z :bw<CR>

          nnoremap <Leader>. :action FindInPath<CR>

          noremap <Leader>rl :source ~/.config/ideavim/ideavimrc<CR>

          nnoremap <Leader>p :action SearchEverywhere<CR>
          nnoremap <Leader>c :action GotoClass<CR>
          nnoremap <Leader>t :action FileStructurePopup<CR>
          nnoremap <Leader>f :action GotoFile<CR>
          nnoremap <Leader>r :action RecentFiles<CR>
          nnoremap <Leader>u :action GotoTest<CR>

          nnoremap zd :action CollapseDocComments<CR>
          nnoremap zD :action ExpandDocComments<CR>

          inoremap <C-s> <esc>/
          nnoremap <C-s> /

          nnoremap mm :tabnext<CR>
          nnoremap M :tabnew<CR>
          nnoremap <C-x> :noh<CR>
        '';
      };
    };
  };
}
