{
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
      packages = [pkgs.jetbrains.idea-community];

      # https://github.com/JetBrains/ideavim
      file.".config/ideavim/ideavimrc".text = ''
        let mapleader = " "

        set clipboard+=unnamed

        nnoremap QQ :q!<CR>

        set incsearch
        set hlsearch
        set nu
        set relativenumber
        set showmode
        set so=7

        imap <C-e> <esc>$i<right>
        imap <C-a> <esc>0i

        nnoremap <c-h> <c-w>h
        nnoremap <c-l> <c-w>l
        nnoremap <c-j> <c-w>j
        nnoremap <c-k> <c-w>k

        nnoremap <leader>, :bNext<CR>
        nnoremap <leader>v :vsplit<CR>
        nnoremap <leader>h :split<CR>
        nnoremap <leader>x :action UnsplitAll<CR>
        nnoremap <leader><leader> :ls<CR>
        nnoremap <leader>z :bw<CR>

        nnoremap <Leader>. :action FindInPath<CR>

        noremap <Leader>rl :source ~/.config/ideavim/ideavimrc<CR>

        nnoremap <Leader>p :action SearchEverywhere<CR>
        nnoremap <Leader>t :action FileStructurePopup<CR>
        nnoremap <Leader>f :action GotoFile<CR>
        nnoremap <Leader>r :action RecentFiles<CR>
        nnoremap <Leader>u :action GotoTest<CR>
        nnoremap <Leader>b :action ToggleLineBreakpoint<CR>
        nnoremap <Leader>gn :action GotoNextError<CR>
        nnoremap <Leader>gp :action GotoPreviousError<CR>
        nnoremap <Leader>gR :action RenameElement<CR>
        nnoremap <Leader>go :action NextOccurence<CR>
        nnoremap <Leader>ge :action Generate<CR>

        "nnoremap <leader>c :action CommentByLineComment<CR>
        "nnoremap <leader># :action CommentByLineComment<CR>
        "nnoremap <leader>C :action CommentByBlockComment<CR>

        vnoremap <leader>c :action CommentByLineComment<CR>
        vnoremap <leader># :action CommentByLineComment<CR>
        vnoremap <leader>C :action CommentByBlockComment<CR>

        " CamelCase/snake_case word motions
        " nnoremap w [w
        " nnoremap b [b

        nnoremap zd :action CollapseDocComments<CR>
        nnoremap zD :action ExpandDocComments<CR>

        inoremap <C-s> <esc>/
        nnoremap <C-s> /

        nnoremap mm :tabnext<CR>
        nnoremap M :tabnew<CR>
        nnoremap <C-x> :noh<CR>
        " Easy visual indentation
        vnoremap < <gv
        vnoremap > >gv
      '';
    };
  };
}
