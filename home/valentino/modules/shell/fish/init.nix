{
  shellInit = ''
		set -l foreground ffffff
		set -l selection 7030af
		set -l comment 989898
		set -l red ff5f59
		set -l orange fec43f
		set -l yellow d0bc00
		set -l green 44bc44
		set -l purple b6a0ff
		set -l cyan 00d3d0
		set -l pink feacd0

		# Syntax Highlighting Colors
		set -g fish_color_normal $foreground
		set -g fish_color_command $cyan
		set -g fish_color_keyword $pink
		set -g fish_color_quote $yellow
		set -g fish_color_redirection $foreground
		set -g fish_color_end $orange
		set -g fish_color_error $red
		set -g fish_color_param $purple
		set -g fish_color_comment $comment
		set -g fish_color_selection --background=$selection
		set -g fish_color_search_match --background=$selection
		set -g fish_color_operator $green
		set -g fish_color_escape $pink
		set -g fish_color_autosuggestion $comment

		# Completion Pager Colors
		set -g fish_pager_color_progress $comment
		set -g fish_pager_color_prefix $cyan
		set -g fish_pager_color_completion $foreground
		set -g fish_pager_color_description $comment
		set -g fish_pager_color_selected_background --background=$selection

		set fish_greeting # disable greeting
		set __fish_git_prompt_show_informative_status 1

		set __fish_git_prompt_showdirtystate "yes"
		set __fish_git_prompt_showupstream "yes"

		set __fish_git_prompt_color_branch brown
		set __fish_git_prompt_color_dirtystate FCBC47
		set __fish_git_prompt_color_stagedstate yellow
		set __fish_git_prompt_color_upstream cyan
		set __fish_git_prompt_color_cleanstate green
		set __fish_git_prompt_color_invalidstate red

		set __fish_git_prompt_char_dirtystate "*"
		set __fish_git_prompt_char_stateseparator " "
		set __fish_git_prompt_char_untrackedfiles " …"
		set __fish_git_prompt_char_cleanstate "✓"
		set __fish_git_prompt_char_stagedstate "⇢ "
		set __fish_git_prompt_char_conflictedstate "✕"

		set __fish_git_prompt_char_upstream_prefix ""
		set __fish_git_prompt_char_upstream_ahead "⇡"
		set __fish_git_prompt_char_upstream_behind "⇣"
		set __fish_git_prompt_char_upstream_diverged "⇡⇣"

		set TERM xterm-256color

    # Functions needed for !! and !$
    function __history_previous_command
      switch (commandline -t)
      case "!"
        commandline -t $history[1]; commandline -f repaint
      case "*"
        commandline -i !
      end
    end

    function __history_previous_command_arguments
      switch (commandline -t)
      case "!"
        commandline -t ""
        commandline -f history-token-search-backward
      case "*"
        commandline -i '$'
      end
    end

    # The bindings for !! and !$
    if [ $fish_key_bindings = fish_vi_key_bindings ];
      bind -Minsert ! __history_previous_command
      bind -Minsert '$' __history_previous_command_arguments
    else
      bind ! __history_previous_command
      bind '$' __history_previous_command_arguments
    end
  '';
}
