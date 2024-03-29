#!/usr/bin/env bash
set -e

# Check that the user has passed in exactly 2 arguments, the AUTHOR and the COMMIT.
# If they number of arguments is wrong, print out a usage message.
if [ $# -lt 1 ] || [ $# -gt 2 ]; then
  >&2 echo "Usage: $(basename "$0") <author> [commit]"
  exit 1
fi

INSULTS=(
    "You're officially an asshole."
    "Great job."
    "You dirty animal."
    "They owe you one."
    "They'll never live this down."
)

INDEX=$((RANDOM % ${#INSULTS[@]}))


# Assign the author input to the author variable.
# Author must be formatted like:  John Smith <johnsmith@example.com>
AUTHOR=${1:?}

# Use perl to find the author name.
# This is using a regex capture group.
# Ex: John Smith
AUTHOR_NAME=$(echo "${AUTHOR}" | perl -wlne '/^([^<]+).*>$/ and print $1')

# Use perl to find the author email.
# This is using a regex capture group.
# Ex: johnsmith@example.com
AUTHOR_EMAIL=$(echo "$AUTHOR" | perl -wlne '/^.*\s*<(.*)>$/ and print $1')

# Gets the short version of the commit hash
# This is needed below so that we can replace the correct commit
# when editing.
# Ex: b3d501f
COMMIT=$(git rev-parse --short "${2:-HEAD}")

{
    # Sets the editor to use for updating the sequence of commits.
    # Normally the sequence editor is your editor (ex: vim).
    # Here we are using a command that will change pick to edit.
    # This chooses the right commit for the rebase.
    GIT_SEQUENCE_EDITOR="sed -i -e 's/^pick $COMMIT/edit $COMMIT/'" git rebase -i "$COMMIT"~1^^
    # Set the committer name and email, AND the author.
    # Git tracks both committer and author as they could be different people.
    # Then we amend the commit in question with the new author and committer.
    GIT_COMMITTER_NAME="$AUTHOR_NAME" GIT_COMMITTER_EMAIL="$AUTHOR_EMAIL" git commit --amend --no-edit --author="$AUTHOR"

    # Finish up the rebase
    git rebase --continue
} &> /dev/null

# Success message.
echo "$AUTHOR_NAME is now the author of $COMMIT. ${INSULTS[$INDEX]}";

