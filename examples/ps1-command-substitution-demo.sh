#!/usr/bin/env gsh
# Demo: Command substitution in PS1

echo "=== PS1 Command Substitution Demo ==="
echo ""

# Define a shell function
get_git_branch() {
  git branch 2>/dev/null | grep '^\*' | sed 's/^* //'
}

echo "Example 1: Simple command"
export PS1='[\u@\h $(echo "inline-cmd")]$ '
echo "PS1='[\u@\h \$(echo \"inline-cmd\")]$ '"
echo "(Above prompt should show: [user@host inline-cmd]$ )"
echo ""

echo "Example 2: Date/time in prompt"
export PS1='[\t]$ '
echo "PS1='[\t]$ '"
echo "(Above prompt shows: [HH:MM:SS]$ )"
echo ""

echo "Example 3: Current directory basename"
export PS1='[\W]$ '
echo "PS1='[\W]$ '"
echo ""

echo "Example 4: git branch (if in a repo)"
export PS1='[\w$(get_git_branch)]$ '
echo "PS1='[\w\$(get_git_branch)]$ '"
echo "(Above prompt would show git branch if in a repo)"
echo ""

echo "Example 5: Colorful prompt with git"
export PS1='\[\033[32m\]\u@\h\[\033[0m\]:\[\033[34m\]\w\[\033[0m\]$(get_git_branch)\n$ '
echo 'PS1='\''\[\033[32m\]\u@\h\[\033[0m\]:\[\033[34m\]\w\[\033[0m\]$(get_git_branch)\n$ '\'''
echo "(Green user@host, blue path, git branch on first line, $ on second)"
echo ""

echo "=== Demo complete ==="
echo ""
echo "Your .bashrc PS1 with \$(get_git_branch) should now work in gsh!"
echo "Just define the get_git_branch function in your .bashrc or startup script."
