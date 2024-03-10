# Mini_Ocaml
Creating an ocaml interpreter in ocaml
First real project 
<h2>Important Git commands</h2>
<ul>
<li>git clone <repo dir>: downloads a remote (stored online) repository (project) onto your local computer (offline)</li>
<li>cd: helps you navigate directories (folders) and create new directories (cd ..: to go back one directory) (cd <name of dir>/: to go forward one dir)</li>
<li>dir: to list files in a dir </li>
<li>git config --list: lists your personal details. To change any of them globally (overrides any other settings) use git config --global <name of detail> <change></li>
<li>use git init after creating a directory to initialize the folder</li>
<li>use git status to check the status of your file whether it is synced with the online repo </li>
<li>use echo 'folder name' > filename to add a file to your repo</li>
<li>to update a file (add to the staging area)to be committed use git add <file></li>
<li>to discard changes use git restore. It removes all changes while git reset simply unstages the file <file></li>
<li>use git config --edit to open your repo in your default editor</li>
<li>use start <file> to open a file</li>
<li>use git diff to get a more detailed version of git status and q + enter to exit this 'mode' or git log mode. Use git log to see all commits for a repo</li>
<li>use git commit to save your changes (you've added to the staging area) officially to your repo. A shortcut is git commit -m "message"</li>
<li>To stage a file for deletion use git rm <file> and git reset to unstage anything</li>
<li>use git remote to show which remote repos you've accessed and changed (configured) and git remote -v to show where it is accessing the repo from and where it is pushing new commits to. (*To add a new folder use git remote add <repo name> url. (advanced)*) to remove folders use git remote remove <foldername></li>
<li>use git fetch <repo name> to get data from a folder in the same url without updating the local repo. git pull combines git fetch and git merge</li>
<li>to push any commits online, use git push <reponame> <branchname></li>
<li>use git branch <name> to create a new branch and git checkout <name> to switch branches. use git merge <name> to merge a branch with main (master)</li>
<li>use git log just like git status. It should be less detailed than git diff but more detailed than git status(maybe)</li>
<li>use git checkout to switch between branches</li>
<li>other commands include git stash(probably advanced for now)</li>
</ul>
go to this <a href = "https://www.loginradius.com/blog/engineering/git-commands/">link</a> for more info
