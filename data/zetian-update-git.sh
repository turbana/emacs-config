#!/usr/bin/env sh

COPY_TO=~/sync/org-roam-readonly

echo
echo starting $0 at `date`
set -ex

changes=`git status --short | wc -l`

# stash any local changes
if [ $changes -ne 0 ]; then
    git stash push --all --message "zetian-update-git"
fi

# pull from origin
git pull origin master

if [ $changes -ne 0 ]; then
    # restore stash
    git stash pop "stash@{0}"
    # stage files
    git add --all .
    # commit
    git commit --all -m "zetian auto update"
    # push to origin
    git push origin master
fi

# copy to destination
mkdir -p $COPY_TO
cp special/zetian-tasks.org $COPY_TO/
cp special/zetian-people.org $COPY_TO/
