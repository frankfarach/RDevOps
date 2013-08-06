#!/bin/bash
# Name: templateR.sh 
# Author: Frank Farach
# Description: Script to clone templateR repo from user's
# GitHub account, with option to name the local working directory and RStudio
# project. This depends on the existence of a repository called templateR
# in the GitHub account of user named in the global variable USER below.

# GitHub user name where templateR repo resides
USER=frankfarach

# Name of project, if specified as first argument
PROJ=$1

# Exit if named project already exists in current directory
if [ -d "$PROJ" ]
  then
    echo "Directory named $PROJ already exists. Aborted."
    exit 0
fi

# Use default project name if none is supplied
if [ -z "$1" ]
  then
    echo "No project name supplied as first argument."
    echo "Using default project name, NewProj."
    PROJ=NewProject
fi

# Clone templateR repo from user's GitHub account
# setting the local copy's name to the first argument
git clone https://github.com/$USER/templateR $PROJ
echo "Successfully cloned $PROJ repository from ${USER}'s GitHub account."

# Change the name of the RStudio project file
mv $PROJ/NewProject.Rproj $PROJ/${PROJ}.Rproj

echo "Done!"
