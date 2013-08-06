#!/bin/bash

##############################################################################
# Name: templateR.sh 
# Author: Frank Farach
# Description: Script to clone templateR repo from  GitHub account, 
# with options to name the local working directory and RStudio
# project. This depends on the existence of a repository called templateR
# in the GitHub account of user named in the global variable USER below.
# See: http://github.com/frankfarach/templateR
#
# Usage: 
# ./templateR.sh <project name (optional)> <GitHub user name (optional)>
#
# Unless otherwise modified, default project name = NewProject
# and default user name = frankfarach.
##############################################################################

# GitHub user name where templateR repo resides
# If you've forked templateR, replace below with your own GitHub user account
USER=frankfarach

# Use 2nd argument as user name if supplied
if [ -n "$2" ]
  then
    USER=$2
fi

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
