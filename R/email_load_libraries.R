#
# 0_start.R
# set environment variables specifying path locations,
# load prerequistive libraries 
#

setwd("/Users/rraju/Repositories/git/personal/github/email_clustering")
projectDir = getwd()
VERBOSE=TRUE

codeDir = file.path(projectDir, 'R')
dataDir = file.path(projectDir, 'data')
outputDir = file.path(projectDir, 'output')

if (VERBOSE)
  print("Loading libraries and functions for project")

# Loading Libraries
library(twitteR)
library(plyr)
library(ggplot2)
library(textcat)
library(sqldf)
library(rjson)
library(RCurl)
library(RJSONIO)
library(plyr)
library(tm)
library(tm.plugin.mail)

source(file.path(codeDir, 'common_functions.R'))

if (VERBOSE)
  print("Completed loading libraries & functions")