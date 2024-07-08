#!/bin/bash
chmod +x ~/Documents/ucsb/repos/mortality/src/workflow/workflow.sh

#-- parameters
wd=~/Documents/ucsb/repos/mortality
src=~/Documents/ucsb/repos/mortality/src

cd $wd

chmod 744 $src/poc/plot-data-summary.r #Use to make executable

$src/poc/clean-euromammal-data.r # clean raw EuroMammal data
