# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(statgsa)

# Dependencias Obrigatorias
library(stats)
library(rlang)
library(dplyr)
library(janitor)
library(effsize)
library(caret)
library(DescTools)
library(car)
library(pROC)
library(MuMIn)

# Outras dependencias
library(magrittr)
library(tidyr)

test_check("statgsa")
