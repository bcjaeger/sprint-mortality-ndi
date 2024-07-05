## library() calls go here
library(conflicted)
library(dotenv)
library(targets)
library(tarchetypes)

library(ggsci)
library(tidyverse)
library(survival)
library(splines)
library(Greg)           # for timeSplitter
library(timereg)
library(multcomp)
library(riskRegression) # for Score
library(prodlim)
library(viridis)
library(table.glue)
library(flextable)
library(survminer)
library(cmprsk)
library(officer)
library(cowplot)
library(gtsummary)
library(magrittr)
library(grid)
library(glue)
library(broom)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("border", "flextable")
conflict_prefer("font", "flextable")
conflict_prefer("rotate", "flextable")
conflict_prefer("get_legend", "cowplot")
conflict_prefer("as_flextable", "flextable")


library(rmarkdown)
