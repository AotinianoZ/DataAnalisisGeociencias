# Load libraries for the NADA online course 
loadlibs <- function(x =0){
  require(fitdistrplus)
  require(Kendall)
  require(multcomp)
  require(NADA)
  require(perm)
  require (survminer)
  #   additional packages
  require(car)
  require(bestglm)
  require(rms)
  require(vegan) 
  require (cenGAM)
  require (mgcv)
  require(nlme)
  require(coin)
  require(NADA2)
  require(EnvStats)
  
  require(tidyverse)
  require(readxl)
  require(psych)
  require(plotly)
  require(ggrepel)
  require(gridExtra)
  require(grid)
}

loadlibs()
cat(" R libraries loaded for Nondetects And Data Analysis LBG","\n", " written by A. Otiniano.",'\n')



