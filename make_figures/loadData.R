library(dplyr)
library(RCurl)
library(grid)
library(ggplot2)
library(downloader)
library(gridExtra)
library(extrafont)

# Code adapted from

#download("https://osf.io/fgjvw/?action=download",destfile="rpp_data.csv")


dat = utils::read.csv("rpp_data.csv",stringsAsFactors=F)
fdat = dplyr::filter(dat, !is.na(T_pval_USE..O.), !is.na(T_pval_USE..R.))

if(nrow(fdat)!=99) stop("99 rows expected, but got",nrow(fdat))


idOK <- stats::complete.cases(fdat$T_r..O.,fdat$T_r..R.)

## 97 expected
if(sum(idOK)!=97) stop("97 OKs expected, but got",sum(idOK))



#source("../../code/utils.R")

colnames(dat)[1] = "ID"

cor_orig = dat$T_r..O.
cor_rep = dat$T_r..R.

n_orig = dat$T_df2..O. + 2
n_rep = dat$T_df2..R. + 2


### Partial correlation, so degrees of freedom plus 2 in order to get N
n_orig[dat$ID == 82] <- dat$T_df1..O.[82]+2
n_rep[dat$ID == 82] <- dat$T_df1..R.[82]+2

### Correlation
n_orig[dat$ID == 120] <- dat$T_N..O.[120]
n_rep[dat$ID == 120] <- dat$T_N..R.[120]
n_orig[dat$ID == 154] <- dat$T_N..O.[154]
n_rep[dat$ID == 154] <- dat$T_N..R.[154]
n_orig[dat$ID == 155] <- dat$T_N..O.[155]
n_rep[dat$ID == 155] <- dat$T_N..R.[155]

### t
n_orig[dat$ID == 121] <- dat$T_N..O.[121]
n_rep[dat$ID == 121] <- dat$T_N..R.[121]

### Transform to Fisher's z
fish_orig = atanh(cor_orig)
fish_rep = atanh(cor_rep)


se_total <- sqrt(1/(n_orig-3) + 1/(n_rep-3))
low = tanh(fish_orig - se_total * 1.96)
high = tanh(fish_orig + se_total * 1.96)
too_high = (cor_rep > high)
too_low = (cor_rep < low)
use_index = (!is.na(dat$T_pval_USE..O.) & !is.na(dat$T_pval_USE..R.))

pval = dat$T_pval.recalc..O.
# Convert p_rep to p value
pr = .92
pval[dat$ID == 134] <- 1/((1/pr - 1)^(-3/2)+1)

df1_o = dat$T_df1..O.
df1_o[is.na(df1_o)]=0

pi_dat = data.frame(cor_orig, cor_rep, low, high, se_total, too_low, too_high, n_orig, n_rep,val = (too_low+2*too_high+1),use_index,ID=dat$ID,pval=pval,df1_o=df1_o)

pi_dat = dplyr::filter(pi_dat, (use_index > 0) & (df1_o<2) )
pi_dat_nona = pi_dat[rowSums(is.na(pi_dat[,1:9]))==0,]


# This code computes a sigle prediction interval. Useful
# for apply()ing.
do.one = function(v,cc = .95){
  fish_orig = atanh(v['cor_orig'])
  fish_rep = atanh(v['cor_rep'])
  se_total <- sqrt(1/(v['n_orig']-3) + 1/(v['n_rep']-3))
  low = tanh(fish_orig - se_total * -qnorm((1-cc)/2))
  high = tanh(fish_orig + se_total * -qnorm((1-cc)/2))
  too_high = (v['cor_rep'] > high)
  too_low = (v['cor_rep'] < low)
  
  pi_dat = c(v['cor_orig'], v['cor_rep'], low, high, se_total, too_low, too_high, v['n_orig'], v['n_rep'], val = (too_low+2*too_high+1),v['use_index'],v['ID'],v['pval'],v['df1_o'])
  names(pi_dat) = names(v)
  return(pi_dat)
}

z.score = (atanh(pi_dat_nona$cor_orig) - atanh(pi_dat_nona$cor_rep)) / (pi_dat_nona$se_total)
nsig = !pi_dat_nona$too_low & !pi_dat_nona$too_high


