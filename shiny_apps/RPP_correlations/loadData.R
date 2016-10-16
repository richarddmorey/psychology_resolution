library(dplyr)
#library(RCurl)
#library(grid)
#library(ggplot2)
#library(downloader)
#library(gridExtra)
#library(extrafont)

# Code adapted from

#fn = tempfile()
#download("https://osf.io/fgjvw/?action=download",destfile=fn)
#dat = utils::read.csv(fn,stringsAsFactors=F)
dat = utils::read.csv("rpp_data.csv",stringsAsFactors=F)
fdat = dplyr::filter(dat, !is.na(T_pval_USE..O.), !is.na(T_pval_USE..R.))

if(nrow(fdat)!=99) stop("99 rows expected, but got",nrow(fdat))


idOK <- stats::complete.cases(fdat$T_r..O.,fdat$T_r..R.)

## 97 expected
if(sum(idOK)!=97) stop("97 OKs expected, but got",sum(idOK))

colnames(dat)[1] = "ID"

cor_orig = dat$T_r..O.
cor_rep = dat$T_r..R.

n_orig = dat$T_df2..O. + 2
n_rep = dat$T_df2..R. + 2

df1_o = dat$T_df1..O.
df1_o[is.na(df1_o)]=0



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

### Authors
orig.label = paste(dat$Authors..O., ": ", dat$Journal..O.,", ", dat$Volume..O.," (",dat$Pages..O.,")", sep="")
orig.label.short = sub(",.*:",orig.label,perl=TRUE,replacement = " et al. :")

pval = dat$T_pval.recalc..O.
# Convert p_rep to p value
pr = .92
pval[dat$ID == 134] <- 1/((1/pr - 1)^(-3/2)+1)

se_total <- sqrt(1/(n_orig-3) + 1/(n_rep-3))
low = tanh(fish_orig - se_total * 1.96)
high = tanh(fish_orig + se_total * 1.96)
too_high = (cor_rep > high)
too_low = (cor_rep < low)
use_index = (!is.na(dat$T_pval_USE..O.) & !is.na(dat$T_pval_USE..R.))

pi_dat = data.frame(cor_orig, cor_rep, low, high, se_total, 
                    too_low, too_high, n_orig, 
                    n_rep,val = (too_low+2*too_high+1),use_index,ID=dat$ID, 
                    lab = as.character(orig.label.short),pval=pval,df1_o=df1_o)

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
  
  pi_dat = c(v['cor_orig'], v['cor_rep'], low, high, se_total, too_low, too_high, v['n_orig'], v['n_rep'], val = (too_low+2*too_high+1),v['use_index'],v['ID'])
  names(pi_dat) = names(v)
  return(pi_dat)
}

z.score = (atanh(pi_dat_nona$cor_orig) - atanh(pi_dat_nona$cor_rep)) / (pi_dat_nona$se_total)


#######



all.idx = c(5, 29, 60, 66, 68)
all.idx.in.dat = match(pi_dat_nona[all.idx,"ID"],dat$ID)
Ns = dat[all.idx.in.dat,c("N..O.","N..R.")]
cors = pi_dat_nona[all.idx,c("cor_orig","cor_rep","se_total")]
spec_tab = cbind(Ns,cors[,1:2],atanh(cors[,1])-atanh(cors[,2]),cors[,3])
rownames(spec_tab) = letters[1:nrow(spec_tab)]
colnames(spec_tab) = c("Orig. N","Repl. N","Orig. r", "Repl. r","Deviation (Fisher's z)","Deviation SE (Fisher's z)")

pi_dat_nona$lab = as.character(pi_dat_nona$lab) 

pi_dat_nona$lab[all.idx] = paste("[",letters[1:length(all.idx)],"] ", pi_dat_nona$lab[all.idx] ,sep="")

my.labs = 1:nrow(pi_dat_nona)
names(my.labs) = as.character(pi_dat_nona$lab)
my.labs = c(all=0, my.labs)
