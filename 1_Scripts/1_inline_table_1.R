#### SETUP ####
rm(list = ls())
# set up relative paths
library(here)
source("global_options.R")
here::i_am("1_Scripts/1_inline_table_1.R")
set.seed(02138)

# read in the data
df = read_excel("./0_Data/1_Parameter_Estimates/table_s1_r1.xlsx")
df = df[!is.na(df$Code),]

# helpers
vsl = as.numeric(df$Value[df$Code=="VSL"])
daly =  eval(parse(text = sub(",", "", df$Value[df$Code=="DALY"])))/1000000
n.trial = 200
n.trial.small = 50
n.trial.large = 1000

#### POWER CALCULATION ####

# power function
calc_pwr = function(n, baseline, trt_risk, n.sim = 100000, rule_out = 0, type = "wald",
                    pwr = 0.8, return = "diff", ni = F, sig = 0.05){
  out = foreach(i=1:n.sim, .combine = function(...) rbindlist(list(...), use.names = T)) %dopar% {
    
    set.seed(i)
    
    # draw from each distribution
    g1 = rbinom(1, size = round(n), prob = baseline)
    g2 = rbinom(1, size = round(n), prob = trt_risk)
    
    # tests
    if(type=="wald"){
      ci = prop.test(x = c(g1, g2), n = c(n, n), correct = FALSE, conf.level = 1-sig)$conf.int
    }else if(type=="waldcc"){
      ci = prop.test(x = c(g1, g2), n = c(n, n), conf.level = 1-sig)$conf.int
    }else if(type=="fisher"){
      ci = fisher.test(matrix(c(g1, n-g1, g2, n-g2), ncol = 2), conf.level = 1-sig)$conf
    }else if(type=="score"){
      ci = BinomDiffCI(g1,n,g2,n, method = "score", conf.level = 1-sig)[-1]
    }else if(type=="scorecc"){
      ci = BinomDiffCI(g1,n,g2,n, method = "scorecc", conf.level = 1-sig)[-1]
    }
    if(ni){
      d = data.table(pwr = sum(!(ci[1] <= rule_out)))
    }else{d = data.table(pwr = sum(!(ci[1] <= rule_out & ci[2] >= rule_out)))}
    
    return(d)
  }
  
  if(return=="diff"){
    val = mean(out[["pwr"]])-pwr
    return(val)}
  if(return=="abs"){return(mean(out[["pwr"]]))}
  
}

#### THALIDOMIDE ####

# helpers
thal.death.perc = 1-as.numeric(df$Value[df$Code=="thal.n.surv"])/as.numeric(df$Value[df$Code=="thal.n"])

# observed -- assume n expected rounds to 0
thal.death.actual = as.numeric(df$Value[df$Code=="thal.n"])*(thal.death.perc)
thal.surv.actual = as.numeric(df$Value[df$Code=="thal.n.surv"])

# trial size
thal.baseline = eval(parse(text = sub(",", "", df$Value[df$Code=="base.ild"])))
thal.trt = eval(parse(text = df$Value[df$Code=="thal.dec"]))

# trial power
pwr.thal = calc_pwr(n.trial, thal.baseline, thal.trt, type = "score", return = "abs")
pwr.thal.small = calc_pwr(n.trial.small, thal.baseline, thal.trt, type = "score", return = "abs")
perc.two.ilds = (1-pbinom(2, n.trial.small, thal.trt))*100
pwr.thal.large = calc_pwr(n.trial.large, thal.baseline, thal.trt, type = "score", return = "abs")

# deaths
# incremental from trial
# note exact value is ~12.4984999999999999 so it rounds to 12 in the text :) sprintf("%.16f", thal.death.rct)
thal.death.rct = (n.trial*thal.trt - n.trial*thal.baseline)*thal.death.perc

# observed in trial
thal.death.rct.full = n.trial*thal.trt*thal.death.perc

# difference in value: obs real world vs. obs in trial
thal.death.rct.exp.diff = pwr.thal*(thal.death.actual-thal.death.rct.full)
thal.death.rct.value = pwr.thal*(thal.death.actual-thal.death.rct.full)*vsl

# difference in smaller or larger study
thal.death.rct.exp.diff.small = pwr.thal.small*(thal.death.actual-n.trial.small*thal.trt*thal.death.perc)
thal.death.rct.exp.diff.large = pwr.thal.large*(thal.death.actual-n.trial.large*thal.trt*thal.death.perc)


# ILD
# incremental from trial
thal.ild.rct = (n.trial*thal.trt - n.trial*thal.baseline)*(1-thal.death.perc)

# observed in trial
thal.ild.rct.full = (n.trial*thal.trt)*(1-thal.death.perc)

# difference in value: obs real world vs. obs in trial
thal.ild.rct.exp.diff = pwr.thal*(thal.surv.actual-thal.ild.rct.full)
thal.ild.rct.value = pwr.thal*(thal.surv.actual-thal.ild.rct.full)*as.numeric(df$Value[df$Code=="thal.ild.duration"])*daly*(as.numeric(df$Value[df$Code=="thal.disab.ild"]))

# difference in smaller or larger study
thal.ild.rct.exp.diff.small = pwr.thal.small*(thal.surv.actual-(n.trial.small*thal.trt)*(1-thal.death.perc))
thal.ild.rct.exp.diff.large = pwr.thal.large*(thal.surv.actual-(n.trial.large*thal.trt)*(1-thal.death.perc))

#### COVID-19 ####

load(here("0_Data", "1_Parameter_Estimates", "covid_health_ests_v2.RData"))

# trial size
covid.deaths.actual = covid_health_vals$deaths.obs[1]
covid.sbs.actual = covid_health_vals$sb.obs[1]

# deaths
covid.death.min = min(covid_health_vals$n_deaths_averted)
covid.death.max = max(covid_health_vals$n_deaths_averted)

covid.death.min.value = (covid.death.min)*vsl
covid.death.max.value = (covid.death.max)*vsl

  # death percentages
  covid.perc.deaths_MAT.min = min(covid_health_vals$perc_deaths)
  covid.perc.deaths_MAT.max = max(covid_health_vals$perc_deaths)
  
  covid.perc.deaths_ALL.min = min(covid_health_vals$perc_deaths_all)
  covid.perc.deaths_ALL.max = max(covid_health_vals$perc_deaths_all)

# stillbirths
covid.sb.min = min(covid_health_vals$n_stillbirths_averted)
covid.sb.max = max(covid_health_vals$n_stillbirths_averted)

covid.sb.min.value = (covid.sb.min)*vsl
covid.sb.max.value = (covid.sb.max)*vsl

# sb percentages
  covid.perc.sb_ALL.min = min(covid_health_vals$perc_sbs_averted)
  covid.perc.sb_ALL.max = max(covid_health_vals$perc_sbs_averted)

#### DOLUTEGRAVIR ####

# trial size
dtg.baseline = eval(parse(text = sub(",", "", df$Value[df$Code=="base.ntd"])))
dtg.false.RR = as.numeric(df$Value[df$Code=="DTG.false.signal"]) 

# observed
dtg.deaths.survey = as.numeric(df$Value[df$Code=="DTG.perc.post.diff"])/100 * as.numeric(df$Value[df$Code=="DTG.perc.red"]) * as.numeric(sub(",", "", df$Value[df$Code=="DTG.n.survey"])) 
dtg.deaths.total = as.numeric(df$Value[df$Code=="DTG.perc.post.diff"])/100 * as.numeric(df$Value[df$Code=="DTG.perc.red"]) * as.numeric(df$Value[df$Code=="DTG.n.tot.ART"])*1000000

# trial
dtg.deaths.rct.value = (dtg.deaths.total)*vsl

# trial power
pwr.dtg = calc_pwr(n.trial, dtg.baseline, dtg.baseline*dtg.false.RR, type = "score", return = "abs")
pwr.dtg.small = calc_pwr(n.trial.small, dtg.baseline, dtg.baseline*dtg.false.RR, type = "score", return = "abs")
pwr.dtg.large = calc_pwr(n.trial.large, dtg.baseline, dtg.baseline*dtg.false.RR, type = "score", return = "abs")
perc.two.ntds = (1-pbinom(2, n.trial, dtg.baseline*dtg.false.RR))*100
perc.two.ntds.large = (1-pbinom(2, n.trial.large, dtg.baseline*dtg.false.RR))*100

#### VALUE OF INFORMATION ####

# THAL
# (40 days / 365 days/year * 0.114 DALYs * 100,000 Value of a year ) * x > $100M
thal.voi.n = 100/(as.numeric(df$Value[df$Code=="thal.nausea.duration"])/365*as.numeric(df$Value[df$Code=="thal.disab.nausea"])*daly)
thal.m = (thal.death.actual+thal.surv.actual)/(n.trial*thal.trt)*.8

# DTG
# taking before detection
dtg.n.take = as.numeric(df$Value[df$Code=="DTG.perc.prior"])/100*as.numeric(df$Value[df$Code=="DTG.n.preg"])*1000000

# number needed to take
perc.death = as.numeric(gsub(",", "", df$Value[df$Code=="DTG.deaths.HIV"]))/(as.numeric(df$Value[df$Code=="DTG.n.tot.ART"])*1000000/82)/100
dtg.voi.n = 100/(perc.death*(1-as.numeric(df$Value[df$Code=="DTG.RR.efv"]))*vsl)
dtg.voi.n.perc = (dtg.voi.n/1000000)/as.numeric(df$Value[df$Code=="DTG.n.tot.ART"])

#### SAVE OUTPUT ####

save(list = ls(),
     
     file = "2_Output/t1_values.RData")
