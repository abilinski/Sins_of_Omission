# set up relative paths
library(here)
here::i_am("1_Scripts/2_power_calcs.R")
set.seed(02138)

# source global options
source("global_options.R")

# baseline risk
baseline_risk = c(1/50000, 1/5000, 1/1000, 1/200, 1/100, 1/25, 1/10)

# treated risk
trt_risk = c(1/10000, 1/5000, 1/2000, 1/1000, 1/500, 1/200, 1/100, 1/50, 1/20, 1/10, 1/6, 1/5, 1/2)

# types
type = c("wald", "scorecc", "fisher", "waldcc", "score")

# matrix setup
dt = expand_grid(baseline_risk, trt_risk, type) %>% filter(baseline_risk <= trt_risk) %>%
  mutate(rule_out = ifelse(type=="fisher", 1, 0))
dt$n = NA
dt$chk = NA

# values of n
n_test = c(100, 1000, 10000, 100000, 1000000)
dt_t1e = expand_grid(n_test, baseline_risk, type) %>%
  mutate(rule_out = ifelse(type=="fisher", 1, 0))

# power function
calc_pwr = function(n, baseline, trt_risk, n.sim = 10000, rule_out = 0, type = "wald",
                    pwr = 0.8, return = "diff", ni = F, sig = 0.05){
  out = foreach(i=1:n.sim, .combine = function(...) rbindlist(list(...), use.names = T)) %dopar% {
    
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

# function to get n
get_n = function(baseline, trt_risk, q = qnorm(.975), pwr = 0.95, rule_out = 0, type_val = "wald", non_inf_risk = 0,
                 ni_val = F, sig = .05){
  
  if(ni_val){
    calc1 = power.prop.test(power = pwr, 
                            p1 = baseline, 
                            p2 = non_inf_risk,
                            sig.level = sig,
                            alternative = "two.sided")$n
  }else{
    calc1 = power.prop.test(power = pwr, 
                            p1 = baseline, 
                            p2 = trt_risk,
                            sig.level = sig,
                            alternative = "two.sided")$n
  }
    
    calc2 = max(binsearch(fun = calc_pwr, range = c(calc1/7, round(calc1*11)), baseline = baseline, 
                      rule_out = rule_out, sig = sig,
                      trt_risk = trt_risk, type = type_val, pwr = pwr, ni = ni_val, return = "diff")$where)
    
    chk = calc_pwr(n = as.numeric(calc2), baseline = baseline, trt_risk = trt_risk, return = "abs", type = type_val,
                   rule_out = rule_out, ni = ni_val, sig = sig)
    
    return(c(n = calc2, base = calc1, chk = chk))

}

# check type 1 error
for(i in 1:nrow(dt_t1e)){
  
  tic()
  dt_t1e$t1e[i] = calc_pwr(n = dt_t1e$n_test[i], baseline = dt_t1e$baseline_risk[i], trt_risk = dt_t1e$baseline_risk[i], 
                        n.sim = 50000,
                      rule_out = dt_t1e$rule_out[i], type = dt_t1e$type[i],
                      return = "abs")
  print(i)
  toc()
}

ggplot(dt_t1e, aes(x = factor(n_test), y = factor(baseline_risk), fill = t1e)) + geom_tile() + 
  geom_text(aes(label = round(t1e*100, 1))) + 
  facet_grid(.~type)

# get sample size
dt = dt %>% filter(type %in% c("score", "fisher") & baseline_risk < trt_risk)
for(i in 1:nrow(dt)){
  tic()
  temp = get_n(baseline = dt$baseline_risk[i],
                              trt_risk = dt$trt_risk[i],
                              pwr = 0.95,
                              rule_out = dt$rule_out[i],
                              type_val = dt$type[i])
  
  dt$n_base[i] = temp[2]; dt$n[i] = temp[1]; dt$chk[i] = temp[3]
  print(i)
  toc()
  
}
save(dt, file = here("2_Output", "power_files.RData"))

# add column for rule_out
dt_NI = dt %>% 
  filter(type%in%c("score")) %>%
  mutate(non_inf = trt_risk,
         trt_risk = baseline_risk,
         delta = baseline_risk-non_inf,
         rule_out_NI = ifelse(type=="fisher", (baseline_risk/(1-baseline_risk))/((non_inf/(1-non_inf))), delta)) %>%
  filter(non_inf!=baseline_risk) %>% unique()

# type I error
for(i in 1:nrow(dt_NI)){
  tic()
  dt_NI$t1e[i] = calc_pwr(n = 200000, baseline = dt_NI$baseline_risk[i], trt_risk = dt_NI$non_inf[i], 
                           n.sim = 50000, sig = .1, ni = T,
                           rule_out = dt_NI$rule_out_NI[i], type = dt_NI$type[i],
                           return = "abs")
  print(i)
  toc()
}

# get sample size
for(i in 1:nrow(dt_NI)){
  tic()
  temp = get_n(baseline = dt_NI$baseline_risk[i],
               trt_risk = dt_NI$trt_risk[i],
               pwr = 0.8,
               rule_out = dt_NI$rule_out_NI[i],
               type_val = dt_NI$type[i], 
               non_inf_risk = dt_NI$non_inf[i],
               ni_val = T, sig = .1)
  
  dt_NI$n_base_NI[i] = temp[2]; dt_NI$n_NI[i] = temp[1]; dt_NI$chk_NI[i] = temp[3]
  print(i)
  toc()
  
}

save(dt_NI, file = here("2_Output", "power_files2.RData"))

#### MAKE PLOTS ####

# load saved output
load(here("2_Output", "power_files.RData"))
load(here("2_Output", "power_files2.RData"))

# baseline risk
baseline_risk_vec_plain = rev(c("1/10", "1/20", "1/100", 
                          "1/200", "1/1000",  "1/5000",  "1/50,000"))
baseline_risk_vec = rev(c("1/10 Miscarriage", "1/20 Postpartum hemorrhage", "1/100 Congestive heart defects", 
                      "1/200 Stillbirth", "1/1000 Neural tube defects",  "1/5000 Anencephaly",  "1/50,000 Intercalary limb deficiencies"))

# treated risk
trt_risk_vec = c("1/10000", "1/5000", "1/2000", "1/1000", "1/500", "1/200", "1/100", "1/50", 
                 "1/20", "1/10", "1/6", "1/5", "1/2")

# n tested
n_test_vec = c("100", "1,000", "10,000", "100,000", "1,000,000")


####*************************** FIGURES **************************####

#### functions

# label data frame
make_labs = function(dt){
dt2 = dt %>% 
  
  # make labels as factors
  mutate(baseline_fac = fct_rev(factor(baseline_risk, labels = baseline_risk_vec)),
         trt_fac = fct_rev(factor(trt_risk, labels = trt_risk_vec)),

  # calculate increment
        n_inc = round(n*(trt_risk-baseline_risk)),
        lab = paste(comma(n), " (", comma(n_inc), ")", sep = "")
         ) %>%
  
  # filter
  filter(trt_risk>=1/5000)
}

# make plots
make_plot = function(dt2, type_val, y_lab = "Treated risk", show_legend = T){
  dt2 %>%
    filter(type==type_val) %>%
    ggplot(aes(x = baseline_fac, y = factor(trt_fac), fill = ifelse(n > 1000, ">1000/arm", "<1000/arm"))) +
    geom_tile() + 
    geom_text(col = "black", aes(label = lab)) + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    labs(x = "Baseline risk", y = y_lab, title = "") + 
    g15c_style 
}

#### LABEL DATA FRAMES ####
# label data frames
dt2 = make_labs(dt)
dt_NI2 = make_labs(dt_NI %>% mutate(trt_risk = non_inf, n = n_NI))

#### FIGURE 1 ####

# percentage observed in observational study
obs = 0.03

# score test
p1 = make_plot(dt2, "score")
ggsave(filename = "2_Output/samp_size_95.png", width = 10, height = 5)

p2 = make_plot(dt2 %>% mutate(lab = comma(round(n_inc/obs))), type_val = "score", show_legend = F)
ggsave(filename = "2_Output/samp_size_95_obs.png", width = 10, height = 5)

p2_01 = make_plot(dt2 %>% mutate(lab = comma(round(n_inc/.01))), type_val = "score")
ggsave(filename = "2_Output/samp_size_95_obs_01.png", width = 10, height = 5)

p2_10 = make_plot(dt2 %>% mutate(lab = comma(round(n_inc/.1))), type_val = "score")
ggsave(filename = "2_Output/samp_size_95_obs_10.png", width = 10, height = 5)

# save file
dt_out = dt2 %>% filter(type=="score")
save(dt_out, file = "2_Output/power_table.RData")

#### FIGURE 1 WITH ALTERNATIVE SPECIFICATIONS -- SUPPLEMENT ####

# fisher test
p1_fisher = make_plot(dt2, "fisher")
ggsave(filename = "2_Output/samp_size_95_fisher.png", width = 10, height = 5)


# non-inferiority test
p_NI = make_plot(dt_NI2, "score", y_lab = "Non-inferiority threshold")
ggsave(filename = "2_Output/samp_size_95_NI.png", width = 10, height = 5)


#### TYPE I ERROR RATES ####

# type I error rates (main)
dt_t1e = dt_t1e %>%
  mutate(type_fac = factor(type, levels = c("wald", "waldcc", "score", "scorecc", "fisher"),
                           labels = c("Wald", "Wald (CC)", "Score", "Score (CC)", "Fisher")),
         n_fac = factor(comma(n_test), levels = n_test_vec),
         baseline_risk_fac = fct_rev(factor(baseline_risk, labels = baseline_risk_vec_plain)))

t1_error = ggplot(dt_t1e, aes(x = factor(baseline_risk_fac), y = n_fac, fill = t1e*100)) + geom_tile() + 
  geom_text(aes(label = round(t1e*100, 1))) + 
  scale_fill_continuous("Type I error (%)") + 
  facet_wrap(.~type_fac) + labs(y = "Sample size", x = "Baseline risk") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())

ggsave(filename = "2_Output/t1_error.png", width = 12, height = 5)

# type I error rates (NI)
t1_error_NI = ggplot(dt_NI2 %>% filter(trt_risk==.5), 
                  aes(x = factor(baseline_risk), y = factor(200000))) + 
  geom_tile(fill = "white") + 
  geom_text(aes(label = round(t1e*100, 1))) + 
  labs(y = "Sample size", x = "Baseline risk") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())



