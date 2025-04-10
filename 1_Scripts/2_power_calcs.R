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

#### MAKE PLOTS ####

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

#### TYPE I ERROR RATES ####

# type I error rates (main)
dt_t1e = dt_t1e %>%
  mutate(type_fac = factor(type, levels = c("wald", "waldcc", "score", "scorecc", "fisher"),
                           labels = c("Wald", "Wald (CC)", "Score", "Score (CC)", "Fisher")),
         n_fac = factor(comma(n_test), levels = n_test_vec),
         baseline_risk_fac = fct_rev(factor(baseline_risk, labels = baseline_risk_vec_plain)))

# make plot
t1_error = ggplot(dt_t1e, aes(x = factor(baseline_risk_fac), y = n_fac, fill = t1e*100)) + geom_tile() + 
  geom_text(aes(label = round(t1e*100, 1))) + 
  scale_fill_continuous("Type I error (%)") + 
  facet_wrap(.~type_fac) + labs(y = "Sample size", x = "Baseline risk") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())

ggsave(filename = "2_Output/t1_error.png", width = 12, height = 5)
