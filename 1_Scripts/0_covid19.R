####================ MAIN ANALYSIS =====================####

# set up relative paths
#rm(list = ls())
library(here)
here::i_am("1_Scripts/0_covid19.R")

# source global options
source("global_options.R")

# read helper data
df = read_excel("./0_Data/1_Parameter_Estimates/table_s1_r1.xlsx")
df = df[!is.na(df$Code),]

# helpers
vsl = as.numeric(df$Value[df$Code=="VSL"])
wildtype.inc = as.numeric(df$Value[df$Code=="covid.sb.w"])
delta.inc = as.numeric(df$Value[df$Code=="covid.sb.d"])
vsd_locations = c("CA", "WA", "OR", "CO", "MN", "WI") # See Table S2

####-------------- Read in Data ---------------####
# function to merge vaccination rates with weights and generate weighted estimates of uptake
run_ests = function(v, p_weight2, e_weight, filter_loc = TRUE,
                    demo = c("Female_Ages_12-17_yrs", "Female_Ages_18-24_yrs",
                             "Female_Ages_25-39_yrs", "Female_Ages_40-49_yrs")){
  
  # merge folders
  v_out = v %>% 
    # merge with fertility weights
    inner_join(p_weight2, c("Location" = "abbv", "Demographic_Category" = "var2")) %>%
    # merge with VSD enrollment weights
    left_join(e_weight, c("Location" = "abbv", "Demographic_Category" = "var2")) %>%
    
    # limit to demographic categories
    filter(Demographic_Category %in% demo)
  
  # if limited to VSD
  if(filter_loc) v_out = v_out %>% filter(Location %in% vsd_locations)
  
  # calculate weights
  v_out = v_out %>% group_by(date, month, Year, min_date) %>%
    mutate(weight_f = births/sum(births, na.rm = T), # make fertility weights
           weight_e = helper_e/sum(helper_e, na.rm = T), # make enrollment weights
           weight_b = state_births/sum(state_births, na.rm = T), # state birth weights, ignore for age matching (ONLY 40-49)
           weight_p = state_pop/sum(state_pop, na.rm = T)) # state population weights, ignore for age matching (ONLY 40-49)
  
  # check totals
  # v_out %>% group_by(date) %>% summarize(sum(weight_f, na.rm = T), sum(weight_e, na.rm = T))
  
  # estimate values
  v_out = v_out %>% group_by(date, month, Year, min_date) %>%
    summarize(vax_rate_f = sum(Series_Complete_Pop_pct_agegroup*weight_f), # fertility weighting
              vax_rate_p = sum(Series_Complete_Yes)/sum(census)*100,       # population weighting
              vax_rate_e = sum(Series_Complete_Pop_pct_agegroup*weight_e),  # enrollment weighting
              vax_rate_b = sum(Series_Complete_Pop_pct_agegroup*weight_b),  # state birth weighting, ignore for age matching (ONLY 40-49)
              vax_rate_pop = sum(Series_Complete_Pop_pct_agegroup*weight_p)) # state population weighting, ignore for age matching (ONLY 40-49)
}

# generate relative risk and adjusted estimated uptake for a selected group
run_comp = function(v, p_weight2, e_weight, demo_val = c("Female_Ages_12-17_yrs", "Female_Ages_18-24_yrs",
                                                         "Female_Ages_25-39_yrs", "Female_Ages_40-49_yrs"),
                    lab = "Age- and state-matched", v_preg_val = v_preg){
  
  # run for subset of VSD locations
  v_sub = run_ests(v, p_weight2, e_weight, demo = demo_val) %>% gather(var, value, vax_rate_f, vax_rate_p, vax_rate_e, vax_rate_b, vax_rate_pop) %>%
    # join for pregnant
    left_join(v_preg_val, c("month" = "month", "Year" = "Year")) %>% mutate(RR = coverage/value)
  
  # prep for join
  v_sub_JOIN = v_sub %>% dplyr::select(month, Year, var, RR)
  
  # run for ALL locations
  v_ALL = run_ests(v, p_weight2, e_weight, filter_loc = FALSE, demo = demo_val) %>%
    mutate(vax_rate_e = vax_rate_f) %>% gather(var, value, vax_rate_f, vax_rate_p, vax_rate_e, vax_rate_b, vax_rate_pop)
  
  # join with full data and calculate adjusted estimates
  v_ALL = v_ALL %>% left_join(v_sub_JOIN, c("month" = "month", "Year" = "Year", "var" = "var")) %>%
    mutate(cov_preg = value*RR, id = lab)
  
}

# process pregnancy uptake
wrapper = function(start_val = 15, month_shift = 1){
  #### PREGNANT VACCINATION ####
  
  # vaccination in pregnant folks 
  v_preg0 = read.csv(here("0_Data", "0_Raw_Data",
                          "Weekly_Data__COVID-19_vaccination_among_pregnant_people_ages_18-49_years_before_and_during_pregnancy_overall__by_race_ethnicity__and_week_ending_date_-_Vaccine_Safety_Datalink___United_States.csv")) %>%
    # filter to fully vaccinated
    # all races
    filter(Vaccination.Coverage.Status=="Fully Vaccinated" & 
             Race.and.Ethnicity=="All Races/Ethnicity") %>%
    
    # format dates
    mutate(date = as.Date(Week_Ending_Date, format = "%m/%d/%Y"), month = month(date), Year = year(date)) %>%
    
    # group by preg status and date
    group_by(Pregnancy.Status.at.Vaccination, month, Year, date) %>% 
    
    # rename coverage variables
    summarize(coverage = sum(Vaccination.Coverage.Percentage....),
              total = sum(Vaccination.Coverage.Percentage....*Denominator)/100,
              min_denom = min(Denominator),
              max_denom = max(Denominator)) %>%
    
    # choose start date
    mutate(min_date = date[which.min(abs(date-as.Date(paste(month, "-", start_val, "-", Year, sep = ""), format = "%m-%d-%Y")))]
    ) %>% filter(date==min_date) %>% 
    
    # label as pregnant people
    mutate(label = "Pregnant people, observed") 
  
  # filter to any pregnancy status at vaccination 
  v_preg = v_preg0 %>% filter(Pregnancy.Status.at.Vaccination=="Any")
  
  #### ENROLLMENT ####
  e = read.csv(here("0_Data", "1_Parameter_Estimates", "table_s2.csv")) %>%
    
    # group by state for states with multiple location
    group_by(State) %>% 
    
    # added enrollment
    summarize(t = sum(Enrollment)) 
  
  #### FERTILITY ####
  # fertility rates by state and age
  f = read.csv(here("0_Data", "0_Raw_Data", "tabula_extracted_nvsr72-01.csv")) %>%
    # filter out US total estimates
    filter(Area!="UnitedStates2") %>%
    gather(var, value, -Area, -Birth.rate, -Fertility.rate, -fertility.rate, -Total) %>%
    # get rid of symbols
    mutate(
      value2 = value,
      
      # (NAs for some territories and young age groups)
      value = as.numeric(value),
      value = ifelse(is.na(value), 0, value))
  
  #### POPULATION WEIGHTS ####
  # population
  pop_df_ALL = read.csv(here("0_Data", "0_Raw_Data", "sc-est2019-agesex-civ.csv")) %>% 
    # filter on ages
    filter(AGE >= 10 & AGE <= 49) %>%
    # filter on women
    filter(SEX == 2) %>%
    # make age categories %>%
    mutate(age_cat = case_when((AGE>=10 & AGE<=14)~"X10_14",
                               (AGE>=15 & AGE<=17)~"X15_17",
                               (AGE>=18 & AGE<=19)~"X18_19",
                               (AGE>=20 & AGE<=24)~"X20_24",
                               (AGE>=25 & AGE<=29)~"X25_29",
                               (AGE>=30 & AGE<=34)~"X30_34",
                               (AGE>=35 & AGE<=39)~"X35_39",
                               (AGE>=40 & AGE<=44)~"X40_44",
                               (AGE>=45 & AGE<=49)~"X45_49")) %>%
    group_by(age_cat, NAME) %>% summarize(pop = sum(POPEST2019_CIV)) %>%
    group_by(NAME) %>% mutate(pop_perc = pop/sum(pop), tot = sum(pop_perc)) %>%
    
    # get state abbreviations for merging
    left_join(data.frame(Area = state.name, abbv = state.abb), c("NAME" = "Area")) %>%
    mutate(abbv = ifelse(NAME=="District of Columbia", "DC", abbv)) 
  
  # combine fertility weight with population by location and age category
  # first summarize and obtain number of births
  f_weight = f %>% inner_join(pop_df_ALL, c("Area" = "NAME", "var" = "age_cat")) %>%
    mutate(births = value*pop/1000,
           var2 = case_when(var%in%c("X10_14", "X15_17")~"Female_Ages_12-17_yrs",
                            var%in%c("X18_19", "X20_24")~"Female_Ages_18-24_yrs",
                            var%in%c("X25_29", "X30_34", "X35_39")~"Female_Ages_25-39_yrs",
                            var%in%c("X40_44", "X45_49")~"Female_Ages_40-49_yrs"))
  
  # quick checks
  length(unique(f_weight$Area))
  table(f_weight$var)
  
  # total births over age groups and take percentage 
  f_weight2 = f_weight %>% group_by(Area, var2, abbv) %>%
    # sum births and population
    summarize(births = sum(births), pop = sum(pop)) %>%
    # percentage of births in age group in a given state
    group_by(Area, abbv) %>%
    mutate(f_weight_state = births/sum(births),
           state_births = sum(births),
           state_pop = sum(pop))
  
  #View(p_weight2 %>% summarize(sum(p_weight_state)))
  
  #### ENROLLMENT WEIGHTS ####
  # update enrollment weights
  e_weight = f_weight2 %>% inner_join(e, c("abbv" = "State")) %>%
    
    # multiply percentage of births in age group by helper in that state
    mutate(helper_e = f_weight_state*t) %>% dplyr::select(abbv, var2, helper_e)
  
  #### OVERALL VACCINATION RATES ####
  
  # vaccination rates by state, age, and sex -- ALL STATES (not just VSD)
  v = read.csv(here("0_Data", "0_Raw_Data", "COVID-19_Vaccination_Age_and_Sex_Trends_in_the_United_States_National_and_Jurisdictional.csv")) %>%
    
    # format dates as in v_preg
    separate(Date, sep = " ", into = c("date", "time")) %>%
    mutate(date = as.Date(date, format = "%m/%d/%Y"), month = month(date), Year = year(date)) %>%
    
    # filtering to the first week in the month as above
    group_by(month, Location, Demographic_Category, Year) %>% 
    mutate(min_date = date[which.min(abs(date-as.Date(paste(month, "-", start_val, "-", Year, sep = ""), format = "%m-%d-%Y")))]
    ) %>% filter(date==min_date) %>% 
    
    # subsetting to the relevant age groups
    filter(Demographic_Category%in%c("Female_Ages_12-17_yrs", "Female_Ages_18-24_yrs",
                                     "Female_Ages_25-39_yrs", "Female_Ages_40-49_yrs"))
  
  #### PROCESS DATA FRAMES ####
  
  # estimate relative risks
  rr1 = run_comp(v, f_weight2, e_weight, v_preg_val = v_preg)
  rr2 = run_comp(v, f_weight2, e_weight, demo_val = "Female_Ages_40-49_yrs", "40-49 year-olds", v_preg_val = v_preg)
  
  # combine relative risks
  rrs = bind_rows(rr1, rr2) %>% filter(Year==2021) %>%
    mutate(month2 = month+month_shift) %>%
    filter(month2 >= 3 & month2 <= 11) 
  
  #### OUTCOMES ####
  
  # stillbirths, estimated via cases among pregnant folks \citet{}
  sbs = read.csv(here("0_Data", "0_Raw_Data", "cases_of_covid19_among_pregnant_women_by_week_of_diagnosis_download.csv")) %>%
    # format dates
    mutate(date = as.Date(Week.of.COVID.19.Diagnosis, format = "%m/%d/%y"),
           month = month(date), Year = year(date)) %>%
    group_by(month, Year) %>% summarize(cases = sum(Number.Of.Cases)) %>%
    rename(preg_cases = "cases")

  # ICU + hospitalizations (citet{cdc_covid_deaths})
  outcomes = read.csv(here("0_Data", "0_Raw_Data", "CDC_Wonder", 
                           "Multiple Cause of Death, 2018-2021, Single Race_YEAR_MONTH.csv")) %>%
    # format dates
    mutate(date = as.Date(paste(Month.Code, "01", sep = "/"), 
                          format = "%Y/%m/%d"), month = month(date),
           
           # calculate value for Jun 2021
           # Value of 432 is from: Multiple Cause of Death, 2018-2021, Single Race_YEAR.txt
           Deaths = ifelse(Month.Code=="2021/06", 432 - sum(Deaths[Year.Code==2021], na.rm = T), Deaths)
           ) %>%
    
    # join sbs
    left_join(sbs, c("month" = "month", "Year" = "Year")) %>%
    
    # join averted figs
    right_join(rrs, c("month" = "month2", "Year" = "Year")) %>%
    mutate(deaths_averted = (1-RR)*Deaths,
           sb_fac = ifelse(date.x>="2021-07-01", delta.inc/1000, wildtype.inc/1000),
           sbs_averted = (1-RR)*preg_cases*sb_fac,
           costs_averted = (deaths_averted + sbs_averted)*vsl)
  
  # total maternal deaths
  maternal_deaths = read.csv(here("0_Data", "0_Raw_Data", "CDC_Wonder", "Underlying Cause of Death, 2018-2021, Single Race.csv")) %>%
    filter(!Month.Code%in%c("2021/01", "2021/02", "2021/12")) %>% summarize(deaths = sum(Deaths))
  
  # total stillbirths
  fetal_deaths = read.csv(here("0_Data", "0_Raw_Data", "CDC_Wonder", "Fetal Deaths, 2005-2021.csv")) %>%
    filter(!Month%in%c("January", "February", "December")) %>% summarize(deaths = sum(Fetal.Deaths))
  
  # values for export
  covid_health_vals = outcomes %>% group_by(id, var) %>% 
    summarize(n_deaths_averted = sum(deaths_averted), perc_deaths = sum(deaths_averted)/sum(Deaths),
              perc_deaths_all = sum(deaths_averted)/maternal_deaths$deaths, deaths.obs = sum(Deaths),
              n_stillbirths_averted = sum(sbs_averted), perc_sbs_averted = sum(sbs_averted)/fetal_deaths$deaths,
              sb.obs = fetal_deaths$deaths)
  
  # return outputs
  return(list(outcomes, covid_health_vals))
}

# run estimates with varying parameters
ests_start_1_month_0 = wrapper(start_val = 1, month_shift = 0)
ests_start_15_month_1 = wrapper(start_val = 15, month_shift = 1)
ests_start_1_month_1 = wrapper(start_val = 1, month_shift = 1)

# extract health values for main text
covid_health_vals = ests_start_15_month_1[[2]] %>% filter(var == "vax_rate_f")
save(covid_health_vals, file = here("0_Data", "1_Parameter_Estimates", "covid_health_ests_v2.RData"))

# check for vaccination at start of March in main text
# ests_start_1_month_0[[1]] %>% filter(date.x=="2021-03-01")

####================ Main Plots =====================####

# filter outcomes to fertility weights
outcomes = ests_start_15_month_1[[1]] %>% filter(var == "vax_rate_f")

(p1 = ggplot(outcomes, aes(x = date.y, y = value, col = id)) +
    geom_line(lty = 2, lwd = .75) + 
    geom_line(data = outcomes %>% filter(id=="40-49 year-olds"), aes(x = date.y, y = cov_preg), col = "black", lwd = .75) + 
    scale_size_manual(values = c(rep(0.5, 4), 1.25, rep(1, 3))) +
    g15c_style +
    guides(color = "none", lwd = "none", lty = "none") +
    scale_color_manual(values = c(g15c_colors[c(3,1,5)], "black")) + 
    annotate("text", label = "Pregnant", x = as.Date("2021-10-22"), hjust=0, y = 56, color = "black")+
    annotate("text", label = "Females, state-matched\nCOVID-19 booster reference", x = as.Date("2021-10-22"), hjust=0, y = 73, color = g15c_colors[3])+
    annotate("text", label = "Females, age-\nand state-matched", x = as.Date("2021-10-22"), hjust=0, y = 63, color = g15c_colors[1])+
    labs(x = "", y = "Series Complete (%)") +
    #theme(legend.position = "right") + 
    scale_x_date(limits = c(as.Date("2021-01-15"), as.Date("2022-01-31")), 
                 breaks = c(as.Date("2021-04-01"), as.Date("2021-07-01"), as.Date("2021-10-01")),
                 labels = c("Apr 2021", "Jul 2021", "Oct 2021")) + 
    ylim(0, 92))
p1
ggsave(here("2_Output", "covid_uptake_counterfactuals.png"), plot = p1, width = 9, height = 4.5, units = "in")

# cumulative bounds
outcomes %>% 
  group_by(id) %>%
  arrange(date.x) %>%
  mutate(cumsum = cumsum(costs_averted)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = date.x, names_from = id, values_from = cumsum) %>% 
  ggplot(aes(x = date.x)) +
  geom_ribbon(aes(ymin = `Age- and state-matched`, ymax = `40-49 year-olds`), fill = "grey", alpha = 0.5) + 
  geom_line(aes(y = `40-49 year-olds`),color = g15c_colors[3], lwd = .75) +
  geom_line(aes(y = `Age- and state-matched`), color = g15c_colors[1], lwd = .75)+
  theme_g15c() +
  #expand_limits(x = as.Date(c("2021-02-01", "2021-10-25")))+
  labs(x = "", y = "Health Value ($, millions) of an RCT", title = "") + 
  scale_x_date(limits = c(as.Date("2021-01-15"), as.Date("2022-01-31")), 
               breaks = c(as.Date("2021-04-01"), as.Date("2021-07-01"), as.Date("2021-10-01")),
               labels = c("Apr 2021", "Jul 2021", "Oct 2021")) +
  scale_y_continuous(label=comma)+
  theme(legend.position = "right") + 
  theme(legend.key.width = unit(.5, 'cm')) +
  annotate("text", label = "Females, state-matched\nCOVID-19 booster reference", x = as.Date("2021-11-02"), y = 2600, color = g15c_colors[3], hjust=0)+
  annotate("text", label = "Females, age- \nand state-matched", x = as.Date("2021-11-02"), y = 1700, color = g15c_colors[1], hjust=0) +
  geom_hline(yintercept = 100, color = "black", lty=2) + ylim(0, 2800)
ggsave(here("2_Output", "covid_savings_counterfactuals_cumulative_bounds.png"), width = 9, height = 4.5, units = "in")

####================ NNT Analyses =====================####

# generate data frame
m = c(.001, .005, .01) # maternal mortality risk
sb = seq(0, .02, length.out = 3)  # sb risk
p = c(.005, .01, .05, .1) # probability of contracting COVID
p_lab_lev = paste(c(.5, 1, 5, 10), "%", " risk of COVID-19", sep = "")

# arrange results
df = expand_grid(m, sb, p) %>% mutate(NNT = 100/(p*(m+sb)*vsl)) %>% arrange(NNT) %>%
  mutate(p_lab = factor(paste(p*100, "%", " risk of COVID-19", sep = ""),
                        levels = p_lab_lev))

ggplot(df, aes(x = m*100, y = sb*100)) + geom_tile(fill = NA) +
         geom_text(aes(label = comma(round(NNT)))) + facet_grid(.~p_lab) + theme_classic() + 
  labs(x = "Absolute incremental maternal mortality risk from COVID-19 (%)", y = "Absolute incremental stillbirth risk from COVID-19 (%)", title = "NNT to achieve $100m health value")

ggsave(here("2_Output", "nnt.png"), width = 10, height = 4, units = "in")

####================ Sensitivity Analyses =====================####

# process data frame
mod = rbindlist(list(ests_start_1_month_0[[2]] %>% mutate(lab = "Vax rate time +2 weeks"),
                     ests_start_1_month_1[[2]] %>% mutate(lab = "Vax rate time -2 weeks"),
                     ests_start_15_month_1[[2]] %>% mutate(lab = "Vax rate time (base case)"))) %>%
  mutate(var_lab = case_when(var=="vax_rate_b"~ "State birth weight",
                             var=="vax_rate_pop"~"State fertility weight",
                             var=="vax_rate_f"~"Fertility weight",
                             var=="vax_rate_e"~"Enrollment weight",
                             var=="vax_rate_p"~"Population weight"),
         var_lab = factor(var_lab, levels = rev(c("Fertility weight", "Enrollment weight", "Population weight", "State birth weight", "State fertility weight")))) %>%
  filter(!grepl("State", var_lab) | id=="40-49 year-olds")

# tile plot
m = ggplot(mod %>% gather(var2, value2, n_deaths_averted, n_stillbirths_averted) %>%
              mutate(var2_lab = ifelse(var2=="n_deaths_averted", "Maternal deaths averted", "Stillbirths averted")), 
            aes(x = id, y = var_lab)) + geom_tile(fill = "NA") +
  geom_text(aes(label = round(value2, 1))) + facet_grid(var2_lab~lab) + theme_classic() + 
  labs(x = "", y = "", title = "")

# save
ggsave(here("2_Output", "sens_analysis.png"), plot = m, width = 9, height = 6, units = "in")

####================ Flu Vaccination =====================####
# Sources:
# pregnant uptake: https://www.acog.org/news/news-articles/2020/10/flu-vaccination-coverage-among-pregnant-people-2019-20
# https://www.cdc.gov/flu/fluvaxview/coverage-1920estimates.htm

flu <- tribble(~demog_cat, ~uptake, ~filling,
               "18-49", 38.4, "non-preg",
               "50-64", 50.6,"non-preg",
               "65+", 69.8,"non-preg",
               "Pregnant", 61.2, "pregnant") %>% 
  mutate(filling = factor(filling, levels = c("pregnant", "non-preg")))
flu %>% 
  ggplot(aes(x = demog_cat, y = uptake, fill = filling)) +
  geom_bar(stat = "identity") +
  guides(fill = "none") +
  geom_text(aes(y = uptake + 2, label = paste0(round(uptake), "%")), size = 5) +
  g15c_style + labs(x = "", y = "Uptake")

# save
ggsave(file = "2_Output/flu_uptake.png", width = 8, height = 4)

####-------------- Boosters --------------####

# demographics for boosters
b1 = read.csv(here("0_Data", "0_Raw_Data", "COVID-19_Vaccination_Age_and_Sex_Trends_in_the_United_States_National_and_Jurisdictional.csv")) %>%
  filter(Demographic_Category %in% c("Female_Ages_18-24_yrs", "Female_Ages_25-39_yrs", "Female_Ages_40-49_yrs", "Female_Ages_50-64_yrs")) %>%
  filter(Location=="US") %>%
  separate(Date, sep = " ", into = c("date", "time")) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  gather(var, value, Series_Complete_Pop_pct_agegroup, Booster_Doses_Vax_pct_agegroup) %>%
  mutate(lab = ifelse(var=="Series_Complete_Pop_pct_agegroup", "2-dose", "Booster"))

# pregnant boosters
b2 = read.csv(here("0_Data","0_Raw_Data",
                   "Weekly_Data__COVID-19_vaccination_among_pregnant_people_ages_18-49_years_before_and_during_pregnancy_overall__by_race_ethnicity__and_week_ending_date_-_Vaccine_Safety_Datalink___United_States.csv")) %>%
  mutate(date = as.Date(Week_Ending_Date, format = "%m/%d/%Y")) %>%
  filter(Pregnancy.Status.at.Vaccination=="Any" & # flag
           Race.and.Ethnicity=="All Races/Ethnicity") %>%
  mutate(lab = ifelse(Vaccination.Coverage.Status=="Fully Vaccinated", "2-dose", "Booster"),
         value = Vaccination.Coverage.Percentage....,
         Demographic_Category = "Pregnant")

# combine
df_out = bind_rows(b1 %>% dplyr::select(Demographic_Category, date, lab, value), 
                   b2 %>% dplyr::select(Demographic_Category, date, lab, value))

# make booster plot
df_out %>% 
  #filter(lab == "Booster") %>% 
  mutate(demog = factor(Demographic_Category,
                        levels = c("Pregnant", "Female_Ages_50-64_yrs", "Female_Ages_40-49_yrs",
                                   "Female_Ages_25-39_yrs","Female_Ages_18-24_yrs"),
                        labels = c("Pregnant", "Female, 50-64", "Female, 40-49",
                                   "Female, 25-39","Female, 18-24"
                        ))) %>% 
  ggplot(aes(x = date, y = value, group = demog, col = demog)) + 
  geom_line(aes(lty = demog!="Pregnant")) + 
  facet_wrap(.~lab, ncol = 1) +
  g15c_style + labs(x = "", y = "Uptake") +
  scale_linetype(guide = "none") +
  theme(legend.position = "right")

# save
ggsave(file = "2_Output/covid_fig_booster.png", width = 8, height = 4)
