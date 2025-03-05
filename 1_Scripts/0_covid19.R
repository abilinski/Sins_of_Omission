# set up relative paths
library(here)
here::i_am("1_Scripts/0_covid19.R")

# source global options
source("global_options.R")

# read helper data
df = read_excel("./0_Data/1_Parameter_Estimates/table_s1_r1.xlsx")
df = df[!is.na(df$Code),]

# helpers
vsl = as.numeric(df$Value[df$Code=="VSL"])

####-------------- Read in Data ---------------####

#### VACCINATION ####

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
  
  # use start of month b/c 2 weeks to kick in & 3 from vax to death
  mutate(min_date = min(date)==date) %>% filter(min_date) %>% 
  
  # label as pregnant people
  mutate(label = "Pregnant people, observed") 

# filter to any pregnancy status at vaccination 
v_preg = v_preg0 %>% filter(Pregnancy.Status.at.Vaccination=="Any")

# vaccination rates by state, age, and sex -- LOCATIONS IN Vaccine Safety Datalink
v = read.csv(here("0_Data", "0_Raw_Data", "COVID-19_Vaccination_Age_and_Sex_Trends_in_the_United_States_National_and_Jurisdictional.csv")) %>%
  
  # filter by VSD location
  filter(Location%in%c("CA", "WA", "OR", "CO", "MN", "WI")) %>%
  
  # format dates as in v_preg
  separate(Date, sep = " ", into = c("date", "time")) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"), month = month(date), Year = year(date)) %>%
  
  # filtering to the first week in the month as above
  group_by(month, Location, Demographic_Category, Year) %>% 
  mutate(min_date = min(date)==date) %>% filter(min_date) %>%
  
  # subsetting to the relevant age groups
  filter(Demographic_Category%in%c("Female_Ages_12-17_yrs", "Female_Ages_18-24_yrs",
                                   "Female_Ages_25-39_yrs", "Female_Ages_40-49_yrs", "Female_Ages_50-64_yrs"))

# vaccination rates by state, age, and sex -- ALL STATES (not just VSD)
v_ALL = read.csv(here("0_Data", "0_Raw_Data", "COVID-19_Vaccination_Age_and_Sex_Trends_in_the_United_States_National_and_Jurisdictional.csv")) %>%
  
  # format dates as in v_preg
  separate(Date, sep = " ", into = c("date", "time")) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"), month = month(date), Year = year(date)) %>%
  
  # filtering to the first week in the month as above
  group_by(month, Location, Demographic_Category, Year) %>% 
  mutate(min_date = min(date)==date) %>% filter(min_date) %>%
  
  # subsetting to the relevant age groups
  filter(Demographic_Category%in%c("Female_Ages_12-17_yrs", "Female_Ages_18-24_yrs",
                                   "Female_Ages_25-39_yrs", "Female_Ages_40-49_yrs", "Female_Ages_50-64_yrs"))

#### ENROLLMENT WEIGHTS ####
e = read.csv(here("0_Data", "1_Parameter_Estimates", "table_s2.csv")) %>%
  
  # group by state for states with multiple location
  group_by(State) %>% 
  
  # added enrollment
  summarize(t = sum(Enrollment)) %>% 
  
  # percentage of VSD population by state
  mutate(p = t/sum(t))

#### FERTILITY WEIGHTS ####
# fertility rates by state and age
f = read.csv(here("0_Data", "0_Raw_Data", "tabula_extracted_nvsr72-01.csv")) %>%
  # filter out US total estimates
  filter(Area!="UnitedStates2") %>%
  gather(var, value, -Area, -Birth.rate, -Fertility.rate, -fertility.rate, -Total) %>%
  # get rid of symbols
  mutate(
    value2 = value,
    
    # (NAs for some territories and young age groups)
    value = as.numeric(value))

# population by state and age -- VSD states
pop_df = read.csv(here("0_Data", "0_Raw_Data", "sc-est2019-agesex-civ.csv")) %>% 
  # filter on ages
  filter(AGE >= 10 & AGE <= 49) %>%
  # filter on women
  filter(SEX == 2) %>%
  # filter on states
  filter(NAME %in% c("California", "Wisconsin", "Oregon", "Colorado", "Minnesota", "Washington")) %>%
  # make age categories that match to fertility data
  mutate(age_cat = case_when((AGE>=10 & AGE<=14)~"X10_14",
                             (AGE>=15 & AGE<=17)~"X15_17",
                             (AGE>=18 & AGE<=19)~"X18_19",
                             (AGE>=20 & AGE<=24)~"X20_24",
                             (AGE>=25 & AGE<=29)~"X25_29",
                             (AGE>=30 & AGE<=34)~"X30_34",
                             (AGE>=35 & AGE<=39)~"X35_39",
                             (AGE>=40 & AGE<=44)~"X40_44",
                             (AGE>=45 & AGE<=49)~"X45_49")) %>%
  
  # calculate the total female population in each group
  group_by(age_cat, NAME) %>% summarize(pop = sum(POPEST2019_CIV)) %>%
  
  # percentages by state
  group_by(NAME) %>% mutate(pop_perc = pop/sum(pop), tot = sum(pop_perc)) %>%
  
  # get state abbreviations for merging
  left_join(data.frame(Area = state.name, abbv = state.abb), c("NAME" = "Area")) %>%
  mutate(abbv = ifelse(NAME=="District of Columbia", "DC", abbv)) %>%
  
  # join to enrollment
  left_join(e, c("abbv" = "State")) 

# population by state and age -- ALL states (not just VSD)
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
  mutate(abbv = ifelse(NAME=="District of Columbia", "DC", abbv)) %>%
  
  # join to enrollment
  left_join(e, c("abbv" = "State")) 

# merge fertility rates and population & put into vax categories
f_weight = f %>% inner_join(pop_df, c("var" = "age_cat", "Area" = "NAME")) %>%
  filter(Area %in% c("California", "Wisconsin", "Oregon", "Colorado", "Minnesota", "Washington")) %>%
  
         # births per total population
  mutate(births = value/1000*pop,  
         # births per enrollment population
         births2 = value/1000*t,   
         
         # renaming age category
         var2 = case_when(var%in%c("X10_14", "X15_17")~"Female_Ages_12-17_yrs",
                          var%in%c("X18_19", "X20_24")~"Female_Ages_18-24_yrs",
                          var%in%c("X25_29", "X30_34", "X35_39")~"Female_Ages_25-39_yrs",
                          var%in%c("X40_44", "X45_49")~"Female_Ages_40-49_yrs")) %>%
  group_by(var2, Area, abbv) %>% 
  
  # calculating total births per age group with different definitions
  summarize(births_1 = sum(births, na.rm = T), 
            births_2 = sum(births2, na.rm = T)) %>%
  ungroup() %>%
  # fraction of births
  mutate(w1 = births_1/sum(births_1, na.rm = T),
         w2 = births_2/sum(births_2, na.rm = T))

# merge fertility rates and population & put into vax categories
f_weight_ALL = f %>% inner_join(pop_df_ALL, c("var" = "age_cat", "Area" = "NAME")) %>%
  mutate(births = value/1000*pop,
         var2 = case_when(var%in%c("X10_14", "X15_17")~"Female_Ages_12-17_yrs",
                          var%in%c("X18_19", "X20_24")~"Female_Ages_18-24_yrs",
                          var%in%c("X25_29", "X30_34", "X35_39")~"Female_Ages_25-39_yrs",
                          var%in%c("X40_44", "X45_49")~"Female_Ages_40-49_yrs")) %>%
  group_by(var2, Area, abbv) %>% 
  summarize(births_1 = sum(births, na.rm = T)) %>%
  group_by(var2) %>%
  mutate(births_grp = sum(births_1, na.rm = T)) %>%
  ungroup() %>%
  mutate(w1 = births_1/sum(births_1, na.rm = T),
         w2 = births_1/births_grp)


#### CONSTRUCT WEIGHTED UPTAKE TRAJECTORIES ####
# merge overall (non-pregnant) vaccination rates with fertility weights
v_weighted = v %>% inner_join(f_weight, c("Location" = "abbv", "Demographic_Category" = "var2")) 
v_weighted_ALL = v_ALL %>% inner_join(f_weight_ALL, c("Location" = "abbv", "Demographic_Category" = "var2")) 

#### LIKE AGES ####

# subset group
COVID_like_ages = v_weighted %>% group_by(month, Year, date) %>%
  
  # coverage 1: sum(complete number x fraction of births in total population)
  summarize(coverage1 = sum(Series_Complete_Pop_pct_agegroup*w1), 
  # coverage 2: sum(complete number x fraction of births in enrollment population)
            coverage2 = sum(Series_Complete_Pop_pct_agegroup*w2), 
            w1_chk = sum(w1),
            w2_chk = sum(w2)) %>%
  mutate(id = "Age-and state-matched") %>% arrange(date)

# full group
COVID_like_ages_ALL = v_weighted_ALL %>%
  group_by(month, Year, date) %>%
  summarize(coverage_ALL = sum(Series_Complete_Pop_pct_agegroup*w1), 
            w1_chk = sum(w1)) %>% arrange(date)

# relative risks
rr1 = COVID_like_ages %>% left_join(v_preg, c("month" = "month", "Year" = "Year")) %>%
  group_by(month, Year, date.x, id) %>% summarize(RR = coverage/coverage2) %>% arrange(date.x) %>%
  mutate(Demographic_Category = "all") %>%
  left_join(COVID_like_ages_ALL, c("month" = "month", "Year" = "Year"))

#### 40-49 ####
COVID_40_49 = v_weighted %>% filter(Demographic_Category=="Female_Ages_40-49_yrs") %>%
  group_by(month, Year, date) %>%
  summarize(coverage1 = sum(Series_Complete_Pop_pct_agegroup*w1)/sum(w1), 
            coverage2 = sum(Series_Complete_Pop_pct_agegroup*w2)/sum(w2), 
            w1_chk = sum(w1),
            w2_chk = sum(w2)) %>%
  mutate(id = "40-49") %>% arrange(date)

COVID_40_49_ALL = v_weighted_ALL %>% filter(Demographic_Category=="Female_Ages_40-49_yrs") %>%
  group_by(month, Year, date) %>%
  summarize(coverage_ALL = weighted.mean(Series_Complete_Pop_pct_agegroup, w = w2),
            w2_chk = sum(w2))

rr2 = COVID_40_49 %>% left_join(v_preg, c("month" = "month", "Year" = "Year")) %>%
  group_by(month, Year, date.x, id) %>% summarize(RR = coverage/coverage2) %>% arrange(date.x) %>%
  mutate(Demographic_Category = "all") %>%
  left_join(COVID_40_49_ALL, c("month" = "month", "Year" = "Year"))

# combine relative risks
rrs = bind_rows(rr1, rr2) %>% filter(Year==2021) %>%
  mutate(month2 = month+1) %>%
  filter(month2 >= 3 & month2 <= 11) %>%
  mutate(cov_preg = coverage_ALL*RR)

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
                        format = "%Y/%m/%d"), month = month(date)) %>%
  
  # join sbs
  left_join(sbs, c("month" = "month", "Year" = "Year")) %>%
  
  # join averted figs
  right_join(rrs, c("month" = "month2", "Year" = "Year")) %>%
  mutate(deaths_averted = (1-RR)*Deaths,
         sb_fac = ifelse(date.x>="2021-07-01", .02, 3/1000),
         sbs_averted = (1-RR)*preg_cases*sb_fac,
         costs_averted = (deaths_averted + sbs_averted)*vsl)

####================ VALUES FOR EXPORT =====================####

# total stillbirths
fetal_deaths = read.csv(here("0_Data", "0_Raw_Data", "CDC_Wonder", "Fetal Deaths, 2005-2021.csv")) %>%
  filter(!Month%in%c("January", "February", "December")) %>% summarize(deaths = sum(Fetal.Deaths))

# total maternal deaths
maternal_deaths = read.csv(here("0_Data", "0_Raw_Data", "CDC_Wonder", "Underlying Cause of Death, 2018-2021, Single Race.csv")) %>%
  filter(!Month.Code%in%c("2021/01", "2021/02", "2021/12")) %>% summarize(deaths = sum(Deaths))

# covid vaccination before/during
v_preg0 = v_preg0 %>% filter(Year==2021 & date=="2021-10-02") %>% ungroup()
covid.vacc.before = unlist(v_preg0 %>% filter(Pregnancy.Status.at.Vaccination=="During Pregnancy") %>% dplyr::select(total))
covid.vacc.before_during = unlist(v_preg0 %>% filter(Pregnancy.Status.at.Vaccination=="Any") %>% dplyr::select(total))


# values for export
covid_health_vals = outcomes %>% group_by(id) %>% 
  summarize(n_deaths_averted = sum(deaths_averted), perc_deaths = sum(deaths_averted)/sum(Deaths),
            perc_deaths_all = sum(deaths_averted)/maternal_deaths$deaths, deaths.obs = sum(Deaths),
            n_stillbirths_averted = sum(sbs_averted), perc_sbs_averted = sum(sbs_averted)/fetal_deaths$deaths,
            sb.obs = fetal_deaths$deaths)
save(covid_health_vals, covid.vacc.before, covid.vacc.before_during, file = here("0_Data", "1_Parameter_Estimates", "covid_health_ests.RData"))

####================ Main Plots =====================####


(p1 = ggplot(outcomes, aes(x = date.y, y = coverage_ALL, col = id)) +
   geom_line(lty = 2) + 
   geom_line(data = outcomes %>% filter(id=="40-49"), aes(x = date.y, y = cov_preg), col = "black") + 
   scale_size_manual(values = c(rep(0.5, 4), 1.25, rep(1, 3))) +
   g15c_style +
   guides(color = "none", lwd = "none", lty = "none") +
   scale_color_manual(values = c(g15c_colors[c(1,3,5)], "black")) + 
   annotate("text", label = "Pregnant", x = as.Date("2021-11-02"), y = 55, color = "black")+
   annotate("text", label = "COVID-19 booster reference", x = as.Date("2021-11-02"), y = 73, color = g15c_colors[1])+
   annotate("text", label = "Females age- and state-matched", x = as.Date("2021-11-02"), y = 60, color = g15c_colors[3])+
   labs(x = "", y = "Percent Receiving 2 Doses") +
   #theme(legend.position = "right") + 
   scale_x_date(limits = c(as.Date("2021-01-15"), as.Date("2021-12-25")), 
                breaks = c(as.Date("2021-04-01"), as.Date("2021-07-01"), as.Date("2021-10-01")),
                labels = c("Apr 2021", "Jul 2021", "Oct 2021")) + 
   ylim(0, 92))
p1
ggsave(here("2_Output", "covid_uptake_counterfactuals.png"), plot = p1, width = 9, height = 4.5, units = "in")


# cumulative bounds
outcomes %>% 
  group_by(id) %>%
  mutate(cumsum = cumsum(costs_averted)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = date.x.x, names_from = id, values_from = cumsum) %>% 
  ggplot(aes(x = date.x.x)) +
  geom_ribbon(aes(ymin = `Age-and state-matched`, ymax = `40-49`), fill = "grey", alpha = 0.5) + 
  geom_line(aes(y = `40-49`),color = g15c_colors[1]) +
  geom_line(aes(y = `Age-and state-matched`), color = g15c_colors[3])+
  theme_g15c() +
  #expand_limits(x = as.Date(c("2021-02-01", "2021-10-25")))+
  labs(x = "", y = "Health Value ($, millions) of an RCT", title = "") + 
  scale_x_date(limits = c(as.Date("2021-01-15"), as.Date("2021-12-25")), 
               breaks = c(as.Date("2021-04-01"), as.Date("2021-07-01"), as.Date("2021-10-01")),
               labels = c("Apr 2021", "Jul 2021", "Oct 2021")) +
  scale_y_continuous(label=comma)+
  theme(legend.position = "right") + 
  theme(legend.key.width = unit(.5, 'cm')) +
  annotate("text", label = "COVID-19 \n booster reference", x = as.Date("2021-11-02"), y = 2600, color = g15c_colors[1], hjust=0)+
  annotate("text", label = "Females age- \nand state-matched", x = as.Date("2021-11-02"), y = 1700, color = g15c_colors[3], hjust=0) +
  geom_hline(yintercept = 100, color = "black", lty=2)
ggsave(here("2_Output", "covid_savings_counterfactuals_cumulative_bounds.png"), width = 9, height = 4.5, units = "in")

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
  g15c_style + labs(x = "", y = "Uptake")
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

df_out = bind_rows(b1 %>% dplyr::select(Demographic_Category, date, lab, value), 
                   b2 %>% dplyr::select(Demographic_Category, date, lab, value))

# make booster plot
df_out %>% 
  filter(lab == "Booster") %>% 
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
#scale_color_brewer(palette = "Set2", name = "", direction = -1)
ggsave(file = "2_Output/covid_fig_booster.png", width = 8, height = 4)


####================ Trial Costs =====================####

# generate different worlds
a_out = outcomes %>% expand_grid(world = c("Observed incidence", "Reduced incidence (no delta)"),
                                 prior = c("High maternal benefit", "Low maternal benefit"),
                                 risk_diff = seq(0, .02, by = .0001)) %>%
  mutate(
    # vary according to maternal benefit
    maternal_costs = ifelse(prior=="High maternal benefit", deaths_averted*vsl*.9, deaths_averted*vsl*.75),
    
    # vary according to SB risk
    sb_costs = (1-RR)*preg_cases*risk_diff*vsl) %>%
  gather(var, value, maternal_costs, sb_costs) %>%
  group_by(prior, risk_diff, var, id) %>%
  mutate(
    # make no delta scenario
    diff_out = mean(value[Month.Code%in%c("2021/05", "2021/06")]),
    value = ifelse(world=="Reduced incidence (no delta)" & date.x.x>="2021-07-01", diff_out, value)
  )

# summarize benefits
a_plot = a_out %>% 
  group_by(id, world, prior, risk_diff) %>%
  summarize(benefits = sum(value)) 

ggplot(a_plot, aes(x = risk_diff, y = benefits, group = paste(id, world), lty = world, col = id)) + geom_line() + 
  facet_wrap(.~prior) + 
  g15c_style + 
  theme_minimal() + 
  scale_color_manual("",values = c(g15c_colors[3], g15c_colors[1])) + 
  scale_linetype("") + 
  theme(
    legend.title=element_text(face = "bold"),
    legend.position="bottom",
    legend.text=element_text(size=13),
    axis.text=element_text(size=13),
    #axis.title=element_text(size=15,face="bold"),
    axis.line = element_line(colour = "grey80"),
    strip.text = element_text(size = 15, face = "bold"),
    plot.title = element_text(hjust = 0.5, face="bold"),
    legend.key.width = unit(2, 'cm'),
    strip.background = element_rect(color="white", fill="white", size=1.5, linetype="solid"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank()
  ) + scale_y_continuous(label = comma) +
  scale_linetype("") + 
  labs(x = "Prior on stillbirth risk difference", y = "A priori RCT value ($, m)") +
  geom_segment(x = 0, xend = 0.02, y = 100, yend = 100, lty = 1, col = "darkgrey") +
  geom_text(x = .019, y = 300, label = "$100m", col = "darkgrey")

ggsave(here("2_Output", "covid_priors.png"), width = 12, height = 4.5, units = "in")
