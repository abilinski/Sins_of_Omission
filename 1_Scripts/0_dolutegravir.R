# set up relative paths
library(here)
here::i_am("1_Scripts/0_dolutegravir.R")

# source global options
source("global_options.R")

#### Romo #####
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8808594/

# data extraction: Figure 1
# FLAG -- I take number at risk as different from sample size (i.e. number not yet taking DTG.)
# Can we check on this?
romo <- tribble(~dt, ~female16_49, ~male16_49, ~female50, ~male_50,
                as.Date("2018-05-18"), 65166, 31354, 16875, 12474,
                as.Date("2018-12-01"), 61423, 28161, 15435, 11229,
                as.Date("2019-07-22"), 49695, 15559, 8438, 5606)

# table 3: Table 3
# FLAG -- should the first time point be 2017 or 2018?
romo <- 	tribble(~age_group, ~"1/2/2018", ~"18/5/2018",	~"22/7/2019", ~"31/3/2020",
                 "Females aged 16–49 y", 0,	2.8,	16.2,	29.4,
                 "Males aged 16–49 y", 0, 2.9, 40.4, 57.7,
                 "Females aged ≥50 y",	0, 4.3,	46.4,	62.6,
                 "Males aged ≥50 y",0, 4.3,	49.4,	64.7) %>% 
  pivot_longer(-age_group, names_to = "date", values_to = "rate") %>% 
  mutate(date = as.Date(date, "%d/%m/%Y"))

# uptake plot
romo %>% 
  mutate(ltype = ifelse(age_group=="Females aged 16–49 y", T, F)) %>% 
  ggplot(aes(x = date, y = rate, color = factor(age_group), lty = ltype)) +
  geom_line() +
  g15c_style +
  guides(color = "none", linetype = "none")+
  labs(x = "", y = "Dolutegravir Uptake Rate") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_color_manual(values = c(g15c_colors[1], "black",g15c_colors[5], g15c_colors[3])) +
  annotate("text", label = "Males aged ≥50 y", x = as.Date("2020-04-02"), y = 65.5, color = g15c_colors[5], hjust=0)+
  annotate("text", label = "Females aged ≥50 y", x = as.Date("2020-04-02"), y = 62, color = g15c_colors[1], hjust=0)+
  annotate("text", label = "Males aged 16–49 y", x = as.Date("2020-04-02"), y = 57, color = g15c_colors[3], hjust=0)+
  annotate("text", label = "Females aged 16–49 y", x = as.Date("2020-04-02"), y = 29, color = "black", hjust=0) +
  scale_x_date(limits = c(as.Date("2018-02-01"), as.Date("2020-08-30")), 
               breaks = c(as.Date("2018-01-18"),as.Date("2018-07-18"), as.Date("2019-01-22"),as.Date("2019-07-22"), as.Date("2020-01-01")),
               labels = c("Jan 2018", "July 2018", "Jan 2019", "July 2019", "Jan 2020")) 

ggsave(here("2_Output", "dolutegravir_uptake.png"), width = 9, height = 4.5, units = "in")


# value plot
money <- romo %>% 
  mutate(gap = case_when(date == as.Date("2018-05-18") ~ rate - 2.8,
                         date == as.Date("2019-07-22") ~ rate - 16.2,
                         date == as.Date("2020-03-31") ~ rate - 29.4,
                         T ~ 0),
         n = gap/100*12500000, # FLAG -- where does the 12.5 come from?
         deaths = n * 0.00088,
         costM = deaths * 10) # FLAG -- should this be x8 for VSL?

# FLAG -- One additional note: we don't actually compare to each of these groups in the text.
#         Should we be showing our diff-in-diff here?
money %>% 
  filter(age_group != "Females aged 16–49 y") %>% 
  pivot_wider(id_cols = date, names_from = age_group, values_from = costM)  %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = `Males aged 16–49 y`), color = g15c_colors[5]) +
  geom_line(aes(y = `Females aged ≥50 y`), color = g15c_colors[1]) +
  geom_line(aes(y = `Males aged ≥50 y`), color = g15c_colors[3]) +
  g15c_style + labs(x = "", y = "Social Value of an RCT ($ Millions)") +
  annotate("text", label = "Males aged 16–49 y", x = as.Date("2020-04-01"), y = 31000, color = g15c_colors[5], hjust=0)+
  annotate("text", label = "Females aged ≥50 y", x = as.Date("2020-04-01"), y = 36500, color = g15c_colors[1], hjust=0)+
  annotate("text", label = "Males aged ≥50 y", x = as.Date("2020-04-01"), y = 39000, color = g15c_colors[3], hjust=0) +
  geom_hline(yintercept = 100, color = "black", lty=2) +
  guides(color = "none") +
  geom_ribbon(data = money %>% pivot_wider(id_cols = date, names_from = age_group, values_from = costM),
              aes(x = date, ymin = `Males aged 16–49 y`, ymax = `Males aged ≥50 y`), fill = "grey", alpha = 0.5) +
  scale_x_date(limits = c(as.Date("2018-02-01"), as.Date("2020-08-30")), 
               breaks = c(as.Date("2018-01-18"),as.Date("2018-07-18"), as.Date("2019-01-22"),as.Date("2019-07-22"), as.Date("2020-01-01")),
               labels = c("Jan 2018", "July 2018", "Jan 2019", "July 2019", "Jan 2020")) +
  scale_y_continuous(label = comma)

ggsave(here("2_Output", "dolutegravir_uptake_counterfactual_bounds.png"), width = 9, height = 4.5, units = "in")
