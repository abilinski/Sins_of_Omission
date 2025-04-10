# set up relative paths
#rm(list=ls())
library(here)
here::i_am("1_Scripts/0_dolutegravir.R")

# source global options
source("global_options.R")

# read helper data
df = read_excel("./0_Data/1_Parameter_Estimates/table_s1_r1.xlsx")
df = df[!is.na(df$Code),]

# helpers
vsl = as.numeric(df$Value[df$Code=="VSL"])
DTG.perc.red  = as.numeric(df$Value[df$Code=="DTG.perc.red"])
DTG.n.tot.ART  = as.numeric(df$Value[df$Code=="DTG.n.tot.ART"])
#### Romo #####
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8808594/

# table 3: Table 3
romo <- 	tribble(~age_group, ~"1/2/2017", ~"18/5/2018",	~"22/7/2019", ~"31/3/2020",
                 "Females aged 16–49 y", 0,	2.8,	16.2,	29.4,
                 "Males aged 16–49 y", 0, 2.9, 40.4, 57.7,
                 "Females aged ≥50 y",	0, 4.3,	46.4,	62.6,
                 "Males aged ≥50 y",0, 4.3,	49.4,	64.7,
                 "Trend-matched counterfactual", 0, 4.3-(4.3-2.9), 46.4-(49.4-40.4), 62.6-(64.7-57.7)) %>% 
  pivot_longer(-age_group, names_to = "date", values_to = "rate") %>% 
  mutate(date = as.Date(date, "%d/%m/%Y"))

# uptake plot
romo %>% 
  mutate(
    ltype = ifelse(age_group == "Females aged 16–49 y", TRUE, FALSE),
    lwd_val = ifelse(age_group %in% c("Females aged 16–49 y", "Trend-matched counterfactual"), 0.75, 0.25)
  ) %>% 
  ggplot(aes(x = date, y = rate, color = factor(age_group), lty = ltype, size = lwd_val)) +
  geom_line() +
  g15c_style +
  guides(color = "none", linetype = "none", size = "none") +
  labs(x = "", y = "Dolutegravir Uptake (%)") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_size_identity() +
  scale_color_manual(values = c(g15c_colors[4], "black", g15c_colors[5], g15c_colors[3], g15c_colors[1])) +
  annotate("text", label = "Males aged ≥50", x = as.Date("2020-04-02"), y = 65.5, color = g15c_colors[5], hjust = 0) +
  annotate("text", label = "Females aged ≥50", x = as.Date("2020-04-02"), y = 62, color = g15c_colors[4], hjust = 0) +
  annotate("text", label = "Males aged 16–49", x = as.Date("2020-04-02"), y = 58, color = g15c_colors[3], hjust = 0) +
  annotate("text", label = "Trend-matched\nreference", x = as.Date("2020-04-02"), y = 53, color = g15c_colors[1], hjust = 0) +
  annotate("text", label = "Females aged 16–49", x = as.Date("2020-04-02"), y = 29, color = "black", hjust = 0) +
  scale_x_date(
    limits = c(as.Date("2017-01-02"), as.Date("2020-10-30")), 
    breaks = c(as.Date("2017-01-18"), as.Date("2017-07-18"), as.Date("2018-01-18"),
               as.Date("2018-07-18"), as.Date("2019-01-22"), as.Date("2019-07-22"),
               as.Date("2020-01-01")),
    labels = c("Jan 2017", "July 2017", "Jan 2018", "July 2018", "Jan 2019", "July 2019", "Jan 2020")
  )
ggsave(here("2_Output", "dolutegravir_uptake.png"), width = 9, height = 4.5, units = "in")


# value plot
money <- romo %>% 
  mutate(gap = case_when(date == as.Date("2018-05-18") ~ rate - 2.8,
                         date == as.Date("2019-07-22") ~ rate - 16.2,
                         date == as.Date("2020-03-31") ~ rate - 29.4,
                         T ~ 0),
         n = gap/100*DTG.n.tot.ART*1000000, 
         deaths = n * DTG.perc.red,
         costM = deaths * vsl) 

money %>% 
  filter(age_group != "Females aged 16–49 y") %>% 
  pivot_wider(id_cols = date, names_from = age_group, values_from = costM)  %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = `Males aged 16–49 y`), color = g15c_colors[5], size = .25) +
  geom_line(aes(y = `Females aged ≥50 y`), color = g15c_colors[4], size = .25) +
  geom_line(aes(y = `Males aged ≥50 y`), color = g15c_colors[3], size = .25) +
  geom_line(aes(y = `Trend-matched counterfactual`), color = g15c_colors[1], size = .75) +
  g15c_style + labs(x = "", y = "Health Value ($, millions) of an RCT") +
  annotate("text", label = "Males aged 16–49", x = as.Date("2020-04-01"), y = 28000, color = g15c_colors[5], hjust=0)+
  annotate("text", label = "Females aged ≥50", x = as.Date("2020-04-01"), y = 33000, color = g15c_colors[4], hjust=0)+
  annotate("text", label = "Males aged ≥50", x = as.Date("2020-04-01"), y = 35000, color = g15c_colors[3], hjust=0) +
  annotate("text", label = "Trend-matched\nreference", x = as.Date("2020-04-01"), y = 24500, color = g15c_colors[1], hjust=0) +
  geom_hline(yintercept = 100, color = "black", lty=2) +
  guides(color = "none") +
  geom_ribbon(data = money %>% pivot_wider(id_cols = date, names_from = age_group, values_from = costM),
              aes(x = date, ymin = `Trend-matched counterfactual`, ymax = `Males aged ≥50 y`), fill = "grey", alpha = 0.5) +
  scale_x_date(limits = c(as.Date("2017-01-02"), as.Date("2020-10-30")), 
               breaks = c(as.Date("2017-01-18"),as.Date("2017-07-18"), as.Date("2018-01-18"),as.Date("2018-07-18"), as.Date("2019-01-22"),as.Date("2019-07-22"), as.Date("2020-01-01")),
               labels = c("Jan 2017", "July 2017", "Jan 2018", "July 2018", "Jan 2019", "July 2019", "Jan 2020")) +
  scale_y_continuous(label = comma)

ggsave(here("2_Output", "dolutegravir_uptake_counterfactual_bounds.png"), width = 9, height = 4.5, units = "in")
