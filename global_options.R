#*********************************** GLOBAL OPTIONS ***************************************#
#                                                                                          #
#                                                                                          #
#******************************************************************************************#

#### KNITR GLOBAL OPTIONS ####
knitr::opts_chunk$set(echo = F, message = F, warning = F)

#### LIBRARIES ####
for (library_name in c("tidyverse", "lubridate", "ISOweek", "knitr", "here", "tictoc", 
                       "scales", "kableExtra", "car",  "data.table", "doMC", "sandwich",
                       "lmtest", "lfe", "pwr", "statmod", "truncnorm", "gtools", "beepr",
                       "ggpubr", "ggthemes", "RColorBrewer", "ggrepel",
                       "janitor", "xtable", "TrialSize", "EQUIVNONINF", "readxl",
                       "DescTools", "data.table", "kableExtra")) {
  library_name
  if (!require(library_name,character.only=TRUE)) {
    install.packages(library_name, character.only=TRUE)
  }
  library(library_name, character.only=TRUE)
}
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
options(lfe.eps=1e-6)
# beep if there's an error
#options(error = beep)

# ggplot theme and color options
theme_opts = theme_minimal() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(face = "bold"))
  
pal = c("#457b9d", "#449187", "#52bdd3", "#a8ccb4")
col = "#0b2358"

# other options
size = 12
font = 3
lim = 8

# parallelization
detectCores()
doMC::registerDoMC(cores = detectCores())
foreach::getDoParWorkers()


options("scipen" =100, "digits" = 4) # override R's tendency to use scientific notation
# Style settings: color-blind-friendly and simple graphs
g15c_colors <- c("#0072B2", #sea blue
                 "#999999", #grey
                 "#009E73", #greeny blueish
                 "#56B4E9", #sky
                 "#D55E00", #vermillion
                 "#F0E442", #yellow
                 "#E69F00", #orange
                 "#CC79A7", #pinkish red   
                 "#000000", #black
                 "#9932CC", #darkorchid
                 "tomato1", 
                 "seagreen",
                 "red4",
                 "royalblue",
                 "palevioletred4"
)
g15c_linetypes <- c("solid", "dashed", "longdash", "dotted", "dotdash", "twodash",  "F1", "4C88C488", "12345678")
g15c_shapes <- c(16, 17, 18, 15, 1, 2, 5, 0, 8, 3)

theme_g15c <- function () { 
  theme_bw(base_size=15) %+replace% 
    theme(
      legend.position="bottom",
      #legend.title=element_blank(),
      legend.text=element_text(size=13),
      axis.text=element_text(size=13),
      #axis.title=element_text(size=15,face="bold"),
      axis.line = element_line(colour = "grey80"),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      strip.text = element_text(size = 15, face = "bold"),
      plot.title = element_text(hjust = 0.5, face="bold"),
      panel.border = element_blank(),
      legend.key.width = unit(2, 'cm'),
      strip.background = element_rect(color="white", fill="white", size=1.5, linetype="solid")
    )
}

g15c_style <- list(theme_g15c(), 
                   scale_color_manual("", values = g15c_colors), 
                   scale_fill_manual("", values = g15c_colors),
                   scale_shape_manual(values = g15c_shapes), 
                   scale_linetype_manual(values = g15c_linetypes))
                   #scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                                 # scientific = FALSE))) #scale_y_continuous(labels = comma)



g15c_style_cont <- list(theme_g15c(), 
                        scale_color_gradient2(low = "peachpuff", mid = "darkred", high = "black"), 
                        scale_fill_gradient2(low = "peachpuff", mid = "darkred", high = "black"))

theme_g15c_blank <- function () { 
  theme_hc(base_size=14, base_family="sans") %+replace% 
    theme(
      legend.title=element_blank(),
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
    )
}
g15c_style_no_grid <- list(theme_g15c_blank(), scale_color_manual(values = g15c_colors),
                           scale_fill_manual(values = g15c_colors), 
                           scale_shape_manual(values = g15c_shapes), 
                           scale_linetype_manual(values = g15c_linetypes),
                           scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                                          scientific = FALSE))) #scale_y_continuous(labels = comma)

theme_g15c_with_legend <- function () { 
  theme_hc(base_size=14, base_family="sans") %+replace% 
    theme(
      legend.position = "right",
      axis.line = element_line(colour = "grey80"),
      panel.grid.major.x = element_line(colour = "#F7F7F7"),
      panel.grid.major.y = element_line(colour = "#F7F7F7")
    )
}
g15c_style_with_legend <- list(theme_g15c_with_legend(), 
                               scale_color_manual(values = g15c_colors), 
                               scale_fill_manual(values = g15c_colors), 
                               scale_shape_manual(values = g15c_shapes), 
                               scale_linetype_manual(values = g15c_linetypes),
                               scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                                              scientific = FALSE))) #scale_y_continuous(labels = comma)

####============================= Factor Grid lines: annotate ================================####

ann_y <- annotate("segment", x=-Inf, xend= -Inf, y=-Inf, yend= Inf, colour = "grey80", linewidth = 1) 
ann_x <- annotate("segment", x=-Inf, xend=  Inf, y=-Inf, yend= -Inf, colour = "grey80", linewidth = 1) 
ann_x_date <- annotate("segment", x=as.Date(-Inf), xend= as.Date( Inf), y=-Inf, yend= -Inf, colour = "grey80", linewidth = 1) 
ann_y_date <- annotate("segment", x=as.Date(-Inf), xend= as.Date(-Inf), y=-Inf, yend= Inf, colour = "grey80", linewidth = 1) 

####============================= Setting NAs ================================####

# set so always see NAs if they exist
table = function (..., useNA = 'ifany') base::table(..., useNA = useNA)

####------------------------- Summarise Fit ---------------------------------####
summarise_fit <- function(fit, fit_type) {
  if(length(coef(summary(fit))[,1]) == 1) {
    return(tibble(coef = coef(summary(fit))[,1],
                  se = coef(summary(fit))[,2],
                  pval = coef(summary(fit))[,4],
                  fit_type = fit_type,
                  input = rownames(coef(summary(fit))),
                  stars = case_when(coef(summary(fit))[,4] <= 0.01 ~ "***",
                                    coef(summary(fit))[,4] <= 0.05 ~ "**",
                                    coef(summary(fit))[,4] <= 0.1 ~ "*",
                                    T  ~ ""),
                  N = fit$N,
                  res_line1 = sprintf("%2.3f%s", coef, stars),
                  res_line2 = sprintf("(%2.3f)", se))) 
  } else {
    return(tibble(coef = coef(summary(fit))[,1],
                  se = coef(summary(fit))[,2],
                  pval = coef(summary(fit))[,4],
                  fit_type = fit_type,
                  input = names(coef(summary(fit))[,1]),
                  stars = case_when(coef(summary(fit))[,4] <= 0.01 ~ "***",
                                    coef(summary(fit))[,4] <= 0.05 ~ "**",
                                    coef(summary(fit))[,4] <= 0.1 ~ "*",
                                    T  ~ ""),
                  N = fit$N,
                  res_line1 = sprintf("%2.3f%s", coef, stars),
                  res_line2 = sprintf("(%2.3f)", se),
                  tab_line1 = sprintf(paste0("%2.3f","$^{","%s", "}$"),coef, stars)) )  
  }
}

