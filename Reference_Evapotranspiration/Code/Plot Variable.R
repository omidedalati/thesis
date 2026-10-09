
# Initial Setting -----------------------------------------------------------------------------

rm(list = ls())

Sys.setlocale(locale = "Persian")

# extrafont::font_import()

extrafont::loadfonts(device = "win")

library(package = "dplyr")
library(package = "ggplot2")
library(package = "lubridate")
library(package = "ggridges")
library(package = "viridis")
library(package = "hrbrthemes")
library(package = "rasterVis")

# Load Data -----------------------------------------------------------------------------------

df <- readRDS(file = "Result/RBSN_Dataset_Khorasan_Daily_1959_2018.rds")

# Create Plot ---------------------------------------------------------------------------------

ST <- c("407440-99999", "407450-99999")
Var <- "tmax"

data <- df %>% 
    dplyr::filter(STNID %in% ST) %>% 
    dplyr::filter(YEAR >= 1368 & YEAR <= 1396) %>% 
    dplyr::select(STNID, YEAR, MONTH, DAY, all_of(Var))

data$STNID  <- factor(x = data$STNID)
data$YEAR  <- factor(x = data$YEAR)
data$MONTH <- factor(x = data$MONTH)
data$DAY   <- factor(x = data$DAY)

lable.x <- "ماه"
lable.y <- "ساعات آفتابی روزانه (ساعت)"
month.name <- c("فروردین",
                "اردیبهشت",
                "خرداد",
                "تیر",
                "مرداد",
                "شهریور",
                "مهر",
                "آبان",
                "آذر",
                "دی",
                "بهمن",
                "اسفند")


P <- data %>% 
    ggplot(mapping = aes_string(x = "MONTH", y = Var)) +
    theme_classic() +
    geom_boxplot(outlier.colour = "red",
                 outlier.fill = "red",
                 outlier.size = 2) +
    xlab(label = "") +
    ylab(label = lable.y) +
    # ylim(c(-10, 50)) +
    scale_x_discrete(breaks = 1:12, labels = month.name) +
    theme(text = element_text(size = 32, family = "B Zar", color = "black"),
          axis.text = element_text(color = "black"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0,
                                     margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
    facet_wrap(~ STNID)

ggsave(filename = paste0(ST, "_", Var, ".jpeg"),
       plot = P,
       device = "jpeg",
       path = "Result/Variable/",
       width = 10,
       height = 8,
       dpi = 600)
    





