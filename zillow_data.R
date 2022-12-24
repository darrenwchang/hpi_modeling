# read packages
library(tidyverse)
library(vroom)
library(ggplot2)
library(lubridate)
library(viridis)
library(quantreg)
library(scales)

fig_path <- 'hpi_modeling/figures/'

# recessions
recessions <- read_csv(
    "peak, trough
    1948-11-01, 1949-10-01
    1953-07-01, 1954-05-01
    1957-08-01, 1958-04-01
    1960-04-01, 1961-02-01
    1969-12-01, 1970-11-01
    1973-11-01, 1975-03-01
    1980-01-01, 1980-07-01
    1981-07-01, 1982-11-01
    1990-07-01, 1991-03-01
    2001-03-01, 2001-11-01
    2007-12-01, 2009-06-01
    2020-02-01, 2020-04-01")

# home values -- zillow home value index
zhvi <- vroom("https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_month.csv?t=1667088753")

zhvi %>% 
    distinct(RegionName)

zhvi_us <- zhvi %>% 
    pivot_longer(!c(RegionID, SizeRank, RegionName, RegionType, StateName)
        , names_to = 'date'
        , values_to = 'zhvi') %>% 
    filter(RegionID %in% c(102001, 394913, 395057)) %>% 
    mutate(date = as_date(str_c(str_sub(date, 1, 4), str_sub(date, 6, 7), '01', sep = "-"))) %>% 
    group_by(RegionName) %>% 
    mutate(yoy = zhvi / lag(zhvi, 12) - 1) %>% 
    ungroup()

# plot index
ggplot(zhvi_us, aes(date, zhvi, color=RegionName)) +
    geom_line() +
    scale_fill_viridis() +
    theme_minimal() %>% 
    labs(x="", y="", title = "Zillow Home Value Index (raw not SA)"
        , caption = "Source: Zillow @darrenwchang")

# plot yoy growth
ggplot(zhvi_us, aes(date, yoy, color=RegionName)) +
    geom_line() +
    scale_fill_viridis() +
    theme_minimal() +
    scale_y_continuous(labels=scales::percent) +
    labs(x="", y="", title = "Zillow Home Value Index Y/Y Growth"
        , caption = "Source: Zillow @darrenwchang")

# deflate by pce
pce <- tidyquant::tq_get("PCEPI", get="economic.data", from='1996-01-01')
pce <- pce %>% 
    mutate(pce_yoy = price / lag(price, 12) - 1)

zhvi_us <- zhvi_us %>% 
    left_join(pce, on = 'date') %>% 
    mutate(rzhvi = yoy - pce_yoy) %>% 
    group_by(RegionID) %>% 
    mutate(rzhvi12 = lag(rzhvi, 12)) %>% 
    select(date, RegionID, RegionName, zhvi, yoy, pce_yoy, rzhvi, rzhvi12)

# plot yoy growth of real and nominal
p  <- ggplot(zhvi_us, aes(date, color = RegionName)) +
    geom_line(aes(y = rzhvi)) +
    geom_line(aes(y = yoy), lty = 'dashed') +
    geom_line(aes(y = pce_yoy, color = "PCEPI Y/Y"), lwd = 1) + 
    scale_fill_viridis() +
    theme_minimal() +
    scale_y_continuous(labels=scales::percent) +
    theme(legend.title = element_blank(), plot.caption = element_text(hjust = 0)) +
    labs(x="", y="", title = "Zillow Home Value Index Y/Y Growth (Nominal = Dashed)"
        , caption = "Source: Zillow, BEA\nNote: Nominal values deflated by Personal Consumption Expenditures Chain Type Price Index (PCEPI) \n@darrenwchang") +
    geom_rect(data = recessions %>%
                        filter(peak >= '1996-01-01'), 
                    inherit.aes = F, 
                aes(xmin = peak, 
                    xmax = trough, 
                    ymin = -Inf, 
                    ymax = +Inf), 
                    fill='darkgray', 
                    alpha=0.5)

p

ggsave(str_c(fig_path, 'real_zhvi.png'), plot = p, bg = 'white')

# 1. quantile autoregression using 12 month lag
qar <- rq(data=zhvi_us %>% filter(RegionID == 102001), tau = seq(0.05, 0.95, 0.05), rzhvi ~ rzhvi12) %>% 
    broom::tidy()

# estimates and intercept vs tau (quantile)
q <- rq(data=zhvi_us %>% filter(RegionID == 102001), tau = seq(0.05, 0.95, 0.05), rzhvi ~ rzhvi12) %>% 
    broom::tidy() %>% 
    mutate(term=if_else(term=="rzhvi12","Lagged Real HVI",term)) %>%
    ggplot(aes(x=tau,y=estimate))+
    geom_point(color="#27408b")+ 
    geom_ribbon(aes(ymin=conf.low,ymax=conf.high),alpha=0.25, fill="#27408b")+
    geom_line(color="#27408b")+
    theme_minimal()+
    theme(text = element_text(color = "#27408b"))+
    theme(plot.caption=element_text(hjust=0))+
    scale_x_continuous(breaks=seq(0,1,0.1))+
    facet_wrap(~term,scales="free_y",ncol=2)

q

ggsave(str_c(fig_path, 'qar_coeffs.png'), plot = q, bg = 'white')   

# plot predicted based on 25%, 50%, and 75% quantile
pred <- zhvi_us %>% 
    ungroup() %>% 
    mutate(rzhvi_50 = rzhvi * (qar %>% 
                filter(tau == '0.5', term == 'rzhvi12') %>% .$estimate) + (qar %>% filter(tau == '0.5', term == '(Intercept)') %>% .$estimate),
            rzhvi_25 = rzhvi * (qar %>%
                filter(tau == '0.25', term == 'rzhvi12') %>% .$estimate) + (qar %>% filter(tau == '0.25', term == '(Intercept)') %>% .$estimate),
            rzhvi_75 = rzhvi * (qar %>% 
                filter(tau == '0.75', term == 'rzhvi12') %>% .$estimate) + (qar %>% filter(tau == '0.75', term == '(Intercept)') %>% .$estimate)) %>% 
    filter(RegionID == 102001)

# calculate MSE
pred %>%
    mutate(err = abs(rzhvi - rzhvi_50)) %>% 
    summarize(mse = mean(err^2, na.rm=T),
                max_err = max(err, na.rm=T),
                min_err = min(err, na.rm=T))

# create long df for plotting
plot_df <- pred %>% 
    select(-c(RegionID, RegionName, yoy, pce_yoy, rzhvi12, zhvi)) %>% 
    pivot_longer(cols=c('rzhvi', 'rzhvi_50')) %>% 
    mutate(name = if_else(name == 'rzhvi_50', '50%ile', name))

r <- plot_df %>% 
    ggplot(aes(x=date))+
        scale_fill_viridis() +
        geom_ribbon(aes(ymin=rzhvi_25,ymax=rzhvi_75), alpha=0.25, fill = "#27408b") +
        geom_line(aes(y=value, color = name)) +
        theme_minimal()+
        scale_y_continuous(labels=scales::percent) +
        theme(text = element_text(color = "#27408b"))+
        theme(plot.caption=element_text(hjust=0), legend.title = element_blank(), ) +
        labs(x="", y="", title = "Quantile AutoRegression 50%ile Estimated vs Actuals\nZillow Home Value Index Y/Y Growth (Shaded CI = 25/75%ile estimates)"
        , caption = "Source: Zillow, BEA\nNote: Nominal values deflated by Personal Consumption Expenditures Chain Type Price Index (PCEPI) \n@darrenwchang") +
        geom_rect(data = recessions %>%
                            filter(peak >= '1996-01-01'), 
                        inherit.aes = F, 
                    aes(xmin = peak, 
                        xmax = trough, 
                        ymin = -Inf, 
                        ymax = +Inf), 
                        fill='darkgray', 
                        alpha=0.5)

r

ggsave(str_c(fig_path,'qar_zhvi.png'), plot = r, bg = 'white')

# TO-DO: simulation

# 2. functional data analysis

# plot MSAs on the same plot
zhvi_msa <- zhvi %>% 
    pivot_longer(!c(RegionID, SizeRank, RegionName, RegionType, StateName)
        , names_to = 'date'
        , values_to = 'zhvi') %>% 
    filter(RegionType == 'msa') %>% 
    mutate(date = as_date(str_c(str_sub(date, 1, 4), str_sub(date, 6, 7), '01', sep = "-"))) %>% 
    group_by(RegionName) %>% 
    mutate(yoy = zhvi / lag(zhvi, 12) - 1) %>% 
    ungroup()

s <- zhvi_msa %>% 
    ggplot(aes(date, yoy)) +
    scale_fill_viridis() + 
    geom_line(aes(group=RegionID, linewidth=0.1), color = viridis(2)[1], alpha=0.1) +
    geom_smooth(aes(linewidth=1, color = viridis(2)[2])) +
    theme_minimal() +
    labs(x="", y="", title = "Zillow Home Value Index Y/Y Growth (MSA-level)"
        , caption = "Source: Zillow\n@darrenwchang") +
    scale_y_continuous(labels=scales::percent) + 
    theme(legend.position = 'none', legend.title = element_blank(), plot.caption = element_text(hjust = 0))

s

ggsave(str_c(fig_path, 'zhvi_fda_msa.png'), plot = s, bg = 'white')    
