# 01_import_eda.R
source("setup.R")

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

vroom_write(zhvi_us, "zhvi_us.csv")
