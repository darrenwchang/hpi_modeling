# 03_analysis_fda.R
source("setup.R")
vroom("zhvi_us.csv")

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
