# 03_analysis_fda.R
source("setup.R")
library(fda) # note this masks dplyr::select
zhvi <- vroom("https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_month.csv?t=1667088753")

# visualize MSAs in function manner
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

# fit FDA model

# create matrx inputs
z_mat_input <- zhvi_msa %>% 
    dplyr::select(-c(RegionName, RegionType, StateName, zhvi)) %>% 
    drop_na() %>% 
    filter(date >= '2002-01-01') %>% # find method for imputing growth rates
    pivot_wider(names_from = date, values_from = yoy) %>% 
    filter(if_all(everything(), ~!is.na(.))) 
    
# create matrix
z_mat <- z_mat_input %>% 
    dplyr::select(-SizeRank) %>% 
    column_to_rownames("RegionID") %>% 
    as.matrix()

# create size factor
size <- z_mat_input %>% 
    dplyr::select(SizeRank) %>% 
    .$SizeRank

# fit model using FDA pkg
z_mat_fxn <- fd(t(z_mat))
mod <- fRegress(z_mat_fxn ~ size)

# calculated yhat
fitted <- t(as.matrix(mod$yhatfdobj$coefs))
colnames(fitted) <- colnames(z_mat_input)[3:length(colnames(z_mat_input))]
rownames(fitted) <- c()

# calculate residuals
resid <- z_mat - fitted
