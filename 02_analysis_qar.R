# 02_analysis_qar.R
source("setup.R")
vroom("zhvi_us.csv")

# quantile autoregression using 12 month lag
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
