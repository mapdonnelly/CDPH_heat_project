d %>% 
  filter(day %in% seq(1,365,30)[-13]) %>% 
  ggplot(aes(lon, lat, color = temp)) + 
  geom_point() + 
  facet_wrap(~day, nrow = 3) + 
  scale_color_viridis_c() +
  theme_void()

