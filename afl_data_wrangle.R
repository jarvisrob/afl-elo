
rating.time.series.run %>%
  rownames_to_column("season_rnd") %>% 
  mutate(idx = seq(1, nrow(.))) %>% 
  separate(season_rnd, c("season", "rnd"), sep = " ") %>% 
  gather(
    adelaide, 
    brisbane.bears, 
    brisbane.lions, 
    carlton, 
    collingwood, 
    essendon, 
    fitzroy, 
    fremantle, 
    geelong, 
    gold.coast, 
    greater.western.sydney, 
    hawthorn, 
    melbourne, 
    north.melbourne, 
    port.adelaide, 
    richmond, 
    st.kilda, 
    sydney, 
    university, 
    west.coast.eagles, 
    western.bulldogs, 
    key = "team", 
    value = "rating"
  ) %>% 
  write_csv("./out/rating_time_series_to_2019-r12.csv")
