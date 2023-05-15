library(tidyverse)
library(tidyquant)
library(broom)
library(umap)

# STOCK PRICES
#sp_500_prices_tbl

# SECTOR INFORMATION
#sp_500_index_tbl


#Step 1
sp_500_daily_returns_tbl <- sp_500_prices_tbl %>% 
  select(symbol, date, adjusted) %>% 
  filter(date >= '2018-01-01') %>% 
  group_by(symbol) %>% 
  mutate(lag = lag(adjusted)) %>% 
  na.omit %>% 
  mutate(pct_return = (adjusted - lag)/lag) %>% 
  select(symbol, date, pct_return)

#Step 2
stock_date_matrix_tbl <- sp_500_daily_returns_tbl %>% 
  spread(date, pct_return) %>% 
  replace(is.na(.),0) %>% 
  ungroup()

#Step 3
kmeans_obj <- stock_date_matrix_tbl %>% 
  within(rm("symbol")) %>% 
  kmeans(centers = 4, nstart = 20)

#Step 4
kmeans_mapper <- function(center = 3) {
  stock_date_matrix_tbl %>%
    select(-symbol) %>%
    kmeans(centers = center, nstart = 20)
}

k_means_mapped_tbl <- tibble(centers = 1:30) %>% 
  mutate(k_means = centers %>%  map(kmeans_mapper)) %>% 
  mutate(glance = k_means %>% map(glance)) %>% 
  
k_means_mapped_tbl %>% 
  unnest(glance) %>% 
  select(centers, tot.withinss) %>% 
  # Visualization
  ggplot(aes(centers, tot.withinss)) +
  geom_point(color = "#2DC6D6", size = 4) +
  geom_line(color = "#2DC6D6", size = 1) +
  # Add labels (which are repelled a little)
  ggrepel::geom_label_repel(aes(label = centers), color = "#2DC6D6") + 
  
  # Formatting
  labs(title = "Skree Plot")

#Step 5
umap_results <- stock_date_matrix_tbl %>% 
  select(-symbol) %>% 
  umap()

umap_results_tbl <- umap_results$layout %>% 
  as_tibble(.name_repair = "unique") %>% 
  set_names(c("x", "y")) %>%
  bind_cols(stock_date_matrix_tbl %>% select(symbol)) 

umap_results_tbl %>% 
  ggplot(aes(x, y), ) +
  geom_point(alpha = 0.5) + 
  theme_tq() + 
  ggrepel::geom_label_repel(aes(label = symbol), size = 3) + 
  labs(title = "UMAP Projection")

#Step 6
kmeans_3_obj <- k_means_mapped_tbl %>%
  pull(k_means) %>%
  pluck(3)

umap_kmeans_results_tbl <- kmeans_3_obj %>% 
  augment(stock_date_matrix_tbl) %>% 
  select(symbol, .cluster) %>% 
  left_join(umap_results_tbl) %>% 
  left_join(sp_500_index_tbl %>% select(symbol, company, sector))
  
umap_kmeans_results_tbl %>% 
  ggplot(aes(x, y, color = .cluster)) +
  
  # Geometries
  geom_point(alpha = 0.5) +
  ggrepel::geom_label_repel(aes(label = symbol), size = 2, fill = "#282A36") +
  
  # Formatting
  scale_color_manual(values=c("#2d72d6", "#2dc6d6", "#2dd692")) + 
  theme(legend.position = "none")

  

  