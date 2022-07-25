Sys.setlocale("LC_ALL", "English")

# packages ---------------------------------------------------------------------
library(lubridate)
library(tidyverse)

# generate data ----------------------------------------------------------------
population <- tibble(
  x = runif(10000),
  y = runif(10000),
  q = sample(letters[1:2], 10000, replace = TRUE, prob = c(0.25, 0.85)),
  day = sample(1:10, 10000, replace = TRUE)
) %>%
  mutate(day = ymd(paste0("2022-01-", day)))

sample <- slice_sample(population, n = 1000)

max_queries <- sample %>%
  count(day) %>%
  filter(n == max(n)) %>%
  slice(1) %>%
  pull(n)

# prepare plots ----------------------------------------------------------------
population %>%
  ggplot() +
  geom_point(aes(x, y, colour = q)) +
  labs(tag = "A") +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
ggsave("vignettes/plot_sample01.png", height = 5, width = 5)

sample %>%
  ggplot() +
  geom_point(aes(x, y, colour = q)) +
  labs(tag = "B") +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
ggsave("vignettes/plot_sample02.png", height = 5, width = 5)

sample %>%
  ggplot() +
  geom_bar(aes(day, fill = q)) +
  labs(
    x = NULL,
    y = "Number of queries",
    tag = "C"
  ) +
  ylim(c(0, max_queries)) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )
ggsave("vignettes/plot_sample03.png", height = 5, width = 5)

sample %>%
  mutate(q = q == "a") %>%
  group_by(day) %>%
  summarise(share = sum(q) / n()) %>%
  mutate(share = round(100 * share / max(share))) %>%
  ggplot() +
  geom_line(aes(x = day, y = share)) +
  labs(
    x = NULL,
    y = "Google Trends search volume",
    tag = "D"
  ) +
  ylim(0, 100) +
  theme_bw()
ggsave("vignettes/plot_sample04.png", height = 5, width = 5)
