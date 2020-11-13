### create package hex

# packages ---------------------------------------------------------------------
library(globaltrends)
library(tidyverse)

# parameters -------------------------------------------------------------------
start_db()

# load globaltrends data -------------------------------------------------------
data_doi <- export_doi(control = 1, locations = "countries") %>%
  filter(type == "score_obs" & locations == "countries") %>%
  select(name = keyword, date, gini)
disconnect_db()

# prepare plot -----------------------------------------------------------------
names_doi <- c(
  "fox",
  "geert hofstede",
  "michael porter",
  "walt disney"
)

data_doi %>%
  filter(name %in% names_doi) %>%
  ggplot() +
  geom_line(aes(x = date, y = gini, colour = name), size = 2) +
  theme_void() +
  theme(legend.position = "none") +
  scale_color_manual(values=c("firebrick", "forestgreen", "goldenrod1", "blue4"))

# save plot -------------------------------------------------------------------
ggsave("hex-globaltrends-raw.jpg", width = 28.51, height = 19, unit = "cm", dpi = 600)
