library(remotes)
library(tidyverse)
library(gitsum)

mygit_commit <- parse_log_simple()

write_csv(mygit_commit, path = "mygit_commit.csv")

parse_log_detailed()

library("gitsum")
library("tidyverse")
library("forcats")

init_gitsum()

tbl <- parse_log_detailed() %>%
  select(short_hash, short_message, total_files_changed, nested)
tbl 

tbl$nested[[3]]

log <- parse_log_detailed()
log %>%
  group_by(author_name) %>%
  summarize(n = n())

log %>%
  mutate(weekday = factor(weekday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) %>% 
  ggplot(aes(x = weekday)) + geom_bar() + 
  theme_minimal()

log %>%
  unnest_log() %>%
  mutate(changed_file = fct_lump(fct_infreq(changed_file), n = 10)) %>%
  filter(changed_file != "Other") %>%
  ggplot(aes(x = changed_file)) + geom_bar() + coord_flip() + 
  theme_minimal()

lines <- log %>%
  unnest_log() %>%
  set_changed_file_to_latest_name() %>%
  add_line_history()

r_files <- grep("^R/", lines$changed_file, value = TRUE)

to_plot <- lines %>%
  filter(changed_file %in% r_files) %>%
  add_n_times_changed_file() %>%
  filter(n_times_changed_file >= 10)

ggplot(to_plot, aes(x = date, y = current_lines)) + 
  geom_step() + 
  scale_y_continuous(name = "Number of Lines", limits = c(0, NA)) + 
  facet_wrap(~changed_file, scales = "free_y")

commit.dat <- data.frame(
  edits = rep(c("Insertions", "Deletions"), each = nrow(log)),
  commit = rep(1:nrow(log), 2),
  count = c(log$total_insertions, -log$total_deletions))

ggplot(commit.dat, aes(x = commit, y = count, fill = edits)) + 
  geom_bar(stat = "identity", position = "identity") +  
  theme_minimal()
