library(tidyverse)
library(ggthemes)
library(cowplot)
library(extrafont)
font_import()
postscriptFonts()
loadfonts(device = "all")

# data
d = read_rds("/Users/dafitze/ristretti/GIT/arnie/data/arnie.rds")

# theme
theme_arnie = function() {
  theme_minimal() + #) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.line.x = element_line(linewidth = .4),
      axis.line.y = element_line(linewidth = .4),
      axis.ticks = element_line(),
      text = element_text(size = 10,
                          family = 'Arial'),
      axis.text.x = element_text(size = 10, color = 'black'),
      axis.text.y = element_text(size = 10, color = 'black'),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      plot.title = element_text(size = 25, vjust = 1.9), #family = "BarlowSemiCondensed-Bold"),
      axis.title = element_text(size = 18), #family = "BarlowSemiCondensed-Medium"),
      strip.text = element_text( #family = "BarlowSemiCondensed-Bold",
        size = rel(1), hjust = 0),
      strip.background = element_rect(fill = "grey80", color = NA))
}

# ==============================================
# geom_point with geom smooth
# ==============================================
# -----------------------
# Plot: Source of style
# -----------------------
p1 = d %>%
  filter(!is.na(movie_overall)) |>
  mutate(
    rank = ifelse(movie_overall < median(movie_overall, na.rm = T), 'Bad movies', 'Good movies'),
    rank = factor(rank, levels = c('Good movies','Bad movies')),
    # rank = case_when(movie_overall < 25.5 ~ 'Bad',
    #                  movie_overall > 25.5 & movie_overall < 60 ~ 'Medium',
    #                  movie_overall > 60 ~ 'Good'),
    # rank = factor(rank, levels = c('Good','Medium', 'Bad'))
    ) |>
  ggplot(aes(x = style, y = arnies_performance, linetype = rank, shape = rank)) +
  geom_jitter(size = 3) +
  scale_shape_manual(values=c(1,2, 3)) +
  geom_smooth(method = 'lm', se = F, color = 'black', linewidth = .4) +
  lims(y = c(0,10),
       x = c(0,10)) +
  labs(x = 'Style',
       y = 'Arnies Performance') +
  theme_arnie() +
  theme(
    legend.title = element_blank(),
    legend.position=c(.8,.15)
    # axis.text.x = element_text(angle = 45, vjust = 1.5, hjust=0.8)
  ) +
    scale_x_continuous(breaks = c(0:10)) +
    scale_y_continuous(breaks = c(0:10))
# theme(axis.title.x = element_blank())
p1

# -----------------------
# Plot: Rating
# -----------------------
p2 = d %>%
  # select(year, rotten_tomato_ranking, movie_overall) |>
  # pivot_longer(c(rotten_tomato_ranking, movie_overall)) |>
  ggplot(aes(x = year, y = rotten_tomato_ranking)) +
  geom_point(size = 3, shape = 1) +
  geom_smooth(method = 'lm', se = F, color = 'black', linewidth = .4) +
  lims(y = c(0,10)) +
  labs(x = '',
       y = 'Rotten Tomatoes') +
  theme_arnie() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.4),
        # aspect.ratio = 1.5
        ) +
    scale_y_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10))
# theme(axis.title.x = element_blank())
p2

# -----------------------
# Plot 2:
# -----------------------
p3 = d %>%
  filter(!is.na(movie_overall)) |>
  mutate(
    rank = ifelse(movie_overall < median(movie_overall, na.rm = T), 'Bad', 'Good'),
    rank = factor(rank, levels = c('Good','Bad'))) |>
  ggplot(aes(x = movie_overall, y = arnies_performance)) +
  geom_jitter(size = 3, shape = 1) +
  geom_smooth(method = 'lm', se = F, color = 'black') +
  scale_shape_manual(values=c(1,2, 3)) +
  theme_arnie() +
  theme(
    plot.caption = element_text(hjust = 0)
  ) +
  labs(x = 'Movie Overall',
       y = 'Arnies Performance') +
  lims(x = c(0,10),
       y = c(0,10)) 
# theme(axis.title.x = element_blank())
p3

# ==============================================
# geom_line
# ==============================================
# -----------------------
# Rating
# -----------------------
p4 = d |>
  filter(!is.na(movie_overall)) |>
  mutate(
    rank = ifelse(movie_overall < median(movie_overall, na.rm = T), 'Bad movies', 'Good movies'),
    rank = factor(rank, levels = c('Good movies','Bad movies'))) |>
  pivot_longer(c(arnies_performance, story, style), names_to = 'par', values_to = 'val') |>
  # filter(par == 'style') |>
  ggplot(aes(x = val, linetype = par)) +
  geom_density(show.legend = T, linewidth = .4) +
  labs(x = 'Rating',
       y = '',
       linetype = '',
       caption = '') +
  facet_wrap(~rank, ncol = 1) +
  theme_arnie() +
  theme(axis.text.y = element_blank(),
        legend.position = 'bottom',
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        strip.background = element_rect(color = 'black', fill = NA),
        strip.text = element_text(hjust = .5)) +
    scale_x_continuous(breaks = c(0:10))
p4

# -----------------------
# freqpoly
# -----------------------
p5 = d |>
  group_by(year) |>
  summarise(
    movie_overall = mean(movie_overall, na.rm = T),
    arnies_performance = mean(arnies_performance, na.rm = T),
    story = mean(story, na.rm = T),
    style = mean(style, na.rm = T)
  ) |>
  pivot_longer(-year, names_to = 'par', values_to = 'val') |>
# filter(par %in% c('story')) |>
  ggplot(aes(x = year, y = val)) +
  geom_path(alpha = .6, position = position_dodge(width = 0.5), linewidth = .4) +
  # geom_density(position = 'jitter', alpha = .5) +
  facet_wrap(~par, ncol = 1) +
  theme_arnie() +
  theme(legend.position = 'bottom',
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        strip.background = element_rect(color = NA, fill = NA),
        strip.text = element_text(hjust = .5),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.4)) +
  labs(y = '',
       x = '') 

p5



# -----------------------
# Plot 3: Movies watched per month
# -----------------------
p6 = d %>%
  filter(!is.na(movie_overall)) |>
  mutate(
    rank = ifelse(movie_overall < median(movie_overall, na.rm = T), 'Bad', 'Good'),
    rank = factor(rank, levels = c('Good','Bad'))) |>
  mutate(month_watched = month(date_watched, label = T),
         day_watched = day(date_watched)) |>
  group_by(month_watched) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = month_watched, y = n)) +
  # geom_col() +
  geom_col(fill = 'white', color = 'black') +
  # scale_y_continuous(breaks = c(1:20)) +
  theme_arnie() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6, hjust=0.4)) +
  labs(x = '',
       y = '',
       title = 'Nb. movies watched')
p6

 ggsave('/Users/dafitze/ristretti/GIT/arnie/p1.png', p1, 
              width = 102, 
              height = 180, 
              units = 'mm', 
              dpi = 400)

 ggsave('/Users/dafitze/ristretti/GIT/arnie/p2.png', p2, 
        width = 102, 
        height = 80, 
        units = 'mm', 
        dpi = 400)
 
 ggsave('/Users/dafitze/ristretti/GIT/arnie/p3.png', p4, 
        width = 100, 
        height = 80, 
        units = 'mm', 
        dpi = 400)
 
 ggsave('/Users/dafitze/ristretti/GIT/arnie/p4.png', p5, 
        width = 100, 
        height = 180, 
        units = 'mm', 
        dpi = 400)

plot_grid(p1,p4,p2,p5,p3,p6,
          ncol = 2,
          labels = 'AUTO',
          label_fontfamily = 'mono',
          label_size = 20)

plot_grid(p1,
          p4,
          ncol = 1
          # labels = 'AUTO',
          # label_fontfamily = 'Arial',
          # label_size = 20
          )


plot_grid(p2,
          p5,
          ncol = 1
          # labels = 'AUTO',
          # label_fontfamily = 'Arial',
          # label_size = 20
          )
