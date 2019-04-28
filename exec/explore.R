


library(blackjack)
library(dplyr)


# create data frame with possible player cards and dealer cards
df <- expand.grid(p1 = 1:10,
                  p2 = 1:10,
                  d = 10:1) %>%
  filter(! ((p1 == 1 & p2 == 10) | (p1 == 10 & p2 == 1))) %>%
  mutate(is_same = p1 == p2,
         is_ace = pmin(p1, p2) == 1) %>%
  filter(!duplicated(cbind(p1 + p2, d, is_ace, is_same))) %>%
  arrange(desc(p1 + p2), desc(p1)) %>%
  select(p1, p2, d)

if(file.exists("data/hit_stand_choices.csv")) {
  all_choices <- read.csv("data/hit_stand_choices.csv")
} else {
  all_choices <- NULL
}

for(i in 1:nrow(df)) {
  print(i)
  # check this to not waste time refitting
  is_row_i_done <- any(all_choices$player_card_1 == df$p1[i] &
                         all_choices$player_card_2 == df$p2[i] &
                         all_choices$dealer_card == df$d[i])

  if(!is_row_i_done) {
    hit_stand_choice <- hitStandProbs(p1 = df$p1[i],
                                      p2 = df$p2[i],
                                      d = df$d[i],
                                      n_decks = 6)
    print(hit_stand_choice)
    all_choices <- bind_rows(all_choices, hit_stand_choice)
    write.csv(all_choices, file = "data/hit_stand_choices.csv", row.names = F)
  }
}


# test ggplot function

library(ggplot2)
library(dplyr)

dd <- expand.grid(x = 12:20,
                  y = 1:10) %>%
  mutate(result = rnorm(length(12:20) * length(1:10), mean = x*y))

ggplot(dd, aes(x = x, y = y, colour = result), main = "probs") +
  scale_color_gradient(low="yellow", high="green",
                       limits = c(50, max(dd$result)),
                       na.value = "red") +
  geom_point(size = 10, shape ="square") +
  ggtitle("player win probabilities in blackjack") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(name = "player total", breaks = 12:20) +
  scale_y_continuous(name = "dealer card", breaks = 1:10)






library(profvis)
profvis({
  hit_stand_choice <- hitStandProbs(p1 = 7,
                                    p2 = 6,
                                    d = 4,
                                    n_decks = 6)
})
