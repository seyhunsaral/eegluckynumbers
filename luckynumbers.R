library(tidyr)
library(dplyr)
library(ggplot2)
library(primes)
numbers  <- c(3,7,5,13,5,2,8,4,7)

df  <- tibble(number=numbers)

df <- df  %>%
  mutate(prime = if_else(is_prime(number), "prime", "not prime"))  %>% 
  mutate(even_odd = if_else((number %% 2 == 1), "odd", "even"))  %>%
  mutate(lucky = if_else(number == 7, "lucky", "not lucky"))



df  %>%
  group_by(number)  %>%
  summarise(number_of_times = n())  %>% 
  ggplot(aes(x=number, y=number_of_times)) +
  geom_col() +
  scale_x_continuous(breaks = 1:14) +
  scale_y_continuous(breaks = 1:14) +
  theme_bw()


df  %>%
  ggplot(aes(x="", y=number, fill=prime)) +
  geom_bar(stat="identity") +
  coord_polar("y", start=0) +
  theme_void()

df  %>%
  ggplot(aes(x="", y=number, fill=even_odd)) +
  geom_bar(stat="identity") +
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_manual(values=c("darkred","darkblue"))

df  %>%
  ggplot(aes(x="", y=number, fill=lucky)) +
  geom_bar(stat="identity") +
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_manual(values=c("darkgreen","darkorange"))
