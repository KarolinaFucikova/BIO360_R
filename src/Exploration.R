# another bare-bones script that the students should be able to make on their own
# using just prompts in handout

# import seed data
seeds <- read.csv("data/seeds.csv")

hist(seeds$maple2)
hist(seeds$maple)

mean(seeds$maple2)
median(seeds$maple2)

# quick aside: mode does not produce the number for the actual mode:
mode(seeds$maple2)
# it is more complicated than that, probably unnecessary for us at this point:
as.numeric(names(which.max(table(seeds$maple))))

colMeans(seeds, na.rm = TRUE)
apply(seeds, MARGIN=2, FUN=median, na.rm = TRUE)
apply(seeds, MARGIN=1, FUN=median, na.rm = TRUE)

seeds[12,4]
fewer_seeds <- seeds[1:5,1:3]
fewer_seeds

max(seeds$maple2)
min(seeds$maple2)
max(seeds$maple2) - min(seeds$maple2)
range(seeds$maple2)
summary(seeds$maple2)
quantile(seeds$maple2)
sd(seeds$maple2)

apply(seeds, MARGIN=2, FUN = sd, na.rm = TRUE)

shapiro.test(seeds$maple2)
