Lab 6, Q3

3. Pick a different scenario to examine.  This could be a shot rate model (e.g.
conditioned on team) but need not be.  For instance, you could construct a 
model that estimates the success rate of a shot from different areas on the
ice, where success is defined as a shot being "on goal" (as opposed to a MISS).

nhl_data<-read.csv("C:\\Users\\18043\\Downloads\\nhl_pbp20162017.csv")

library(tidyverse)

nhl_data<-nhl_data%>%mutate(on_goal = ifelse(Event %in% c("GOAL", "SHOT"), 1, 0))

nhl_data<-nhl_data%>%mutate(goal_diff = ifelse(Event_Team == Home_Team, 
                                               print(Home_Score-Away_Score),
                                               print(Away_Score-Home_Score)))) 

shot_success_model<-lm(on_goal ~ xC + yC + Type + goal_diff, data=nhl_data)

summary(shot_success_model)

ggplot(shots, aes(x = x_coord, y = y_coord, color = on_goal)) +
  geom_point(alpha = 0.5) +
  scale_color_gradient(low = "red", high = "blue") +
  labs(title = "Shot Success by Location", x = "X Coordinate", y = "Y Coordinate")

