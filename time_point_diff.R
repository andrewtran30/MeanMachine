# Load required libraries
library(tidyverse)
library(lubridate)

# Read the data
nhl_data <- read.csv("nhl_pbp20162017.csv")

# Filter for shot events only
shot_data <- nhl_data %>%
  filter(Event %in% c("SHOT", "GOAL", "MISS"))

# Task 1: Shot Rate Model with Time and Point Differential
# Create time variables using the correct column names
shot_data <- shot_data %>%
  mutate(
    # Convert period time to seconds using Time_Elapsed
    period_seconds = Seconds_Elapsed - ((Period - 1) * 1200),
    # Create time blocks (4 per period)
    time_block = cut(period_seconds, 
                     breaks = c(0, 300, 600, 900, 1200),
                     labels = c("0-5 min", "5-10 min", "10-15 min", "15-20 min"))
  )

# Calculate point differential for the shooting team
shot_data <- shot_data %>%
  group_by(Game_Id) %>%
  mutate(
    home_score = cumsum(ifelse(Event == "GOAL" & Ev_Team == Home_Team, 1, 0)),
    away_score = cumsum(ifelse(Event == "GOAL" & Ev_Team == Away_Team, 1, 0)),
    point_diff = ifelse(Ev_Team == Home_Team, 
                        home_score - away_score, 
                        away_score - home_score)
  ) %>%
  ungroup()

# Fit Poisson regression model
model1 <- glm(count ~ time_block + point_diff, 
              family = poisson(link = "log"), 
              data = shot_data %>% 
                group_by(time_block, point_diff) %>% 
                summarize(count = n()))

summary(model1)

# Task 2: Shot Rate Model with Spatial Location
# Prepare data for spatial analysis
spatial_data <- shot_data %>%
  filter(!is.na(xC) & !is.na(yC))

# Approach 1: Define regions on the ice
spatial_data <- spatial_data %>%
  mutate(
    # Define x regions (3 regions)
    x_region = cut(xC, 
                   breaks = c(-100, -25, 25, 100),
                   labels = c("Defensive", "Neutral", "Offensive")),
    # Define y regions (2 regions)
    y_region = cut(yC, 
                   breaks = c(-50, 0, 50),
                   labels = c("Left", "Right")),
    # Combined region
    ice_region = paste(x_region, y_region)
  )

# Fit spatial Poisson model using regions
model2 <- glm(count ~ ice_region, 
              family = poisson(link = "log"), 
              data = spatial_data %>% 
                group_by(ice_region) %>% 
                summarize(count = n()))

summary(model2)

# Task 3: Shot Success Rate Model
success_data <- shot_data %>%
  filter(!is.na(xC) & !is.na(yC)) %>%
  mutate(
    # Define success as shot being on goal (not a MISS)
    success = ifelse(Event != "MISS", 1, 0),
    # Calculate distance from net
    net_x = ifelse(xC > 0, 89, -89),
    distance = sqrt((xC - net_x)^2 + yC^2),
    # Calculate angle (in radians)
    angle = abs(atan2(yC, net_x - xC))
  )

# Fit logistic regression model
model3 <- glm(success ~ distance + angle + poly(distance, 2) + poly(angle, 2),
              family = binomial(link = "logit"),
              data = success_data)

summary(model3)

# Visualize predicted probabilities
grid_data <- expand.grid(
  distance = seq(0, 100, by = 5),
  angle = seq(0, pi/2, length.out = 10)
)

grid_data$pred_prob <- predict(model3, newdata = grid_data, type = "response")

# Plot
ggplot(grid_data, aes(x = distance, y = angle, fill = pred_prob)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Predicted Probability of Shot on Goal",
       x = "Distance from Net (ft)",
       y = "Angle (radians)",
       fill = "Probability") +
  theme_minimal()
