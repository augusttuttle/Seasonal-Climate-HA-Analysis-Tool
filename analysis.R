library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

tz <- "America/New_York"
dir.create("figures", showWarnings = FALSE)

# Hourly long-term statistics exported from Home Assistant.
# readr parses start_utc as a UTC date-time; convert before grouping by day.
indoor <- read_csv("data/indoor_hourly.csv", show_col_types = FALSE) |>
  mutate(
    time    = with_tz(start_utc, tz),
    date    = as_date(time),
    floor   = if_else(grepl("upstairs", entity), "Upstairs", "Main floor"),
    measure = if_else(grepl("humidity", entity), "humidity", "temperature")
  )

daily <- indoor |>
  group_by(date, floor, measure) |>
  summarise(value = mean(mean), .groups = "drop")

# 1. Daily mean temperature on each floor
p1 <- daily |>
  filter(measure == "temperature") |>
  ggplot(aes(date, value, colour = floor)) +
  geom_line() +
  labs(title = "Daily mean indoor temperature", x = NULL, y = "°F", colour = NULL) +
  theme_minimal()
ggsave("figures/1_daily_temperature.png", p1, width = 8, height = 4, dpi = 120)

# 2. How much warmer upstairs is than the main floor, by month
gap <- daily |>
  filter(measure == "temperature") |>
  pivot_wider(names_from = floor, values_from = value) |>
  mutate(gap = Upstairs - `Main floor`, month = month(date, label = TRUE))

p2 <- ggplot(gap, aes(month, gap)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_boxplot(fill = "steelblue", alpha = 0.4) +
  labs(title = "Upstairs minus main floor (daily mean)", x = NULL, y = "°F") +
  theme_minimal()
ggsave("figures/2_upstairs_gap_by_month.png", p2, width = 8, height = 4, dpi = 120)

gap |>
  group_by(month) |>
  summarise(days = n(), mean_gap = round(mean(gap), 2), sd = round(sd(gap), 2)) |>
  print()

# 3. Furnace hours vs heating degree days, one point per full day.
# The *_total sensors are running totals, so the change in `sum` is the usage.
hvac <- read_csv("data/hvac_hourly.csv", show_col_types = FALSE) |>
  mutate(time = with_tz(start_utc, tz), date = as_date(time))

per_day <- hvac |>
  filter(entity %in% c("sensor.furnace_heat_hours_total", "sensor.heating_degree_days_total")) |>
  arrange(entity, time) |>
  group_by(entity) |>
  mutate(used = sum - lag(sum, default = 0)) |>
  group_by(entity, date) |>
  summarise(used = sum(used), hours_seen = n(), .groups = "drop") |>
  filter(hours_seen == 24) |>                       # full days only
  select(-hours_seen) |>
  pivot_wider(names_from = entity, values_from = used) |>
  rename(furnace_h = sensor.furnace_heat_hours_total, hdd = sensor.heating_degree_days_total)

print(per_day)
fit <- lm(furnace_h ~ hdd, data = per_day)
cat(sprintf("\nFurnace hours per heating degree day: %.3f (n = %d days)\n",
            coef(fit)[["hdd"]], nrow(per_day)))

p3 <- ggplot(per_day, aes(hdd, furnace_h)) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "red", se = FALSE) +
  geom_point(size = 3, colour = "steelblue") +
  geom_text(aes(label = format(date, "%b %d")), vjust = -1, size = 3) +
  labs(title = "Furnace runtime vs heating degree days",
       subtitle = sprintf("%d days so far: too few to trust the slope yet", nrow(per_day)),
       x = "Heating degree days (base 65 °F)", y = "Furnace hours") +
  theme_minimal()
ggsave("figures/3_furnace_vs_hdd.png", p3, width = 6, height = 4, dpi = 120)
