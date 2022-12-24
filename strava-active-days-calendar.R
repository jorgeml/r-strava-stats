library(ggplot2)
library(lubridate)
library(tidyverse)
library(rStrava)
library(dotenv)

load_dot_env(file = ".env")

# import Strava data

app_name <- Sys.getenv("APP_NAME") # chosen by user
app_client_id  <-
  Sys.getenv("CLIENT_ID") # an integer, assigned by Strava
app_secret <-
  Sys.getenv("CLIENT_SECRET") # an alphanumeric secret, assigned by Strava
athlete_id <- Sys.getenv("ATHLETE_ID")

# create the authentication token
stoken <-
  httr::config(token = strava_oauth(app_name, app_client_id, app_secret, app_scope =
                                      "activity:read_all"))

#function to produce calendar
get_calendar <- function(start_date, end_date) {
  n_days <- interval(start_date, end_date) / days(1)
  date <- start_date + days(0:n_days)
  month_name <- format(date, "%B")
  month_num <- format(date, "%m")
  year <- format(date, "%Y")
  day_num <- format(date, '%d')
  day <- wday(date, label = TRUE)
  week_num <- strftime(date, format = "%V")
  cal <-
    data.frame(date, year, month_name, month_num, day_num, day, week_num)
  cal[cal$week_num >= 52 & cal$month_num == "01", "week_num"] = 00
  
  week_month <- cal %>%
    group_by(year, month_name, week_num) %>%
    summarise() %>%
    mutate(week_month_num = row_number())
  
  cal <-
    merge(
      cal,
      week_month,
      by = c(
        "month_name" = "month_name",
        "week_num" = "week_num",
        "year" = "year"
      )
    )
  cal$month_name <-
    factor(
      cal$month_name,
      levels = c(
        "January",
        "February",
        "March",
        "April",
        "May",
        "June",
        "July",
        "August",
        "September",
        "October",
        "November",
        "December"
      )
    )
  cal$day <-
    factor(cal$day,
           levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
  
  return(cal)
  
}

#create date range
start_date <- as.Date('2022-01-01')
end_date <- as.Date('2022-12-31')

#create calendar
cal <- get_calendar(start_date, end_date)

my_acts <-
  get_activity_list(stoken, before = end_date, after = start_date) %>% compile_activities()

my_acts$workout_type <- as.factor(my_acts$workout_type)
my_acts$start_date <- as.Date(my_acts$start_date)
my_acts$commute <- as.logical(my_acts$commute)
my_acts$trainer <- as.logical(my_acts$trainer)
my_acts$type <- as.factor(my_acts$type)

my_cycling_acts <- my_acts %>%
  filter(type %in% c("Ride", "EBikeRide"))

totalKm <- my_cycling_acts$distance %>% sum() %>% as.integer()
totalHours <- my_cycling_acts$elapsed_time %>% sum() %/% (60 * 60)
totalActivities <- my_cycling_acts %>% nrow()

#summarise workout information
workout_by_day <- my_cycling_acts %>%
  group_by(start_date) %>%
  summarise(
    workouts = n(),
    workout_min = sum(moving_time) / 60,
    commute,
    trainer,
    workout_type,
    type
  ) %>%
  rename(date = start_date) %>%
  mutate(
    ride_type = case_when(
      workout_type == 11 ~ "Race",
      commute == TRUE ~ "Commute",
      trainer == TRUE ~ "Indoor",
      type == "EBikeRide" ~ "E-bike",
      TRUE ~ "Other"
    )
  ) %>%
  arrange(date)

#create a factor out of class types
workout_by_day$ride_type <-
  factor(
    workout_by_day$ride_type,
    levels = c("Commute", "Race", "Indoor", "E-bike", "Other"),
    ordered = TRUE
  )

# Summarise the workout type of the day.
workout_by_day <-
  workout_by_day %>% summarise(day_workout_type = min(ride_type)) %>% mutate(did_workout =
                                                                               1)

#merge workout info summary with calendar, left join to preserve all calendar days (all.x=TRUE)
cal_workout <-
  merge(cal,
        workout_by_day,
        by = c("date" = "date"),
        all.x = TRUE)

#custom color palette
pal <- c('#26547c', "#fc5200", '#ef476f', '#FFBC1F',  '#05C793')

subtitle <-
  paste0(
    "This year I cycled ",
    totalActivities,
    " times, riding ",
    totalKm,
    " km over ",
    totalHours,
    " hours"
  )

#creating the plot
ggplot(cal_workout) +
  geom_tile(mapping = aes(x = day, y = week_month_num), fill = NA) +
  geom_text(
    mapping = aes(x = day, y = week_month_num, label = day_num),
    color = "black",
    family = "Roboto Slab Light"
  ) +
  geom_point(
    data = cal_workout %>% filter(did_workout == 1),
    mapping = aes(x = day, y = week_month_num, color = day_workout_type),
    size = 8
  ) +
  geom_text(
    data = cal_workout %>% filter(did_workout == 1),
    mapping = aes(x = day, y = week_month_num, label = day_num),
    color = "white",
    family = "Roboto Slab Light"
  ) +
  scale_y_reverse() +
  scale_color_manual(
    values = pal,
    guide = guide_legend(
      title.position  = "top",
      title.hjust = 0.5,
      title = "Ride Type"
    )
  ) +
  coord_fixed() +
  labs(
    y = "",
    x = "",
    title = 'My Cycling in 2022',
    subtitle = subtitle,
    caption = "Data from Strava API | Chart by @jorgeml, original work by @tanya_shapiro"
  ) +
  facet_wrap( ~ month_name) +
  theme(
    text = element_text(family = "Roboto Slab"),
    legend.position = "top",
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(
      hjust = 0.5,
      family = "Roboto Slab ExtraBold",
      size = 18
    ),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(size=8),
    legend.key = element_blank(),
    legend.spacing.x = unit(0.5, 'cm'),
    plot.margin = unit(c(0.8, 0, 0.4, 0), "cm"),
  )
