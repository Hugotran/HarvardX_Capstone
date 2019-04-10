# Lubridate package

# Dates in a data frame
hosts ,- tibble::tribble( ~host, ~bday, ~premiere,
                          "Mary", "24 Marck 1935", "August 17th, 2010",
                          "Paul", "1 March 1966", "August 17th, 2010")

hosts

# "bday" and "premiere" are character, and we want them to be date. In order to do that we parse the "bday" and "premiere" to date use "lubridate" package.

hosts <- hosts %>%
  mutate(bday = dmy(bday),
         premiere = mdy(premiere))

hosts

# Types of timespans
# Create an time interval

hosts <- hosts %>% mutate(age_int = interval(bday, premiere))

# Converting units of timespans
years(1)

hosts %>% mutate(year_decimal = age_int/years(1),
                 years_whole = age_int %/% years(1))

hosts %>% mutate(age_y = age_int/years(1),
                 age_m = age_int %/% months(12))

# Add a line to extract labeled month

# Add a line to your code to create a new variable called last_month_us, with the month of each bakers' last date appeared in the US as a character string such as "Jan."
baker_dates_cast <- baker_dates %>% 
  mutate(last_date_appeared_us = dmy(last_date_appeared_us),
         last_month_us = month(last_date_appeared_us, label = TRUE))

# Make bar chart by last month
# Use a bar chart to plot the frequencies of bakers by last_month_us- in which month have we said goodbye to the most bakers? 
baker_dates_cast %>% ggplot(aes(last_month_us)) + geom_bar()

#Calculate timespans
#As we saw in the video, the first step to calculating a timespan in lubridate is to make an interval, then use division to convert the units to what you want (like weeks(x) or months(x)). The x refers to the number of time units to be included in the period. In this exercise, you'll calculate timespans with the baker_time data we just made.
# Create a new interval called time_on_air, calculated as the timespan between each baker's first and last date that they appeared in the UK.
#Add another line to your mutate() call to create another variable called weeks_on_air, which converts time_on_air to the number of weeks (time unit = 1).
#Add another line to your mutate() call to create another variable called months_on_air, which stores the number of months each baker was "on air" in whole numbers.
baker_time <- baker_time  %>% 
  mutate(time_on_air = interval(first_date_appeared_uk, last_date_appeared_uk),
         weeks_on_air = time_on_air / weeks(1),
         months_on_air = time_on_air %/% months(1))
# Here again, lubridate gives you many options for working with periods from years down to picoseconds. You may also end up needing to work with durations too, which have similar functions that always start with a d from dyears to dpicoseconds.

#parse_number

series5 <- series5 %>% separate(about, into = c("age", "occupation"), sep = ", ") %>%
                                  mutate(age = parse_number(age))
# String basics

series5 <- series5 %>%
  mutate(baker = str_to_upper(baker),
         showstopper = str_to_lower(showstopper))
# string detect and return logical variable
series5 %>%
  mutate(pie = str_detect(showstopper, "pie"))

# Replace string patterns
series5 %>% mutate(showstopper = str_replace(showstopper, "pie", "tart"))

# Remove string patterns
series5 %>% 
  mutate(showstopper = str_remove(showstopper, "pie"))
# Trim white space
series5 %>% 
  mutate(showstopper = str_remove(showstopper, "pie"),
         showstopper = str_trim(showstopper))



