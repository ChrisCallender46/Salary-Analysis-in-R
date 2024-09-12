data <- Data.Science.Job_Listing

my_data <- data %>%                            # Select certain columns
  select("Location",
         "Salary")

my_data <- my_data %>%                            # Rename columns
  rename(location = "Location",
         salary = "Salary")

my_data <- my_data %>%                            # Replace values by NA
  mutate(salary = if_else(str_detect(salary, "Per Hour"), NA, salary))

my_data <- my_data %>%                            # Convert range to mean
  mutate(salary = str_extract_all(salary, "\\d+(?:\\.\\d+)?") %>%
           map(~ as.numeric(.x)) %>%
           map_dbl(~ mean(.x)))

my_data <- my_data%>%                             # Remove NA rows
  drop_na(salary)

my_data <- my_data %>%                            # Extract states
  mutate(location = sub(".*, ", "", location))

my_data <- my_data %>%                            # Remove certain rows
  filter(location != "United States")

my_data %>%                                       # Draw ggplot2 density plot
  ggplot(aes(x = salary)) +
  geom_density() +
  labs(title = "Density Plot of Data Science Salaries in Thousands")

my_data %>%                                       # Draw ggplot2 boxplot
  ggplot(aes(x = salary)) +
  geom_boxplot()

my_data %>%                                       # Grouped boxplot
  ggplot(aes(x = salary,
             fill = location)) +
  geom_boxplot()

my_data %>%                                       # Filter data by count
  group_by(location) %>%
  filter(n() >= 10) %>%
  ungroup() %>% 
  ggplot(aes(x = salary,
             fill = location)) +
  geom_boxplot()

my_data %>%                                       # Ordered boxplot
  group_by(location) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(location = fct_reorder(location, salary, .fun = median)) %>% 
  ggplot(aes(x = salary,
             fill = location)) +
  geom_boxplot()

my_data %>%                                       # Modify boxplot layout
  group_by(location) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(location = fct_reorder(location, salary, .fun = median)) %>% 
  ggplot(aes(x = salary,
             fill = location)) +
  geom_boxplot() +
  labs(title = "Data Science Salary by States in Thousands",
       x = NULL) +
  theme(legend.title = element_blank(),
        axis.text.y = element_blank())