library(tidyverse)

# Countries of interest
countries <- c("China", "India", "United States", "Canada", "France")

co2_ndc <- co2 %>%
  filter(country %in% countries) %>%
  select(country, iso_code, year,
         co2, gdp, co2_per_gdp,
         total_ghg_excluding_lucf)

### China ----
china <- co2_ndc %>% filter(country == "China")

china_intensity <- china %>%
  filter(!is.na(co2_per_gdp)) %>%
  mutate(
    base_intensity_2005 = co2_per_gdp[year == 2005],
    reduction_vs_2005 = (1 - co2_per_gdp / base_intensity_2005) * 100
  )

china_intensity %>%
  filter(year == max(year)) %>%
  select(year, co2_per_gdp, reduction_vs_2005)

ggplot(china_intensity, aes(year, reduction_vs_2005)) +
  geom_hline(yintercept = 65, linetype = "dashed", color = "red") +
  geom_line(color = "goldenrod") +
  labs(
    title = expression(China~~CO[2]~intensity~reduction~vs~2005),
    y = "Reduction vs 2005 (%)",
    x = NULL
  )

ggplot(china, aes(year, co2 / 1000)) +
  geom_line(color = "goldenrod") +
  labs(
    title = expression(China~~CO[2]~emissions~(Gt)),
    y = expression(Gt~CO[2]),
    x = NULL
  )

china %>% filter(year >= 2000) %>%
  arrange(year) %>%
  summarise(
    peak_year = year[which.max(co2)],
    peak_co2  = max(co2, na.rm = TRUE) / 1000
  )

### India ----
india <- co2_ndc %>% filter(country == "India")

india_intensity <- india %>%
  filter(!is.na(co2_per_gdp)) %>%
  mutate(
    base_intensity_2005 = co2_per_gdp[year == 2005],
    reduction_vs_2005 = (1 - co2_per_gdp / base_intensity_2005) * 100
  )

india_intensity %>%
  filter(year == max(year)) %>%
  select(year, co2_per_gdp, reduction_vs_2005)

ggplot(india_intensity, aes(year, reduction_vs_2005)) +
  geom_hline(yintercept = 45, linetype = "dashed", color = "red") +
  geom_line(color = "steelblue") +
  labs(
    title = expression(India~~CO[2]~intensity~reduction~vs~2005),
    y = "Reduction vs 2005 (%)",
    x = NULL
  )


### US ----
us <- co2_ndc %>% filter(country == "United States")

# Get 2005 GHG baseline
us_base_2005 <- us %>%
  filter(year == 2005) %>%
  pull(total_ghg_excluding_lucf)

us_target_2030_low  <- us_base_2005 * (1 - 0.50)
us_target_2030_high <- us_base_2005 * (1 - 0.52)

us_ghg <- us %>%
  filter(!is.na(total_ghg_excluding_lucf))

# Check latest year vs target
us_ghg %>%
  filter(year == max(year)) %>%
  mutate(
    target_low  = us_target_2030_low,
    target_high = us_target_2030_high
  ) %>%
  select(year, total_ghg_excluding_lucf, target_low, target_high)

ggplot(us_ghg, aes(year, total_ghg_excluding_lucf / 1000)) +
  geom_line(color = "purple") +
  geom_hline(yintercept = us_target_2030_low / 1000,
             linetype = "dashed", color = "red") +
  geom_hline(yintercept = us_target_2030_high / 1000,
             linetype = "dashed", color = "red") +
  labs(
    title = expression(United~States~~GHG~emissions~vs~2030~NDC),
    y = expression(Gt~CO[2]*e~(excl.~LULUCF)),
    x = NULL
  )

### Canada ----
canada <- co2_ndc %>% filter(country == "Canada")

can_base_2005 <- canada %>%
  filter(year == 2005) %>%
  pull(total_ghg_excluding_lucf)

can_target_2030_low  <- can_base_2005 * (1 - 0.40)
can_target_2030_high <- can_base_2005 * (1 - 0.45)

can_ghg <- canada %>%
  filter(!is.na(total_ghg_excluding_lucf))

can_ghg %>%
  filter(year == max(year)) %>%
  mutate(
    target_low  = can_target_2030_low,
    target_high = can_target_2030_high
  ) %>%
  select(year, total_ghg_excluding_lucf, target_low, target_high)

ggplot(can_ghg, aes(year, total_ghg_excluding_lucf / 1000)) +
  geom_line(color = "red") +
  geom_hline(yintercept = can_target_2030_low / 1000,
             linetype = "dashed", color = "red") +
  geom_hline(yintercept = can_target_2030_high / 1000,
             linetype = "dashed", color = "red") +
  labs(
    title = expression(Canada~~GHG~emissions~vs~2030~NDC),
    y = expression(Gt~CO[2]*e~(excl.~LULUCF)),
    x = NULL
  )


### France ----
france <- co2_ndc %>% filter(country == "France")

fr_base_1990 <- france %>%
  filter(year == 1990) %>%
  pull(total_ghg_excluding_lucf)

fr_target_2030 <- fr_base_1990 * (1 - 0.50)

fr_ghg <- france %>%
  filter(!is.na(total_ghg_excluding_lucf))

fr_ghg %>%
  filter(year == max(year)) %>%
  mutate(target_2030 = fr_target_2030) %>%
  select(year, total_ghg_excluding_lucf, target_2030)

ggplot(fr_ghg, aes(year, total_ghg_excluding_lucf / 1000)) +
  geom_line(color = "darkgreen") +
  geom_hline(yintercept = fr_target_2030 / 1000,
             linetype = "dashed", color = "red") +
  labs(
    title = expression(France~~GHG~emissions~vs~2030~SNBC~target),
    y = expression(Gt~CO[2]*e~(excl.~LULUCF)),
    x = NULL
  )

