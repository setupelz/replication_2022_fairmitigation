# Replication code for: Fairness considerations in Global Mitigation Investments
# Shonali Pachauri, Setu Pelz, Christoph Bertram, Silvie Kreibiehl, Narasimha D. Rao, 
# Youba Sokona and Keywan Riahi, (forthcoming, Science)

# Regional characteristic data processing --------------------------------------
# Setu Pelz (TISS, ECE, IIASA, pelz@iiasa.ac.at)

# LOAD PACKAGES ----------------------------------------------------------------

#install.packages("pacman")
library(pacman)

#tidyverse
p_load(dplyr, tidyr, readr, readxl, writexl, stringr, purrr, lubridate, 
       patchwork, forcats, ggplot2, ggrepel)

# misc data   
p_load(WDI, countrycode)

# misc
p_load(here)

# REGIONAL GROUPING ------------------------------------------------------------

# Take country-region R10 groupings from IPCC emissions dataset for consistency
rgniso <- 
  read_xlsx(here("Data", "Characteristics", "Responsibility", "AR6WGIII", 
                              "essd_ghg_data_gwp100.xlsx"), sheet = 5) %>% 
  select(iso3c = ISO, name, region = region_ar6_10)

# Standard region ordering for figures and setting name abbreviations
rgnorder <- tibble(
  # Necessary to match up differing R10 long-names in distinct datasets
  regionold  = c("South-East Asia and Pacific", "Middle East",
              "Australia, Japan, New Zealand", "Africa", "Southern Asia", 
              "Latin America and Caribbean", "Eastern Europe and West-Central Asia", 
              "Eastern Asia", "Europe", "North America"),
  region = c("South-East Asia and developing Pacific", "Middle East",
             "Asia-Pacific Developed", "Africa", "Southern Asia", 
             "Latin America and Caribbean", "Eastern Europe and West-Central Asia", 
             "Eastern Asia", "Europe", "North America"),
  regionshort = c("SAP", "MEA", "APD", "AFR", "SAS", "LAC", "EEA", "EAS", "EUR", "NAM"))

# Add short region names to country-region R10 groupings
rgniso <- full_join(rgniso, rgnorder, by = c("region" = "regionold")) %>% 
  select(iso3c, name, region = region.y, regionshort) %>% 
  mutate(regionshort = factor(regionshort, levels = rgnorder$regionshort),
         region = factor(region, levels = rgnorder$region)) %>% 
  arrange(regionshort, name)

# Export cleaned country-region grouping for appendices
rgniso %>% 
  group_by(regionshort, region) %>% 
  summarise(iso3c = paste(iso3c, collapse = ", "),
            name = paste(name, collapse = ", ")) %>% 
  ungroup() %>% 
  write_csv(here("Manuscript", "Tables", "appendix_rgniso.csv"))

# CAP --------------------------------------------------------------------------

# Population and GDP (WDI) -----------------------------------------------------

# See https://data.worldbank.org
#
# Set population and GDP indicator names (in $2017 international PPP) from WDI
popgdp_indicators <- tibble(
  long = c("totalpop", "gdp2017ppp"), 
  indicator = c("SP.POP.TOTL", "NY.GDP.MKTP.PP.KD"))

# Source data from WDI (requires active internet connection to retrieve original)
# popgdp_iso3c <- WDI::WDI(country = rgniso$iso3c, indicator = popgdp_indicators$indicator)
# write_csv(popgdp_iso3c, here("Data", "Characteristics", "Capability", "popgdp_iso3c_wdiextract.csv"))
popgdp_iso3c <- read_csv(here("Data", "Characteristics", "Capability", "popgdp_iso3c_wdiextract.csv"))

# Transform iso2c to iso3c, set variable names
popgdp_iso3c <- popgdp_iso3c %>% 
  mutate(iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
         # Correct iso3c code for Turkiye
         iso3c = ifelse(country == "Turkiye", "TUR", iso3c)) %>% 
  rename(!!!tibble::deframe(popgdp_indicators)) %>% 
  select(iso3c, year, popgdp_indicators$long)%>% 
  full_join(rgniso %>% select(-region))

# Check which iso3c are missing in WDI data for the year 2019
miss_popgdp <- popgdp_iso3c %>% 
  filter(year == 2019, (is.na(gdp2017ppp) | is.na(totalpop))) %>% 
  arrange(name) %>% 
  pull(name) %>% 
  unique()

# After checking missing iso3c above, confirm least-worst option is to remove
# these for our regional analysis purposes, and then aggregate to regional level
popgdp_r10 <- popgdp_iso3c %>% 
  filter(!is.na(totalpop), !is.na(gdp2017ppp)) %>% 
  # Aggregate population and GDP to regional level
  group_by(regionshort, year) %>% 
  summarise(across(c(totalpop, gdp2017ppp), ~sum(.))) %>% 
  # Calculate regional GDP per capita
  mutate(gdp2017pppcapita = gdp2017ppp / totalpop) %>% 
  # Set region ordering
  mutate(regionshort = factor(regionshort, levels = rgnorder$regionshort)) %>% 
  arrange(regionshort) %>% 
  ungroup()

# Visualise
popgdp_r10 %>% 
  filter(year >= 1990, year <= 2019) %>% 
  select(year, regionshort, gdp2017ppp, gdp2017pppcapita) %>% 
  mutate(gdp2017ppp = gdp2017ppp / 1e12) %>% 
  rename("GDP (1e12 $PPP 2017)" = gdp2017ppp,
         "GDP per capita ($PPP 2017)" = gdp2017pppcapita) %>% 
  pivot_longer(-c(regionshort, year)) %>% 
  mutate(name = factor(name, levels = c("GDP (1e12 $PPP 2017)", 
                                        "GDP per capita ($PPP 2017)"))) %>% 
  ggplot(aes(year, value, colour = regionshort, group = regionshort, label = regionshort)) +
  geom_line(show.legend = F) +
  geom_text_repel(data = . %>% filter(year == last(year)), 
                  direction = "y", hjust = 0, show.legend = F,
                  size = 3) +
  facet_wrap(~name, scales = "free_y", ncol = 2) +
  labs(x = NULL, y = NULL, 
       title = "Selected World Development Indicators (WDI)",
       subtitle = "IPCC R10 aggregation calculated summing country totals. Per Capita aggregates subsequently calculated.",
       caption = 
       "SAP - South-East Asia and developing Pacific, MEA - Middle East, APD - Asia-Pacific Developed, AFR - Africa, SAS - Southern Asia, 
       LAC - Latin America and Caribbean, EEA - Eastern Europe and West-Central Asia, EAS - Eastern Asia, EUR - Europe, NAM - North America") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Extract population, GDP and GDP per capita in 2019
equity_popgdp <- popgdp_r10 %>% 
  filter(year == 2019) %>% 
  rename_with(.cols = c(gdp2017pppcapita), ~paste0("CAP_",.)) %>% 
  select(regionshort, year, matches("CAP"))

# Write population and GDP in 2019 for later use
popgdp_r10 %>% filter(year == 2019) %>% 
  write_csv(here("Data", "Characteristics", "Capability", "popgdp_r10.csv"))

# Remove unnecessary variable(s)
rm(popgdp_indicators)

# Capital stock (Penn World Tables) --------------------------------------------

# See Feenstra, Inklaar, & Timmer (2015). https://doi.org/10.1257/aer.20130954
# www.ggdc.net/pwt
# 
capstock <- 
  read_xlsx(here("Data", "Characteristics", "Capability", "Capital", 
                           "PWT_100capitalstock.xlsx"), sheet = 3) %>%
  # cn = 'Capital stock at current PPPs (in mil. 2017US$)'
  select(iso3c = countrycode, year, capstock2017ppp = cn) %>% 
  filter(year >= 1990)

# Check mislabeled iso3c codes in pwt dataset (none)
misslabel_capstock <- full_join(capstock %>% distinct(iso3c), rgniso) %>%
  filter(is.na(name))

# Add population and regional country groupings for aggregation
capstock <- capstock %>%
  mutate(year = as.numeric(year)) %>%
  full_join(rgniso %>% select(-region)) %>% 
  full_join(popgdp_iso3c)

# Check and report which iso3c missing for the year 2019 in PWT data
miss_capstock <- capstock %>%
  filter((year == 2019 | is.na(year)) & (is.na(capstock2017ppp) | is.na(totalpop))) %>%
  filter(!is.na(name)) %>% 
  arrange(name) %>% 
  pull(name) %>% 
  unique()

# After checking missing iso3c above, confirm least-worst option is to remove
# for our regional analysis purposes and aggregate to regional level
capstock_r10 <- capstock %>%
  filter(!is.na(capstock2017ppp), !is.na(totalpop)) %>%
  # Aggregate to regional level
  group_by(regionshort, year) %>%
  summarise(capstock2017ppp = sum(capstock2017ppp, na.rm = T),
            totalpop = sum(totalpop)) %>%
  ungroup() %>% 
  mutate(
    # Original data in 1e6, convert to 1e12 (trillion $) for visualisation
    capstock2017ppp = capstock2017ppp / 1e6,
    # Calculate capstock per capita in $PPP2017
    capstock2017pppcapita = capstock2017ppp * 1e12 / totalpop,
    regionshort = factor(regionshort, levels = rgnorder$regionshort))

# Visualise
capstock_r10 %>%
  select(-totalpop) %>% 
  rename("Capital stock (1e12 PPP 2017)" = capstock2017ppp,
         "Capital stock per capita ($PPP 2017)" = capstock2017pppcapita) %>%
  filter(year >= 1990) %>%
  pivot_longer(-c(regionshort, year)) %>%
  ggplot(aes(year, value, colour = regionshort, group = regionshort, label = regionshort)) +
  geom_line(show.legend = F) +
  geom_text_repel(data = . %>% filter(year == last(year)),
                  direction = "y", hjust = 0, show.legend = F,
                  size = 3) +
  scale_x_continuous(breaks = seq(1990,2020,5)) +
  facet_wrap(~name, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        strip.text = element_text(size = 8)) +
  labs(title = "Capital stock in 2017 PPP USD (Penn World Tables)",
       subtitle = "IPCC R10 aggregation calculated summing country totals. Per Capita aggregates subsequently calculated.",
       x = NULL,
       y = NULL,
       caption = 
         "SAP - South-East Asia and developing Pacific, MEA - Middle East, APD - Asia-Pacific Developed, AFR - Africa, SAS - Southern Asia, 
       LAC - Latin America and Caribbean, EEA - Eastern Europe and West-Central Asia, EAS - Eastern Asia, EUR - Europe, NAM - North America")

# Extract capital stock in 2019
equity_capstock <- capstock_r10 %>%
  select(year, regionshort, capstock2017pppcapita) %>% 
  filter(year == 2019) %>%
  arrange(regionshort) %>%
  ungroup() %>%
  rename_with(.cols = capstock2017pppcapita, ~paste0("CAP_",.))

rm(misslabel_capstock)

# RES --------------------------------------------------------------------------

# Labeling and ordering of selected cumulative emissions categories
emisslabels <- tibble(
  indicator = c("emiss_1850co2", "emiss_1990co2"),
  indicatorlong = c("1850_CO2FFI", "1990_CO2FFI"))

# Emissions 1850 (AR6) ---------------------------------------------------------

# See IPCC AR6 SPM.2B, https://doi.org/10.48490/g19x-6k84 
#
# Cumulative emissions since 1850, CO2-FFI, excluding LULUCF
#
# Note that this comes from the AR6 WGIII dataset which provides cumulative 
# regional CO2-FFI emissions excluding LULUCF for IPCC R10.
emiss_1850co2 <- 
  read_xlsx(here("Data", "Characteristics", "Responsibility", "AR6WGIII", 
                 "ipcc_ar6_figure_spm_2b_archive.xlsx")) %>% 
  # remove cumulative LULUCF and int. air and shipping emissions
  filter(source == "CO2-FFI",
         region_ar6_10 != "Int. Aviation and Shipping") %>% 
  transmute(region = region, 
            emiss_1850co2 = co2_cumulative) %>% 
  full_join(rgnorder %>% select(-regionold)) %>% 
  select(-region)  %>% 
  mutate(regionshort = factor(regionshort, levels = rgnorder$regionshort)) %>% 
  arrange(regionshort)

# Emissions 1990 (AR6) ---------------------------------------------------------

# See IPCC AR6 Ch2, https://zenodo.org/record/6483002#.YtAU5HZBxD8 
#
# Cumulative emissions since 1990, CO2-FFI excluding LULUCF
#
emiss_1990co2 <- read_xlsx(here("Data", "Characteristics", "Responsibility", 
                                "AR6WGIII", "essd_ghg_data_gwp100.xlsx"),
                           sheet = 3)

# List all sectors for transparency and then select desired (i.e. exclude AFOLU)
emiss_1990co2_allsec <- c("Buildings", "Energy systems", "Industry", "Transport", "AFOLU")
emiss_1990co2_secdes <- c("Buildings", "Energy systems", "Industry", "Transport")
emiss_1990co2 <- emiss_1990co2 %>% filter(sector_title %in% emiss_1990co2_secdes)

# Next, ensure all emissions associated with the regions SEA and AIR (int. shipping 
# / aviation) are removed. 
emiss_1990co2 <- emiss_1990co2 %>% filter(!ISO %in% c("SEA", "AIR"))
  
# Join with IPCC R10 regions
emiss_1990co2 <- emiss_1990co2 %>% 
  select(iso3c = ISO, year = year, CO2) %>% 
  filter(year >= 1990) %>% 
  full_join(rgniso)  %>% 
  mutate(regionshort = factor(regionshort, levels = rgnorder$regionshort)) %>% 
  arrange(regionshort)

# Check which iso3c are missing in AR6 GWP100 emissions data
miss_1990co2 <- emiss_1990co2 %>% filter(is.na(CO2)) %>% 
  arrange(name) %>% 
  pull(name) %>% 
  unique()

# After checking missing iso3c above, confirm least-worst option is to remove
# for our regional analysis purposes and aggregate to regional level
emiss_1990co2 <- emiss_1990co2 %>% 
  filter(!is.na(CO2)) %>% 
  # Aggregate and transform original data in tons of CO2 to Gigatons of CO2
  group_by(regionshort) %>% 
  summarise(emiss_1990co2 = sum(CO2 / 1e9)) %>% 
  ungroup()  %>% 
  mutate(regionshort = factor(regionshort, levels = rgnorder$regionshort)) %>% 
  arrange(regionshort)

# Combine emissions data -------------------------------------------------------

# Create equity indicator dataframe
equity_emiss <- 
  list(emiss_1850co2, emiss_1990co2) %>% 
  reduce(left_join, by = "regionshort") %>% 
  select(regionshort, everything()) %>% 
  mutate(regionshort = factor(regionshort, levels = rgnorder$regionshort)) %>% 
  arrange(regionshort) %>% 
  ungroup() %>% 
  rename_with(.cols = matches("emiss"), ~paste0("RES_",.))

# Visualise    
equity_emiss %>% 
  pivot_longer(-regionshort) %>% 
  separate(name, into = c("PRN", "indicator"), sep = "_", extra = "merge") %>% 
  left_join(emisslabels) %>% 
  ggplot(aes(regionshort, value, fill = indicatorlong)) +
  geom_col(position = "dodge") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = NULL,
       title = "Cumulative global emissions at different starting years for different categories.",
       subtitle = "IPCC R10 aggregation calculated summing country totals.",
       caption = 
         "SAP - South-East Asia and developing Pacific, MEA - Middle East, APD - Asia-Pacific Developed, AFR - Africa, SAS - Southern Asia, 
       LAC - Latin America and Caribbean, EEA - Eastern Europe and West-Central Asia, EAS - Eastern Asia, EUR - Europe, NAM - North America",
       fill = "Data")

# NED --------------------------------------------------------------------------

# Climate exposure (IIASA Hotspots explorer) -----------------------------------

# See Byers et al. (2018), https://doi.org/10.1088/1748-9326/aabf45 
# https://hotspots-explorer.org/
#
exposure <- read_xlsx(here("Data", "Characteristics", "Needs", "Vulnerability", 
                           "ByersHotspots", "IIASA_Hotspots_Explorer_Data.xlsx"),
                      sheet = "data_Hotspots_ISOs_Pop_Vul") %>%
  filter(Scenario %in% c("SSP1_1p5", "SSP1_2p0", 
                         "SSP2_1p5", "SSP2_2p0", 
                         "SSP3_1p5", "SSP3_2p0"), 
         Variable %in% c("Multisector|EV20", "Multisector|Exposed_population"),
         Unit == "people",
         Region != "World") %>%
  select(iso3c = Region, scen = Scenario, var = Variable, `2030`) %>% 
  pivot_longer(-c(iso3c, scen, var), names_to = "year", values_to = "exppop") %>% 
  mutate(year = as.numeric(year)) %>% 
  separate(scen, into = c("scen", "temp"), sep = "_")

ssppop <- read_csv(here("Data", "Characteristics", "Needs", "Vulnerability", 
                         "ByersHotspots", "SspDb_country_data_2013-06-12.csv")) %>%
  filter(MODEL == "IIASA-WiC POP", 
         SCENARIO %in% c("SSP1_v9_130115", "SSP2_v9_130115", "SSP3_v9_130115"), 
         VARIABLE == "Population") %>% 
  select(iso3c = REGION, scen = SCENARIO, `2030`) %>% 
  mutate(across(-c(iso3c,scen), ~ . * 1e6))  %>% 
  pivot_longer(-c(iso3c, scen), names_to = "year", values_to = "estpop") %>% 
  mutate(year = as.numeric(year),
         scen = strtrim(scen, 4))

exp_r10 <- rgniso %>% 
  full_join(exposure) %>%
  full_join(ssppop)
  
# Check and report which iso3c missing for the year hotspots or population data
miss_exp <- exp_r10 %>% filter(is.na(exppop) | is.na(estpop)) %>% 
  arrange(name) %>% 
  pull(name) %>% 
  unique()

# After checking missing iso3c above, confirm least-worst option is to remove
# for our regional analysis purposes and aggregate to regional level
exp_r10 <- exp_r10 %>% 
  filter(!is.na(exppop), !is.na(estpop)) %>% 
  group_by(scen, temp, var, year, regionshort) %>%
  summarise(exppop = sum(exppop), estpop = sum(estpop)) %>%
  ungroup() %>% 
  mutate(expshare = exppop / estpop) %>% 
  arrange(regionshort, scen, year)

# Sensitivity - population exposure and vulnerability
exp_r10 %>%
  ggplot(aes(regionshort, expshare, fill = factor(scen))) +
  scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0,0.6)) +
  geom_col(position = "dodge") +
  theme_bw() +
  facet_wrap(fct_rev(var)~temp) +
  labs(x = NULL, y = "Share of regional population", fill = "SSP",
       title = "Sensitivity analysis - population exposed to climate risks through water-land-energy sectors (IIASA Hotspots)",
       subtitle = "IPCC R10 aggregation calculated as sum of exposed / total population. ",
       caption = 
         "SAP - South-East Asia and developing Pacific, MEA - Middle East, APD - Asia-Pacific Developed, AFR - Africa, SAS - Southern Asia, 
       LAC - Latin America and Caribbean, EEA - Eastern Europe and West-Central Asia, EAS - Eastern Asia, EUR - Europe, NAM - North America
       EV20| indicates exposed & income deprived at 20USD/capita/day.")

# Sensitivity
exp_r10 %>%
  left_join(popgdp_r10 %>% filter(year == 2019) %>% select(regionshort, gdp2017ppp)) %>% 
  group_by(scen, temp, var, year) %>% 
  mutate(exppenaltyfunc = ifelse(expshare < 0.01, log(1/0.01), log(1/expshare)),
         fscontrib = exppenaltyfunc * gdp2017ppp / sum(exppenaltyfunc * gdp2017ppp)) %>% 
  ggplot(aes(regionshort, fscontrib, fill = factor(scen))) +
  scale_y_continuous(breaks = seq(0,0.35,0.05), limits = c(0,0.35)) +
  geom_col(position = "dodge") +
  theme_bw() +
  facet_wrap(fct_rev(var)~temp) +
  labs(x = NULL, y = "FS", fill = "SSP",
       title = "Sensitivity analysis - FS contribution to annual mitigation needs",
       caption = 
         "SAP - South-East Asia and developing Pacific, MEA - Middle East, APD - Asia-Pacific Developed, AFR - Africa, SAS - Southern Asia, 
       LAC - Latin America and Caribbean, EEA - Eastern Europe and West-Central Asia, EAS - Eastern Asia, EUR - Europe, NAM - North America
       EV20| indicates exposed & income deprived at 20USD/capita/day.")

# Simple linear interpolation of exposure between 1p5 and 2p0 for middle of the 
# road SSP2 at the end of our analysis period - the year 2030. We select 
# Multisector|EV20 as we the indicator should reflect not just exposure but also
# vulnerability and therefore acute climate risk.
equity_exp <- exp_r10 %>%
  filter(var == "Multisector|EV20", scen == "SSP2") %>% 
  group_by(regionshort) %>% 
  summarise(NED_expshare2030_1p75 = mean(expshare))

# Decent living standards (IIASA DLE article) ----------------------------------

# See Kikstra et al. (2021), https://doi.org/10.1088/1748-9326/ac1c27
#
dls <- read_csv(here("Data", "Characteristics", "Needs", "Development", "DLS_v1.csv")) %>% 
  select(iso3c = iso, everything(), -country_name)

dls <- dls %>% 
  full_join(rgniso) %>% 
  full_join(popgdp_iso3c %>% filter(year == 2015) %>% select(iso3c, totalpop))

# Check and report which iso3c missing in DLS or WDI data
miss_dls <- dls %>% filter(is.na(Housing) | (is.na(totalpop))) %>% 
  arrange(name) %>% 
  pull(name) %>% 
  unique()

# After checking missing iso3c above, confirm least-worst option is to remove those missing
dls <- dls %>% 
  select(-name, -region) %>% 
  filter(!(is.na(totalpop) | is.na(Housing)))

# Country aggregation of DLS deprivation indicators taking average across all indicators
dls <- dls %>% 
  group_by(regionshort, iso3c, totalpop) %>% 
  rowwise() %>% 
  summarise(dlsdep = mean(c_across(Transport:Heating)))

# Regional aggregation weighted by population
dls_r10 <- dls %>% 
  ungroup() %>% 
  group_by(regionshort) %>% 
  summarise(dlsdepshare = weighted.mean(dlsdep, w = totalpop)) %>% 
  mutate(regionshort = factor(regionshort, levels = rgnorder$regionshort))

# Visualise
dls_r10 %>% 
  ggplot(aes(regionshort, dlsdepshare)) +
  geom_col() +
  scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0,0.6)) +
  geom_text(aes(label = regionshort), angle = 90, hjust = 0, size = 3) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(x = NULL, y = "Average of the shares of deprived populations",
       title = "Deprivation of decent living standards (2015)",
       subtitle = "IPCC R10 aggregation calculated average share of deprived population across all DLS dimensions, weighted by population",
       caption = 
         "SAP - South-East Asia and developing Pacific, MEA - Middle East, APD - Asia-Pacific Developed, AFR - Africa, SAS - Southern Asia, 
       LAC - Latin America and Caribbean, EEA - Eastern Europe and West-Central Asia, EAS - Eastern Asia, EUR - Europe, NAM - North America")

equity_dls <- dls_r10 %>% 
  rename_with(.cols = matches("dls"), ~paste0("NED_",.))

# AGGREGATE EQUITY MEASURES ----------------------------------------------------

equity <- list(equity_emiss,
               equity_dls,
               equity_exp,
               equity_popgdp,
               equity_capstock
               ) %>% 
  reduce(left_join, by = "regionshort") %>%
  select(-matches("year")) %>% 
  mutate(regionshort = factor(regionshort, rgnorder$regionshort)) %>% 
  select(regionshort, matches("RES"), matches("CAP"), matches("NED"))

write_csv(equity, here::here("Manuscript", "Tables", "equity_raw.csv"))

# Save missing indicators table
miss_tbl <- tibble(miss_1990co2 = paste(miss_1990co2, collapse = "; "),
                   miss_capstock = paste(miss_capstock, collapse = "; "),
                   miss_dls = paste(miss_dls, collapse = "; "),
                   miss_exp = paste(miss_exp, collapse = "; "),
                   miss_popgdp = paste(miss_popgdp, collapse = "; ")) %>% 
  pivot_longer(everything(), names_to = "indicator", values_to = "countries") %>% 
  mutate(countries = iconv(countries, "UTF-8"))

write_xlsx(miss_tbl, here("Manuscript", "Tables", "appendix_missingindicators.xlsx"))
