# Replication code for: Fairness considerations in Global Mitigation Investments
# Shonali Pachauri, Setu Pelz, Christoph Bertram, Silvie Kreibiehl, Narasimha D. Rao, 
# Youba Sokona and Keywan Riahi, (forthcoming, Science)

# Main analysis ----------------------------------------------------------------
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

# LOAD DATA --------------------------------------------------------------------

# Set region ordering for figures and name abbreviations
rgnorder <- tibble(
  region = c("South-East Asia and developing Pacific", "Middle East",
             "Asia-Pacific Developed", "Africa", "Southern Asia", 
             "Latin America and Caribbean", "Eastern Europe and West-Central Asia", 
             "Eastern Asia", "Europe", "North America"),
  regionshort = c("SAP", "MEA", "APD", "AFR", "SAS", "LAC", "EEA", "EAS", "EUR", "NAM"))

# Load processed regional population and GDP (2019)
popgdp <- read_csv(here("Data", "Characteristics", "Capability", "popgdp_r10.csv"))

# Load processed regional equity consideration indicator characteristics
equityraw <- read_csv(here("Manuscript", "Tables", "equity_raw.csv")) %>% 
  mutate(regionshort = factor(regionshort, levels = rgnorder$regionshort))

# Load annual mitigation investment needs, WG3 AR6 TS25
investTS25R10 <- read_xlsx(here("Data", "InvestmentNeeds", "Koenig", 
                                "TS.25_Data.xlsx")) %>% 
  filter(Panel == "panel_r10_region") %>% 
  # Correct region name for APD
  mutate(Region = case_when(
    Region == "Japan, Australia and New Zealand" ~ "Asia-Pacific Developed",
    TRUE ~ Region)) %>% 
  select(region = Region, value_flow = Value_Flow, value_need_high = Value_Need_High,
         value_need_low = Value_Need_Low) %>% 
  left_join(rgnorder) %>% 
  mutate(regionshort = factor(regionshort, levels = rgnorder$regionshort)) %>% 
  arrange(regionshort)

# Set standard equity consideration indicator names and ordering
equitynames <- 
  tibble(variable = c("emiss_1850co2", "emiss_1990co2", 
                      "gdp2017pppcapita", "capstock2017pppcapita", 
                      "dlsdepshare", "expshare2030_1p75"),
         indicator = c("1850-2019 CO2", "1990-2019  CO2", 
                       "GDP per capita (2019)", "Capital stock per capita (2019)",
                       "DLS deprivation (2015)", "Climate risk (2030)"),
         indicatorunits = c("1850-2019  CO2 (Gt)", "1990-2019  CO2 (Gt)", 
                            "GDP per cap. (2019) ($ PPP 2017)", "Capital stock per cap. (2019) ($ PPP 2017)",
                            "DLS deprivation (2015) (% deprived)", "Climate risk (2030) (% at risk)"),
         indicatorshort = paste0(c(rep(c("R", "C", "N"), each = 2)), rep(c(1:2), 3)),
         consideration = c(rep(rep(c("Responsibility", "Capability", "Needs"), each = 2))),
         considerationshort = c(rep(c("R", "C", "N"), each = 2)))

# DEFINE REDISTRIBUTION SHARES -------------------------------------------------

# Determine investment share for RES indicators --------------------------------
#
equity_shares <- equityraw %>% 
  left_join(popgdp %>% select(regionshort, gdp2017ppp)) %>% 
  mutate(across(matches("RES"), ~ . / sum(.)))

# Determine investment share for CAP indicators --------------------------------
# 
# As we are working with per capita indicators we we must relate these back to 
# proportional shares of regional GDP. See SI for details.
#
equity_shares <- equity_shares %>% 
  mutate(across(matches("CAP"), ~ (. * gdp2017ppp) / sum(. * gdp2017ppp)))

# Determine investment share for NED indicators --------------------------------
# 
# Apply penalty function to take the scaled inverse of NED indicator values,
# necessary as these as deprivation indicators. Set a floor of deprivation to 1%.
#
# As we are working with an average deprivation (ratio) indicators we must 
# relate these back to relative shares of regional GDP. See SI for details.
#
equity_shares <- equity_shares %>% 
  mutate(
    across(matches("NED"), ~ ifelse(. < 0.01, log(1/0.01), log(1/.))),
    across(matches("NED"), ~ (. * gdp2017ppp) / sum(. * gdp2017ppp)))

# REDISTRIBUTION BY EQUITY CONSIDERATIONS --------------------------------------

# Prepare regional FS contribution shares and CE needs (and current contributions) -----
equity_investments <- equity_shares %>% 
  pivot_longer(-c(regionshort, gdp2017ppp)) %>% 
  separate(name, into = c("considerationshort", "variable"), 
           sep = "_", extra = "merge") %>% 
  mutate(considerationshort = strtrim(considerationshort,1)) %>% 
  left_join(equitynames) %>% 
  select(regionshort, indicator, value) %>% 
  mutate(indicator = factor(indicator, levels = equitynames$indicator),
         regionshort = factor(regionshort, levels = rgnorder$regionshort)) %>% 
  arrange(regionshort, indicator) %>% 
  pivot_wider(names_from = indicator, values_from = value) %>% 
  full_join(investTS25R10 %>% left_join(rgnorder)) %>%
  select(regionshort, Recent = value_flow, CE_low = value_need_low, 
         CE_high = value_need_high, equitynames$indicator) %>% 
  mutate(regionshort = factor(regionshort, levels = rgnorder$regionshort)) %>% 
  arrange(regionshort)

# Write equity consideration FS shares and investment contributions to file
write_csv(equity_investments, here("Manuscript", "Tables", "equity_shares.csv"))

# Determine regional annual mitigation investment contributions ----------------
regionalmitigation <- equity_investments %>% 
  pivot_longer(-c(regionshort, Recent, CE_low, CE_high), names_to = "indicator") %>% 
  group_by(indicator) %>% 
  mutate(FS_low = value * sum(CE_low),
         FS_high = value *  sum(CE_high)) %>% 
  ungroup() %>% 
  left_join(equitynames) %>% 
  select(regionshort, considerationshort, indicatorshort, Recent, CE_low, CE_high, 
         FS_low, FS_high)

# Prepare low and high regional mitigation distribution tables -----------------

# Lower bound
regionalmitigation_low <- regionalmitigation %>% 
  select(regionshort, considerationshort, indicatorshort, Recent, 
         FS = FS_low, CE = CE_low) %>% 
  mutate(FSCEdiff = FS - CE) %>% 
  group_by(regionshort, indicatorshort) %>% 
  mutate(
    value_interregionincontributions = ifelse(FSCEdiff < 0, FSCEdiff, 0),
    value_interregionoutcontributions = ifelse(FSCEdiff > 0, FSCEdiff, 0),
    value_intraregioncontributions = CE + value_interregionincontributions)

# Upper bound
regionalmitigation_high <- regionalmitigation %>% 
  select(regionshort, considerationshort, indicatorshort, Recent, 
         FS = FS_high, CE = CE_high) %>% 
  mutate(FSCEdiff = FS - CE) %>% 
  group_by(regionshort, indicatorshort) %>% 
  mutate(
    value_interregionincontributions = ifelse(FSCEdiff < 0, FSCEdiff, 0),
    value_interregionoutcontributions = ifelse(FSCEdiff > 0, FSCEdiff, 0),
    value_intraregioncontributions = CE + value_interregionincontributions)

# Figure (A) Ranges of CE needs and FS contributions ---------------------------

a <- ggplot() +
  # WGIII AR6 CE Needs
  geom_errorbar(aes(x = regionshort, ymin = value_need_low, ymax = value_need_high, 
                    linetype = "WGIII AR6 cost-effective needs range"), 
                data = investTS25R10, width = 0.2) +
  # WGIII AR6 current contributions
  geom_col(aes(x = regionshort, y = value_flow, fill = "Recent regional average investments 2017-2020"), alpha = 1,
                data = investTS25R10, width = 0.3) +
  # FS contributions
  geom_errorbar(aes(x = regionshort, ymin = lower, ymax = upper, colour = indicatorshort),
                position = position_dodge2(width = 0.3, reverse = F), width = 0.6, size = 0.4,
                data = rbind(regionalmitigation_low %>% mutate(bound = "lower"),
                             regionalmitigation_high %>% mutate(bound = "upper")) %>% 
                  select(regionshort, indicatorshort, FS, bound) %>% 
                  pivot_wider(names_from = bound, values_from = FS) %>% 
                  mutate(indicatorshort = factor(indicatorshort, levels = equitynames$indicatorshort,
                                                  labels = paste0(
                                                    equitynames$indicatorshort, " - ", equitynames$indicator)))) +
  # X-axis labels
  geom_text(aes(x = regionshort, y = 0, label = regionshort),
            hjust = 0.5, vjust = 1.5, data = investTS25R10, size = 3, angle = 0) +
  # Scales, colour-blind safe - https://personal.sron.nl/~pault/#sec:qualitative
  scale_colour_manual(values = c("#EE6677", "#AA3377", "#4477AA", "#66CCEE", "#228833", "#CCBB44")) +
  scale_y_continuous(breaks = seq(0,1600,200)) +
  scale_fill_manual(values = c("grey")) +
  coord_cartesian(ylim = c(0,1700)) +
  # Theme and legends
  theme_minimal() +
  theme(legend.position = c(0.415,.755), 
        # legend.box.background = element_rect(color="white", size=1),
        legend.spacing.y = unit(0.01, "cm"),
        legend.margin = unit(0.01, "cm"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        plot.margin = margin(25,0.1,0,0)) +
  guides(colour = guide_legend(nrow = 3, title.position = "top", title.hjust = 0,
                               label.position = "right",
                               label.theme = element_text(size = 7.5),
                               label.hjust = 0, order = 1, byrow = T,
                               title.theme = element_text(size = 8)),
         fill = guide_legend(order = 3, label.theme = element_text(size = 7.5)),
         linetype = guide_legend(order = 2, label.theme = element_text(size = 7.5))) +
  labs(x = NULL, y = expression("Billion USD PPP 2015 \u00D7 year "^{-1}),
       colour = "'Fair share' contribution ranges by equity consideration",
       fill = NULL, linetype = "")

a

ggsave(here("Manuscript", "Figures", "figure_a.png"),
       width = 5, height = 3.1, bg = "white")

ggsave(here("Manuscript", "Figures", "figure_a.eps"),
       width = 5, height = 3.1, bg = "white")

write_csv(a$layers[[1]]$data %>% 
            select(region = regionshort, recentcontributions = value_flow, 
                   ce_upper = value_need_high, ce_lower = value_need_low), 
          here("Manuscript", "Figures", "appendix_figuredata_a_1.csv"))

write_csv(a$layers[[3]]$data %>% 
            select(region = regionshort, indicator = indicatorshort, 
                   fs_upper = upper, fs_lower = lower), 
          here("Manuscript", "Figures", "appendix_figuredata_a_2.csv"))

# Figure (B) Inter-regional mitigation contributions visualization ---------------------

b <- 
  # Prepare regional lower-bound CE needs and FS contributions as share of total CE needs 
  regionalmitigation_low %>% 
  select(regionshort, considerationshort, indicatorshort, CE, FS, matches("value")) %>% 
  mutate(value_interregionincontributions = -value_interregionincontributions,
         across(c(CE, FS, matches("value")), ~ . / sum(equity_investments$CE_low)),
         considerationshort = factor(considerationshort, levels = unique(equitynames$considerationshort)),
         indicatorshort = factor(indicatorshort, levels = equitynames$indicatorshort)) %>% 
  pivot_longer(-c(regionshort, considerationshort, indicatorshort, CE, FS)) %>% 
  mutate(name = factor(name, 
                       levels = c("value_intraregioncontributions", 
                                  "value_interregionincontributions", 
                                  "value_interregionoutcontributions"),
                       labels = c("'Fair-share' in-region contributions", 
                                  "'Fair-share' in-region gap", 
                                  "'Fair-share' inter-regional contributions"))) %>% 
  arrange(considerationshort, regionshort) %>% 
  ggplot() +
  # Flows
  geom_col(aes(x = indicatorshort, y = value, fill = fct_rev(name)), 
           width = 0.8, alpha = 1, position = "stack") +
  # Labels
  geom_text(aes(x = indicatorshort, y = CE, vjust = -0.3), 
            label = "WGIII AR6 regional CE needs", size = 2.5, hjust = 0.1,
            data = . %>% filter(indicatorshort == "R1")) +
  # CE needs
  geom_hline(aes(yintercept = CE), linetype = 2) +
  # Annotations
  geom_curve(x = "C1", y = .30, xend = "N1", yend = .25,
             curvature = .3, arrow = arrow(length = unit(2, "mm")),
             data = . %>% filter(regionshort == "EUR")) +
  geom_text(x = "C1", y = 0.34, size = 3,
            label = "Contributions to mitigation \n needs outside the region",
            data = . %>% filter(regionshort == "EUR")) +
  geom_curve(x = "N1", y = .15, xend = "N2", yend = .06,
             curvature = -.3, arrow = arrow(length = unit(2, "mm")),
             data = . %>% filter(regionshort == "SAS")) +
  geom_text(x = "C2", y = 0.2, size = 3, hjust = .6,
            label = "Investment needs met by \n inter-regional contributions",
            data = . %>% filter(regionshort == "SAS")) +
  geom_curve(x = "N1", y = .13, xend = "N2", yend = .04,
             curvature = -.3, arrow = arrow(length = unit(2, "mm")),
             data = . %>% filter(regionshort == "LAC")) +
  geom_text(x = "C2", y = 0.17, size = 3, hjust = .6,
            label = "Within region contributions \n to investment needs",
            data = . %>% filter(regionshort == "LAC")) +
  # Scales, colour-blind safe - https://personal.sron.nl/~pault/#sec:qualitative
  scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(-0.02,0.36), labels = scales::percent_format(), 
                     position = "left") +
  scale_fill_manual(values = c("#DDAA33", "#BB5566","#4477AA"), drop = F) +
  # Facets
  facet_wrap(~regionshort, ncol = 5, strip.position = "top", scales = "free_x") +
  # Theme and legends
  labs(x = NULL, y = "Share of total global cost-effective mitigation needs",
       fill = NULL,
       colour = NULL) +
  theme_bw() +
  theme(
    axis.ticks.x = element_line(size = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
    strip.background = element_rect(fill = "white")) +
  guides(fill = "none")

b

ggsave(here("Manuscript", "Figures", "figure_b.png"),
       width = 10, height = 5, bg = "white")

ggsave(here("Manuscript", "Figures", "figure_b.eps"),
       width = 10, height = 5, bg = "white")

write_csv(b$data %>% 
            select(region = regionshort, ce_needs = CE, consideration = considerationshort,
                   indicator = indicatorshort, flowtype = name, flowvalue = value), 
          here("Manuscript", "Figures", "appendix_figuredata_b.csv"))

# FIGURES FOR THE SUPPLEMENTARY MATERIALS --------------------------------------

# Figure (S1) Aggregate inter-regional contribution visualization --------------

s1 <- 
  # Aggregate lower and upper FS inter-regional contribution bounds
  left_join(
    regionalmitigation_low %>% transmute(regionshort = regionshort, 
                                         considerationshort = considerationshort,
                                         indicatorshort = indicatorshort,
                                         irf_low = value_interregionoutcontributions), 
    regionalmitigation_high %>% transmute(regionshort = regionshort, 
                                          indicatorshort = indicatorshort,
                                          irf_high = value_interregionoutcontributions)) %>% 
  group_by(considerationshort, indicatorshort) %>% 
  summarise(across(matches("irf"), ~sum(.))) %>% 
  mutate(indicatorshort = 
           factor(indicatorshort, levels = equitynames$indicatorshort,
                  labels = paste0(equitynames$indicatorshort, " - ", equitynames$indicator))) %>% 
  pivot_longer(c(irf_low , irf_high)) %>%
  mutate(name = ifelse(name == "irf_low", "Lower-bound", "Upper-bound")) %>% 
  ggplot(aes(indicatorshort, value, alpha = name)) +
  # Lower bound
  geom_col( data = . %>% filter(grepl(name, pattern = "Lower")), 
            width = 0.7, fill = "#DDAA33") +
  # Upper bound
  geom_col( data = . %>% filter(grepl(name, pattern = "Upper")), 
            width = 0.3, fill = "#DDAA33") +
  # Scales, colour-blind safe - https://personal.sron.nl/~pault/#sec:qualitative
  scale_alpha_discrete(range = c(1, 0.3), guide = guide_legend()) +
  scale_y_continuous(limits = c(0,1700), breaks = seq(0,1600,200)) +
  # Theme and legends
  labs(x = NULL, y = expression("Billion USD PPP 2015 \u00D7 year "^{-1}), 
       alpha = "Global inter-regional contributions to meet 'fair share' contributions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
        legend.position = c(0.48,1.05), 
        legend.box.background = element_rect(color="white", size = 2),
        plot.margin = margin(25,0.1,0.1,0.1)) +
  guides(fill = "none",
         alpha = guide_legend(override.aes = list(fill = "#DDAA33"),
                              nrow = 2, 
                              label.position = "right", title.position = "left",
                              title.theme = element_text(size = 9),
                              label.hjust = 1, label.theme = element_text(size = 8)))

s1

ggsave(here("Manuscript", "Figures", "figure_s1.png"),
       width = 8, height = 5, bg = "white")

s2 <- 
  # Prepare regional upper-bound CE needs and FS contributions as share of total CE needs 
  regionalmitigation_high %>% 
  select(regionshort, considerationshort, indicatorshort, CE, FS, matches("value")) %>% 
  mutate(value_interregionincontributions = -value_interregionincontributions,
         across(c(CE, FS, matches("value")), ~ . / sum(equity_investments$CE_high)),
         considerationshort = factor(considerationshort, levels = unique(equitynames$considerationshort)),
         indicatorshort = factor(indicatorshort, levels = equitynames$indicatorshort)) %>% 
  pivot_longer(-c(regionshort, considerationshort, indicatorshort, CE, FS)) %>% 
  mutate(name = factor(name, 
                       levels = c("value_intraregioncontributions", 
                                  "value_interregionincontributions", 
                                  "value_interregionoutcontributions"),
                       labels = c("'Fair-share' in-region contributions", 
                                  "'Fair-share' in-region gap", 
                                  "'Fair-share' inter-regional contributions"))) %>% 
  arrange(considerationshort, regionshort) %>% 
  ggplot() +
  # Flows
  geom_col(aes(x = indicatorshort, y = value, fill = fct_rev(name)), 
           width = 0.8, alpha = 1, position = "stack") +
  # Labels
  geom_text(aes(x = indicatorshort, y = CE, vjust = -0.3), 
            label = "WGIII AR6 regional CE needs", size = 2.5, hjust = 0.1,
            data = . %>% filter(indicatorshort == "R1")) +
  # CE needs
  geom_hline(aes(yintercept = CE), linetype = 2) +
  # Annotations
  geom_curve(x = "C1", y = .30, xend = "N1", yend = .25,
             curvature = .3, arrow = arrow(length = unit(2, "mm")),
             data = . %>% filter(regionshort == "EUR")) +
  geom_text(x = "C1", y = 0.34, size = 3,
            label = "Contributions to mitigation \n needs outside the region",
            data = . %>% filter(regionshort == "EUR")) +
  geom_curve(x = "N1", y = .15, xend = "N2", yend = .06,
             curvature = -.3, arrow = arrow(length = unit(2, "mm")),
             data = . %>% filter(regionshort == "SAS")) +
  geom_text(x = "C2", y = 0.2, size = 3, hjust = .6,
            label = "Investment needs met by \n inter-regional contributions",
            data = . %>% filter(regionshort == "SAS")) +
  geom_curve(x = "N1", y = .13, xend = "N2", yend = .04,
             curvature = -.3, arrow = arrow(length = unit(2, "mm")),
             data = . %>% filter(regionshort == "LAC")) +
  geom_text(x = "C2", y = 0.17, size = 3, hjust = .6,
            label = "Within region contributions \n to investment needs",
            data = . %>% filter(regionshort == "LAC")) +
  # Scales, colour-blind safe - https://personal.sron.nl/~pault/#sec:qualitative
  scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(-0.02,0.36), labels = scales::percent_format(), 
                     position = "left") +
  scale_fill_manual(values = c("#DDAA33", "#BB5566","#4477AA"), drop = F) +
  # Facets
  facet_wrap(~regionshort, ncol = 5, strip.position = "top", scales = "free_x") +
  # Theme and legends
  labs(x = NULL, y = "Share of total global cost-effective mitigation needs",
       fill = NULL,
       colour = NULL) +
  theme_bw() +
  theme(
    axis.ticks.x = element_line(size = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
    strip.background = element_rect(fill = "white")) +
  guides(fill = "none")

s2

ggsave(here("Manuscript", "Figures", "figure_s2.png"),
       width = 10, height = 5, bg = "white")

s3a <- 
  # Prepare regional lower-bound CE needs and FS contributions as share of regional GDP 
  regionalmitigation_low %>% 
  select(regionshort, considerationshort, indicatorshort, CE, FS, matches("value")) %>% 
  left_join(popgdp %>% select(regionshort, gdp2017ppp)) %>% 
  mutate(value_interregionincontributions = -value_interregionincontributions,
         across(c(CE, FS, matches("value")), ~ . / (gdp2017ppp / 1e9)),
         considerationshort = factor(considerationshort, levels = unique(equitynames$considerationshort)),
         indicatorshort = factor(indicatorshort, levels = equitynames$indicatorshort),
         regionshort = factor(regionshort, levels = rgnorder$regionshort)) %>% 
  select(-gdp2017ppp) %>% 
  pivot_longer(-c(regionshort, considerationshort, indicatorshort, CE, FS)) %>% 
  mutate(name = factor(name, 
                       levels = c("value_intraregioncontributions", 
                                  "value_interregionincontributions", 
                                  "value_interregionoutcontributions"),
                       labels = c("'Fair-share' in-region contributions", 
                                  "'Fair-share' in-region gap", 
                                  "'Fair-share' inter-regional contributions"))) %>% 
  arrange(considerationshort, regionshort) %>% 
  ggplot() +
  # Flows
  geom_col(aes(x = indicatorshort, y = value, fill = fct_rev(name)), 
           width = 0.8, alpha = 1, position = "stack") +
  # Labels
  geom_text(aes(x = indicatorshort, y = CE, vjust = -0.3), 
            label = "WGIII AR6 regional CE needs", size = 2.5, hjust = 0.1,
            data = . %>% filter(indicatorshort == "R1")) +
  # CE needs
  geom_hline(aes(yintercept = CE), linetype = 2) +
  # Annotations
  geom_curve(x = "C1", y = .04, xend = "N1", yend = .02,
             curvature = .3, arrow = arrow(length = unit(2, "mm")),
             data = . %>% filter(regionshort == "EUR")) +
  geom_text(x = "C1", y = 0.045, size = 3,
            label = "Contributions to mitigation \n needs outside the region",
            data = . %>% filter(regionshort == "EUR")) +
  geom_curve(x = "N1", y = .03, xend = "N2", yend = .01,
             curvature = -.3, arrow = arrow(length = unit(2, "mm")),
             data = . %>% filter(regionshort == "SAS")) +
  geom_text(x = "C2", y = 0.035, size = 3, hjust = .6,
            label = "Investment needs met by \n inter-regional contributions",
            data = . %>% filter(regionshort == "SAS")) +
  geom_curve(x = "N1", y = .03, xend = "N2", yend = .01,
             curvature = -.3, arrow = arrow(length = unit(2, "mm")),
             data = . %>% filter(regionshort == "LAC")) +
  geom_text(x = "C2", y = 0.035, size = 3, hjust = .6,
            label = "Within region contributions \n to investment needs",
            data = . %>% filter(regionshort == "LAC")) +
  # Scales, colour-blind safe - https://personal.sron.nl/~pault/#sec:qualitative
  scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(-0.002,0.05), 
                     labels = scales::percent_format(accuracy = 1), 
                     position = "left") +
  scale_fill_manual(values = c("#DDAA33", "#BB5566","#4477AA"), drop = F) +
  # Facets
  facet_wrap(~regionshort, ncol = 5, strip.position = "top", scales = "free_x") +
  # Theme and legends
  labs(x = NULL, y = "Share of regional GDP in 2019",
       fill = NULL,
       colour = NULL,
       subtitle = "Lower-bound") +
  theme_bw() +
  theme(
    axis.ticks.x = element_line(size = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
    strip.background = element_rect(fill = "white")) +
  guides(fill = "none")

s3b <- 
  # Prepare regional upper-bound CE needs and FS contributions as share of regional GDP 
  regionalmitigation_high %>% 
  select(regionshort, considerationshort, indicatorshort, CE, FS, matches("value")) %>% 
  left_join(popgdp %>% select(regionshort, gdp2017ppp)) %>% 
  mutate(value_interregionincontributions = -value_interregionincontributions,
         across(c(CE, FS, matches("value")), ~ . / (gdp2017ppp / 1e9)),
         considerationshort = factor(considerationshort, levels = unique(equitynames$considerationshort)),
         indicatorshort = factor(indicatorshort, levels = equitynames$indicatorshort),
         regionshort = factor(regionshort, levels = rgnorder$regionshort)) %>% 
  select(-gdp2017ppp) %>% 
  pivot_longer(-c(regionshort, considerationshort, indicatorshort, CE, FS)) %>% 
  mutate(name = factor(name, 
                       levels = c("value_intraregioncontributions", 
                                  "value_interregionincontributions", 
                                  "value_interregionoutcontributions"),
                       labels = c("'Fair-share' in-region contributions", 
                                  "'Fair-share' in-region gap", 
                                  "'Fair-share' inter-regional contributions"))) %>% 
  arrange(considerationshort, regionshort) %>% 
  ggplot() +
  # Flows
  geom_col(aes(x = indicatorshort, y = value, fill = fct_rev(name)), 
           width = 0.8, alpha = 1, position = "stack") +
  # Labels
  geom_text(aes(x = indicatorshort, y = CE, vjust = -0.3), 
            label = "WGIII AR6 regional CE needs", size = 2.5, hjust = 0.1,
            data = . %>% filter(indicatorshort == "R1")) +
  # CE needs
  geom_hline(aes(yintercept = CE), linetype = 2) +
  # Annotations
  geom_curve(x = "C1", y = .06, xend = "N1", yend = .04,
             curvature = .3, arrow = arrow(length = unit(2, "mm")),
             data = . %>% filter(regionshort == "EUR")) +
  geom_text(x = "C1", y = 0.07, size = 3,
            label = "Contributions to mitigation \n needs outside the region",
            data = . %>% filter(regionshort == "EUR")) +
  geom_curve(x = "N1", y = .05, xend = "N2", yend = .02,
             curvature = -.3, arrow = arrow(length = unit(2, "mm")),
             data = . %>% filter(regionshort == "SAS")) +
  geom_text(x = "C2", y = 0.06, size = 3, hjust = .6,
            label = "Investment needs met by \n inter-regional contributions",
            data = . %>% filter(regionshort == "SAS")) +
  geom_curve(x = "N1", y = .05, xend = "N2", yend = .02,
             curvature = -.3, arrow = arrow(length = unit(2, "mm")),
             data = . %>% filter(regionshort == "LAC")) +
  geom_text(x = "C2", y = 0.06, size = 3, hjust = .6,
            label = "Within region contributions \n to investment needs",
            data = . %>% filter(regionshort == "LAC")) +
  # Scales, colour-blind safe - https://personal.sron.nl/~pault/#sec:qualitative
  scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(-0.002,0.1), labels = scales::percent_format(), 
                     position = "left") +
  scale_fill_manual(values = c("#DDAA33", "#BB5566","#4477AA"), drop = F) +
  # Facets
  facet_wrap(~regionshort, ncol = 5, strip.position = "top", scales = "free_x") +
  # Theme and legends
  labs(x = NULL, y = "Share of regional GDP in 2019",
       fill = NULL,
       colour = NULL,
       subtitle = "Upper-bound") +
  theme_bw() +
  theme(
    axis.ticks.x = element_line(size = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
    strip.background = element_rect(fill = "white")) +
  guides(fill = "none")

s3 <- wrap_plots(s3a, s3b, ncol = 1)

s3

ggsave(here("Manuscript", "Figures","figure_s3.png"),
       width = 10, height = 10, bg = "white")
