# ================================================================================
# The Future of Military Engines 
# By Gabriel Coll
# --------------------------------------------------------------------------------
# engine-related contract numbers from the Federal Procurement Data System 
# ================================================================================

# load packages ------------------------------------------------------------------

library(tidyverse)
library(Cairo)
library(ggthemes)

# read data ----------------------------------------------------------------------

read_engine_contracts <-
  read.csv(
    "Project_SP_EngineAllVendorHistorycompetitionFundingMechanismVendorSizeProdServAreaSubcustomer - Copy.csv"
  )
read_topline_contracts <- read.csv("FPDS_data.csv")

# summarise contract data --------------------------------------------------------

engine_contracts <- read_engine_contracts %>%
  rename(
    fy = "ï..fiscal_year",
    amount = SumOfobligatedAmount,
    category = Simple,
    platform_portfolio = PlatformPortfolio,
    customer_2 = Customer,
    customer = SubCustomer,
    competition = CompetitionClassification,
    contract_type = typeofcontractpricingtext,
    vendor_size = VendorSize,
    parent = ParentID, 
    project = ProjectName
  ) %>%
  group_by(fy, customer, category, parent, project) %>%
  summarise(amount = sum(amount, na.rm = TRUE))

topline_contracts <- read_topline_contracts %>%
  rename(
    fy = FY,
    amount = Amount,
    category = Category,
    platform_portfolio = Portfolio,
    customer = Customer,
    vendor_size = VendorSize
  ) %>%
  group_by(fy, customer, category) %>%
  summarise(amount = sum(amount, na.rm = TRUE))

# total --------------------------------------------------------------------------

(
  total <- engine_contracts %>%
    group_by(fy) %>%
    summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = amount, x = fy), stat = "identity") +
    theme_minimal() +
    ggtitle("total engines")
)

(
  total <- topline_contracts %>%
    group_by(fy) %>%
    summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = amount, x = fy), stat = "identity") +
    theme_minimal() +
    ggtitle("total")
)

# army ---------------------------------------------------------------------------

(
  army <- engine_contracts %>%
    group_by(fy) %>%
    filter(customer == "Army") %>%
    summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = amount, x = fy), stat = "identity") +
    theme_minimal() +
    ggtitle("army")
)


# --------------------------------------------------------------------------------
# navy
# --------------------------------------------------------------------------------
(
  navy <- engine_contracts %>%
    group_by(fy) %>%
    filter(customer == "Navy") %>%
    summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = amount, x = fy), stat = "identity") +
    theme_minimal() +
    ggtitle("navy")
)

# --------------------------------------------------------------------------------
# air force
# --------------------------------------------------------------------------------
(
  airforce <- engine_contracts %>%
    group_by(fy) %>%
    filter(customer == "Air Force") %>%
    summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = amount, x = fy), stat = "identity") +
    theme_minimal() +
    ggtitle("air force")
)

# --------------------------------------------------------------------------------
# service facet
# --------------------------------------------------------------------------------
(
  service <- engine_contracts %>%
    group_by(fy, customer) %>%
    summarise(amount = sum(amount, na.rm = TRUE)) %>%
    filter(customer == "Army" |
             customer == "Navy" | customer == "Air Force") %>%
    ggplot() +
    geom_area(aes(y = amount, x = fy), stat = "identity") +
    facet_wrap( ~ customer, nrow = 1) +
    theme_minimal() +
    ggtitle("by service")
)

# --------------------------------------------------------------------------------
# category facet
# --------------------------------------------------------------------------------
(
  category <- engine_contracts %>%
    group_by(fy, category) %>%
    summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = amount, x = fy), stat = "identity") +
    facet_wrap( ~ category, nrow = 1) +
    theme_minimal() +
    ggtitle("by category")
)

# --------------------------------------------------------------------------------
# army / category facet
# --------------------------------------------------------------------------------
(
  army_category <- engine_contracts %>%
    filter(customer == "Army") %>%
    group_by(fy, customer, category) %>%
    summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = amount, x = fy), stat = "identity") +
    facet_wrap( ~ category, nrow = 1) +
    theme_minimal() +
    ggtitle("army by category")
)

# --------------------------------------------------------------------------------
# navy / category facet
# --------------------------------------------------------------------------------
(
  navy.category <- engine_contracts %>%
    filter(customer == "Navy") %>%
    group_by(fy, customer, category) %>%
    summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = amount, x = fy), stat = "identity") +
    facet_wrap( ~ category, nrow = 1) +
    theme_minimal() +
    ggtitle("navy by category")
)

# --------------------------------------------------------------------------------
# air force / category facet
# --------------------------------------------------------------------------------
(
  airforce.category <- engine_contracts %>%
    filter(customer == "Air Force") %>%
    group_by(fy, customer, category) %>%
    summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = amount, x = fy), stat = "identity") +
    facet_wrap( ~ category, nrow = 1) +
    theme_minimal() +
    ggtitle("air force by category")
)

# --------------------------------------------------------------------------------
# comparison to the topline
# --------------------------------------------------------------------------------
engine_contracts <- engine_contracts %>%
  mutate(type = "Engines")
topline_contracts <- topline_contracts %>%
  mutate(type = "Topline")

comparison_contracts <- engine_contracts %>%
  rbind(topline_contracts) %>%
  group_by(fy, type) %>%
  summarise(amount = sum(amount, na.rm = TRUE))

dyear <- comparison_contracts %>%
  rename(fyb = fy) %>%
  mutate(fy = fyb + 1) %>%
  select(-fyb)

comparison_contracts <- comparison_contracts %>%
  left_join(dyear, by = c("fy", "type")) %>%
  filter(fy >= 2001) %>%
  select(fy, amount.x, amount.y, type) %>%
  mutate(amount_change = amount.x - amount.y) %>%
  mutate(amount_percent_change = (amount_change) / amount.y * 100) %>%
  rename(amount = amount.x) %>%
  select(fy, amount, amount_change, amount_percent_change, type) %>%
  group_by(fy, type) %>%
  summarise(
    amount = sum(amount, na.rm = TRUE),
    amount_change = sum(amount_change, na.rm = TRUE),
    amount_percent_change = sum(amount_percent_change, na.rm = TRUE)
  )

# --------------------------------------------------------------------------------
# difference in amount

ggplot(data = comparison_contracts) +
  geom_line(aes(
    x = fy,
    y = amount,
    group = type,
    color = type
  )) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  ggtitle("difference in amount")

# --------------------------------------------------------------------------------
# difference in year-to-year change 

ggplot(data = comparison_contracts) +
  geom_line(aes(
    x = fy,
    y = amount_change,
    group = type,
    color = type
  )) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  ggtitle("difference in year-to-year change")

# --------------------------------------------------------------------------------
# difference in year-to-year % change 

# filter(fy <= 2017) %>%
# mutate(fy = as.factor(as.character(fy)))
ggplot(data = comparison_contracts) +
  geom_line(aes(
    x = fy,
    y = amount_percent_change,
    group = type,
    color = type
  )) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  ggtitle("difference in year-to-year % change")

# --------------------------------------------------------------------------------
# parent (top 12)
# --------------------------------------------------------------------------------

(
  parent <- engine_contracts %>%
    filter(
      parent %in% c(
        "UNITED TECH",
        "GENERAL ELECTRIC",
        "NULL",
        "ROLLS ROYCE",
        "HONEYWELL",
        "TEXTRON",
        "GE ROLLS-ROYCE FIGHTER ENGINE TEAM [GE/ROLLS ROYCE]",
        "KELLY AVIATION CENTER [Joint Venture Lockheed Martin/Rolls Royce]",
        "CFM INTERNATIONAL",
        "BOEING",
        "L3 COMMUNICATIONS",
        "NORTHROP GRUMMAN"
      )
    ) %>%
    group_by(fy, parent) %>%
    summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = amount, x = fy), stat = "identity") +
    facet_wrap(~ parent) +
    theme_minimal() +
    ggtitle("top 12 engine contractors")
)

# --------------------------------------------------------------------------------
# GE vs Pratt
# --------------------------------------------------------------------------------

(
  parent <- engine_contracts %>%
    filter(parent %in% c("UNITED TECH",
                         "GENERAL ELECTRIC")) %>%
    group_by(fy, parent, customer) %>%
    summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    geom_bar(aes(y = amount, x = fy), stat = "identity") +
    facet_grid(customer ~ parent) +
    theme_minimal() +
    ggtitle("ge vs pratt")
)

# --------------------------------------------------------------------------------
# project (top 17)
# --------------------------------------------------------------------------------

(project <- engine_contracts %>% 
  filter(project %in% c("NULL",
                        "JSF (F-35) ",
                        "F/A-22 ",
                        "F-100 Engine",
                        "H-1 UPGRADE ",
                        "V22 ",
                        "F/A-18 E/F ",
                        "BJN",
                        "C-17A ",
                        "C-17A CARGO TRANSPORT",
                        "F-110",
                        "F414-GE-400",
                        "C130-J ",
                        "BLACK HAWK (UH-60A/L) ",
                        "UH-60 BLACKHAWK UTTAS",
                        "BSJ",
                        "F-18 HORNET"
                        )) %>% 
  group_by(fy, project) %>% 
  summarise(amount = sum(amount, na.rm = TRUE)) %>%
  ggplot() +
  geom_area(aes(y = amount, x = fy), stat = "identity") +
  facet_wrap(~ project) +
  theme_minimal() +
  ggtitle("top 17 engine projects"))

# --------------------------------------------------------------------------------
# jsf 
# --------------------------------------------------------------------------------

(project <- engine_contracts %>% 
    filter(project %in% c(
                          "JSF (F-35) "
    )) %>% 
    group_by(fy, project) %>% 
    summarise(amount = sum(amount, na.rm = TRUE)) %>%
    ggplot() +
    geom_area(aes(y = amount, x = fy), stat = "identity") +
    facet_wrap(~ project) +
    theme_minimal() +
   ggtitle("jsf"))
