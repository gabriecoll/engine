# ================================================================================
# The Future of Military Engines
# By Gabriel Coll
# --------------------------------------------------------------------------------
# engine-related budget numbers from the Future Years Defense Program
# ================================================================================

library(tidyverse)
library(Cairo)
library(ggthemes)
library(car)

# read data ----------------------------------------------------------------------

d19 <- read.csv("19.csv")
d18 <- read.csv("18.csv")
d17 <- read.csv("17.csv")
d16 <- read.csv("16.csv")
d15 <- read.csv("15.csv")
d14 <- read.csv("14.csv")
d13 <- read.csv("13.csv")
d12 <- read.csv("12.csv")
d11 <- read.csv("11.csv")
d10 <- read.csv("10.csv")
d09 <- read.csv("09.csv")
d08 <- read.csv("08.csv")
d07 <- read.csv("07.csv")
d06 <- read.csv("06.csv")
d05 <- read.csv("05.csv")
d04 <- read.csv("04.csv")
d03 <- read.csv("03.csv")
d02 <- read.csv("02.csv")
d01 <- read.csv("01.csv")
d00 <- read.csv("00.csv")
d99 <- read.csv("99.csv")

names(d19)[1] <- "FYDP.Year"
names(d18)[1] <- "FYDP.Year"
names(d17)[1] <- "FYDP.Year"
names(d16)[1] <- "FYDP.Year"

# long data ----------------------------------------------------------------------

d19 <- d19 %>%
  gather(`X2017`:`X2023`, key = "FY", value = "Amount")
d18 <- d18 %>%
  gather(`X2016`:`X2022`, key = "FY", value = "Amount")
d17 <- d17 %>%
  gather(`X2015`:`X2021`, key = "FY", value = "Amount")
d16 <- d16 %>%
  gather(`X2014`:`X2020`, key = "FY", value = "Amount")
d15 <- d15 %>%
  gather(`X2013`:`X2019`, key = "FY", value = "Amount")
d14 <- d14 %>%
  gather(`X2012`:`X2018`, key = "FY", value = "Amount")
d13 <- d13 %>%
  gather(`X2011`:`X2017`, key = "FY", value = "Amount")
d12 <- d12 %>%
  gather(`X2010`:`X2016`, key = "FY", value = "Amount")
d11 <- d11 %>%
  gather(`X2009`:`X2015`, key = "FY", value = "Amount")
d10 <- d10 %>%
  gather(`X2008`:`X2014`, key = "FY", value = "Amount")
d09 <- d09 %>%
  gather(`X2007`:`X2013`, key = "FY", value = "Amount")
d08 <- d08 %>%
  gather(`X2006`:`X2013`, key = "FY", value = "Amount")
d07 <- d07 %>%
  gather(`X2005`:`X2011`, key = "FY", value = "Amount")
d06 <- d06 %>%
  gather(`X2004`:`X2011`, key = "FY", value = "Amount")
d05 <- d05 %>%
  gather(`X2003`:`X2009`, key = "FY", value = "Amount")
d04 <- d04 %>%
  gather(`X2002`:`X2009`, key = "FY", value = "Amount")
d03 <- d03 %>%
  gather(`X2001`:`X2007`, key = "FY", value = "Amount")
d02 <- d02 %>%
  gather(`X2000`:`X2007`, key = "FY", value = "Amount")
d01 <- d01 %>%
  gather(`X1999`:`X2005`, key = "FY", value = "Amount")
d00 <- d00 %>%
  gather(`X1998`:`X2005`, key = "FY", value = "Amount")
d99 <- d99 %>%
  gather(`X1997`:`X2003`, key = "FY", value = "Amount")

# combine data -------------------------------------------------------------------

engine_budget <- rbind(
  d99,
  d00,
  d01,
  d02,
  d03,
  d04,
  d05,
  d06,
  d07,
  d08,
  d09,
  d10,
  d11,
  d12,
  d13,
  d14,
  d15,
  d16,
  d17,
  d18,
  d19
)

# clean data ---------------------------------------------------------------------

engine_budget <- engine_budget %>%
  separate(FY, into = c("X", "FY"), sep = 1) %>%
  rename(
    fydp_year = FYDP.Year,
    account = Type,
    organization = Force,
    program_number = Program.Number,
    program_name = Program.Name,
    project_number = Project.Number,
    project_name = Project.Name,
    fy= FY,
    amount = Amount
  ) %>%
  mutate(fydp_year = as.factor(fydp_year)) %>%
  mutate(fydp = "FYDP") %>%
  unite(fydp_year, fydp_year, fydp, sep = " ") %>%
  select(fydp_year:project_name, fy, amount) %>%
  mutate(
    project_name = recode(
      project_name,
      "'Acft Demo Engines' = 'ACFT Demo Engines';
      'ACFT Demo Engines ' = 'ACFT Demo Engines';
      'Aircraft Demonstration Engine' = 'ACFT Demo Engines';
      'Aircraft Demonstration Engines' = 'ACFT Demo Engines';
      'Adv Propulsion Rsch' = 'Advanced Propulsion Research';
      'Adv Propulsion' = 'Advanced Propulsion Research';
      'Aerospace Fuels and Atmospheric Propulsion' = 'Aerospace Fuels';
      'Aircraft Engine Component Improvement Program (CIP) (USN)' = 'Aircraft Engine Component Improvement Program (USN)';
      'Aircraft Engine Component Improvement Program (CIP) (USA)' = 'Aircraft Engine Component Improvement Program (USA)';
      'Aircraft Engine Component Improvement Program' = 'Aircraft Engine Component Improvement Program (USAF)';
      'Aircraft Engine CIP' = 'Aircraft Engine Component Improvement Program (USN)';
      'Aircraft Component Improvement Program (CIP)' = 'Aircraft Engine Component Improvement Program (USA)';
      'A/C Eng Comp Imp (CIP)' = 'Aircraft Engine Component Improvement Program (USN)';
      'Acft Engines Comp Imp Prog' = 'Aircraft Engine Component Improvement Program (USN)';
      'F-35' = 'F135 Aircraft Engine Component Improvement Program';
      'Aircraft Propulsion Subsystem Integration' = 'Aircraft Propulsion Subsystems Int';
      'Vehicle Propulsion and Structures Technology' = 'Veh Prop & Struct Tech';
      'Propulsion and Power Component Improvement Program' = 'Aircraft Engine Component Improvement Program (USN)';
      'Aircraft Engine Component Improvement Program (CIP)' = 'Aircraft Engine Component Improvement Program (USA)';
      'A/C Compon Improv Prog' = 'Aircraft Engine Component Improvement Program (USA)';
      'F135 Aircraft Engine Component Improvement Program' = 'Aircraft Engine Component Improvement Program (F135)';
      'Materials for Structures, Propulsion and Subsystems' = 'Materials for Structures, Propulsion, and Subsystems';
      'Fuels and Lubrication' = 'Combustion and Mechanical Systems';
      'Propulsion' = 'Advanced Aerospace Propulsion';
      'Aerospace Fuel Technology' = 'Combustion and Mechanical Systems'"
    )
    )

# note: the funding in the last project (above) was moved from PE 0602203F Project 3048 starting in FY 2010
# to more accurately align efforts with organizational structure.

engine_budget_wide <-
  spread(engine_budget, key = "fy", value = "amount") # to view discrepancies 

engine_budget <- engine_budget %>%
  filter(
    project_name %in% c(
      "ACFT Demo Engines",
      # "Adv Propulsion",
      "Advanced Aerospace Propulsion",
      "Advanced Propulsion Research",
      "Advanced Propulsion Technology",
      "Advanced Turbine Engine Gas Generator",
      # "Aerospace Fuel Technology",
      "Aerospace Fuels",
      "Aircraft Engine Component Improvement Program (F135)",
      "Aircraft Engine Component Improvement Program (USA)",
      "Aircraft Engine Component Improvement Program (USAF)",
      "Aircraft Engine Component Improvement Program (USN)",
      "Aircraft Propulsion Subsystems Int",
      "AV-8B",
      # "Aviation Advanced Technology Initiatives",
      "Combustion and Mechanical Systems",
      # "Fuels and Lubrication",
      "Improved Turbine Engine Program",
      # "Materials",
      "Materials for Structures, Propulsion, and Subsystems",
      # "Materials Technology for Sustainment",
      # "Materials Transition",
      # "Propulsion",
      # "Propulsion ",
      "Turbine Engine Technology",
      # "Vectored Thrust Ducted Propeller (CA)",
      "Veh Prop & Struct Tech"
    )
  ) %>%
  filter(fydp_year != "1999 FYDP")

engine_budget_wide <- spread(engine_budget, key = "fy", value = "amount") # to view discrepancies 

# ================================================================================

# filter the data to actual values -----------------------------------------------

# note: this filtering section creates a dataset for only actual values in 
# the budget line, in addition to enacted and projected for the most recent FYDP

engine_actual <- engine_budget %>%
  filter(
    fydp_year == "2000 FYDP" & fy == "1998" |
      fydp_year == "2001 FYDP" & fy == "1999" |
      fydp_year == "2002 FYDP" & fy == "2000" |
      fydp_year == "2003 FYDP" & fy == "2001" |
      fydp_year == "2004 FYDP" & fy == "2002" |
      fydp_year == "2005 FYDP" & fy == "2003" |
      fydp_year == "2006 FYDP" & fy == "2004" |
      fydp_year == "2007 FYDP" & fy == "2005" |
      fydp_year == "2008 FYDP" & fy == "2006" |
      fydp_year == "2009 FYDP" & fy == "2007" |
      fydp_year == "2010 FYDP" & fy == "2008" |
      fydp_year == "2011 FYDP" & fy == "2009" |
      fydp_year == "2012 FYDP" & fy == "2010" |
      fydp_year == "2013 FYDP" & fy == "2011" |
      fydp_year == "2014 FYDP" & fy == "2012" |
      fydp_year == "2015 FYDP" & fy == "2013" |
      fydp_year == "2016 FYDP" & fy == "2014" |
      fydp_year == "2017 FYDP" & fy == "2015" |
      fydp_year == "2018 FYDP" & fy == "2016" |
      fydp_year == "2019 FYDP" & fy == "2017" |
      fydp_year == "2019 FYDP" & fy == "2018" |
      fydp_year == "2019 FYDP" & fy == "2019"
  ) %>%
  filter(amount != 0) %>%
  group_by(fy, project_name) %>%
  summarise(amount = sum(amount, na.rm = FALSE))

engine_actual_wide <- spread(engine_actual, key = "fy", value = "amount")

engine_actual$fy <- as.numeric(engine_actual$fy)

# facet --------------------------------------------------------------------------

(facet <-
  ggplot() + geom_area(aes(y = amount, x = fy), data = engine_actual,
                       stat = "identity") +
  facet_wrap(~ project_name) +
  geom_vline(xintercept = 2016))

engine_actual_total <- engine_actual %>%
  group_by(fy) %>%
  summarise(amount = sum(amount, na.rm = FALSE))

# total --------------------------------------------------------------------------

(total <-
  ggplot() + geom_area(aes(y = amount, x = fy), data = engine_actual_total,
                       stat = "identity") +
  geom_vline(xintercept = 2016))

# ================================================================================

# read topline data --------------------------------------------------------------

read_topline <- read_csv("topline.csv")

topline <- read_topline %>%
  select(fy, army, navy, air_force, dod_total, us_total) %>%
  gather(army:us_total, key = "project_name", value = "amount")

# --------------------------------------------------------------------------------

engine_actual <- filter(
  engine_budget,
  fydp_year == "2000 FYDP" & fy == "1998" |
    fydp_year == "2001 FYDP" & fy == "1999" |
    fydp_year == "2002 FYDP" & fy == "2000" |
    fydp_year == "2003 FYDP" & fy == "2001" |
    fydp_year == "2004 FYDP" & fy == "2002" |
    fydp_year == "2005 FYDP" & fy == "2003" |
    fydp_year == "2006 FYDP" & fy == "2004" |
    fydp_year == "2007 FYDP" & fy == "2005" |
    fydp_year == "2008 FYDP" & fy == "2006" |
    fydp_year == "2009 FYDP" & fy == "2007" |
    fydp_year == "2010 FYDP" & fy == "2008" |
    fydp_year == "2011 FYDP" & fy == "2009" |
    fydp_year == "2012 FYDP" & fy == "2010" |
    fydp_year == "2013 FYDP" & fy == "2011" |
    fydp_year == "2014 FYDP" & fy == "2012" |
    fydp_year == "2015 FYDP" & fy == "2013" |
    fydp_year == "2016 FYDP" & fy == "2014" |
    fydp_year == "2017 FYDP" & fy == "2015" |
    fydp_year == "2018 FYDP" & fy == "2016" |
    fydp_year == "2018 FYDP" & fy == "2017" |
    fydp_year == "2018 FYDP" & fy == "2018"
)

engine_actual <- mutate(engine_actual, amount = amount * 1000000)

engine_actual <- engine_actual %>%
  group_by(project_name, fy) %>%
  summarise(amount = sum(amount, na.rm = TRUE))

engine_actual <- filter(engine_actual, amount != 0)

engine_actual_2 <- engine_actual
# engine_actual_2 <- select(engine_actual, project_name, fy, amount)
# engine_budget2 <- spread(engine_budget, key = "project_name", value = "amount")

engine_actual_3 <- engine_actual_2

engine_actual_3 <-
  spread(engine_actual_2, key = "project_name", value = "amount")

engine_actual_3[is.na(engine_actual_3)] <- 0

engine_actual_3 <-
  gather(
    engine_actual_3,
    "ACFT Demo Engines":"Veh Prop & Struct Tech",
    key = "project_name",
    value = "amount"
  )

data_fy <- select(engine_actual_3, fy)

engine_actual_3 <- select(engine_actual_3,-fy)

engine_actual_3 <- cbind(data_fy, engine_actual_3)

engine_actual_3$fy <- as.numeric(engine_actual_3$fy)

engine_actual_3 <- rbind(engine_actual_3, topline)
data_year <- engine_actual_3
data_year <- mutate(data_year, fy2 = fy + 1)
data_year <- select(data_year,-fy)
colnames(data_year)[colnames(data_year) == "fy2"] <- "fy"
# data_year$FY <- as.character(data_year$FY)
# engine_actual_3$FY <- as.numeric(engine_actual_3$FY)

engine_comparison <- left_join(engine_actual_3, data_year, by = c("project_name", "fy"))
engine_comparison <- filter(engine_comparison, fy != 1998 |
              fy != 1997)
engine_comparison <- select(engine_comparison, fy, project_name, amount.x, amount.y)
engine_comparison_topline <- filter(
  engine_comparison,
  project_name == "us_total" |
    project_name == "dod_total" |
    project_name == "army" |
    project_name == "navy" |
    project_name == "air_force"
)
engine_comparison <- engine_comparison %>% filter(project_name != "us_total") %>%
  filter(project_name != "dod_total") %>%
  filter(project_name != "army") %>%
  filter(project_name != "navy") %>%
  filter(project_name != "air_force")

engine_comparison <- engine_comparison %>%
  group_by(fy) %>%
  summarise(
    amount.x = sum(amount.x, na.rm = TRUE),
    amount.y = sum(amount.y, na.rm = TRUE)
  ) %>%
  mutate(project_name = "Engines") %>%
  select(fy, project_name, amount.x, amount.y) %>%
  rbind(engine_comparison_topline)

# engine_comparison <- rbind(engine_comparison, engine_comparison_topline)


engine_comparison <- mutate(engine_comparison, amount_change = amount.x - amount.y)
engine_comparison <-
  mutate(engine_comparison, amount_percent_Change = (amount.x - amount.y) / amount.y * 100)
colnames(engine_comparison)[colnames(engine_comparison) == "amount.x"] <- "amount"
# colnames(engine_comparison)[colnames(engine_comparison)=="project_name.x"] <- "project_name"
engine_comparison <-
  select(engine_comparison,
         fy,
         project_name,
         amount,
         amount_change,
         amount_percent_Change)
# engine_comparison[ is.na(engine_comparison) ] <- 0
# engine_comparison2 <- spread(engine_comparison, key = "project_name", value = "amount")
# engine_actual4 <- sapply(names(engine_actual_3)[-1], function(x) {
#   engine_actual_3[paste0(x, "_pct")] <<- engine_actual_3[x] / sum(engine_actual_3[x])
# })
engine_comparison$fy <- as.character(engine_comparison$fy)
engine_comparison$fy <- as.factor(engine_comparison$fy)

# engine_comparison <- do.call(data.frame,lapply(engine_comparison, function(x) replace(x, is.infinite(x),NA)))

# engine_budget2 <- filter(engine_budget, project_name == "Advanced Propulsion Research")
engine_comparison2 <- filter(engine_comparison, project_name == "Aerospace Fuels")
# engine_comparison2 <- spread(engine_comparison2, key = "project_name", value = "amount")
# engine_comparison <- filter(engine_comparison, project_name == "Engines" |
#               project_name == "us_total")

ggplot(data = engine_comparison,
       aes(
         x = fy,
         y = amount,
         group = project_name,
         color = project_name
       )) +
  geom_line() +
  geom_hline(yintercept = 0)
# facet_wrap(~ project_name)

# ggplot(data=engine_comparison, aes(x=fy, y=amount_percent_Change, group=1)) +
#   geom_area()


write.csv(engine_comparison, "data4.csv")

write.csv(engine_budget, "data.csv")

write.csv(engine_actual_3, "data3.csv")

