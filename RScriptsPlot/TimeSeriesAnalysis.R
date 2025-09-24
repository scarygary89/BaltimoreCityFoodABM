library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
library(viridis)
library(fishualize)

# Get script directory for relative path construction
mydir <- dirname(normalizePath(sys.frame(1)$ofile))

# Source import functions (relative to project root)
source(file.path("RScriptsPlot", "ImportFunctions.R"))

# Import Data from all Scenarios
output_dir <- file.path("Output")

# Import Data from all Scenarios
BaselineData <- ImportDataForScenarios(output_dir, "Baseline")
MeatlessMondayData <- ImportDataForScenarios(output_dir, "MeatlessMonday")
PriceSurgeData <- ImportDataForScenarios(output_dir, "PriceSurge")
MoreMeatlessOptionsData <- ImportDataForScenarios(output_dir, "MoreMeatlessOptions")
ComprehensiveMarketingData <- ImportDataForScenarios(output_dir, "ComprehensiveMarketing")

# Create long data that can be processed into plot data
BaseLong_Race <- createLongData(BaselineData,'Race')
BaseLong_Poverty <- createLongData(BaselineData,'Poverty')
BaseLong_Income <- createLongData(BaselineData,'Income')

MeatlessMondayLong_Race <- createLongData(MeatlessMondayData,'Race')
MeatlessMondayLong_Poverty <- createLongData(MeatlessMondayData,'Poverty')
MeatlessMondayLong_Income <- createLongData(MeatlessMondayData,'Income')

PriceSurgeLong_Race <- createLongData(PriceSurgeData,'Race')
PriceSurgeLong_Poverty <- createLongData(PriceSurgeData,'Poverty')
PriceSurgeLong_Income <- createLongData(PriceSurgeData,'Income')

MoreMeatlessOptionsLong_Race <- createLongData(MoreMeatlessOptionsData,'Race')
MoreMeatlessOptionsLong_Poverty <- createLongData(MoreMeatlessOptionsData,'Poverty')
MoreMeatlessOptionsLong_Income <- createLongData(MoreMeatlessOptionsData,'Income')

ComprehensiveMarketingLong_Race <- createLongData(ComprehensiveMarketingData,'Race')
ComprehensiveMarketingLong_Poverty <- createLongData(ComprehensiveMarketingData,'Poverty')
ComprehensiveMarketingLong_Income <- createLongData(ComprehensiveMarketingData,'Income')

# Calculate meat consumption by social groups
BaseMeatConsumption_Race <- BaseLong_Race %>% group_by(race, Meal) %>% summarize(Meat = mean(Meat + Poultry), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
BaseMeatConsumption_Poverty <- BaseLong_Poverty %>% group_by(poverty, Meal) %>% summarize(Meat = mean(Meat + Poultry), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
BaseMeatConsumption_Income <- BaseLong_Income %>% group_by(income, Meal) %>% summarize(Meat = mean(Meat + Poultry), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)

MeatlessMondayMeatConsumption_Race <- MeatlessMondayLong_Race %>% group_by(race, Meal) %>% summarize(Meat = mean(Meat + Poultry), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
MeatlessMondayMeatConsumption_Poverty <- MeatlessMondayLong_Poverty %>% group_by(poverty, Meal) %>% summarize(Meat = mean(Meat + Poultry), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
MeatlessMondayMeatConsumption_Income <- MeatlessMondayLong_Income %>% group_by(income, Meal) %>% summarize(Meat = mean(Meat + Poultry), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)

PriceSurgeMeatConsumption_Race <- PriceSurgeLong_Race %>% group_by(race, Meal) %>% summarize(Meat = mean(Meat + Poultry), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
PriceSurgeMeatConsumption_Poverty <- PriceSurgeLong_Poverty %>% group_by(poverty, Meal) %>% summarize(Meat = mean(Meat + Poultry), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
PriceSurgeMeatConsumption_Income <- PriceSurgeLong_Income %>% group_by(income, Meal) %>% summarize(Meat = mean(Meat + Poultry), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)

MoreMeatMeatConsumption_Race <- MoreMeatlessOptionsLong_Race %>% group_by(race, Meal) %>% summarize(Meat = mean(Meat + Poultry), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
MoreMeatMeatConsumption_Poverty <- MoreMeatlessOptionsLong_Poverty %>% group_by(poverty, Meal) %>% summarize(Meat = mean(Meat + Poultry), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
MoreMeatMeatConsumption_Income <- MoreMeatlessOptionsLong_Income %>% group_by(income, Meal) %>% summarize(Meat = mean(Meat + Poultry), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)

ComprehensiveMarketingMeatConsumption_Race <- ComprehensiveMarketingLong_Race %>% group_by(race, Meal) %>% summarize(Meat = mean(Meat + Poultry), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
ComprehensiveMarketingMeatConsumption_Poverty <- ComprehensiveMarketingLong_Poverty %>% group_by(poverty, Meal) %>% summarize(Meat = mean(Meat + Poultry), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
ComprehensiveMarketingMeatConsumption_Income <- ComprehensiveMarketingLong_Income %>% group_by(income, Meal) %>% summarize(Meat = mean(Meat + Poultry), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)

# Calculate average meat consumption for each scenario
BaseAvgTSdata <- BaseMeatConsumption_Poverty %>% 
    mutate(weightedMeat = Meat * Population) %>%
    group_by(Meal) %>%
    summarize(weightedMeat = sum(weightedMeat, na.rm = T), sumPopulation = sum(Population, na.rm =T), Population = mean(Population)) %>%
    mutate(MeatPerc = weightedMeat/sumPopulation/Population/100)

MeatlessMondayAvgTSdata <- MeatlessMondayMeatConsumption_Poverty %>% 
    mutate(weightedMeat = Meat * Population) %>%
    group_by(Meal) %>%
    summarize(weightedMeat = sum(weightedMeat, na.rm = T), sumPopulation = sum(Population, na.rm =T), Population = mean(Population)) %>%
    mutate(MeatPerc = weightedMeat/sumPopulation/Population/100)

PriceSurgeAvgTSdata <- PriceSurgeMeatConsumption_Poverty %>% 
    mutate(weightedMeat = Meat * Population) %>%
    group_by(Meal) %>%
    summarize(weightedMeat = sum(weightedMeat, na.rm = T), sumPopulation = sum(Population, na.rm =T), Population = mean(Population)) %>%
    mutate(MeatPerc = weightedMeat/sumPopulation/Population/100)

MoreMeatAvgTSdata <- MoreMeatMeatConsumption_Poverty %>% 
    mutate(weightedMeat = Meat * Population) %>%
    group_by(Meal) %>%
    summarize(weightedMeat = sum(weightedMeat, na.rm = T), sumPopulation = sum(Population, na.rm =T), Population = mean(Population)) %>%
    mutate(MeatPerc = weightedMeat/sumPopulation/Population/100)

ComprehensiveMarketingAvgTSdata <- ComprehensiveMarketingMeatConsumption_Poverty %>% 
    mutate(weightedMeat = Meat * Population) %>%
    group_by(Meal) %>%
    summarize(weightedMeat = sum(weightedMeat, na.rm = T), sumPopulation = sum(Population, na.rm =T), Population = mean(Population)) %>%
    mutate(MeatPerc = weightedMeat/sumPopulation/Population/100)

TSPlotData <- rbind(
    cbind(BaseAvgTSdata, Scenario = "Baseline"),
    cbind(MeatlessMondayAvgTSdata, Scenario = "Scenario 1"),
    cbind(PriceSurgeAvgTSdata, Scenario = "Scenario 2"),
    cbind(MoreMeatAvgTSdata, Scenario = "Scenario 3"),
    cbind(ComprehensiveMarketingAvgTSdata, Scenario = "Scenario 4"))  %>%
    mutate(Scenario = factor(Scenario, 
        levels = c(
            "Baseline",
            'Scenario 1',
            'Scenario 2',
            'Scenario 3',
            'Scenario 4')))


tplotAvg <- ggplot(TSPlotData,
    aes(x = Meal, y = MeatPerc, group = Scenario, color = Scenario)) +
    geom_point(aes(color = Scenario)) +
    geom_line(aes(color = Scenario)) +
    # stat_smooth(aes(color = Scenario, fill = Scenario), method = "loess") +
    geom_vline(color = 'black', xintercept = 50, linetype = "longdash") +
    theme_bw() +
    facet_grid(Scenario~.) +
    scale_color_fish_d(option="Cirrhilabrus_solorensis") + 
    ylab("Average Meat Consumption") +
    xlab("Day (Dinner #)") +
    scale_y_continuous(labels = scales::percent)

pdf(file="RScriptsPlot/OutputPlots/Figure2.pdf", width = 12, height = 7)
print(tplotAvg)
dev.off()
