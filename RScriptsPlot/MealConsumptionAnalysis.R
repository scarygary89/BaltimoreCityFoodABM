library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
library(viridis)
library(fishualize)
library(reshape2)

# Get script directory for relative path construction
mydir <- dirname(normalizePath(sys.frame(1)$ofile))

# Source import functions (relative to project root)
source(file.path("RScriptsPlot", "ImportFunctions.R"))

# Import Data from all Scenarios
output_dir <- file.path("Output")
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


WidenFoodConsData <- function(longDataSet, stratifyingFeature) {
    FoodCons <- longDataSet %>% group_by(!!!rlang::syms(c(stratifyingFeature, "Scenario", "zipcode"))) %>%
        filter(Meal >= 50) %>%
        summarize(
            Meat = sum(Meat),
            Veg = sum(Veg),
            Poultry = sum(Poultry),
            Fish = sum(Fish),
            LegumesBeansNuts = sum(LegumesBeansNuts),
            EggCheese = sum(EggCheese),
            Grains = sum(Grains),
            Population = mean(Population)
        )  %>%
        mutate(
            WeightedMeat = Meat * Population,
            WeightedVeg = Veg * Population,
            WeightedPoultry = Poultry * Population,
            WeightedFish = Fish * Population,
            WeightedLegumesBeansNuts = LegumesBeansNuts * Population,
            WeightedEggCheese = EggCheese * Population,
            WeightedGrains = Grains * Population
        )%>%
        ungroup()
    return(FoodCons)
}

BaseWide_race <- WidenFoodConsData(BaseLong_Race, "race")
MeatlessMondayWide_race <- WidenFoodConsData(MeatlessMondayLong_Race, "race")
PriceSurgeWide_race <- WidenFoodConsData(PriceSurgeLong_Race, "race")
MoreMeatlessOptionsWide_race <- WidenFoodConsData(MoreMeatlessOptionsLong_Race, "race")
ComprehensiveMarketingWide_race <- WidenFoodConsData(ComprehensiveMarketingLong_Race, "race")

BaseWide_poverty <- WidenFoodConsData(BaseLong_Poverty, "poverty")
MeatlessMondayWide_poverty <- WidenFoodConsData(MeatlessMondayLong_Poverty, "poverty") 
PriceSurgeWide_poverty <- WidenFoodConsData(PriceSurgeLong_Poverty, "poverty")
MoreMeatlessOptionsWide_poverty <- WidenFoodConsData(MoreMeatlessOptionsLong_Poverty, "poverty")
ComprehensiveMarketingWide_poverty <- WidenFoodConsData(ComprehensiveMarketingLong_Poverty, "poverty")

BaseWide_income <- WidenFoodConsData(BaseLong_Income, "income")
MeatlessMondayWide_income <- WidenFoodConsData(MeatlessMondayLong_Income, "income") 
PriceSurgeWide_income <- WidenFoodConsData(PriceSurgeLong_Income, "income")
MoreMeatlessOptionsWide_income <- WidenFoodConsData(MoreMeatlessOptionsLong_Income, "income")
ComprehensiveMarketingWide_income <- WidenFoodConsData(ComprehensiveMarketingLong_Income, "income")

CalculateWeightedAverages <- function(wideDataset, stratifyingFeature) {
    numScen <- wideDataset %>% pull(Scenario) %>% unique() %>% length()
    FoodCons <- wideDataset %>%
        group_by(!!!rlang::syms(c(stratifyingFeature, "Scenario"))) %>%
        summarize(
            Meat = sum(WeightedMeat) / sum(Population),
            Veg = sum(WeightedVeg) / sum(Population),
            Poultry = sum(WeightedPoultry) / sum(Population),
            Fish = sum(WeightedFish) / sum(Population),
            LegumesBeansNuts = sum(WeightedLegumesBeansNuts) / sum(Population),
            EggCheese = sum(WeightedEggCheese) / sum(Population),
            Grains = sum(WeightedGrains) / sum(Population),
            Population = sum(Population)
        ) %>%
        group_by(!!!rlang::syms(stratifyingFeature)) %>%
        summarize(
            # Calculate 95% CI
            Meat_low = mean(Meat) - qt(0.975,df=numScen-1)*sd(Meat)/sqrt(numScen),
            Meat_high = mean(Meat) + qt(0.975,df=numScen-1)*sd(Meat)/sqrt(numScen),
            Veg_low = mean(Veg) - qt(0.975,df=numScen-1)*sd(Veg)/sqrt(numScen),
            Veg_high = mean(Veg) + qt(0.975,df=numScen-1)*sd(Veg)/sqrt(numScen),
            Poultry_low = mean(Poultry) - qt(0.975,df=numScen-1)*sd(Poultry)/sqrt(numScen),
            Poultry_high = mean(Poultry) + qt(0.975,df=numScen-1)*sd(Poultry)/sqrt(numScen),
            Fish_low = mean(Fish) - qt(0.975,df=numScen-1)*sd(Fish)/sqrt(numScen),
            Fish_high = mean(Fish) + qt(0.975,df=numScen-1)*sd(Fish)/sqrt(numScen),
            LegumesBeansNuts_low = mean(LegumesBeansNuts) - qt(0.975,df=numScen-1)*sd(LegumesBeansNuts)/sqrt(numScen),
            LegumesBeansNuts_high = mean(LegumesBeansNuts) + qt(0.975,df=numScen-1)*sd(LegumesBeansNuts)/sqrt(numScen),
            EggCheese_low = mean(EggCheese) - qt(0.975,df=numScen-1)*sd(EggCheese)/sqrt(numScen),
            EggCheese_high = mean(EggCheese) + qt(0.975,df=numScen-1)*sd(EggCheese)/sqrt(numScen),
            Grains_low = mean(Grains) - qt(0.975,df=numScen-1)*sd(Grains)/sqrt(numScen),
            Grains_high = mean(Grains) + qt(0.975,df=numScen-1)*sd(Grains)/sqrt(numScen),
            # Calculate Averages
            Meat = mean(Meat),
            Veg = mean(Veg),
            Poultry = mean(Poultry),
            Fish = mean(Fish),
            LegumesBeansNuts = mean(LegumesBeansNuts),
            EggCheese = mean(EggCheese),
            Grains = mean(Grains),
            Population = mean(Population)
        )
    return(FoodCons)
}

BaseFoodCons_race <- CalculateWeightedAverages(BaseWide_race, "race")
MeatlessMondayFoodCons_race <- CalculateWeightedAverages(MeatlessMondayWide_race, "race") 
PriceSurgeFoodCons_race <- CalculateWeightedAverages(PriceSurgeWide_race, "race")
MoreMeatlessOptionsFoodCons_race <- CalculateWeightedAverages(MoreMeatlessOptionsWide_race, "race")
ComprehensiveMarketingFoodCons_race <- CalculateWeightedAverages(ComprehensiveMarketingWide_race, "race")


BaseFoodCons_poverty <- CalculateWeightedAverages(BaseWide_poverty, "poverty")
MeatlessMondayFoodCons_poverty <- CalculateWeightedAverages(MeatlessMondayWide_poverty, "poverty") 
PriceSurgeFoodCons_poverty <- CalculateWeightedAverages(PriceSurgeWide_poverty, "poverty")
MoreMeatlessOptionsFoodCons_poverty <- CalculateWeightedAverages(MoreMeatlessOptionsWide_poverty, "poverty")
ComprehensiveMarketingFoodCons_poverty <- CalculateWeightedAverages(ComprehensiveMarketingWide_poverty, "poverty")


BaseFoodCons_income <- CalculateWeightedAverages(BaseWide_income, "income")
MeatlessMondayFoodCons_income <- CalculateWeightedAverages(MeatlessMondayWide_income, "income") 
PriceSurgeFoodCons_income <- CalculateWeightedAverages(PriceSurgeWide_income, "income")
MoreMeatlessOptionsFoodCons_income <- CalculateWeightedAverages(MoreMeatlessOptionsWide_income, "income")
ComprehensiveMarketingFoodCons_income <- CalculateWeightedAverages(ComprehensiveMarketingWide_income, "income")

CalculateFractionalBreakdown <- function(ConsData){
    FracData <- ConsData %>%
        mutate(
            Total = Meat + Veg + Poultry + Fish + LegumesBeansNuts + EggCheese + Grains,
            "Red Meat_Low" = Meat_low / (Total - Meat + Meat_low),
            "Red Meat_High" = Meat_high / (Total - Meat + Meat_high),
            "Vegetables_Low" = Veg_low / (Total - Veg + Veg_low),
            "Vegetables_High" = Veg_high / (Total - Veg + Veg_high),
            "Poultry_Low" = Poultry_low / (Total - Poultry + Poultry_low),
            "Poultry_High" = Poultry_high / (Total - Poultry + Poultry_high),
            "Fish_Low" = Fish_low / (Total - Fish + Fish_low),
            "Fish_High" = Fish_high / (Total - Fish + Fish_high),
            "Legumes, Beans, & Nuts_Low" = LegumesBeansNuts_low / (Total - LegumesBeansNuts + LegumesBeansNuts_low),
            "Legumes, Beans, & Nuts_High" = LegumesBeansNuts_high / (Total - LegumesBeansNuts + LegumesBeansNuts_high),
            "Eggs & Cheese_Low" = EggCheese_low / (Total - EggCheese + EggCheese_low),
            "Eggs & Cheese_High" = EggCheese_high / (Total - EggCheese + EggCheese_high),
            "Grains_Low" = Grains_low / (Total - Grains + Grains_low),
            "Grains_High" = Grains_high / (Total - Grains + Grains_high),
            "Red Meat" = Meat / Total,
            "Vegetables" = Veg / Total,
            "Poultry" = Poultry / Total,
            "Fish" = Fish / Total,
            "Legumes, Beans, & Nuts" = LegumesBeansNuts / Total,
            "Eggs & Cheese" = EggCheese / Total,
            "Grains" = Grains / Total
        )
    return(FracData)
}

BaseFoodConsFrac_race <- CalculateFractionalBreakdown(BaseFoodCons_race)
MeatlessMondayFoodConsFrac_race  <- CalculateFractionalBreakdown(MeatlessMondayFoodCons_race)
PriceSurgeFoodConsFrac_race  <- CalculateFractionalBreakdown(PriceSurgeFoodCons_race)
MoreMeatlessOptionsFoodConsFrac_race  <- CalculateFractionalBreakdown(MoreMeatlessOptionsFoodCons_race)
ComprehensiveMarketingFoodConsFrac_race  <- CalculateFractionalBreakdown(ComprehensiveMarketingFoodCons_race)

BaseFoodConsFrac_poverty  <- CalculateFractionalBreakdown(BaseFoodCons_poverty)
MeatlessMondayFoodConsFrac_poverty  <- CalculateFractionalBreakdown(MeatlessMondayFoodCons_poverty)
PriceSurgeFoodConsFrac_poverty  <- CalculateFractionalBreakdown(PriceSurgeFoodCons_poverty)
MoreMeatlessOptionsFoodConsFrac_poverty  <- CalculateFractionalBreakdown(MoreMeatlessOptionsFoodCons_poverty)
ComprehensiveMarketingFoodConsFrac_poverty  <- CalculateFractionalBreakdown(ComprehensiveMarketingFoodCons_poverty)

BaseFoodConsFrac_income  <- CalculateFractionalBreakdown(BaseFoodCons_income)
MeatlessMondayFoodConsFrac_income  <- CalculateFractionalBreakdown(MeatlessMondayFoodCons_income)
PriceSurgeFoodConsFrac_income  <- CalculateFractionalBreakdown(PriceSurgeFoodCons_income)
MoreMeatlessOptionsFoodConsFrac_income  <- CalculateFractionalBreakdown(MoreMeatlessOptionsFoodCons_income)
ComprehensiveMarketingFoodConsFrac_income  <- CalculateFractionalBreakdown(ComprehensiveMarketingFoodCons_income)

FoodConsData_race <- rbind(
    cbind(BaseFoodConsFrac_race, Scenario = "No Change (Baseline)"),
    cbind(MeatlessMondayFoodConsFrac_race, Scenario = "Non-meat marketing campaign (Scenario 1)"),
    cbind(PriceSurgeFoodConsFrac_race, Scenario = "Increase in meat pricing (Scenario 2)"),
    cbind(MoreMeatlessOptionsFoodConsFrac_race, Scenario = "Increase in non-meat options (Scenario 3)"),
    cbind(ComprehensiveMarketingFoodConsFrac_race, Scenario = "Combined non-meat push (Scenario 4)")) %>%
    mutate(Scenario = factor(Scenario, 
        levels = c(
            "No Change (Baseline)",
            'Non-meat marketing campaign (Scenario 1)',
            'Increase in meat pricing (Scenario 2)',
            'Increase in non-meat options (Scenario 3)',
            'Combined non-meat push (Scenario 4)')))

FoodConsData_poverty <- rbind(
    cbind(BaseFoodConsFrac_poverty, Scenario = "No Change (Baseline)"),
    cbind(MeatlessMondayFoodConsFrac_poverty, Scenario = "Non-meat marketing campaign (Scenario 1)"),
    cbind(PriceSurgeFoodConsFrac_poverty, Scenario = "Increase in meat pricing (Scenario 2)"),
    cbind(MoreMeatlessOptionsFoodConsFrac_poverty, Scenario = "Increase in non-meat options (Scenario 3)"),
    cbind(ComprehensiveMarketingFoodConsFrac_poverty, Scenario = "Combined non-meat push (Scenario 4)")) %>%
    mutate(Scenario = factor(Scenario, 
        levels = c(
            "No Change (Baseline)",
            'Non-meat marketing campaign (Scenario 1)',
            'Increase in meat pricing (Scenario 2)',
            'Increase in non-meat options (Scenario 3)',
            'Combined non-meat push (Scenario 4)')))
    
FoodConsData_income <- rbind(
    cbind(BaseFoodConsFrac_income, Scenario = "No Change (Baseline)"),
    cbind(MeatlessMondayFoodConsFrac_income, Scenario = "Non-meat marketing campaign (Scenario 1)"),
    cbind(PriceSurgeFoodConsFrac_income, Scenario = "Increase in meat pricing (Scenario 2)"),
    cbind(MoreMeatlessOptionsFoodConsFrac_income, Scenario = "Increase in non-meat options (Scenario 3)"),
    cbind(ComprehensiveMarketingFoodConsFrac_income, Scenario = "Combined non-meat push (Scenario 4)")) %>%
    mutate(Scenario = factor(Scenario, 
        levels = c(
            "No Change (Baseline)",
            'Non-meat marketing campaign (Scenario 1)',
            'Increase in meat pricing (Scenario 2)',
            'Increase in non-meat options (Scenario 3)',
            'Combined non-meat push (Scenario 4)')))

createFracTableOverall <- function(FoodConsFrac){
    FoodConsFracOverall <- FoodConsFrac %>% summarize(
        "Red Meat" =  sum(`Red Meat` * Population) / sum(Population),
        "Vegetables" = sum(`Vegetables` * Population) / sum(Population),
        "Poultry" = sum(`Poultry` * Population) / sum(Population),
        "Fish" = sum(`Fish` * Population) / sum(Population),
        "Legumes, Beans, & Nuts" =  sum(`Legumes, Beans, & Nuts` * Population) / sum(Population),
        "Eggs & Cheese" = sum(`Eggs & Cheese` * Population) / sum(Population),
        "Grains" = sum(`Grains` * Population) / sum(Population)
    )
    return(FoodConsFracOverall)
}

BaseFoodConsFrac <- createFracTableOverall(BaseFoodConsFrac_poverty)
MeatlessMondayFoodConsFrac <- createFracTableOverall(MeatlessMondayFoodConsFrac_poverty)
PriceSurgeFoodConsFrac <- createFracTableOverall(PriceSurgeFoodConsFrac_poverty)
MoreMeatlessOptionsFoodConsFrac <- createFracTableOverall(MoreMeatlessOptionsFoodConsFrac_poverty)
ComprehensiveMarketingFoodConsFrac <- createFracTableOverall(ComprehensiveMarketingFoodConsFrac_poverty)

FoodConsData <- rbind(
    cbind(BaseFoodConsFrac, Scenario = "No Change (Baseline)"),
    cbind(MeatlessMondayFoodConsFrac, Scenario = "Non-meat marketing campaign (Scenario 1)"),
    cbind(PriceSurgeFoodConsFrac, Scenario = "Increase in meat pricing (Scenario 2)"),
    cbind(MoreMeatlessOptionsFoodConsFrac, Scenario = "Increase in non-meat options (Scenario 3)"),
    cbind(ComprehensiveMarketingFoodConsFrac, Scenario = "Combined non-meat push (Scenario 4)")) %>%
    mutate(Scenario = factor(Scenario,
        levels = c(
            "No Change (Baseline)",
            'Non-meat marketing campaign (Scenario 1)',
            'Increase in meat pricing (Scenario 2)',
            'Increase in non-meat options (Scenario 3)',
            'Combined non-meat push (Scenario 4)')))

plotvars <- c("Red Meat", "Poultry", "Fish", "Vegetables", "Legumes, Beans, & Nuts", "Eggs & Cheese", "Grains")
mFoodConsData_race <- reshape2::melt(FoodConsData_race, id.vars=c("Scenario","race")) %>% 
    filter(variable %in% plotvars) %>% 
    mutate(
        variable = factor(variable, 
            levels = c(
                "Red Meat",
                "Poultry",
                "Fish",
                "Vegetables",
                "Legumes, Beans, & Nuts",
                "Eggs & Cheese",
                "Grains")))
mFoodConsData_poverty <- reshape2::melt(FoodConsData_poverty, id.vars=c("Scenario","poverty")) %>% 
    filter(variable %in% plotvars) %>% 
    mutate(variable = factor(variable, 
        levels = c(
            "Red Meat",
            "Poultry",
            "Fish",
            "Vegetables",
            "Legumes, Beans, & Nuts",
            "Eggs & Cheese",
            "Grains")))
mFoodConsData_poverty$poverty[mFoodConsData_poverty$poverty == 1] <- "In Poverty" 
mFoodConsData_poverty$poverty[mFoodConsData_poverty$poverty == 0] <- "Not in Poverty"
mFoodConsData_poverty$poverty <- factor(mFoodConsData_poverty$poverty, levels = c("In Poverty","Not in Poverty"))

mFoodConsData_income <- reshape2::melt(FoodConsData_income, id.vars=c("Scenario","income")) %>% 
    filter(variable %in% plotvars) %>% 
    mutate(
        variable = factor(variable, 
            levels = c(
                "Red Meat",
                "Poultry",
                "Fish",
                "Vegetables",
                "Legumes, Beans, & Nuts",
                "Eggs & Cheese",
                "Grains")),
        income = factor(income,
            levels = c("Less than $25k", "$25k - $55k", "$55k - $75k", "More than $75k")
        )
    ) 

mFoodConsData <- reshape2::melt(FoodConsData, id.vars=c("Scenario")) %>% 
    filter(variable %in% plotvars) %>%
    mutate(variable = factor(variable,
        levels = c(
            "Red Meat",
            "Poultry",
            "Fish",
            "Vegetables",
            "Legumes, Beans, & Nuts",
            "Eggs & Cheese",
            "Grains")))


# Use only lighter/mid-range colors from viridis for all barcharts
custom_viridis <- viridisLite::viridis(7, begin = 0.15, end = 0.85, direction = 1)

barchart_race <- ggplot(mFoodConsData_race,
    aes(x=race, y=value, fill = variable, group = variable)) +
    geom_bar(stat="identity", color="black", position="stack") +
    coord_cartesian() + 
    coord_flip() + 
    theme_minimal() +
    xlab("") +
    ylab("Average Dinner Composition") +
    labs(fill="Food Groups") +
    scale_fill_manual(values = custom_viridis) +
    facet_wrap(Scenario~.) +
    theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=1)) +
    scale_y_continuous(labels = scales::percent, breaks = round(seq(0, 1, by = .1),1))


barchart_poverty <- ggplot(mFoodConsData_poverty,
    aes(x=poverty, y=value, fill = variable, group = variable)) +
    geom_bar(stat="identity", color="black", position="stack") +
    coord_cartesian() + 
    coord_flip() + 
    theme_minimal() +
    xlab("") +
    ylab("Average Dinner Composition") +
    labs(fill="Food Groups") +
    scale_fill_manual(values = custom_viridis) +
    facet_wrap(Scenario~.) +
    theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=1)) +
    scale_y_continuous(labels = scales::percent, breaks = round(seq(0, 1, by = .1),1))


barchart_income <- ggplot(mFoodConsData_income,
    aes(x=income, y=value, fill = variable, group = variable)) +
    geom_bar(stat="identity", color="black", position="stack") +
    coord_cartesian() + 
    coord_flip() + 
    theme_minimal() +
    xlab("") +
    ylab("Average Dinner Composition") +
    labs(fill="Food Groups") +
    scale_fill_manual(values = custom_viridis) +
    facet_wrap(Scenario~.) +
    theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=1)) +
    scale_y_continuous(labels = scales::percent, breaks = round(seq(0, 1, by = .1),1))

# calculate percent labels for each segment
mFoodConsData <- mFoodConsData %>%
    mutate(pct_label = scales::percent(value, accuracy = 1, trim = TRUE))


barchart <- ggplot(mFoodConsData,
    aes(x = Scenario, y = value, fill = variable)) +
        geom_bar(stat = "identity", color = "black", position = "stack") +
        geom_text(
            aes(label = pct_label),
            position = position_stack(vjust = 0.5),
            size = 2.5,
            color = "black",
            fontface = "bold",
            check_overlap = TRUE,
            stroke = 0.7,
            family = "sans"
        ) +
        coord_flip() +
        theme_minimal() +
        xlab("") +
        ylab("Average Dinner Composition") +
        labs(fill = "Food Groups") +
        scale_fill_manual(values = custom_viridis) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust = 1))


pdf(file="RScriptsPlot/OutputPlots/MealBreakdownByRace.pdf", width = 15, height = 10)
print(barchart_race)
dev.off()

pdf(file="RScriptsPlot/OutputPlots/MealBreakdownByIncome.pdf", width = 15, height = 10)
print(barchart_income)
dev.off()

pdf(file="RScriptsPlot/OutputPlots/MealBreakdownByPoverty.pdf", width = 15, height = 10)
print(barchart_poverty)
dev.off()

pdf(file="RScriptsPlot/OutputPlots/Figure3.pdf", width = 10, height = 6)
print(barchart)
dev.off()

# Create Tables
createFracTableOverallWithCI <- function(FoodConsFrac){
    FoodConsFracOverall <- FoodConsFrac %>% summarize(
        "Red Meat" = sum(`Red Meat` * Population) / sum(Population),
        "Red Meat_Low" = sum(`Red Meat_Low` * Population) / sum(Population),
        "Red Meat_High" = sum(`Red Meat_High` * Population) / sum(Population),
        "Vegetables" = sum(`Vegetables` * Population) / sum(Population),
        "Vegetables_Low" = sum(`Vegetables_Low` * Population) / sum(Population),
        "Vegetables_High" = sum(`Vegetables_High` * Population) / sum(Population),
        "Poultry" = sum(`Poultry` * Population) / sum(Population),
        "Poultry_Low" = sum(`Poultry_Low` * Population) / sum(Population),
        "Poultry_High" = sum(`Poultry_High` * Population) / sum(Population),
        "Fish" = sum(`Fish` * Population) / sum(Population),
        "Fish_Low" = sum(`Fish_Low` * Population) / sum(Population),
        "Fish_High" = sum(`Fish_High` * Population) / sum(Population),
        "Legumes, Beans, & Nuts" =  sum(`Legumes, Beans, & Nuts` * Population) / sum(Population),
        "Legumes, Beans, & Nuts_Low" =  sum(`Legumes, Beans, & Nuts_Low` * Population) / sum(Population),
        "Legumes, Beans, & Nuts_High" =  sum(`Legumes, Beans, & Nuts_High` * Population) / sum(Population),
        "Eggs & Cheese" = sum(`Eggs & Cheese` * Population) / sum(Population),
        "Eggs & Cheese_Low" = sum(`Eggs & Cheese_Low` * Population) / sum(Population),
        "Eggs & Cheese_High" = sum(`Eggs & Cheese_High` * Population) / sum(Population),
        "Grains" = sum(`Grains` * Population) / sum(Population),
        "Grains_Low" = sum(`Grains_Low` * Population) / sum(Population),
        "Grains_High" = sum(`Grains_High` * Population) / sum(Population)
    )
    return(FoodConsFracOverall)
}

BaseFoodConsFracWithCI <- createFracTableOverallWithCI(BaseFoodConsFrac_poverty)
MeatlessMondayFoodConsFracWithCI <- createFracTableOverallWithCI(MeatlessMondayFoodConsFrac_poverty)
PriceSurgeFoodConsFracWithCI <- createFracTableOverallWithCI(PriceSurgeFoodConsFrac_poverty)
MoreMeatlessOptionsFoodConsFracWithCI <- createFracTableOverallWithCI(MoreMeatlessOptionsFoodConsFrac_poverty)
ComprehensiveMarketingFoodConsFracWithCI <- createFracTableOverallWithCI(ComprehensiveMarketingFoodConsFrac_poverty)

FoodConsDataWithCI <- rbind(
    cbind(BaseFoodConsFracWithCI, Scenario = "No Change (Baseline)"),
    cbind(MeatlessMondayFoodConsFracWithCI, Scenario = "Non-meat marketing campaign (Scenario 1)"),
    cbind(PriceSurgeFoodConsFracWithCI, Scenario = "Increase in meat pricing (Scenario 2)"),
    cbind(MoreMeatlessOptionsFoodConsFracWithCI, Scenario = "Increase in non-meat options (Scenario 3)"),
    cbind(ComprehensiveMarketingFoodConsFracWithCI, Scenario = "Combined non-meat push (Scenario 4)")) %>%
    mutate(Scenario = factor(Scenario,
        levels = c(
            "No Change (Baseline)",
            'Non-meat marketing campaign (Scenario 1)',
            'Increase in meat pricing (Scenario 2)',
            'Increase in non-meat options (Scenario 3)',
            'Combined non-meat push (Scenario 4)')))
tablevars <- c(
    "Red Meat",
    "Red Meat_Low",
    "Red Meat_High",
    "Poultry",
    "Poultry_Low",
    "Poultry_High",
    "Fish", 
    "Fish_Low", 
    "Fish_High", 
    "Vegetables", 
    "Vegetables_Low", 
    "Vegetables_High", 
    "Legumes, Beans, & Nuts", 
    "Legumes, Beans, & Nuts_Low", 
    "Legumes, Beans, & Nuts_High", 
    "Eggs & Cheese", 
    "Eggs & Cheese_Low", 
    "Eggs & Cheese_High", 
    "Grains",
    "Grains_Low",
    "Grains_High"
)

mFoodConsDataWithCI <- reshape2::melt(FoodConsDataWithCI, id.vars=c("Scenario")) %>% 
    filter(variable %in% tablevars) %>%
    mutate(variable = factor(variable,
        levels = tablevars))

mFoodConsDataWithCI_race <- reshape2::melt(FoodConsData_race, id.vars=c("Scenario","race")) %>% 
    filter(variable %in% tablevars) %>% 
    mutate(
        variable = factor(variable, 
            levels = tablevars))
mFoodConsDataWithCI_poverty <- reshape2::melt(FoodConsData_poverty, id.vars=c("Scenario","poverty")) %>% 
    filter(variable %in% tablevars) %>% 
    mutate(variable = factor(variable, 
        levels = tablevars))
mFoodConsDataWithCI_poverty$poverty[mFoodConsDataWithCI_poverty$poverty == 1] <- "In Poverty" 
mFoodConsDataWithCI_poverty$poverty[mFoodConsDataWithCI_poverty$poverty == 0] <- "Not in Poverty"
mFoodConsDataWithCI_poverty$poverty <- factor(mFoodConsDataWithCI_poverty$poverty, levels = c("In Poverty","Not in Poverty"))

mFoodConsDataWithCI_income <- reshape2::melt(FoodConsData_income, id.vars=c("Scenario","income")) %>% 
    filter(variable %in% tablevars) %>% 
    mutate(
        variable = factor(variable, 
            levels = tablevars),
        income = factor(income,
            levels = c("Less than $25k", "$25k - $55k", "$55k - $75k", "More than $75k")
        )
    )

tableData_race <- mFoodConsDataWithCI_race %>% spread(Scenario,value)
tableData_income <- mFoodConsDataWithCI_income %>% spread(Scenario,value)
tableData_poverty <- mFoodConsDataWithCI_poverty %>% spread(Scenario,value)
tableData <- mFoodConsDataWithCI %>% spread(Scenario,value)

write.csv(tableData_race, row.names =F, file ="RScriptsPlot/OutputPlots/MealBreakdownByRace.csv")
write.csv(tableData_income, row.names =F, file ="RScriptsPlot/OutputPlots/MealBreakdownByIncome.csv")
write.csv(tableData_poverty, row.names =F, file ="RScriptsPlot/OutputPlots/MealBreakdownByPoverty.csv")
write.csv(tableData, row.names = F, file ="RScriptsPlot/OutputPlots/MealBreakdown.csv")

meatRedTableData_race <- tableData_race %>%
    filter(variable %in% c("Red Meat", "Poultry")) %>%
    group_by(race) %>%
    summarize(
        "No Change (Baseline)" = sum(`No Change (Baseline)`),
        "Non-meat marketing campaign (Scenario 1)" = sum(`Non-meat marketing campaign (Scenario 1)`),
        "Increase in meat pricing (Scenario 2)" = sum(`Increase in meat pricing (Scenario 2)`),
        "Increase in non-meat options (Scenario 3)" = sum(`Increase in non-meat options (Scenario 3)`),
        "Combined non-meat push (Scenario 4)" = sum(`Combined non-meat push (Scenario 4)`)
    ) %>%
    mutate(
        "Non-meat marketing campaign (Scenario 1)" = (`No Change (Baseline)` - `Non-meat marketing campaign (Scenario 1)`)/`No Change (Baseline)`,
        'Increase in meat pricing (Scenario 2)' = (`No Change (Baseline)` - `Increase in meat pricing (Scenario 2)`)/`No Change (Baseline)`,
        'Increase in non-meat options (Scenario 3)' = (`No Change (Baseline)` - `Increase in non-meat options (Scenario 3)`)/`No Change (Baseline)`,
        'Combined non-meat push (Scenario 4)' = (`No Change (Baseline)` - `Combined non-meat push (Scenario 4)`)/`No Change (Baseline)`
    ) %>% select(-c(`No Change (Baseline)`))

meatRedTableData_income <- tableData_income %>% 
    filter(variable %in% c("Red Meat", "Poultry")) %>%
    group_by(income) %>%
    summarize(
        "No Change (Baseline)" = sum(`No Change (Baseline)`),
        "Non-meat marketing campaign (Scenario 1)" = sum(`Non-meat marketing campaign (Scenario 1)`),
        "Increase in meat pricing (Scenario 2)" = sum(`Increase in meat pricing (Scenario 2)`),
        "Increase in non-meat options (Scenario 3)" = sum(`Increase in non-meat options (Scenario 3)`),
        "Combined non-meat push (Scenario 4)" = sum(`Combined non-meat push (Scenario 4)`)
    ) %>%
    mutate(
        "Non-meat marketing campaign (Scenario 1)" = (`No Change (Baseline)` - `Non-meat marketing campaign (Scenario 1)`)/`No Change (Baseline)`,
        'Increase in meat pricing (Scenario 2)' = (`No Change (Baseline)` - `Increase in meat pricing (Scenario 2)`)/`No Change (Baseline)`,
        'Increase in non-meat options (Scenario 3)' = (`No Change (Baseline)` - `Increase in non-meat options (Scenario 3)`)/`No Change (Baseline)`,
        'Combined non-meat push (Scenario 4)' = (`No Change (Baseline)` - `Combined non-meat push (Scenario 4)`)/`No Change (Baseline)`
    ) %>% select(-c(`No Change (Baseline)`))

meatRedTableData_poverty <- tableData_poverty %>% 
    filter(variable %in% c("Red Meat", "Poultry")) %>%
    group_by(poverty) %>%
    summarize(
        "No Change (Baseline)" = sum(`No Change (Baseline)`),
        "Non-meat marketing campaign (Scenario 1)" = sum(`Non-meat marketing campaign (Scenario 1)`),
        "Increase in meat pricing (Scenario 2)" = sum(`Increase in meat pricing (Scenario 2)`),
        "Increase in non-meat options (Scenario 3)" = sum(`Increase in non-meat options (Scenario 3)`),
        "Combined non-meat push (Scenario 4)" = sum(`Combined non-meat push (Scenario 4)`)
    ) %>%
   mutate(
        "Non-meat marketing campaign (Scenario 1)" = (`No Change (Baseline)` - `Non-meat marketing campaign (Scenario 1)`)/`No Change (Baseline)`,
        'Increase in meat pricing (Scenario 2)' = (`No Change (Baseline)` - `Increase in meat pricing (Scenario 2)`)/`No Change (Baseline)`,
        'Increase in non-meat options (Scenario 3)' = (`No Change (Baseline)` - `Increase in non-meat options (Scenario 3)`)/`No Change (Baseline)`,
        'Combined non-meat push (Scenario 4)' = (`No Change (Baseline)` - `Combined non-meat push (Scenario 4)`)/`No Change (Baseline)`
    ) %>% select(-c(`No Change (Baseline)`))


write.csv(meatRedTableData_race, row.names =F, file ="RScriptsPlot/OutputPlots/MeatReductionByRace.csv")
write.csv(meatRedTableData_income, row.names =F, file ="RScriptsPlot/OutputPlots/MeatReductionByIncome.csv")
write.csv(meatRedTableData_poverty, row.names =F, file ="RScriptsPlot/OutputPlots/MeatReductionByPoverty.csv")