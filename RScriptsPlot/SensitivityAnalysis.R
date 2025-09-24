library(rgdal)
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
library(fishualize)
library(reshape2)
library(ggpubr)

mydir <- setwd('C:/Users/aadam/Desktop/TestABM')
source("RScriptsPlot/ImportFunctions.R")

MarketingAvailabilityData <- DetectAndImportSensitivity(paste0(mydir,"/Output_Sensitivity"), '_MarketingSensitivity', combo = TRUE)
PriceAvailiabilityData <- DetectAndImportSensitivity(paste0(mydir,"/Output_Sensitivity"), '_PricingSensitivity', combo = TRUE)
PriceData <- DetectAndImportSensitivity(paste0(mydir,"/Output_Sensitivity"), 'PricingSensitivity')
SupplyData <- DetectAndImportSensitivity(paste0(mydir,"/Output_Sensitivity"), 'SupplySensitivity')
AvailabilityData <- DetectAndImportSensitivity(paste0(mydir,"/Output_Sensitivity"), 'AvailabilitySensitivity')
MarketingData <- DetectAndImportSensitivity(paste0(mydir,"/Output_Sensitivity"), 'MarketingSensitivity')

MarketingAvailabilityMat <- createLongDataSensitivtyAnalysis("Availability", "Marketing", MarketingAvailabilityData, 'Poverty')
PriceAvailabilityMat <- createLongDataSensitivtyAnalysis("Availability", "Pricing", PriceAvailiabilityData, "Poverty")
PriceMat <- createLongDataSensitivtyAnalysis('Pricing', "none", PriceData, "Poverty")
SupplyMat <- createLongDataSensitivtyAnalysis('Supply', "none", SupplyData, "Poverty")
AvailabilityMat <- createLongDataSensitivtyAnalysis('Availability', "none", AvailabilityData, "Poverty")
MarketingMat <- createLongDataSensitivtyAnalysis('Marketing', "none", MarketingData, "Poverty")


calculatePercentage <- function(InputMat, terms){
    MeatPercSummary <- InputMat %>%
        filter(Meal >= 50) %>%
        group_by(!!!rlang::syms(terms), poverty) %>% 
        summarize(
            AllMeat = sum(Meat + Poultry),
            RedMeat = sum(Meat),
            Poultry = sum(Poultry),
            Veg = sum(Veg),
            Fish = sum(Fish),
            LegumesBeansNuts = sum(LegumesBeansNuts),
            EggCheese = sum(EggCheese),
            Grains = sum(Grains),
            TotalCons = sum(Veg + RedMeat + Poultry + Fish + LegumesBeansNuts + EggCheese + Grains),
            Population = mean(Population),
            weightedRedMeatPerc = RedMeat / TotalCons * Population,
            weightedPoultryPerc = Poultry / TotalCons * Population,
            weightedAllMeatPerc = AllMeat / TotalCons * Population,
            weightedVegPerc = Veg / TotalCons * Population,
            weightedFishPerc = Fish / TotalCons * Population,
            weightedLegumesBeansNutsPerc = LegumesBeansNuts / TotalCons * Population,
            weightedEggCheesePerc = EggCheese / TotalCons * Population,
            weightedGrainsPerc = Grains / TotalCons * Population
        ) %>%
        group_by(!!!rlang::syms(terms)) %>% 
        summarize(
            RedMeatPerc = sum(weightedRedMeatPerc) / sum(Population),
            PoultryPerc = sum(weightedPoultryPerc) / sum(Population),
            AllMeatPerc = sum(weightedAllMeatPerc) / sum(Population),
            VegPerc = sum(weightedVegPerc) / sum(Population),
            FishPerc = sum(weightedFishPerc) / sum(Population),
            LegumesBeansNutsPerc = sum(weightedLegumesBeansNutsPerc) / sum(Population),
            EggCheesePerc = sum(weightedEggCheesePerc) / sum(Population),
            GrainsPerc = sum(weightedGrainsPerc) / sum(Population)
        ) 
}




MarketingAvailabilityPerc <- calculatePercentage(MarketingAvailabilityMat, c("Availability", "Marketing")) %>% 
    ungroup() %>%
    mutate(AllMeatPerc_base = (AllMeatPerc - AllMeatPerc[Availability == 1 & Marketing == 1]) / AllMeatPerc[Availability == 1 & Marketing == 1])
PriceAvailabilityPerc <- calculatePercentage(PriceAvailabilityMat, c("Availability", "Pricing")) %>%
    ungroup() %>%
    mutate(AllMeatPerc_base = (AllMeatPerc - AllMeatPerc[Availability == 1 & Pricing == 1]) / AllMeatPerc[Availability == 1 & Pricing == 1])

MarketingAvailabilityPlot <- ggplot(MarketingAvailabilityPerc, aes(Availability, Marketing, fill= AllMeatPerc_base)) + 
  geom_tile() +
  theme_minimal() +
  labs(title = "Two-way Sensitivity Analysis \n(Scenario 4)", x = "Parameter Multiplier (Increase in non-meat options)", y = "Parameter Multiplier (Non-meat marketing campaign)") +
  scale_fill_fish(
    option = 'Cirrhilabrus_solorensis', 
    labels = scales::percent, name = "Relative Impact in \nMeat Consumption"
  ) +
  theme( plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))

PriceAvailabilityPlot <- ggplot(PriceAvailabilityPerc, aes(Availability, Pricing, fill= AllMeatPerc_base)) + 
  geom_tile() +
  theme_minimal() +
  labs(title = "Two-way Sensitivity Analysis", x = "Parameter Multiplier (Availability)", y = "Parameter Multiplier  (Pricing)") +
  scale_fill_fish(option = 'Cirrhilabrus_solorensis', labels = scales::percent, name = "Relative Impact in \nMeat Consumption")
  
CalculatedRelativePercentChange <- function(InputMat){
    BasePercSummary <- InputMat %>%
        mutate(
            RedMeatPerc_base = (RedMeatPerc - RedMeatPerc[SensitivityMult == 1]) / RedMeatPerc[SensitivityMult == 1],
            PoultryPerc_base = (PoultryPerc - PoultryPerc[SensitivityMult == 1]) / PoultryPerc[SensitivityMult == 1],
            AllMeatPerc_base = (AllMeatPerc - AllMeatPerc[SensitivityMult == 1]) / AllMeatPerc[SensitivityMult == 1],
            VegPerc_base = (VegPerc - VegPerc[SensitivityMult == 1]) / VegPerc[SensitivityMult == 1],
            FishPerc_base = (FishPerc - FishPerc[SensitivityMult == 1]) / FishPerc[SensitivityMult == 1],
            LegumesBeansNutsPerc_base = (LegumesBeansNutsPerc - LegumesBeansNutsPerc[SensitivityMult == 1]) / LegumesBeansNutsPerc[SensitivityMult == 1],
            EggCheesePerc_base = (EggCheesePerc - EggCheesePerc[SensitivityMult == 1]) / EggCheesePerc[SensitivityMult == 1],
            GrainsPerc_base = (GrainsPerc - GrainsPerc[SensitivityMult == 1]) / GrainsPerc[SensitivityMult == 1]

        )
    return(BasePercSummary)
}


PricePerc <- calculatePercentage(PriceMat, c('Pricing')) %>% rename(SensitivityMult = Pricing) %>% CalculatedRelativePercentChange() 
SupplyPerc <- calculatePercentage(SupplyMat, c('Supply')) %>% rename(SensitivityMult = Supply) %>% CalculatedRelativePercentChange()
AvailabilityPerc <- calculatePercentage(AvailabilityMat, c('Availability')) %>% rename(SensitivityMult = Availability) %>% CalculatedRelativePercentChange()
MarketingPerc <- calculatePercentage(MarketingMat, c('Marketing')) %>% rename(SensitivityMult = Marketing) %>% CalculatedRelativePercentChange()

singleSensitivityData <- rbind(
    # cbind(SupplyPerc, Parameter = "SupplyShortage"),
    cbind(MarketingPerc, Parameter = "Non-meat marketing campaign \n(Scenario 1)"),
    cbind(PricePerc, Parameter = "Increase in meat pricing \n(Scenario 2)"),
    cbind(AvailabilityPerc, Parameter = "Increase in non-meat options \n(Scenario 3)")
    ) %>%
    mutate(Parameter = factor(Parameter, 
        levels = c(
        "Non-meat marketing campaign \n(Scenario 1)",
        "Increase in meat pricing \n(Scenario 2)",
        "Increase in non-meat options \n(Scenario 3)")))

SingleParmPlot <- ggplot(singleSensitivityData, aes(x = SensitivityMult, y = AllMeatPerc_base, fill = Parameter)) +
  geom_bar(stat="identity") +
  facet_grid(Parameter ~.)

# windows()
# print(SingleParmPlot)

onewayplot <- ggplot(singleSensitivityData, aes(x = SensitivityMult, y = AllMeatPerc_base, fill = SensitivityMult)) +
    coord_flip() + 
    geom_bar(stat = "identity", position = "identity", width = 0.525) +
    facet_grid(Parameter~.) +
    scale_fill_fish_d(option = 'Ostracion_whitleyi') +
    scale_y_continuous(labels = scales::percent) +
    theme_bw() + 
    geom_hline(yintercept = 0, linetype='dashed') +
    labs(title = "One-way Sensitivity Analysis \n(Scenarios 1-3)", x = "Parameter Multiplier", y = "Relative Impact in Meat Consumption") +
    theme(legend.position="none", 
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 10), 
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")
    )

pdf(file="RScriptsPlot/OutputPlots/OneWaySensitivityPlot.pdf", width = 7, height = 7)
print(onewayplot)
dev.off()


SensitivityAnalysisPlot <- ggarrange(
  onewayplot,
  MarketingAvailabilityPlot,
  labels = c("A","B"),
  ncol = 2, nrow = 1,
#   widths = c(1,1),
    font.label = list(size = 20)
)

pdf(file="RScriptsPlot/OutputPlots/ScenariosSensitivityPlot.pdf", width = 15, height = 7)
print(SensitivityAnalysisPlot)
dev.off()

