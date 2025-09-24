import numpy as np
from matplotlib import pylab as pl
import random
from timeit import default_timer as timer
from datetime import timedelta

import src.AgentProcs

# Define a class that contains the equations, parameters, and initial conditions
class FoodModel:
    def __init__(self, globalInputDict, foodPricesDict, supplyTSDict, marketingTSDict, agentList, storeList, storeDict):
        # Model Parms
        self.iterations = 400
        self.agentList = agentList
        self.storeList = storeList
        self.storeDict = storeDict
        self.MealMass = globalInputDict['MealMass']

        # Global Parms
        self.VegPrices = foodPricesDict['VegPrices']
        self.MeatPrices = foodPricesDict['MeatPrices']
        self.PoultryPrices = foodPricesDict['PoultryPrices']
        self.FishPrices = foodPricesDict['FishPrices']
        self.LegumesBeansNutsPrices = foodPricesDict['LegumesBeansNutsPrices']
        self.EggCheesePrices = foodPricesDict['EggCheesePrices']
        self.GrainsPrices = foodPricesDict['GrainsPrices']
        self.OtherPrices = foodPricesDict['OtherPrices']
        self.EatingOutPrices = foodPricesDict['EatingOutPrices']

        self.VegUtilityMultiplier = globalInputDict['VegUtilityMultiplier']
        self.MeatUtilityMultiplier = globalInputDict['MeatUtilityMultiplier']
        self.PoultryUtilityMultiplier = globalInputDict['PoultryUtilityMultiplier']
        self.FishUtilityMultiplier = globalInputDict['FishUtilityMultiplier']
        self.LegumesBeansNutsUtilityMultiplier = globalInputDict['LegumesBeansNutsUtilityMultiplier']
        self.EggCheeseUtilityMultiplier = globalInputDict['EggCheeseUtilityMultiplier']
        self.GrainsUtilityMultiplier = globalInputDict['GrainsUtilityMultiplier']
        self.OtherUtilityMultiplier = globalInputDict['OtherUtilityMultiplier']

        self.VegMarketing = marketingTSDict['Veg']
        self.MeatMarketing = marketingTSDict['Meat']
        self.PoultryMarketing = marketingTSDict['Poultry']
        self.FishMarketing = marketingTSDict['Fish']
        self.LegumesBeansNutsMarketing = marketingTSDict['LegumesBeansNuts']
        self.EggCheeseMarketing = marketingTSDict['EggCheese']
        self.GrainsMarketing = marketingTSDict['Grains']
        self.OtherMarketing = marketingTSDict['Other']

        self.AffordabilityMultiplier = globalInputDict['AffordabilityMultiplier']
        self.MarketingMultiplier = globalInputDict['MarketingMultiplier']
        self.HealthMultiplier = globalInputDict['HealthMultiplier']
        self.PriceMultiplier = globalInputDict['PriceMultiplier']

        self.VegPriceVolatility = globalInputDict['VegPriceVolatility']
        self.MeatPriceVolatility = globalInputDict['MeatPriceVolatility']
        self.PoultryPriceVolatility = globalInputDict['PoultryPriceVolatility']
        self.FishPriceVolatility = globalInputDict['FishPriceVolatility']
        self.LegumesBeansNutsPriceVolatility = globalInputDict['LegumesBeansNutsPriceVolatility']
        self.EggCheesePriceVolatility = globalInputDict['EggCheesePriceVolatility']
        self.GrainsPriceVolatility = globalInputDict['GrainsPriceVolatility']
        self.OtherPriceVolatility = globalInputDict['OtherPriceVolatility']
        self.EatingOutPriceVolatility = globalInputDict['EatingOutPriceVolatility']

        self.StoreSupplyShock = supplyTSDict

    # Define a function which calculates the derivative
    def StepProc(self, t):
        # Marketing Efforts
        VegMarketing = self.VegMarketing[t]
        MeatMarketing = self.MeatMarketing[t]
        PoultryMarketing = self.PoultryMarketing[t]
        FishMarketing = self.FishMarketing[t]
        LegumesBeansNutsMarketing = self.LegumesBeansNutsMarketing[t]
        EggCheeseMarketing = self.EggCheeseMarketing[t]
        GrainsMarketing = self.GrainsMarketing[t]
        OtherMarketing = self.OtherMarketing[t]

        # Price Dynamics
        VegPrices = self.VegPrices[t] + random.uniform(-1,1) * self.VegPriceVolatility
        MeatPrices = self.MeatPrices[t] + random.uniform(-1,1) * self.MeatPriceVolatility
        PoultryPrices = self.PoultryPrices[t] + random.uniform(-1,1) * self.PoultryPriceVolatility
        FishPrices = self.FishPrices[t] + random.uniform(-1,1) * self.FishPriceVolatility
        LegumesBeansNutsPrices = self.LegumesBeansNutsPrices[t] + random.uniform(-1,1) * self.LegumesBeansNutsPriceVolatility
        EggCheesePrices = self.EggCheesePrices[t] + random.uniform(-1,1) * self.EggCheesePriceVolatility
        GrainsPrices = self.GrainsPrices[t] + random.uniform(-1,1) * self.GrainsPriceVolatility
        OtherPrices = self.OtherPrices[t] + random.uniform(-1,1) * self.OtherPriceVolatility
        EatingOutPrices = self.EatingOutPrices[t] + random.uniform(-1,1) * self.EatingOutPriceVolatility

        # Agent Level Processes
        FoodConsumptionDict = {'Veg':0, 'Meat':0, 'Poultry':0, 'Fish':0, 'LegumesBeansNuts':0, 'Grains':0, 'Other':0}

        for store in self.storeList:
            store.Restock(self.StoreSupplyShock[store.Type][t])

        for agent in self.agentList:
            # External influence on Agent's Meal Location and Source Preference
            EatingOutAffordabilityPressure = agent.EatingOutPriceSensitivity * np.exp(-self.PriceMultiplier * EatingOutPrices)
            
            ProbEatingAtHome = agent.ProbEatingAtHome 
            ProbEatingOut = agent.ProbEatingOut * EatingOutAffordabilityPressure
            ProbNotEat = agent.ProbNotEat

            ProbOfVisitConvenienceStores = agent.ProbOfVisitConvenienceStores
            ProbOfVisitFastfood = agent.ProbOfVisitFastfood
            ProbOfVisitRestaurants = agent.ProbOfVisitRestaurants
            ProbOfVisitSupermarketsStores = agent.ProbOfVisitSupermarketsStores

            ProbCookedDinner = agent.ProbCookedDinner
            ProbLeftOverDinner = agent.ProbLeftOverDinner
            ProbFrozenDinner = agent.ProbFrozenDinner
            ProbRestuarantDinner = agent.ProbRestuarantDinner
            ProbFastfoodDinner = agent.ProbFastfoodDinner
            ProbCarryoutDinner = agent.ProbCarryoutDinner

            BaseProbDict = agent.ChooseMealSourceLocs(ProbEatingAtHome, ProbEatingOut, ProbNotEat,  
            ProbOfVisitConvenienceStores, ProbOfVisitFastfood, ProbOfVisitRestaurants, ProbOfVisitSupermarketsStores, 
            ProbCookedDinner, ProbLeftOverDinner, ProbFrozenDinner, ProbRestuarantDinner, ProbFastfoodDinner, ProbCarryoutDinner, self.storeDict)
            if BaseProbDict == 'Nothing':
                VegProb = 0
                MeatProb = 0
                PoultryProb = 0
                FishProb = 0
                LegumesBeansNutsProb = 0
                EggCheeseProb = 0
                GrainsProb = 0
                OtherProb = 0
            else:
                VegMarketingPressure = agent.SusceptibleToMarketingToVeg * VegMarketing * self.MarketingMultiplier
                MeatMarketingPressure = agent.SusceptibleToMarketingToMeat * MeatMarketing * self.MarketingMultiplier
                PoultryMarketingPressure = agent.SusceptibleToMarketingToPoultry * PoultryMarketing * self.MarketingMultiplier
                FishMarketingPressure = agent.SusceptibleToMarketingToFish * FishMarketing * self.MarketingMultiplier
                LegumesBeansNutsMarketingPressure = agent.SusceptibleToMarketingToLegumesBeansNuts * LegumesBeansNutsMarketing * self.MarketingMultiplier
                EggCheeseMarketingPressure = agent.SusceptibleToMarketingToEggCheese * EggCheeseMarketing * self.MarketingMultiplier
                GrainsMarketingPressure = agent.SusceptibleToMarketingToGrains * GrainsMarketing * self.MarketingMultiplier
                OtherMarketingPressure = agent.SusceptibleToMarketingToOther * OtherMarketing * self.MarketingMultiplier

                VegAffordabilityPressure = np.exp(-self.PriceMultiplier * VegPrices ) * agent.VegPriceSensitivity * self.AffordabilityMultiplier
                MeatAffordabilityPressure = np.exp(-self.PriceMultiplier * MeatPrices) * agent.MeatPriceSensitivity * self.AffordabilityMultiplier
                PoultryAffordabilityPressure = np.exp(-self.PriceMultiplier * PoultryPrices) * agent.PoultryPriceSensitivity * self.AffordabilityMultiplier
                FishAffordabilityPressure = np.exp(-self.PriceMultiplier * FishPrices) * agent.FishPriceSensitivity * self.AffordabilityMultiplier
                LegumesBeansNutsAffordabilityPressure = np.exp(-self.PriceMultiplier * LegumesBeansNutsPrices) * agent.LegumesBeansNutsPriceSensitivity * self.AffordabilityMultiplier
                EggCheeseAffordabilityPressure = np.exp(-self.PriceMultiplier * EggCheesePrices) * agent.EggCheesePriceSensitivity * self.AffordabilityMultiplier
                GrainsAffordabilityPressure = np.exp(-self.PriceMultiplier * GrainsPrices) * agent.GrainsPriceSensitivity * self.AffordabilityMultiplier
                OtherAffordabilityPressure = np.exp(-self.PriceMultiplier * OtherPrices) * agent.OtherPriceSensitivity * self.AffordabilityMultiplier

                VegHealthPressure = agent.HealthConsciousToConsumeVeg * agent.health * self.HealthMultiplier
                MeatHealthPressure = agent.HealthConsciousToConsumeMeat * agent.health * self.HealthMultiplier
                PoultryHealthPressure = agent.HealthConsciousToConsumePoultry * agent.health * self.HealthMultiplier
                FishHealthPressure = agent.HealthConsciousToConsumeFish * agent.health * self.HealthMultiplier
                LegumesBeansNutsHealthPressure = agent.HealthConsciousToConsumeLegumesBeansNuts * agent.health * self.HealthMultiplier
                EggCheeseHealthPressure = agent.HealthConsciousToConsumeEggCheese * agent.health * self.HealthMultiplier
                GrainsHealthPressure = agent.HealthConsciousToConsumeGrains * agent.health * self.HealthMultiplier
                OtherHealthPressure = agent.HealthConsciousToConsumeOther * agent.health *  self.HealthMultiplier

                VegProb = BaseProbDict['VegProb'] + self.VegUtilityMultiplier * (VegMarketingPressure + VegAffordabilityPressure + VegHealthPressure)
                MeatProb = BaseProbDict['MeatProb'] + self.MeatUtilityMultiplier * (MeatMarketingPressure + MeatAffordabilityPressure + MeatHealthPressure)
                PoultryProb = BaseProbDict['PoultryProb'] + self.PoultryUtilityMultiplier * (PoultryMarketingPressure + PoultryAffordabilityPressure + PoultryHealthPressure)
                FishProb = BaseProbDict['FishProb'] + self.FishUtilityMultiplier * (FishMarketingPressure + FishAffordabilityPressure + FishHealthPressure)
                LegumesBeansNutsProb = BaseProbDict['LegumesBeansNutsProb'] + self.LegumesBeansNutsUtilityMultiplier * (LegumesBeansNutsMarketingPressure + LegumesBeansNutsAffordabilityPressure + LegumesBeansNutsHealthPressure)
                EggCheeseProb = BaseProbDict['EggCheeseProb'] + self.EggCheeseUtilityMultiplier * (EggCheeseMarketingPressure + EggCheeseAffordabilityPressure + EggCheeseHealthPressure)
                GrainsProb = BaseProbDict['GrainsProb'] + self.GrainsUtilityMultiplier * (GrainsMarketingPressure + GrainsAffordabilityPressure + GrainsHealthPressure)
                OtherProb = BaseProbDict['OtherProb'] + self.OtherUtilityMultiplier * (OtherMarketingPressure + OtherAffordabilityPressure + OtherHealthPressure)

            agentConsumptionDict = agent.ChooseFoodType(VegProb, MeatProb, PoultryProb, FishProb, LegumesBeansNutsProb, EggCheeseProb, GrainsProb, OtherProb, self.MealMass)
            
            FoodConsumptionDict['Veg'] += agentConsumptionDict['Veg']
            FoodConsumptionDict['Meat'] += agentConsumptionDict['Meat']
            FoodConsumptionDict['Poultry'] += agentConsumptionDict['Poultry']
            FoodConsumptionDict['Fish'] += agentConsumptionDict['Fish']
            FoodConsumptionDict['LegumesBeansNuts'] += agentConsumptionDict['LegumesBeansNuts']
            FoodConsumptionDict['Grains'] += agentConsumptionDict['Grains']
            FoodConsumptionDict['Other'] += agentConsumptionDict['Other']

        return FoodConsumptionDict


    def RunModel(self, Zipcode = None):
        outputDict = {}
        for t in range(self.iterations):
            start = timer()
            outputDict[t] = self.StepProc(t) 
            end = timer()
            print('Time Step ' + str(t + 1) + ' out of ' + str(self.iterations)  + ' Completed in ' + str(timedelta(seconds=end - start)) + ' [' + str(Zipcode) + ']')
        
        return outputDict

