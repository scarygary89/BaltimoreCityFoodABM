import random

class agentClass:
    def __init__(self, globalID, popDict, cohortAttributeDict, visitDict, foodPrefDict):
        # Basic agent info
        self.globalID = globalID
        self.income = popDict['Income']
        self.race = popDict['Race']
        self.zipcode =  popDict['Zip']
        self.poverty = popDict['Poverty']
        self.health = popDict['HealthBurden']

        # Socio-Demographic Characteristics
        self.SusceptibleToMarketingToVeg = cohortAttributeDict['SusceptibleToMarketingToVeg']
        self.SusceptibleToMarketingToMeat = cohortAttributeDict['SusceptibleToMarketingToMeat']
        self.SusceptibleToMarketingToPoultry = cohortAttributeDict['SusceptibleToMarketingToPoultry'] 
        self.SusceptibleToMarketingToFish = cohortAttributeDict['SusceptibleToMarketingToFish']
        self.SusceptibleToMarketingToGrains = cohortAttributeDict['SusceptibleToMarketingToGrains']  
        self.SusceptibleToMarketingToLegumesBeansNuts = cohortAttributeDict['SusceptibleToMarketingToLegumesBeansNuts']
        self.SusceptibleToMarketingToEggCheese = cohortAttributeDict['SusceptibleToMarketingToEggCheese']
        self.SusceptibleToMarketingToOther = cohortAttributeDict['SusceptibleToMarketingToOther']   
        
        self.SusceptibleToPeerPressureForVeg = cohortAttributeDict['SusceptibleToPeerPressureForVeg'] 
        self.SusceptibleToPeerPressureForMeat = cohortAttributeDict['SusceptibleToPeerPressureForMeat']
        self.SusceptibleToPeerPressureForPoultry = cohortAttributeDict['SusceptibleToPeerPressureForPoultry'] 
        self.SusceptibleToPeerPressureForFish = cohortAttributeDict['SusceptibleToPeerPressureForFish']
        self.SusceptibleToPeerPressureForGrains = cohortAttributeDict['SusceptibleToPeerPressureForGrains']
        self.SusceptibleToPeerPressureForLegumesBeansNuts = cohortAttributeDict['SusceptibleToPeerPressureForLegumesBeansNuts']
        self.SusceptibleToPeerPressureForEggCheese = cohortAttributeDict['SusceptibleToPeerPressureForEggCheese']
        self.SusceptibleToPeerPressureForOther = cohortAttributeDict['SusceptibleToPeerPressureForOther']   
        
        self.VegPriceSensitivity = cohortAttributeDict['VegPriceSensitivity'] 
        self.MeatPriceSensitivity = cohortAttributeDict['MeatPriceSensitivity']
        self.PoultryPriceSensitivity = cohortAttributeDict['PoultryPriceSensitivity'] 
        self.FishPriceSensitivity = cohortAttributeDict['FishPriceSensitivity']
        self.GrainsPriceSensitivity = cohortAttributeDict['GrainsPriceSensitivity']  
        self.LegumesBeansNutsPriceSensitivity = cohortAttributeDict['LegumesBeansNutsPriceSensitivity']
        self.EggCheesePriceSensitivity = cohortAttributeDict['EggCheesePriceSensitivity']
        self.OtherPriceSensitivity = cohortAttributeDict['OtherPriceSensitivity']
        self.EatingOutPriceSensitivity = cohortAttributeDict['EatingOutPriceSensitivity']

        self.HealthConsciousToConsumeVeg = cohortAttributeDict['HealthConsciousToConsumeVeg']
        self.HealthConsciousToConsumeMeat = cohortAttributeDict['HealthConsciousToConsumeMeat']
        self.HealthConsciousToConsumePoultry = cohortAttributeDict['HealthConsciousToConsumePoultry']
        self.HealthConsciousToConsumeFish = cohortAttributeDict['HealthConsciousToConsumeFish']
        self.HealthConsciousToConsumeGrains = cohortAttributeDict['HealthConsciousToConsumeGrains']
        self.HealthConsciousToConsumeLegumesBeansNuts = cohortAttributeDict['HealthConsciousToConsumeLegumesBeansNuts']
        self.HealthConsciousToConsumeEggCheese = cohortAttributeDict['HealthConsciousToConsumeEggCheese']
        self.HealthConsciousToConsumeOther = cohortAttributeDict['HealthConsciousToConsumeOther']

        # Meal Location Probabilities
        self.ProbEatingAtHome = cohortAttributeDict['CookedDinner_Perc'] + cohortAttributeDict['LeftOverDinner_Perc'] + cohortAttributeDict['FrozenDinner_Perc']
        self.ProbEatingOut = cohortAttributeDict['RestuarantDinner_Perc'] + cohortAttributeDict['FastfoodDinner_Perc'] + cohortAttributeDict['CarryoutDinner_Perc']
        self.ProbNotEat = cohortAttributeDict['NoDinner_Perc']

        # Unpack Visit Dict
        self.VisitZipList = []
        self.VisitConvenienceStoresList = []
        self.VisitSupermarketsStoresList = []
        self.VisitFastfoodList = []
        self.VisitRestaurantsList = []
        self.TotalVisitList = []
        for sz in visitDict.keys():
            self.VisitZipList.append(sz)
            self.VisitConvenienceStoresList.append(visitDict[sz]['Convenience'])
            self.VisitSupermarketsStoresList.append(visitDict[sz]['SupermarketsStores'])
            self.VisitFastfoodList.append(visitDict[sz]['Fastfood'])
            self.VisitRestaurantsList.append(visitDict[sz]['Restaurants'])
            TotVisits = visitDict[sz]['Convenience'] + visitDict[sz]['SupermarketsStores'] + visitDict[sz]['Fastfood'] + visitDict[sz]['Restaurants']
            self.TotalVisitList.append(TotVisits)

        # Weights for Eating At Home
        self.ProbOfVisitConvenienceStores = sum(self.VisitConvenienceStoresList) / sum(self.TotalVisitList)
        self.ProbOfVisitSupermarketsStores = sum(self.VisitSupermarketsStoresList) / sum(self.TotalVisitList)
        
        # Weights for Eating Out
        self.ProbOfVisitFastfood = sum(self.VisitFastfoodList) / sum(self.TotalVisitList)
        self.ProbOfVisitRestaurants = sum(self.VisitRestaurantsList) / sum(self.TotalVisitList)
        
        # Meal Type Probabilities
        self.ProbCookedDinner = cohortAttributeDict['CookedDinner_Perc']
        self.ProbLeftOverDinner = cohortAttributeDict['LeftOverDinner_Perc']
        self.ProbFrozenDinner = cohortAttributeDict['FrozenDinner_Perc']
        self.ProbRestuarantDinner = cohortAttributeDict['RestuarantDinner_Perc']
        self.ProbFastfoodDinner = cohortAttributeDict['FastfoodDinner_Perc']
        self.ProbCarryoutDinner = cohortAttributeDict['CarryoutDinner_Perc']

        # Food Type Probabilities
        self.foodPrefDict = foodPrefDict

        # Initialize Recorders
        self.MealLocationHist = []
        self.StoreVisitHist = []
        self.MealSourceHist = []
        self.MealTypeHist = []
        self.MealConsumedHist = []

        # Temp Vars
        self.EatLoc = None


    def ChooseMealSourceLocs(self, ProbEatingAtHome, ProbEatingOut, ProbNotEat, 
    ProbOfVisitConvenienceStores, ProbOfVisitFastfood, ProbOfVisitRestaurants, ProbOfVisitSupermarketsStores, 
    ProbCookedDinner, ProbLeftOverDinner, ProbFrozenDinner, ProbRestuarantDinner, ProbFastfoodDinner,ProbCarryoutDinner, AgentStoreDict):
        
        # Choose to not eat, eat at home, or eat out
        locations = ['Eat At Home', 'Eat Out', 'Not Eat']
        LocationProbList = [ProbEatingAtHome, ProbEatingOut, ProbNotEat]
        self.EatLoc = random.choices(locations, weights = LocationProbList, k = 1)
        self.MealLocationHist.append(*self.EatLoc)
        
        # Choose location type to eat at
        if self.EatLoc == ['Eat At Home']:
            sources = ['ConvenienceStores','SupermarketsStores']
            SourceProbList = [ProbOfVisitConvenienceStores, ProbOfVisitSupermarketsStores]
        elif self.EatLoc == ['Eat Out']:
            sources = ['Fastfood', 'Restaurants']
            SourceProbList = [ProbOfVisitFastfood, ProbOfVisitRestaurants]
        elif self.EatLoc == ['Not Eat']: 
            sources = ['Nowhere']
            SourceProbList = [1]
        
        SourceOutcome = random.choices(sources, weights = SourceProbList, k = 1)
        self.MealSourceHist.append(*SourceOutcome)

        # Choose store destination zipcode
        if SourceOutcome == ['ConvenienceStores']:
            ziplist = self.VisitZipList
            StoreProbList = self.VisitConvenienceStoresList
        elif SourceOutcome == ['SupermarketsStores']:
            ziplist = self.VisitZipList
            StoreProbList = self.VisitSupermarketsStoresList
        elif SourceOutcome == ['Fastfood']:
            ziplist = self.VisitZipList
            StoreProbList = self.VisitFastfoodList
        elif SourceOutcome == ['Restaurants']:
            ziplist = self.VisitZipList
            StoreProbList = self.VisitRestaurantsList
        elif SourceOutcome == ['Nowhere']:
            ziplist = ['Nothing']
            StoreProbList = [1]
        
        StoreZipOutcome = random.choices(ziplist, weights = StoreProbList, k = 1)
        self.StoreVisitHist.append(*StoreZipOutcome)
        
        # Choose meal type
        if SourceOutcome == ['ConvenienceStores']:
            meals = ['CookedDinner', 'LeftOverDinner', 'FrozenDinner']
            MealProbList = [ProbCookedDinner, ProbLeftOverDinner, ProbFrozenDinner]
        elif SourceOutcome == ['SupermarketsStores']:
            meals = ['CookedDinner', 'LeftOverDinner', 'FrozenDinner']
            MealProbList = [ProbCookedDinner, ProbLeftOverDinner, ProbFrozenDinner]
        elif SourceOutcome == ['Fastfood']:
            meals = ['FastfoodDinner', 'CarryoutDinner']
            MealProbList = [ProbFastfoodDinner, ProbCarryoutDinner]
        elif SourceOutcome == ['Restaurants']:
            meals = ['RestuarantDinner','CarryoutDinner']
            MealProbList = [ProbRestuarantDinner, ProbCarryoutDinner]
        elif SourceOutcome == ['Nowhere']:
            meals = ['Nothing']
            MealProbList = [1]
        
        MealOutcome = random.choices(meals, weights = MealProbList, k = 1)
        self.MealTypeHist.append(*MealOutcome)

        # Look-up food preference
        if self.EatLoc == ['Not Eat']:
            BaseProbDict = 'Nothing'
        else:
            # Choose Store
            if SourceOutcome[0] in AgentStoreDict[StoreZipOutcome[0]].keys():
                StoresInDestList = AgentStoreDict[StoreZipOutcome[0]][SourceOutcome[0]]
            else:
                pk = random.choices(list(AgentStoreDict[StoreZipOutcome[0]]))
                StoresInDestList = AgentStoreDict[StoreZipOutcome[0]][pk[0]]

            StoreVisted = random.choices(StoresInDestList, k = 1)
            BaseProbDict = {}
            MealSourceLocKey = str(*self.EatLoc) + str(*SourceOutcome)
            if MealSourceLocKey not in self.foodPrefDict.keys():
                MealSourceLocKey = 'miscmisc'

            BaseProbDict['VegProb'] = self.foodPrefDict[MealSourceLocKey]['veg_pct'] * StoreVisted[0].VegStock
            BaseProbDict['MeatProb'] = self.foodPrefDict[MealSourceLocKey]['red_meat_pct'] * StoreVisted[0].MeatStock
            BaseProbDict['PoultryProb'] = self.foodPrefDict[MealSourceLocKey]['poultry_pct'] * StoreVisted[0].PoultryStock
            BaseProbDict['FishProb'] = self.foodPrefDict[MealSourceLocKey]['fish_pct'] * StoreVisted[0].FishStock
            BaseProbDict['LegumesBeansNutsProb'] = self.foodPrefDict[MealSourceLocKey]['legumes_nuts_pct'] * StoreVisted[0].LegumesBeansNutsStock
            BaseProbDict['EggCheeseProb'] = self.foodPrefDict[MealSourceLocKey]['dairy_pct'] * StoreVisted[0].EggCheeseStock
            BaseProbDict['GrainsProb'] = self.foodPrefDict[MealSourceLocKey]['grains_pct'] * StoreVisted[0].GrainsStock
            BaseProbDict['OtherProb'] = self.foodPrefDict[MealSourceLocKey]['Liquids_pct'] * StoreVisted[0].OtherStock



        return BaseProbDict

    def ChooseFoodType(self, VegProb, MeatProb, PoultryProb, FishProb, LegumesBeansNutsProb, EggCheeseProb, GrainsProb, OtherProb, MealSize):
        foods = ['Veg','Meat','Poultry','Fish','LegumesBeansNuts', 'EggCheese', 'Grains', 'Other']
        foodcountDic = {}
        if self.EatLoc == ['Not Eat']:
            for fd in foods:
                foodcountDic[fd] = 0
        else:
            MealChoice = random.choices(foods,weights = [VegProb, MeatProb, PoultryProb, FishProb, LegumesBeansNutsProb, EggCheeseProb, GrainsProb, OtherProb],k=int(round(MealSize)))
            for fd in foods:
                foodcountDic[fd] = MealChoice.count(fd)

        self.MealConsumedHist.append(foodcountDic)

        return foodcountDic


class storeClass:
    def __init__(self, globalID, inventDict, initialSupplyShock):
        self.globalID = globalID
        self.Type = inventDict['Type']
        self.VegStockProb = inventDict['VegInventory_pct']
        self.MeatStockProb = inventDict['MeatInventory_pct']
        self.PoultryStockProb = inventDict['PoultryInventory_pct'] 
        self.FishStockProb = inventDict['FishInventory_pct']
        self.GrainsStockProb = inventDict['GrainsInventory_pct']  
        self.LegumesBeansNutsStockProb = inventDict['LegumesBeansNutsInventory_pct']
        self.EggCheeseStockProb = inventDict['EggCheeseInventory_pct']
        self.OtherStockProb = inventDict['OtherInventory_pct']
        self.Restock(initialSupplyShock)

    def Restock(self, supplyShock):
        self.VegStock = self.CalculateInventory(self.VegStockProb, supplyShock)
        self.MeatStock = self.CalculateInventory(self.MeatStockProb, supplyShock)
        self.PoultryStock = self.CalculateInventory(self.PoultryStockProb, supplyShock)
        self.FishStock = self.CalculateInventory(self.FishStockProb, supplyShock)
        self.GrainsStock = self.CalculateInventory(self.GrainsStockProb, supplyShock)
        self.LegumesBeansNutsStock = self.CalculateInventory(self.LegumesBeansNutsStockProb, supplyShock)
        self.EggCheeseStock = self.CalculateInventory(self.EggCheeseStockProb, supplyShock)
        self.OtherStock = self.CalculateInventory(self.OtherStockProb, supplyShock)

    def CalculateInventory(self, stockProb, supplyShock):
        stocklevels = [1,0]
        AdjStockProb = stockProb * supplyShock
        stock = random.choices(stocklevels, weights = [AdjStockProb, 1-AdjStockProb],k=1)[0]
        return stock


def BuildAgents(popInputDict, cohortAttributeInputDict, visitInputDict, foodPrefInputDict):
    agentsList = []
    cohortList =  popInputDict.keys()
    globalID = 0
    # Iterate through each cohort groups and create 
    for chrt in cohortList:
        population = popInputDict[chrt]['Population']
        socdemkey = popInputDict[chrt]['SocioDemKey']
        zipcode = popInputDict[chrt]['Zip']
        visitDict = visitInputDict[zipcode]
        popDict = popInputDict[chrt]
        cohortAttributeDict = cohortAttributeInputDict[socdemkey]
        foodPrefDict = foodPrefInputDict['miscmiscmisc'] # if no data, switch to misc values
        if socdemkey in foodPrefInputDict.keys():
            foodPrefDict.update(foodPrefInputDict[socdemkey])

        for i in range(population):
            agent = agentClass(globalID, popDict, cohortAttributeDict, visitDict, foodPrefDict)
            agentsList.append(agent)
            globalID += 1

    return agentsList


def BuildStores(inventoryDict, supplyShock = 1):
    storesDict = {}
    storesList = []
    zipcodes =  inventoryDict.keys()
    globalID = 0
    for zip in zipcodes:
        tempDict = {}
        zipDict = inventoryDict[zip]
        for type in zipDict.keys():
            ziptypeDict = zipDict[type]
            tempList = []
            for st in range(ziptypeDict['total_stores']):
                store = storeClass(globalID, ziptypeDict, supplyShock)
                tempList.append(store)
                storesList.append(store)
                globalID += 1
            tempDict[type] = tempList
        storesDict[zip] = tempDict
    # Iterate through each cohort zip code
    return storesDict, storesList
