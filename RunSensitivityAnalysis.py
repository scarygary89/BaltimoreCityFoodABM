"""
Food Consumption Agent Based Model
Author: Gary Lin

Packages needed:
- numpy
- scipy
- multiprocessing
- pandas
- datetime
"""

import sys
import numpy as np
import os
import pandas as pd
from datetime import timedelta
from datetime import datetime
import multiprocessing as mp

import src.ProcessData
import src.Model
import src.AgentProcs
import src.PlotFunctions
import src.OutputFunctions

def main(argv):
	print('Initializing Baltimore Food System Agent Based Model...')
	try:
		os.mkdir('Output')
	except:
		pass
	globalInputDict = src.ProcessData.ImportGlobalParmsData('data/GlobalParms.csv')

	# Import Agent Data
	popInputDict = src.ProcessData.ImportZipPopData('data/ZipPopulation.csv')
	cohortAttributeInputDict = src.ProcessData.ImportCohortParmsData('data/CohortParms.csv')
	visitInputDict = src.ProcessData.ImportVisitData('data/Visits.csv')
	foodPrefInputDict = src.ProcessData.ImportFoodPreferences('data/CohortFoodPreferences.csv')

	# Import Environmental, Pricing, and Marketing Data
	inventoryDict = src.ProcessData.ImportStoreData('data/StoreInventory.csv')
	inventoryMoreMeatlessDict = src.ProcessData.ImportStoreData('data/StoreInventoryMoreMeatless.csv')
	foodPricesDict = src.ProcessData.ImportTimeSeriesData('data/FoodPrices.csv')
	meatPricesSpikeDict = src.ProcessData.ImportTimeSeriesData('data/MeatPricesSpike.csv')
	supplyTSDict = src.ProcessData.ImportStoreTimeSeriesData('data/Supply.csv')
	noMarketingTSDict = src.ProcessData.ImportTimeSeriesData('data/MarketingEfforts.csv')
	meatlessMarketingTSDict = src.ProcessData.ImportTimeSeriesData('data/MeatlessMondayMarketing.csv')

	SensitivityMultiplier = [.2, .4, .6, .8, 1, 1.2, 1.4, 1.6, 1.8, 2]
	
	# marketing
	globalInputDict_market = globalInputDict.copy()
	for mult in SensitivityMultiplier:
		runName = "MarketingSensitivity" + str(mult)
		globalInputDict_market['MarketingMultiplier'] = globalInputDict['MarketingMultiplier'] * mult
		print('Running Sensitivity Analysis for Marketing with Multiplier = ' + str(globalInputDict_market['MarketingMultiplier']))
		RunScenarioZipcode(runName, globalInputDict_market, popInputDict, cohortAttributeInputDict, visitInputDict, foodPrefInputDict, foodPricesDict, inventoryDict, supplyTSDict, meatlessMarketingTSDict)

	# increase in meatless
	globalInputDict_availability = globalInputDict.copy()
	for mult in SensitivityMultiplier:
		runName = "AvailabilitySensitivity" + str(mult)
		globalInputDict_availability['AvailabilityMultiplier'] = globalInputDict['AvailabilityMultiplier'] * mult
		print('Running Sensitivity Analysis for  Availability with Multiplier = ' + str(globalInputDict_availability['AvailabilityMultiplier']))
		RunScenarioZipcode(runName, globalInputDict_availability, popInputDict, cohortAttributeInputDict, visitInputDict, foodPrefInputDict, foodPricesDict, inventoryMoreMeatlessDict, supplyTSDict, noMarketingTSDict)

	# price
	globalInputDict_price = globalInputDict.copy()
	for mult in SensitivityMultiplier:
		runName = "PricingSensitivity" + str(mult)
		globalInputDict_price['PriceMultiplier'] = globalInputDict['PriceMultiplier'] * mult
		print('Running Sensitivity Analysis for Pricing with Multiplier = ' + str(globalInputDict_price['PriceMultiplier']))
		RunScenarioZipcode(runName, globalInputDict_price, popInputDict, cohortAttributeInputDict, visitInputDict, foodPrefInputDict, meatPricesSpikeDict, inventoryDict, supplyTSDict, noMarketingTSDict)

	# marketing + increase in meatless availability (comprehensive marketing)
	globalInputDict_marketavail = globalInputDict.copy()
	for mult1 in SensitivityMultiplier:
		for mult2 in SensitivityMultiplier:
			runName = "AvailabilitySensitivity" + str(mult1) + '_' + "MarketingSensitivity" + str(mult2)
			globalInputDict_marketavail['AvailabilityMultiplier'] = globalInputDict['AvailabilityMultiplier'] * mult1
			globalInputDict_marketavail['MarketingMultiplier'] = globalInputDict['MarketingMultiplier'] * mult2
			print('Running Sensitivity Analysis for Availability with Multiplier = ' + str(globalInputDict_marketavail['AvailabilityMultiplier']))
			print('Running Sensitivity Analysis for Marketing with Multiplier = ' + str(globalInputDict_marketavail['MarketingMultiplier']))
			RunScenarioZipcode(runName, globalInputDict_marketavail, popInputDict, cohortAttributeInputDict, visitInputDict, foodPrefInputDict, foodPricesDict, inventoryMoreMeatlessDict, supplyTSDict, meatlessMarketingTSDict)

	# supply shortage
	globalInputDict_price = globalInputDict.copy()
	for mult in SensitivityMultiplier:
		runName = "SupplySensitivity" + str(mult)
		globalInputDict_price['AvailabilityMultiplier'] = globalInputDict['AvailabilityMultiplier'] * mult
		print('Running Sensitivity Analysis for Pricing with Multiplier = ' + str(globalInputDict_price['AvailabilityMultiplier']))
		RunScenarioZipcode(runName, globalInputDict_price, popInputDict, cohortAttributeInputDict, visitInputDict, foodPrefInputDict, foodPricesDict, inventoryDict, supplyTSDict, noMarketingTSDict)


	print('Finished!')


def RunScenarioZipcode(RunName, globalInputDict, popDict, cohortAttDict, visitDict, foodPrefDict, foodPricesDict, inventoryDict, supplyTSDict, marketingTSDict):
	cpus = mp.cpu_count()
	poolCount = cpus*2 - 2
	zipcodeList, povertyList, raceList, incomeList = src.ProcessData.ExtractZipcodesPovertyRaceIncome(popDict)
	args = []
	for runID in range(len(zipcodeList)):
		inputDict = {}
		inputDict['RunName'] = RunName
		inputDict['zip'] = zipcodeList[runID]
		inputDict['cohortAttDict'] = cohortAttDict
		inputDict['visitDict'] = visitDict
		inputDict['foodPrefDict'] = foodPrefDict
		inputDict['globalInputDict'] = globalInputDict
		inputDict['foodPricesDict'] = foodPricesDict
		inputDict['inventoryDict'] = inventoryDict
		inputDict['supplyTSDict'] = supplyTSDict
		inputDict['marketingTSDict'] = marketingTSDict
		inputDict['population'] = src.ProcessData.ExtractZipFromPopDict(inputDict['zip'], popDict)
		args.append((inputDict))

	pool = mp.Pool(processes = poolCount)
	pool.map_async(RunProc, args)
	pool.close()
	pool.join()


def RunProc(inputDict):
	print('Initializing Job for Zip Code ' + str(inputDict['zip']))
	storeDict, storeList = src.AgentProcs.BuildStores(inputDict['inventoryDict'])
	agents = src.AgentProcs.BuildAgents(inputDict['population'], inputDict['cohortAttDict'], inputDict['visitDict'], inputDict['foodPrefDict'])
	model = src.Model.FoodModel(inputDict['globalInputDict'], inputDict['foodPricesDict'], inputDict['supplyTSDict'], inputDict['marketingTSDict'], agents, storeList, storeDict)
	model.RunModel(inputDict['zip'])
	outputDict, zipcode, poverty, race, income = src.OutputFunctions.BreakdownFoodConsumption(inputDict['population'], model.agentList, model.iterations)
	src.OutputFunctions.ExportFoodConsumptionOutput(outputDict, inputDict['RunName'], 'Poverty', poverty, zipcode)
	src.OutputFunctions.ExportFoodConsumptionOutput(outputDict, inputDict['RunName'], 'Race', race, zipcode)
	src.OutputFunctions.ExportFoodConsumptionOutput(outputDict, inputDict['RunName'], 'Income', income, zipcode)


if __name__ == "__main__":
	# execute only if run as a script
	main(sys.argv[1:])