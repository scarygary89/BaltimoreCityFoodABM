import numpy as np
import pandas as pd
import os

def BreakdownFoodConsumption(popInputDict, agentList, iterations):
	outputDict = {}
	for i in range(iterations):
		foodconsDict, zipcode, poverty, race, income = CreateStructuredDictForPopulationFoodConsumption(popInputDict)
		for agent in agentList:
			agentconsDict = agent.MealConsumedHist[i]
			for fd in agentconsDict.keys():
				# count total food consumed
				foodconsDict[agent.zipcode]['Poverty'][agent.poverty][fd] += agentconsDict[fd]
				foodconsDict[agent.zipcode]['Race'][agent.race][fd] += agentconsDict[fd]
				foodconsDict[agent.zipcode]['Income'][agent.income][fd] += agentconsDict[fd]

			# count population
			foodconsDict[agent.zipcode]['Poverty'][agent.poverty]['Population'] += 1
			foodconsDict[agent.zipcode]['Race'][agent.race]['Population'] += 1
			foodconsDict[agent.zipcode]['Income'][agent.income]['Population'] += 1

		outputDict[i] = foodconsDict # dict[time][zipcode][popcharname][popchar][variable]

	return outputDict, zipcode, poverty, race, income

def CreateStructuredDictForPopulationFoodConsumption(popInputDict):
	foodconsDict = {}
	zipcodeList, povertyList, raceList, incomeList = [], [], [], []
	for chrt in popInputDict.keys():
		zipcode = popInputDict[chrt]['Zip']
		poverty = popInputDict[chrt]['Poverty']
		race = popInputDict[chrt]['Race']
		income = popInputDict[chrt]['Income']
		if zipcode not in foodconsDict.keys():
			foodconsDict[zipcode] = {'Poverty':{}, 'Race':{}, 'Income': {}}
			zipcodeList = [*zipcodeList, zipcode]
		if poverty not in povertyList:
			povertyList = [*povertyList, poverty]
		if race not in raceList:
			raceList = [*raceList, race]
		if income not in incomeList:
			incomeList = [*incomeList, income]

		foodconsDict[zipcode]['Poverty'].update({poverty:{'Veg':0,'Meat':0,'Poultry':0,'Fish':0,'LegumesBeansNuts':0,'EggCheese':0,'Grains':0,'Other':0,'Population':0}})
		foodconsDict[zipcode]['Race'].update({race:{'Veg':0,'Meat':0,'Poultry':0,'Fish':0,'LegumesBeansNuts':0,'EggCheese':0,'Grains':0,'Other':0,'Population':0}})
		foodconsDict[zipcode]['Income'].update({income:{'Veg':0,'Meat':0,'Poultry':0,'Fish':0,'LegumesBeansNuts':0,'EggCheese':0,'Grains':0,'Other':0,'Population':0}})

	return foodconsDict, zipcodeList, povertyList, raceList, incomeList

def ExportFoodConsumptionOutput(modResultsDict, runName, popcharname, popchar, zipcode):
	try:
		os.mkdir('Output/' + str(runName))
	except:
		pass
	foodList = ['Veg','Meat','Poultry','Fish','LegumesBeansNuts','EggCheese','Grains','Other','Population']
	for z in zipcode:
		for chrt in popchar:
			runDict = {}
			for t in modResultsDict.keys():
				ts = []
				for vr in foodList:
					value = modResultsDict[t][z][popcharname][chrt][vr]
					ts = [*ts, value]
				runDict[t] = ts

			df = pd.DataFrame.from_dict(runDict, orient='index',columns=foodList)
			df.to_csv('Output/'+ str(runName) + '/FoodConsumption' + str(z) + str(popcharname) + '_' + str(chrt) + '.csv')
