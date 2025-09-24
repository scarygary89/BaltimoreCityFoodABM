##################################################
# Read and Process Input Data
##################################################
import os
import pandas as pd
import numpy as np

def ImportGlobalParmsData(filedirectory):
    data = pd.read_csv(filedirectory)
    parmNames = data['Variables'].to_list()
    parmDict = {}
    for pr in parmNames:
        parmDict[pr] = data[data['Variables'] == pr]['Value'].values[0]
    return parmDict

def ImportCohortParmsData(filedirectory):
    data = pd.read_csv(filedirectory)
    parmNames =  data.columns.to_list()[2:]
    parmDict = {}
    for i in range(data.shape[0]):
        cohortDict = {}
        for vr in parmNames:
            cohortDict[vr] = data[vr][i]
        cohortName = str(data.Income[i]) + str(data.Race[i]) + str(data.Poverty[i])
        parmDict[cohortName] = cohortDict
    return parmDict

def ImportZipPopData(filedirectory):
    data = pd.read_csv(filedirectory)
    parmNames =  data.columns.to_list()
    parmDict = {}
    for i in range(data.shape[0]):
        cohortDict = {}
        for vr in parmNames:
            cohortDict[vr] = data[vr][i]
        cohortDict['SocioDemKey'] = str(data.Income[i]) + str(data.Race[i]) + str(data.Poverty[i])
        cohortName = str(data.Zip[i]) + str(data.Income[i]) + str(data.Race[i]) + str(data.Poverty[i])
        parmDict[cohortName] = cohortDict
    return parmDict

def ImportVisitData(filedirectory):
    data = pd.read_csv(filedirectory)
    parmNames =  data.columns.to_list()[2:]
    Zipcodes = pd.unique(data.zip)
    outDict = {}
    for z in Zipcodes:
        zipDict = {}
        selectData = data[data['zip'] == z]
        storeZip = selectData['store_zip'].to_list()
        for st in storeZip: 
            vrDict = {}
            selectData2 = selectData[selectData['store_zip'] == st]
            for vr in parmNames:
                vrDict[vr] = selectData2[vr].values[0]
            zipDict[st] = vrDict
        outDict[z] = zipDict
    return outDict


def ImportFoodPreferences(filedirectory):
    data = pd.read_csv(filedirectory)
    parmNames =  data.columns.to_list()[6:]
    parmDict = {}
    for i in range(data.shape[0]):
        cohortDict = {}
        SocioDemKey = str(data.Income[i]) + str(data.Race[i]) + str(data.Poverty[i])
        if SocioDemKey not in parmDict.keys():
            parmDict[SocioDemKey] = {}
        for vr in parmNames:
            cohortDict[vr] = data[vr][i]
            MealSourceLocKey = str(data.home[i]) + str(data.Location[i])
        parmDict[SocioDemKey].update({MealSourceLocKey:cohortDict})
    return parmDict

def ImportTimeSeriesData(filedirectory):
    data = pd.read_csv(filedirectory)
    tsDict = {}
    tsDict['Day'] = data['Day'].to_list()
    tsDict['Date'] = data['Date'].to_list()
    tsDict['Food'] =  data.columns.to_list()[2:]
    for fd in tsDict['Food']:
        tsDict[fd] = np.array(data[fd].to_list())
    return tsDict

def ImportStoreData(filedirectory):
    data = pd.read_csv(filedirectory)
    outDict = {}
    Zipcodes = pd.unique(data.zip)
    parmNames =  data.columns.to_list()[1:]
    for z in Zipcodes:
        zipDict = {}
        selectData = data[data['zip'] == z]
        storeType = selectData['Type'].to_list()
        for st in storeType: 
            vrDict = {}
            selectData2 = selectData[selectData['Type'] == st]
            for vr in parmNames:
                vrDict[vr] = selectData2[vr].values[0]
            zipDict[st] = vrDict
        outDict[z] = zipDict
    return outDict

def ImportStoreTimeSeriesData(filedirectory):
    data = pd.read_csv(filedirectory)
    tsDict = {}
    tsDict['Day'] = data['Day'].to_list()
    tsDict['Date'] = data['Date'].to_list()
    tsDict['StoreType'] =  data.columns.to_list()[2:]
    for st in tsDict['StoreType']:
        tsDict[st] = np.array(data[st].to_list())
    return tsDict

def ExtractZipcodesPovertyRaceIncome(popInputDict):
    zipcodeList, povertyList, raceList, incomeList = [], [], [], []
    for chrt in popInputDict.keys():
        zipcode = popInputDict[chrt]['Zip']
        poverty = popInputDict[chrt]['Poverty']
        race = popInputDict[chrt]['Race']
        income = popInputDict[chrt]['Income']
        if zipcode not in zipcodeList:
            zipcodeList = [*zipcodeList, zipcode]
        if poverty not in povertyList:
            povertyList = [*povertyList, poverty]		
        if race not in raceList:
            raceList = [*raceList, race]
        if income not in incomeList:
            incomeList = [*incomeList, income]

    return zipcodeList, povertyList, raceList, incomeList

def ExtractZipFromPopDict(zipcode, popInputDict):
    population = {}
    for chrt in popInputDict.keys():
        if popInputDict[chrt]['Zip'] == zipcode:
            population[chrt] = popInputDict[chrt]
    return population
