import pandas as pd
import numpy as np
import os
import matplotlib.pyplot as plt

def PlotOutputBySeries(outputDict, series, cohortNames, runName):
    try:
        os.mkdir('Output/Plots')
    except:
        pass
    plt.clf()
    for run in outputDict.keys():
        realTdom = outputDict[run]['TimeDomain']
        realData = outputDict[run][series]
        for i in range(len(cohortNames)):
            plt.plot(realTdom, realData[:,i], label=cohortNames[i] + ' (' + str(run) + ')')
        plt.xlabel('Meals')
        plt.ylabel(series)
        plt.legend()
        figure = plt.gcf()
        figure.set_size_inches(8, 6)    
    plt.savefig('Output/Plots/'  + str(runName) + str(series) + '.png', dpi=300, bbox_inches='tight')
