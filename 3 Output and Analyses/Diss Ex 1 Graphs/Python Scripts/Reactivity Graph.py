####Dissertation EX1 Bar Chart -- Reactivity Data####
##set up
##load libraries
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

#Load in data
dat = pd.read_csv("Reactivity data.csv")

#make the 95% confidence intervals
dat['diff'] = dat['UPPER'].sub(dat['LOWER']) #get the length of the bars
dat['diff2'] = dat['diff'].div(2) #length from line to point

##set up the plot
ex1_fig = plt.figure()
ex1_fig.set_size_inches(10,10) #Can tweak this as needed

##Make the subplot
ax1 = ex1_fig.add_subplot(1, 1, 1)

#get all the things to plug into the plots
#subset by task
j1 = dat[dat['TASK'] == 'JOL']
r1 = dat[dat['TASK'] == 'READ']
f1 = dat[dat['TASK'] == 'FREQ']

#separate out averages and conf interval
j1_average = j1['AVERAGE']
r1_average = r1['AVERAGE']
f1_average = f1['AVERAGE']

j1_conf = j1['diff2']
r1_conf = r1['diff2']
f1_conf = f1['diff2']

#Make the initial plot
ind = np.arange(len(j1_average))  # the x locations for the groups
width = 0.25 #bar width

rects1 = ax1.bar(ind - width, j1_average, width, yerr = j1_conf, capsize = 3, color = 'w', edgecolor = 'k',
                label = 'JOL')

rects2 = ax1.bar(ind, f1_average, width, yerr = f1_conf, capsize = 3, color = 'lightgrey', edgecolor = 'k',
                label = 'FREQ')

rects3 = ax1.bar(ind + width, r1_average, width, yerr = r1_conf, capsize = 3, color = 'dimgrey', edgecolor = 'k',
                label = 'Study')

#Make the plot spiffy
ax1.set_title('Experiment 1: Reactivity', fontsize = 18)
ax1.set_ylabel('Mean % Recall', fontsize = 16)
ax1.set_xlabel('Direction', fontsize = 16)
ax1.xaxis.labelpad = 0
ax1.set_xticks(ind)
ax1.set_xticklabels(('Backward', 'Forward', 'Symmetrical', 'Unrelated'), fontsize = 14)
ax1.legend(fontsize = 14)
ax1.set_ylim([0,100])

#save the figure
ex1_fig.savefig('EX 1 - Reactivity.pdf', dip = 10000)
