####Dissertation EX1 Bar Chart -- IOC Data####
##set up
##load libraries
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

dat = pd.read_csv("IOC data.csv")

#make the 95% confidence intervals
dat['diff'] = dat['Upper'].sub(dat['Lower']) #get the length of the bars
dat['diff2'] = dat['diff'].div(2) #length from line to point

##set up the plot
ex1_fig = plt.figure()
ex1_fig.set_size_inches(10,10) #Can tweak this as needed

##Make the subplot
ax1 = ex1_fig.add_subplot(1, 1, 1)

#get all the things to plug into the plots
#subset by task
j1 = dat[dat['Task'] == 'JOL']
r1 = dat[dat['Task'] == 'Recall']

#separate out averages and conf interval
j1_average = j1['Average']
r1_average = r1['Average']

j1_conf = j1['diff2']
r1_conf = r1['diff2']

#Make the initial plot
ind = np.arange(len(j1_average))  # the x locations for the groups
width = 0.35 #bar width

rects1 = ax1.bar(ind - width/2, j1_average, width, yerr = j1_conf, capsize = 3, color = 'w', edgecolor = 'k',
                label ='JOL')

rects2 = ax1.bar(ind + width/2, r1_average, width, yerr = r1_conf, capsize = 3, color = 'grey', edgecolor = 'k',
                label = 'Recall')

#Make the plot spiffy
ax1.set_title('Experiment 1: Forward vs Backward vs Symmetrical vs Unrelated', fontsize = 18)
ax1.set_ylabel('Mean % JOL/Recall', fontsize = 16)
ax1.set_xlabel('Direction', fontsize = 16)
ax1.xaxis.labelpad = 0
ax1.set_xticks(ind)
ax1.set_xticklabels(('Backward', 'Forward', 'Symmetrical', 'Unrelated'), fontsize = 14)
ax1.legend(fontsize = 14)
ax1.set_ylim([0,100])

##Save the figure
#ex1_fig.savefig('EX 1.pdf', dip = 10000)