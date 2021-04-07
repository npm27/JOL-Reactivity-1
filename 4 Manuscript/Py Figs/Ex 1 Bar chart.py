####Experiment 1: IOC (top panel) Reactivity (Bottom Panel)####
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

dat = pd.read_csv("Ex 1 IOC.csv")
dat2 = pd.read_csv("EX 1 Reactivity.csv")

#make the 95% confidence intervals
dat['diff'] = dat['Upper'].sub(dat['Lower']) #get the length of the bars
dat['diff2'] = dat['diff'].div(2) #length from line to point

##set up the initial plot
fig = plt.figure()
fig.set_size_inches(8,6)

ax1 = fig.add_subplot(1, 1, 1)

#make the 95% confidence intervals
dat2['diff'] = dat2['Upper'].sub(dat2['Lower']) #get the length of the bars
dat2['diff2'] = dat2['diff'].div(2) #length from line to point

#subset by task
j2 = dat2[dat2['Task'] == 'JOL']
s1 = dat2[dat2['Task'] == 'Study']

#get all the things to plug into the plots
#separate out averages and conf interval
j2_average = j2['Average']
s1_average = s1['Average']

j2_conf = j2['diff2']
s1_conf = s1['diff2']

ind = np.arange(len(j2_average))  # the x locations for the groups
width = 0.35 #bar width 

rects3 = ax1.bar(ind - width/2, j2_average, width, yerr = j2_conf, capsize = 3, color = 'white', edgecolor = 'k',
                label ='JOL')

rects4 = ax1.bar(ind + width/2, s1_average, width, yerr = s1_conf, capsize = 3, color = 'grey', edgecolor = 'k',
                label = 'No-JOL')

#Make the plot spiffy
ax1.set_title('Experiment 1', fontsize = 20, fontweight = 'bold')
ax1.set_ylabel('Mean % Recall', fontsize = 18, fontweight = 'bold')
ax1.set_xlabel('Direction', fontsize = 18, fontweight = 'bold')
#ax1.xaxis.labelpad = 0
ax1.set_xticks(ind)
ax1.set_xticklabels(('Backward', 'Forward', 'Symmetrical', 'Unrelated'), fontsize = 16)
ax1.legend(fontsize = 16)
ax1.set_ylim([0,100])

##save figure
#fig.savefig('EX1_chart.png', dip = 10000)
