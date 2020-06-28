####Ex 3 Reactivity Bar Chart####
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

dat = pd.read_csv("Ex 3.csv")

#make the 95% confidence intervals
dat['diff'] = dat['Upper'].sub(dat['Lower']) #get the length of the bars
dat['diff2'] = dat['diff'].div(2) #length from line to point

##set up the initial plot
fig = plt.figure()
fig.set_size_inches(14,10)

ax1 = fig.add_subplot(1, 1, 1)

fig.subplots_adjust(hspace = .30) #Controls space between sub plots

#subset by task
j1 = dat[dat['Task'] == 'JOL']
s1 = dat[dat['Task'] == 'Study']
f1 = dat[dat['Task'] == 'Frequency']

#get all the things to plug into the plots
#separate out averages and conf interval
j1_average = j1['Average']
s1_average = s1['Average']
f1_average = f1['Average']

j1_conf = j1['diff2']
s1_conf = s1['diff2']
f1_conf = f1['diff2']

ind = np.arange(len(j1_average))  # the x locations for the groups
width = 0.30 #bar width 

rects1 = ax1.bar(ind - width/2, j1_average, width, yerr = j1_conf, capsize = 3, color = 'white', edgecolor = 'k',
                label ='JOL', align = "center")

rects2 = ax1.bar(ind + width/2, f1_average, width, yerr = f1_conf, capsize = 3, color = 'silver', edgecolor = 'k',
                label = 'Frequency', align = "center")

rects3 = ax1.bar(ind + width + .15, s1_average, width, yerr = s1_conf, capsize = 3, color = 'dimgrey', edgecolor = 'k',
                label = 'Read', align = "center")

#Make the plot spiffy
ax1.set_title('Experiment 3: JOLs vs Frequency vs Read', fontsize = 20, fontweight = 'bold')
ax1.set_ylabel('Mean % Recall', fontsize = 18, fontweight = 'bold')
ax1.set_xlabel('Direction', fontsize = 18, fontweight = 'bold')
#ax1.xaxis.labelpad = 0
ax1.set_xticks(ind + .15)
ax1.set_xticklabels(('Backward', 'Forward', 'Symmetrical', 'Unrelated'), fontsize = 16)
ax1.legend(fontsize = 16)
ax1.set_ylim([0,100])

##save figure
fig.savefig('EX3_chart.png', dip = 10000)
