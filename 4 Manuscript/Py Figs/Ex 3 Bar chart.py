####Ex 3 Reactivity Bar Chart####
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

dat = pd.read_csv("Ex 3 IOC.csv")
dat2 = pd.read_csv("EX 3.csv")

#make the 95% confidence intervals
dat['diff'] = dat['Upper'].sub(dat['Lower']) #get the length of the bars
dat['diff2'] = dat['diff'].div(2) #length from line to point

##set up the initial plot
fig = plt.figure()
fig.set_size_inches(10,12)

ax1 = fig.add_subplot(2, 1, 1)
ax2 = fig.add_subplot(2, 1, 2)

fig.subplots_adjust(hspace = .30) #Controls space between sub plots

#subset by task
j1 = dat[dat['Task'] == 'JOL']
r1 = dat[dat['Task'] == 'Recall']

#get all the things to plug into the plots
#separate out averages and conf interval
j1_average = j1['Average']
r1_average = r1['Average']

j1_conf = j1['diff2']
r1_conf = r1['diff2']

ind = np.arange(len(j1_average))  # the x locations for the groups
width = 0.35 #bar width 

rects1 = ax1.bar(ind - width/2, j1_average, width, yerr = j1_conf, capsize = 3, color = 'white', edgecolor = 'k',
                label ='JOL')

rects2 = ax1.bar(ind + width/2, r1_average, width, yerr = r1_conf, capsize = 3, color = 'grey', edgecolor = 'k',
                label = 'Recall')

#Make the plot spiffy
ax1.set_title('Experiment 3: Illusion of Competence', fontsize = 20, fontweight = 'bold')
ax1.set_ylabel('Mean % JOL/Recall', fontsize = 18, fontweight = 'bold')
ax1.set_xlabel('Direction', fontsize = 18, fontweight = 'bold')
#ax1.xaxis.labelpad = 0
ax1.set_xticks(ind)
ax1.set_xticklabels(('Backward', 'Forward', 'Symmetrical', 'Unrelated'), fontsize = 16)
ax1.legend(fontsize = 16)
ax1.set_ylim([0,100])

#make the 95% confidence intervals
dat2['diff'] = dat2['Upper'].sub(dat2['Lower']) #get the length of the bars
dat2['diff2'] = dat2['diff'].div(2) #length from line to point

#subset by task
jj1 = dat2[dat2['Task'] == 'JOL']
s1 = dat2[dat2['Task'] == 'Study']
f1 = dat2[dat2['Task'] == 'Frequency']

#get all the things to plug into the plots
#separate out averages and conf interval
jj1_average = jj1['Average']
s1_average = s1['Average']
f1_average = f1['Average']

jj1_conf = jj1['diff2']
s1_conf = s1['diff2']
f1_conf = f1['diff2']

ind = np.arange(len(jj1_average))  # the x locations for the groups
width = 0.30 #bar width 

rects1 = ax2.bar(ind - width/2, jj1_average, width, yerr = jj1_conf, capsize = 3, color = 'white', edgecolor = 'k',
                label ='JOL', align = "center")

rects2 = ax2.bar(ind + width/2, f1_average, width, yerr = f1_conf, capsize = 3, color = 'silver', edgecolor = 'k',
                label = 'Frequency', align = "center")

rects3 = ax2.bar(ind + width + .15, s1_average, width, yerr = s1_conf, capsize = 3, color = 'dimgrey', edgecolor = 'k',
                label = 'No-JOL', align = "center")

#Make the plot spiffy
ax2.set_title('Experiment 3: Reactivity', fontsize = 20, fontweight = 'bold')
ax2.set_ylabel('Mean % Recall', fontsize = 18, fontweight = 'bold')
ax2.set_xlabel('Direction', fontsize = 18, fontweight = 'bold')
#ax1.xaxis.labelpad = 0
ax2.set_xticks(ind + .15)
ax2.set_xticklabels(('Backward', 'Forward', 'Symmetrical', 'Unrelated'), fontsize = 16)
ax2.legend(fontsize = 16)
ax2.set_ylim([0,100])

##save figure
fig.savefig('EX3_chart.png', dip = 10000)
