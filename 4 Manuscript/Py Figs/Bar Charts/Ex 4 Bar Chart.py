####Ex 2 Reactivity Bar Chart####
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

dat2 = pd.read_csv("Ex 2 Reactivity.csv")

##set up the initial plot
fig = plt.figure()
fig.set_size_inches(8,6)

ax1 = fig.add_subplot(1, 1, 1)

#make the 95% confidence intervals
dat2['diff'] = dat2['Upper'].sub(dat2['Lower']) #get the length of the bars
dat2['diff2'] = dat2['diff'].div(2) #length from line to point

##Reactivity stuff
#subset by task
jj1 = dat2[dat2['Task'] == 'JOL']
s1 = dat2[dat2['Task'] == 'Study']
rr1 = dat2[dat2['Task'] == 'Relational']
v1 = dat2[dat2['Task'] == 'Vowel']

#get all the things to plug into the plots
#separate out averages and conf interval
jj1_average = jj1['Average']
s1_average = s1['Average']
rr1_average = rr1['Average']
v1_average = v1['Average']

jj1_conf = jj1['diff2']
s1_conf = s1['diff2']
rr1_conf = rr1['diff2']
v1_conf = v1['diff2']

ind = np.arange(len(jj1_average))  # the x locations for the groups
width = 0.20 #bar width 

rects1 = ax1.bar(ind - width/2, jj1_average, width, yerr = jj1_conf, capsize = 3, color = 'white', edgecolor = 'k',
                label ='JOL', align = "center")

rects2 = ax1.bar(ind + width/2, rr1_average, width, yerr = rr1_conf, capsize = 3, color = '.80', edgecolor = 'k',
                label = 'Relational', align = "center")

rects3 = ax1.bar(ind + width + .10, v1_average, width, yerr = v1_conf, capsize = 3, color = '.50', edgecolor = 'k',
                label = 'Vowel', align = "center")

rects4 = ax1.bar(ind + width + .30, s1_average, width, yerr = s1_conf, capsize = 3, color = '.30', edgecolor = 'k',
                label = 'No-JOL', align = "center")

#Make the plot spiffy
ax1.set_title('Experiment 4', fontsize = 20, fontweight = 'bold')
ax1.set_ylabel('Mean % Recall', fontsize = 18, fontweight = 'bold')
ax1.set_xlabel('Direction', fontsize = 18, fontweight = 'bold')
#ax1.xaxis.labelpad = 0
ax1.set_xticks(ind + .15)
ax1.set_xticklabels(('Backward', 'Forward', 'Symmetrical', 'Unrelated'), fontsize = 16)
ax1.legend(fontsize = 16)
ax1.set_ylim([0,100])

##save figure
#fig.savefig('EX4_chart2.png', dip = 10000)
