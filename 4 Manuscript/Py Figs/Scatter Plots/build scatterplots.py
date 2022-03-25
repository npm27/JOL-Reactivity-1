##Set up
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

##read in the data
dat = pd.read_csv("related.csv")
dat2 = pd.read_csv("unrelated.csv")

###Start w/ related pairs
##set up the initial plot
fig = plt.figure()
fig.set_size_inches(8,12)

##adjust space between subplots
fig.subplots_adjust(hspace = .35)

ax1 = fig.add_subplot(3, 1, 1)
ax2 = fig.add_subplot(3, 1, 2)
ax3 = fig.add_subplot(3, 1, 3)

#JOL vs JAM
ax1.plot(dat['JOL'], dat['JAM'], '.', color = 'k')
m, b = np.polyfit(dat['JOL'], dat['JAM'], 1)
ax1.plot(dat['JOL'], m * dat['JOL'] + b, color = 'k')

#JOL vs FREQ
ax2.plot(dat['JOL'], dat['FREQ'], '.', color = 'k')
m, b = np.polyfit(dat['JOL'], dat['FREQ'], 1)
ax2.plot(dat['JOL'], m * dat['JOL'] + b, color = 'k')

#JAM vs FREQ
ax3.plot(dat['JAM'], dat['FREQ'], '.', color = 'k')
m, b = np.polyfit(dat['JAM'], dat['FREQ'], 1)
ax3.plot(dat['JAM'], m * dat['JAM'] + b, color = 'k')

##Make the plot spiffy
#ax1
ax1.set_title('JOLs vs. JAMs', fontsize = 16, fontweight = 'bold')
ax1.set_ylabel('Mean JAM', fontsize = 14, fontweight = 'bold')
ax1.set_xlabel('Mean JOL', fontsize = 14, fontweight = 'bold')
ax1.set_xlim([20,100])
ax1.set_ylim([20,100])

#ax2
ax2.set_title('JOLs vs. Frequency Judgments', fontsize = 16, fontweight = 'bold')
ax2.set_ylabel('Mean Frequency Judgment', fontsize = 14, fontweight = 'bold')
ax2.set_xlabel('Mean JOL', fontsize = 14, fontweight = 'bold')
ax2.set_xlim([20,100])
ax2.set_ylim([20,100])

#ax2
ax3.set_title('JAMs vs. Frequency Judgments', fontsize = 16, fontweight = 'bold')
ax3.set_ylabel('Mean Frequency Judgment', fontsize = 14, fontweight = 'bold')
ax3.set_xlabel('Mean JAM', fontsize = 14, fontweight = 'bold')
ax3.set_xlim([20,100])
ax3.set_ylim([20,100])

##save figure
fig.savefig('Related_chart.png', dip = 10000)

####Now do unrelated pairs####
fig = plt.figure()
fig.set_size_inches(8,12)

##adjust space between subplots
fig.subplots_adjust(hspace = .35)

ax1 = fig.add_subplot(3, 1, 1)
ax2 = fig.add_subplot(3, 1, 2)
ax3 = fig.add_subplot(3, 1, 3)

#JOL vs JAM
ax1.plot(dat2['JOL'], dat2['JAM'], '.', color = 'k')
m, b = np.polyfit(dat2['JOL'], dat2['JAM'], 1)
ax1.plot(dat2['JOL'], m * dat2['JOL'] + b, color = 'k')

#JOL vs FREQ
ax2.plot(dat2['JOL'], dat2['FREQ'], '.', color = 'k')
m, b = np.polyfit(dat2['JOL'], dat2['FREQ'], 1)
ax2.plot(dat2['JOL'], m * dat2['JOL'] + b, color = 'k')

#JAM vs FREQ
ax3.plot(dat2['JAM'], dat2['FREQ'], '.', color = 'k')
m, b = np.polyfit(dat2['JAM'], dat2['FREQ'], 1)
ax3.plot(dat2['JAM'], m * dat2['JAM'] + b, color = 'k')

##Make the plot spiffy
#ax1
ax1.set_title('JOLs vs. JAMs', fontsize = 16, fontweight = 'bold')
ax1.set_ylabel('Mean JAM', fontsize = 14, fontweight = 'bold')
ax1.set_xlabel('Mean JOL', fontsize = 14, fontweight = 'bold')
ax1.set_xlim([0,60])
ax1.set_ylim([0,60])

#ax2
ax2.set_title('JOLs vs. Frequency Judgments', fontsize = 16, fontweight = 'bold')
ax2.set_ylabel('Mean Frequency Judgment', fontsize = 14, fontweight = 'bold')
ax2.set_xlabel('Mean JOL', fontsize = 14, fontweight = 'bold')
ax2.set_xlim([0,60])
ax2.set_ylim([0,60])

#ax2
ax3.set_title('JAMs vs. Frequency Judgments', fontsize = 16, fontweight = 'bold')
ax3.set_ylabel('Mean Frequency Judgment', fontsize = 14, fontweight = 'bold')
ax3.set_xlabel('Mean JAM', fontsize = 14, fontweight = 'bold')
ax3.set_xlim([0,60])
ax3.set_ylim([0,60])

##save figure
fig.savefig('Unrelated_chart.png', dip = 10000)