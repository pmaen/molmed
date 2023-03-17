import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import matplotlib.patches as mpatches
import matplotlib
from matplotlib.lines import Line2D

matplotlib.rcParams['font.sans-serif'] = "Arial"
matplotlib.rcParams['font.family'] = "sans-serif"

filename = input('Filename:\t')
yaxis = input('y-axis label:\t')
project = input('Project name:\t')

def plot_data(filename,yaxis,project):

	df = pd.read_csv(filename, delimiter=',')
	
	fig, ax = plt.subplots()
	
	colors = ['none','silver','none','silver','none','silver','none','silver']
	hatches = ['..','//','..','//','..','//','..','//','..']
	bplot = ax.boxplot(df, labels=df.columns, notch=False, patch_artist=True,\
	 showmeans = True, meanline = True, meanprops={'color': 'tab:blue'})
	
	xticks = np.arange(1.5, len(ax.boxplot(df)['boxes']), 2)
	ax.set_xticks(xticks)
	
	xticklabels = ['-','T','TB','TBZ']
	ax.set_xticklabels(xticklabels)
	
	for patch, color in zip(bplot['boxes'], colors):
			patch.set_facecolor(color)
	
	circ1 = mpatches.Patch(facecolor='none', edgecolor='black',label='no NSA')
	circ2 = mpatches.Patch(facecolor='silver', edgecolor='black',label='+ NSA')
	circ3 = Line2D([0],[0], color = 'tab:blue', linestyle='--', label='mean')
	circ4 = Line2D([0],[0], color = 'tab:orange', label='median')
	
	ax.tick_params(axis='both', which='major', labelsize=12)
	
	ax.set_yscale('log')
	ax.legend(handles = [circ1,circ2,circ3,circ4], fontsize=12)
	ax.set_ylim(0,100)
	
	plt.xlabel('Treatment', fontsize=14)
	plt.ylabel(yaxis, fontsize=14)
	
	plt.savefig(f'{project}_boxplot.svg', transparent=True)
	plt.savefig(f'{project}_boxplot.png', transparent=True)
	
	plt.show()
	
plot_data(filename,yaxis,project)
	
