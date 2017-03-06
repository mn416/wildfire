#!/usr/bin/env python

import matplotlib.pyplot as plt
import numpy as np
import sys

Xs=[[64, 144, 306, 625], [64, 160, 384, 512]]
Ys=[[78.5, 34.3, 17.4, 11.4], [77.4, 30.5, 13.0, 9.7]]
Ys=[[4753.1/x for x in xs] for xs in Ys]

fig = plt.figure(figsize=(4,3))
ax = fig.add_subplot(111)
ax.spines['top'].set_color('none')
ax.spines['right'].set_color('none')
ax.yaxis.set_ticks_position('left')
ax.xaxis.set_ticks_position('bottom')

plt.xticks([64, 128, 256, 512, 1024], fontsize=8)
ax.set_xticklabels(["64", "128", "256", "512", "1024"])
plt.yticks([0, 16, 64, 128, 256, 512, 800], fontsize=8)
plt.gcf().subplots_adjust(bottom=0.15)

plt.xlim([0,650])
plt.ylim([0,800])

plt.ylabel("Time (s)", fontsize=9)
plt.xlabel("Processors", fontsize=9)

bfly, = plt.plot(Xs[1][0:], Ys[1][0:], 'k.-', label="Butterfly")
torus,  = plt.plot(Xs[0][0:], Ys[0][0:], 'b.-', label="Torus")

leg = ax.legend(loc=2, frameon=False, labelspacing=0, numpoints=1,
                handletextpad=0.2, handlelength=0.7)
for label in leg.get_texts():
  label.set_fontsize(8)

#plt.show()
plt.savefig("plot.png")
