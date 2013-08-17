#!/usr/bin/python

import numpy as np

import matplotlib as mp
mp.rc('text', usetex=True)
mp.rc('font', family='serif', size=13)
import matplotlib.pyplot as plt

fp = open("bpsk_awgn_ber.txt", "r")
ebn0 = []
ber_sim = []
ber_theo = []
for line in fp:
    line = line.lstrip('(')
    line = line.rstrip(')\n')
    (a, b, c) = line.split(',')
    ebn0.append(float(a))
    ber_sim.append(float(b))
    ber_theo.append(float(c))

ebn0 = np.array(ebn0)
ber_sim = np.array(ber_sim)
ber_theo = np.array(ber_theo)

plt.semilogy(10*np.log10(ebn0), ber_sim, 'b')
plt.semilogy(10*np.log10(ebn0), ber_theo, 'ob')

plt.legend(['simulation', 'theoretical'], loc = 1)

plt.title("AWGN BPSK")
plt.ylabel(r'BER')
plt.xlabel(r'$E_b/N_0$ in dB')
plt.grid(True)
plt.savefig("bpsk_awgn_ber.pdf", bbox_inches='tight', dpi=300)
plt.axis([-10, 15, 1e-8, 1])
plt.show()
