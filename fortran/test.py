import matplotlib.pyplot as plt


dat, i = [[] for _ in range(500)], 0
with open("dat", 'r') as file:
    for line in file.readlines():
        dat[i] += [int(a) for a in line.split()]
        i += 1
#for j in range(500):
#    lst += [dat[j*500:(j+1)*500]]
#print(len(dat), len([x for x in lst if x]))
#print(lst)
plt.imshow(dat, cmap="magma")
plt.gca().invert_yaxis()
plt.show()
#for r, vals in dat.items():
#    plt.plot(vals, [r]*len(vals), '.')
#plt.show()
