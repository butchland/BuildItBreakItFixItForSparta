f = open("nm.txt")

total = {}

for ln in f:
	data = ln.split()
	if len(data) < 4:
		continue
	if data[2] not in ["d", "r", "t"]:
		continue
	size = int(data[1])
	pkg = data[3].split(".")[0]
	if pkg in total:
		total[pkg] = total[pkg] + size
	else:
		total[pkg] = size

f.close()
all = 0
for pkg in total:
	all += total[pkg]
	if total[pkg] < 10000:
		continue
	print pkg, total[pkg]
print all
