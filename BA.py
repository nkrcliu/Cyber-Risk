import numpy as np
import csv
import networkx as nx   # 导入建网络模型包，命名ne
import matplotlib.pyplot as mp  # 导入科学绘图包，命名mp

# BA scale-free degree network graphy
L = 100
BA = nx.barabasi_albert_graph(L, 1)  #1000个节点，初始m0=2
ps = nx.spring_layout(BA)   # 布置框架
nx.draw(BA, ps, with_labels=None, node_size=20)
mp.show()

G = []
for i in range(0, L):  # 记录所有节点的邻居节点
    G.append(list(BA.neighbors(i)))

links = []
#link = np.zeros(L)
for i in range(0, L):
    link = np.zeros(L)
    linklist = link.tolist()
    for j in range(0, L):
        if j in G[i]:
            linklist[j] = 1
    links.append(linklist)
print(links)

np.savetxt('links.csv', links, delimiter=',')

for i in range(0, L):  #将各个节点的额lable(ID)插入到表格的第一列
    degree = len(G[i])
    G[i].insert(0, degree)#加入每个节点的度数
    G[i].insert(0, i)
print(G)

with open('BA_model.csv', 'w', newline='') as f:
    writer = csv.writer(f)  #生成BA_model.csv文件
    for i in G:
        writer.writerow(i)
