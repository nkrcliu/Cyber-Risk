import numpy as np #导入数组工具包，命名为np
import csv
import networkx as nx   # 导入建网络模型包，命名nx
import matplotlib.pyplot as mp  # 导入科学绘图包，命名mp

# 寻找符合实际情况的BA无标度网络
n2 = 9999#初始化检验条件
while n2 > 8:#度数为2的节点多余8个时重新生成
    L = 100#总节点数
    BA = nx.barabasi_albert_graph(L, 1)  #初始m0=1
    ps = nx.spring_layout(BA)   # 布置框架
    nx.draw(BA, ps, with_labels=None, node_size=20)#绘图
    mp.show()#显示绘图结果

    G1 = []
    for i in range(0, L):  # 记录所有节点的邻居节点
        G1.append(list(BA.neighbors(i)))
    print(G1)

    #计算度数并对度为2的节点计数
    degs = []
    for i in range(0, L):
        deg = len(G1[i])
        degs.append(deg)
    n2 = degs.count(2)

#将节点按照度数大小重新排序
index = np.argsort(degs)
indexs = index.tolist()#返回排序后的元素地址（从小到大）
indexs.reverse()#转化为从大到小
print(indexs)

#将节点按照排序后的结果，并更新邻居节点的名称
former_name = list(range(L))#原来的节点名是依次生成的
print(former_name)
replace_dic = dict(zip(indexs, former_name))#构建替换字典
print(replace_dic)

G2 = []
for i in range(0, L):
    new_link = [replace_dic[j] if j in replace_dic else j for j in G1[i]]#将原来的节点名更新为依据度数高低排序后的节点名称
    G2.append(new_link)
print(G2)

#把节点邻居的信息按照度数大小排列在数组中
G = []
for i in indexs:
    G.append(G2[i])

#构建无向图邻接矩阵
links = []
for i in range(0, L):
    link = np.zeros(L)
    linklist = link.tolist()
    for j in range(0, L):
        if j in G[i]:
            linklist[j] = 1
    links.append(linklist)

print(links)
np.savetxt('links.csv', links, delimiter=',')#导出无向图邻接矩阵

#构建有向图邻接矩阵
directed_link = links#拷贝有向图
for i in range(0, L):
    for j in range(0, i):
        directed_link[i][j] = 0#只存在度数大向度数小的单向连接

print(directed_link)
np.savetxt('directed_links.csv', directed_link, delimiter=',')#导出有向图邻接矩阵

#节点度数及连接信息的具体情况
for i in range(0, L):
    degree = len(G[i])
    G[i].insert(0, degree)#加入每个节点的度数
    G[i].insert(0, i)#加入每个节点的名称
print(G)

#输出到文件中
with open('BA_model.csv', 'w', newline='') as f:
    writer = csv.writer(f)  #生成BA_model.csv文件
    for i in G:
        writer.writerow(i)