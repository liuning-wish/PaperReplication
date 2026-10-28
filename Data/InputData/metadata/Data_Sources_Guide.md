# 数据来源说明（Data Sources Guide）

## 一、项目简介

本项目复现 Wulff 和 Mata（2022）关于风险语义表征的研究，分析风险相关词语联想、语义网络及其相关特征。

## 二、输入数据文件

 文件名:
 `risk_associations.csv` :风险相关词语联想数据 
 `risk_soep.csv` : 风险相关调查数据 
 `sentiment_de.csv` : 德语情绪相关数据 
 `sentiment_en.txt` : 英语情绪相关数据，具体用途待核实
 `SWOW_DU.csv` : 荷兰语词语联想数据 
 `SWOW_EN.csv` : 英语词语联想数据 
 `swow_risk_associates.csv` : SWOW 风险相关词语联想数据 
 `translations.xlsx` : 词语翻译数据 
 `translations_dutch.xlsx` : 荷兰语相关翻译数据 
 `word2vec_risk_vectors.txt` : 词向量数据 

## 三、数据来源与使用说明

以上文件保存在 `Data/InputData/` 目录中。正式提交前，应结合原论文、作者提供的项目材料及对应分析脚本，进一步核实各文件的原始来源、版本、具体用途和引用要求。

## 四、数据管理原则

1. 原始输入数据与分析生成的中间数据分开保存。
2. 作者节点数据与自行计算的节点数据分别管理。
3. 不覆盖原始输入文件。
4. 明确区分作者提供的预计算结果与本项目自行计算的结果。
5. 使用外部数据时，遵守相应的数据许可和引用要求。