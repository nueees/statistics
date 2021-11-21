#!/usr/bin/env python
# coding: utf-8

# # 05_CDA(Confirmatory Data Analysis)

# - 어떤 현상이 '우연'인지 그렇지 않은지를 확인하기 위함

# In[1]:


import scipy.stats as spst
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns


# ## Comparative Type Test

# ### One Sample T-Test
# - 샘플 A의 평균이 x와 다른가? - p-value가 낮으면 '다르다!'
# - 귀무가설 : 같다
# - 대립가설 : 다르다

# In[47]:


A = (8.8,8.4,7.9, 8.7,9.1,9.6, 8.7)
B = (9.9,9.0,11.1,9.6,8.7,10.4,9.5)


# In[26]:


spst.ttest_1samp.__doc__
ttest1 = spst.ttest_1samp(A, popmean = 8)
print(ttest1)


# **pvalue** : 0.02522883506770603
# * 정직한 설명 : 귀무가설이 참이라는 전제하에 이렇게 데이터가 관찰될 확률이 0.0....01%정도라는 뜻이다.
# * 발칙한 설명 : 기존 배너보다 나을 확률이 99%를 넘는다는 뜻이다.

# **statistic** : 3.155273714337536
# * 신호/노이즈가 3.1, 즉 신호가 노이즈보다 3.1배 높다는 뜻

# ### Two Sample T-Test
# - A와 B가 다른가? p-value가 낮으면 '다르다'!

# In[24]:


# 분산의 동일성 검정
spst.levene(A,B)
# pvalue가 통상적인 기준인 0.1보다 크므로 분산 동일하다는 가정 받아들임.


# In[25]:


spst.ttest_ind(A, B, equal_var=True) # equal_var는 등분산 여부인데, 모르면 False


# ### Paired T-Test
# - 한 집단에서 전-후 비교(Before-After) - 당연히 p-value 낮으면 다른 것

# In[31]:


pres_raw = {'before':[142,140,144,144,142,146,149,150,142,148],
            'after':[138,136,147,139,143,141,143,145,136,146] }
pres_df = pd.DataFrame(pres_raw)
print(pres_df)


# In[36]:


plt.subplot(1,2,1)
pres_df['before'].plot(kind = 'box', ylim = (130,160))

plt.subplot(1,2,2)
pres_df['after'].plot(kind = 'box', ylim = (130,160))


# In[34]:


spst.ttest_rel(pres_df.before, pres_df.after) # 얘가 Paried t-test


# In[38]:


pres_df.before.mean() - pres_df.after.mean()


# ### ANOVA(Analysis of Variance)
# **귀무가설 : A,B,C 다 똑같다**  
# **대립가설 : A,B,C 중 '무언가 하나는' 다를 것이다.**
# 
# 대립가설 조심, A, B, C 중 뭐가 다르고, 얼마나 다르고 등은 전혀 알 수 없다. 따로 계산해야 한다.
# 
# [ 분산분석  검정의 가정사항 (assumptions of ANOVA test) ]
# 
#   (1) 독립성: 각 샘플 데이터는 서로 독립이다.  
#   (2) 정규성: 각 샘플 데이터는 정규분포를 따르는 모집단으로 부터 추출되었다.  
#   (3) 등분산성: 그룹들의 모집단의 분산은 모두 동일하다.  
#   

# In[41]:


cotton_raw = {'15p':[7,7,15,11,9],
            '20p':[12,17,12,18,18],
            '25p':[14,18,18,19,19] }
cotten_df = pd.DataFrame(cotton_raw)
print(cotten_df)


# In[44]:


spst.f_oneway(cotten_df['15p'],cotten_df['20p'],cotten_df['25p']) # 얘가 ANOVA


# ##### 정석적인 해석 : 귀무가설이 참일 때, 이러한 데이터가 관측될 확률은 1.85% 정도이다. (1.85%확률을 뚫고 이런 데이터가 관측될 수도 있다.)
# 
# 발칙한 해석 : 뭔가 하나는 차이가 날 확률이 98%는 넘는다.

# ## Associative Type Test

# ### Correlation Coefficient
# **귀무가설 : X와 Y는 상관이 없다.(상관계수 = 0)**  
# **대립가설 : 상관계수가 0이 아니다.**
# - X와 Y를 별도로 시각화 해 볼 것

# In[48]:



spst.pearsonr(A,B) # X와 Y의 상관계수와 p-value


# - 결과는 튜플로 나오는데
# 
# 1. 튜플의 첫 번째 값 : 상관계수를 뜻한다. 두 데이터의 선형성의 정도를 나타낸다.
# 2. p-value는 상관계수가 우연에 의해 일어나진 않았는지 판단한다.
#     * 귀무가설 : 상관 계수가 0이다.
#     * 대립가설 : 상관 계수가 0이 아니다.
# 
# pvalue가 0.57로 0.05보다 크기 때문에 귀무가설 받아들임
# 피어슨상관계쑤는 -0.25이므로 약한 음의 상관관계가 있음

# ### 교차분석(Chisquare_test)
# 
# 티셔츠 구매여부와 반바지 구매여부는 관계가.. 있을까?!  
# **귀무가설 : 티셔츠 구매와 바지 구매는 별개이다.(독립이다)**  
# **대립가설 : 티셔츠를 구매와 바지는 독립이 아니다.관련이 있다..**

# In[56]:


# 고객별 셔츠와 바지의 구매 여부 데이터
shirt_raw = {'shirts':[0,0,0,0,0,0,1,1,0,0,1,1,1,1,1,0,0,0,0,0,1],
            'pants':[0,0,1,0,0,0,1,0,1,1,1,1,1,1,1,0,0,0,1,0,1] }
shirt_df = pd.DataFrame(shirt_raw)
print(shirt_df.head(10))


# In[57]:


# 데이터의 Crosstable
contingency = pd.crosstab(shirt_df['shirts'], shirt_df['pants'])
contingency


# In[58]:


chiresult = spst.chi2_contingency(contingency) # 카이제곱 검정


# In[59]:


# 결과 : 튜플로 4개 값 출력됨
print("카이제곱통계량 : {}".format(chiresult[0]))
print("p-value : {:.20f}".format(chiresult[1]))
print("자유도 : {}".format(chiresult[2]))
print("기대 빈도 분할표: \n", chiresult[3] ) #귀무가설에 대한 기대빈도.


# - 유의수준 0.05 하에 p-value가 매우 낮으므로 두 집단간 차이가 있다(바지 구매는 셔츠 구매와 관련이 있다)
# - pants 0과 pants 1 그룹을 비교했을 때 shirts 0, 1의 차이가 있다. pants가 0, 1, 2였다면 0, 1, 2에 따라 차이가 있다라고 해석할 수 있음
