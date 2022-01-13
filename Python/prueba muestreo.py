# -*- coding: utf-8 -*-
"""
Created on Wed Oct 28 14:12:30 2020

@author: almac
"""
class est_varp:
    def __init__(self,n, N, var):
        self.n = n
        self.N = N
        self.var = var 
    def describir(self):
        a=self.n
        b=self.N
        c=self.var
        result=((c/a)/((b-a)/(b-1)))
        
        return result
#%%
result=est_varp(12, 56, 2.5)
print(result.describir())

