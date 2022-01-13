# -*- coding: utf-8 -*-
"""
Created on Thu Oct 29 10:23:44 2020

@author: almac
"""

def promedio(*muestras):
    return len(muestras), sum(muestras) / len(muestras)
    print(sum(muestras))

#%%

promedio(1, 3, 5, 8, 11, 24, 90, 29)
media = promedio(1, 3, 5, 8, 11, 24, 90, 29)
print(media)
print('El promedio de la muestra de %d elementos es %.3f.' %(media))



