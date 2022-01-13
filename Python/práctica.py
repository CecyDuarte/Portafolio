# -*- coding: utf-8 -*-
"""
Created on Tue Feb 16 21:18:13 2021

@author: almac
"""

def losing_team_captain(teams):
    """Given a list of teams, where each team is a list of names, return the 2nd player (captain)
    from the last listed team
    """
    j=len(teams)
    i = 0
    while i < j:
        print(teams[i][1])
        i += 1 
#%%
Team=[["Pablo", "Juan", "Paco"], ["Jorge", "José", "Pedro"]]
losing_team_captain(Team)

#%%
# Check your answer
Team=[["Pablo", "Juan", "Paco"], ["Jorge", "José", "Pedro"]]
print(len(Team))
print(Team[0][1])

#%%
def takeSecond(elem):
    return elem[1]

def fashionably_late(arrivals, name):
    """Given an ordered list of arrivals to the party and a name, return whether the guest with that
    name was fashionably late."""
    lista=[arrivals,name]
    if len(lista)>1:
        lista=lista.extend([arrivals.name])
        lista.sort(key=takeSecond)
        print(lista)
    else:
        lista.sort(key=takeSecond)
        print(lista)
#%%
fashionably_late(14,"Alex")
#%%
fashionably_late(12,"Jose")

