# -*- coding: utf-8 -*-
"""
Created on Thu Feb 25 14:38:14 2021

@author: almac
"""
class calc:
    print("""
     Selecciona un atributo segun el caso     
    ¿Qué operacion quieres hacer?
    1) Sumar [funcion: sumar]
    2) Restar [funcion: restar]
    3) Multiplicar [funcion: multiplicar]
    4) Dividir [funcion: dividir]
    5) Exponente [funcion: exponente]
    """) 
    def sumar(self, n1, n2):

        print("La suma es: " + str((n1+n2)))
    
    def restar(self, n1, n2):
        print("La resta es de n1-n2: ", str((n1+n2)))
        
    def multiplicar(self, n1, n2):
        print("El producto de los numeros es: " + str((n1*n2)))
    
    def dividir(self, n1, n2):
        print("La división de ni entre n2 es: " + str((n1/n2)))
    def exponente(self, n1, n2):
        print("n1 elevado a la n2 es: " + str((n1**n2)))
prueba=calc()
prueba.sumar(1,4)

    