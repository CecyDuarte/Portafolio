def fun(x):
    """Evalua la función dada en el valor x dado"""
    return eval(expr)

def MS(u,v):
    """Evalua el Metodo Simple o 1/3 en los intervalos acotados"""
    result = ((v - u) / 6.0) * (fun(u) + (4.0 * fun((u + v) / 2.0)) + fun(v))
    return result

#MAIN
#Introduccion de información
print("APROXIMACIÓN DE UNA INTEGRAL POR METODO SIMPSON ITERADO \n\n")
expr = input("Introduce la función en términos de x, usa exclusivamente los siguientes simbolos: \n +, -, *, /, **, (, )\n")
inf = float(input("Ingresa el valor inferior de la integral \n"))
sup = float(input("Ingresa el valor superior de la integral \n"))
errorE = float(input("Ingrese el error deseado en DECIMALES \n"))

#Programa
Ant = MS(inf, sup)
Acum = 0
i=1
while True:
    q= 2**i
    inter = (sup-inf)/q
    a = inf
    b = inf + inter
    for _ in range(q):
        Acum = Acum + MS(a,b)
        a = b
        b = b + inter
    ErrorC = abs((Acum-Ant)/Acum)
    if ErrorC < errorE:
        break
    Ant = Acum
    Acum = 0
    i = i + 1
    
print("\nEl valor de la integral aproximado es: ", Acum, "\nCon un error de: ", (ErrorC*100), "%")
input("\n\nPresione ENTER para terminar")