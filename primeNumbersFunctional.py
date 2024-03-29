'''

CÓDIGO FUNCIONAL

Faça um programa em Python, utilizando os conceitos de programação do paradigma
funcional, que leia dois inteiros x e y, sendo que x é menor que y, e imprima o comprimento do
maior intervalo entre dois números primos consecutivos maiores ou iguais a x e menores ou
iguais a y.

'''

# Função para verificar se um número é primo


def primeNumber(number, i):
    if number <= 1:  # Código base
        return False
    else:
        if i < number:  # Código para parada do IF
            if number % i == 0:  # Se não for primo
                return False
            else:
                return primeNumber(number, i + 1)
        return True  # Se for primo

# Função para adicionar números primos entre x e y numa lista


def primeList(primes, x, y):
    if x > y:  # Código para parada do IF
        return primes
    else:
        if primeNumber(x, 2):  # Se True (quando for primo)
            primes.append(x)
        return primeList(primes, x + 1, y)

# Função calcular o comprimento máximo entre dois primos consecutivos


def percorreList(primes, maxDiff, i):
    if i < len(primes) - 1:
        if (primes[i+1] - primes[i]) > maxDiff:
            return percorreList(primes, primes[i+1] - primes[i], i + 1)
        else:
            return percorreList(primes, maxDiff, i + 1)
    return maxDiff

# Início do cógido


x = int(input())
y = int(input())

lista = list()

primeList(lista, x, y)

print(percorreList(lista, 0, 0))
