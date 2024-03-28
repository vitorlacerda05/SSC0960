'''

CÓDIGO FUNCIONAL

Faça um programa em Python, utilizando os conceitos de programação do paradigma
funcional, que leia dois inteiros x e y, sendo que x é menor que y, e imprima o comprimento do
maior intervalo entre dois números primos consecutivos maiores ou iguais a x e menores ou
iguais a y.

'''

# Função para verificar se um número é primo


def primeNumber(number):
    if number <= 1:  # Código base
        return False
    else:
        for i in range(2, number):  # Se não for primo
            if number % i == 0:
                return False
        return True  # Se for primo

# Função para adicionar números primos entre x e y numa lista


def primeList(primes, x, y):
    for i in range(x, y + 1):
        if primeNumber(i):  # Se True (quando for primo)
            primes.append(i)
    return primes

# Função calcular o comprimento máximo entre dois primos consecutivos


def percorreList(primes):
    maxDiff = 0
    for i in range(0, len(primes) - 1):
        if (primes[i+1] - primes[i]) > maxDiff:
            maxDiff = (primes[i+1] - primes[i])
        else:
            continue
    return maxDiff


# Início do cógido


x = int(input())
y = int(input())

lista = list()

primeList(lista, x, y)

print(percorreList(lista))
