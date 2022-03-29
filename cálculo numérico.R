# AULA DE CÁLCULO NUMÉRICO NO SOFTWARE R

# PARTE 1 - INTRODUÇÃO AO R

## I) Operações entre números reais

# a) Somando dois inteiros

3 + 7
4.6 + 3.5

# b) Multiplicação de dois inteiros

8 * 5

# c) Realizando uma operação que gera erro

8 + * 5

# d) Dividindo dois números

10/7
round(10/7, 3) # Arredondando a divisão para 3 casas decimais
round(10/7, 4) # Arredondando a divisão para 4 casas decimais

# e) Realizando operações conjuntas

2 + 5 * 3

7 / 2 + 3

(2 + 5) * 3

7 / (2 + 3)

## II) UTILIZANDO OBJETOS

# a) Operações entre números

a <- 2
b <- 5

a
b

a + b
a * b
a / b

c <- 7

a - b * c

# b) Vetores

x = c(1, 2, 3, 4)
y = c(5, 6, 7)
z = c(-1, 4, -15, -20, 6)

# A função length() retorna o comprimento do vetor

length(x)
length(y)
length(z)

# A função c() concatena 2 ou mais vetores

concatenação1  = c(x, y)
concatenação1

concatenação2  = c(y, z)
concatenação2

concatenação3  = c(x, y, z)
concatenação3

concatenação4  = c(y, x)
concatenação4

length(concatenação3)

# c) Matrizes

# c.1) Matrizes de números

A = matrix(c(1,2,-3,-4,5,6,7,8,9,10,11,12), 3, 4)
A

# Acessando um valor especifico da matriz
A[1, 2]
A[3, 4]
A[2,  ]
A[ , 3]

B = t(A) # função transposta
B

C = -A # oposta
C

A + B
A + C

A %*% B
B %*% A
A %*% A

# Isso não é multiplicação entre matrizes!!
A * C

D = matrix( c(1,2,3,4), 2, 2)
D

E = solve(D) # inversa da matriz D
E



# c.2) Matrizes de caracteres

a = "Ariano Suassuna"
c = "Casimiro de Abreu"
e = "Eça de Queiroz"
f = "Fernando sabino"
g = "Guimaraes Rosa"
j = "José de Alencar"

# Criando a matriz de autores

Autor = matrix(c(a,c,e,f,g,j), 4, 2)
Autor

Autor[3,2] = ""
Autor[4,2] = "" 
Autor

Autor[3,2] = "George MArtin"
Autor[4,2] = "Gelson Iezzi" 
Autor

# III) CRIANDO FUNÇÕES NO R

# a) Função Soma
# declarando a função Soma

Soma = function(a, b) {
  resultado = a + b
  return(resultado)
}

# utilizando a função Soma
Soma(4, 9.5)
Soma(-3, 7)
Soma(7.4, 1.6)

# b) Função Maior

Maior = function(a,b) {
  maior = 0
  if (a > b) {
    maior = a
  }
  else {
    maior = b
  }
  return(maior)
}

# ultilizando a função Maior
Maior(5, -8)
Maior(-50, 3)
Maior(30, 30)

# c) Função maopr de 3 números

Maior_de_3 = function(a ,b ,c) {
  maior = 0
  if (a > b && a > c) {
    maior = a
  }
  else if (b > c) {
    maior = b
  }
  else {
    maior = c
  }
  return(maior)
}

Maior_de_3(-3,5,0)
Maior_de_3(12,-4,8)
Maior_de_3(6,6,5)
Maior_de_3(9,9,9)

# PARTE 2 - UTILIZANDO O R PARA CÁLCULO NUMÉRIO

# I) SOLUÇÃO NUMÉRICA DE EQUAÇÕES

# a) MÉTODO DA BISSEÇÃO

metodoBisseção = function( funcao, A, B, precisao) {
  
  diferenca = B - A
  # variáveis auxiliares
  a = A
  b = B
  metade = 0
  
  while(diferenca > precisao) {
    
    metade = (a+b)/2
    
    if(funcao(a) * funcao(metade) < 0) {
      # a raíz está no intervalo [a, (a+b)/2]
      b = metade
    }
    else if (funcao(metade) * funcao(b) < 0) {
      # a raíz está no intervalo [(a+b)/2, b]
      a = metade
    }
    else {
      print("Erro")
      break()
    }
    diferenca = b - a
    
  }
  return((a+b)/2)
  
}

f = function(x) {
  y = x^3 - 9*x + 3
  return(y)
}

resultado1 = metodoBisseção(f, 0, 1, 0.1)
round(resultado1, 5)

resultado2 = metodoBisseção(f, 0, 1, 0.01)
round(resultado2, 5)

resultado3 = metodoBisseção(f, 0, 1, 0.001)
round(resultado3, 5)

resultado5 = metodoBisseção(f, -4, -3, 0.1)
round(resultado5, 5)

resultado6 = metodoBisseção(f, 2, 3, 0.1)
round(resultado6, 6)

# implementando a função h(x) = x² - 7
h = function(x) {
  y = x^2 - 7
  return(y)
}

resultado7 = metodoBisseção(h, 2, 3, 0.1)
resultado7

# obtendo raíz de 7 pelo R
sqrt(7)

metodoBisseção(h, 2, 3, 0.1)
metodoBisseção(h, 2, 3, 0.01)
metodoBisseção(h, 2, 3, 0.001)
metodoBisseção(h, 2, 3, 0.0001)
metodoBisseção(h, 2, 3, 0.00001)
metodoBisseção(h, 2, 3, 0.0000001)


# b) MÉTODO DE NEWTON

metodoNewton = function(f, g, x_inicial, precisao) {
  
  x = x_inicial
  x_seguinte = x - (f(x)/g(x))
  
  diferenca = abs(x - x_seguinte)
  
  while (diferenca > precisao) {
    
    x = x_seguinte
    x_seguinte = x - (f(x)/g(x))
    
    diferenca = abs(x - x_seguinte)
  }
  return(x_seguinte)
}

f = function(x) {
  y = x^3 - 9*x + 3
  return(y)
}

# definindo a derivada da função f
g = function(x) {
  y = 3*(x^2) - 9
  return(y)
}

w1 = metodoNewton(f, g, -3.5, 0.001)
w2 = metodoNewton(f, g, 0.5, 0.001)
w3 = metodoNewton(f, g, 2.5, 0.001)
w1
w2
w3


h = function(x) {
  y = x^2 - 7
  return(y)
}

# implementado a derivada da função h(x) = x² - 7
i = function(x) {
  y = 2*x
  return(y)
}

w4 = metodoNewton(h, i, 2.5, 0.001)
w4

sqrt(7)

# II) ITEGRAÇÂO NUMÉRICA

# a) Regra dos Trapézios

regraTrapezios = function(f, a, b, n) {
  
  x = matrix (c(0), n+1, 1)
  y = matrix (c(0), n+1, 1)
  
  delta_x = (b-a)/n
  
  for(i in 1:(n+1)) {
    
    x[i] = a + (i-1) * delta_x
    y[i] = f(x[i])
    
  }
  
  soma = 0
  for (i in 2:n) {
    
    soma = soma + y[i]
    
  }
  
  integral = ((b-a)/(2*n)) * (y[1] + 2*soma + y[n+1])
  return (integral)
  
}

funcaoInversa = function(x) {
  return(1/x)
}

regraTrapezios(funcaoInversa, 1, 2, 4)
log(2)
regraTrapezios(funcaoInversa, 1, 2, 50)
regraTrapezios(funcaoInversa, 1, 2, 400)
regraTrapezios(funcaoInversa, 1, 2, 4000)

funcao_do_e = function(x) {
  e = 2.718281828
  return(e^(x^2))
}

regraTrapezios(funcao_do_e, 0, 1, 5)
regraTrapezios(funcao_do_e, 0, 1, 500)
regraTrapezios(funcao_do_e, 0, 1, 50000)
regraTrapezios(funcao_do_e, 0, 1, 5000000)


# b) Regra do Ponto Médio

regraPontoMedio = function(f, a, b, n) {
  
  x = matrix(c(0), n+1, 1)
  y = matrix(c(0), n, 1)
  x_medio = matrix(c(0), n, 1)
  
  delta_x = (b-a)/n
  
  for(i in 1:(n+1)) {
    
    x[i] = a + (i-1) * delta_x
  }
  
  for (i in 1:n) {
    
    x_medio[i] = (x[i] + x[i+1])/2
    y[i] = f(x_medio[i])
    
  }
  soma = 0
  for (i in 1:n) {
    
    soma = soma + y[i]
    
  } 
  integral = delta_x * soma
  return(integral)
  
}

regraPontoMedio (funcao_do_e, 0, 1, 5)
regraPontoMedio (funcaoInversa, 1, 2, 10)


# c) Regra 1/3 de Simpsom

regraSimpson = function (f, a, b, n) {
  
  x = matrix(c(0), n+1, 1)
  y = matrix(c(0), n+1, 1)
  
  delta_x = (b-a)/n
  
  for (i in 1:(n+1)) {
    
    x[i] = a + (i-1) * delta_x
    y[i] = f(x[i])
    
  }
  
  soma_par = 0
  for (i in 1:(n/2)) {
    
    soma_par = soma_par + y[2*i]
    
  }
  
  soma_impar = 0
  for (i in 1:(n/2-1)) {
    
    soma_impar = soma_impar + y[2*i+1]
    
  }
  
  integral = ((b-a)/(3*n)) * (y[1] + (4*soma_par) + (2*soma_impar) + y[n+1])
  return(integral)
  
}

regraSimpson(funcaoInversa, 1, 2, 4)
regraSimpson(funcao_do_e, 0, 1, 10)



