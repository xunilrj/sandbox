#Estimates of Parameters of GMM: The Expectation Maximization (EM) Algorithm

xs = c(
  0.2,
  -0.9,
  -1,
  1.2,
  1.8
)


# E Step

Lx1j1 = dnorm(x= 0.2, mean=-3, sd=2)
Lx2j1 = dnorm(x=-0.9, mean=-3, sd=2)
Lx3j1 = dnorm(x=-1.0, mean=-3, sd=2)
Lx4j1 = dnorm(x= 1.2, mean=-3, sd=2)
Lx5j1 = dnorm(x= 1.8, mean=-3, sd=2)

Lx1j2 = dnorm(x= 0.2, mean=2, sd=2)
Lx2j2 = dnorm(x=-0.9, mean=2, sd=2)
Lx3j2 = dnorm(x=-1.0, mean=2, sd=2)
Lx4j2 = dnorm(x= 1.2, mean=2, sd=2)
Lx5j2 = dnorm(x= 1.8, mean=2, sd=2)

pj1 = 0.5
pj2 = 0.5

result = c(
  pj1*Lx1j1/(pj1*Lx1j1+pj2*Lx1j2),
  pj1*Lx2j1/(pj1*Lx2j1+pj2*Lx2j2),
  pj1*Lx3j1/(pj1*Lx3j1+pj2*Lx3j2),
  pj1*Lx4j1/(pj1*Lx4j1+pj2*Lx4j2),
  pj1*Lx5j1/(pj1*Lx5j1+pj2*Lx5j2)
);

print(result)

#0.29421497 
#0.62245933 
#0.65135486 
#0.10669059 
#0.05340333

# M Step

library('pracma')

newpj1 = sum(result)/length(result)
newmuj1 = dot(xs,result)/sum(result)
news2j1 = dot(result,(xs-newmuj1)**2)/(sum(result))

print(newpj1)
print(newmuj1)
print(news2j1)
