import numpy as np

X = np.array([
    
    [1,0,0],
    [-1,10,0],
    [-1,-1,0],
])

y = np.array([-1,1,1])

def perceptron_sgd(X, Y):
    w = np.zeros(len(X[0]))
    eta = 1
    epochs = 20

    for t in range(epochs):
        for i, x in enumerate(X):
            if (np.dot(X[i], w)*Y[i]) <= 0:
                print('mistake')
                w = w + eta*X[i]*Y[i]
                print(w)

    return w

w = perceptron_sgd(X,y)
print(w)