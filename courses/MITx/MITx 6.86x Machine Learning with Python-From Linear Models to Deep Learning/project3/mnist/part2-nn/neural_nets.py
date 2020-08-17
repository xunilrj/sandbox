import numpy as np
import math

"""
 ==================================
 Problem 3: Neural Network Basics
 ==================================
    Generates a neural network with the following architecture:
        Fully connected neural network.
        Input vector takes in two features.
        One hidden layer with three neurons whose activation function is ReLU.
        One output neuron whose activation function is the identity function.
"""


def rectified_linear_unit(x):
    """ Returns the ReLU of x, or the maximum between 0 and x."""
    return max(0,x)

def rectified_linear_unit_derivative(x):
    """ Returns the derivative of ReLU."""
    if(x > 0):
        return 1
    else:
        return 0

def output_layer_activation(x):
    """ Linear function, returns input as is. """
    return x

def output_layer_activation_derivative(x):
    """ Returns the derivative of a linear function: 1. """
    return 1

class NeuralNetwork():
    """
        Contains the following functions:
            -train: tunes parameters of the neural network based on error obtained from forward propagation.
            -predict: predicts the label of a feature vector based on the class's parameters.
            -train_neural_network: trains a neural network over all the data points for the specified number of epochs during initialization of the class.
            -test_neural_network: uses the parameters specified at the time in order to test that the neural network classifies the points given in testing_points within a margin of error.
    """

    def __init__(self):

        # DO NOT CHANGE PARAMETERS
        self.input_to_hidden_weights = np.matrix('1 1; 1 1; 1 1')
        self.hidden_to_output_weights = np.matrix('1 1 1')
        self.biases = np.matrix('0; 0; 0')
        self.learning_rate = .001
        self.epochs_to_train = 10
        self.training_points = [((2,1), 10), ((3,3), 21), ((4,5), 32), ((6, 6), 42)]
        self.testing_points = [(1,1), (2,2), (3,3), (5,5), (10,10)]

    def train(self, x1, x2, y):

        # print("y", y)

        vrectified_linear_unit = np.vectorize(rectified_linear_unit)
        voutput_layer_activation = output_layer_activation
        # voutput_layer_activation_derivative = np.vectorize(output_layer_activation_derivative)
        vrectified_linear_unit_derivative = np.vectorize(rectified_linear_unit_derivative)

        # print('Forward propagation')
        ### Forward propagation ###
        input_values = np.matrix([[x1],[x2]]) # 2 by 1

        # Calculate the input and activation of the hidden layer
        hidden_layer_weighted_input = self.input_to_hidden_weights @ input_values + self.biases # TODO (3 by 1 matrix)
        hidden_layer_activation = vrectified_linear_unit(hidden_layer_weighted_input)
        assert(hidden_layer_activation.shape == (3,1))

        output = np.dot(self.hidden_to_output_weights, hidden_layer_activation)
        # print('output', output.shape, output); assert(output.shape == (1,1))
        activated_output = output
        assert(activated_output.shape == (1,1))
        # print('input_values', input_values)
        # print('hidden_layer_weighted_input', hidden_layer_weighted_input.shape, hidden_layer_weighted_input)
        # print('hidden_layer_activation', hidden_layer_activation.shape, hidden_layer_activation)
        # print('activated_output', activated_output.shape, activated_output)

        # print('Backpropagation')
        ### Backpropagation ###
        ## https://mattmazur.com/2015/03/17/a-step-by-step-backpropagation-example/

        # Compute gradients
        Error = 1/2 * (y - activated_output)**2
        #print("Error", Error)

        DErrorDoutput = (activated_output - y); """print(DErrorDoutput.shape)"""; assert(DErrorDoutput.shape == (1,1))
        DoutputDIo = 1
        DIoDW2 = hidden_layer_activation
        DIoDOh = self.hidden_to_output_weights
        DOhDIh = vrectified_linear_unit_derivative(hidden_layer_weighted_input)
        DIhDb = 1
        DIhDW1 = input_values.T

        DErrorDIo = DErrorDoutput * DoutputDIo
        DErrorDW2 = np.multiply(DErrorDIo, DIoDW2); """print(DErrorDW2.shape)"""; assert(DErrorDW2.shape == (3,1))
        DErrorDOh = (DErrorDIo * DIoDOh).T; """print(DErrorDOh.shape)"""; assert(DErrorDOh.shape == (3,1))
        DErrorDIh = np.multiply(DErrorDOh, DOhDIh); """print(DErrorDIh.shape)"""; assert(DErrorDIh.shape == (3,1))
        DErrorDb  = DErrorDIh * DIhDb; assert(DErrorDb.shape == (3,1))
        DErrorDW1 = DErrorDIh * DIhDW1; """print(DErrorDW1.shape)"""; assert(DErrorDW1.shape == (3,2))
        
        # Use gradients to adjust weights and biases using gradient descent
        self.biases = self.biases - self.learning_rate * DErrorDb
        assert(self.biases.shape == (3,1))

        assert(self.input_to_hidden_weights.shape == (3,2))
        self.input_to_hidden_weights = self.input_to_hidden_weights - self.learning_rate * DErrorDW1
        assert(self.input_to_hidden_weights.shape == (3,2))
        
        # print(self.hidden_to_output_weights.shape)
        assert(self.hidden_to_output_weights.shape == (1,3))
        self.hidden_to_output_weights = self.hidden_to_output_weights - self.learning_rate * DErrorDW2.T
        assert(self.hidden_to_output_weights.shape == (1,3))

    def predict(self, x1, x2):

        vrectified_linear_unit = np.vectorize(rectified_linear_unit)
        voutput_layer_activation = output_layer_activation
        # voutput_layer_activation_derivative = np.vectorize(output_layer_activation_derivative)
        # vrectified_linear_unit_derivative = np.vectorize(rectified_linear_unit_derivative)

        input_values = np.matrix([[x1],[x2]])

        # Compute output for a single input(should be same as the forward propagation in training)
        hidden_layer_weighted_input = self.input_to_hidden_weights @ input_values + self.biases
        hidden_layer_activation = vrectified_linear_unit(hidden_layer_weighted_input)
        output = self.hidden_to_output_weights @ hidden_layer_activation
        activated_output =voutput_layer_activation(output)

        return activated_output.item()

    # Run this to train your neural network once you complete the train method
    def train_neural_network(self):

        for epoch in range(self.epochs_to_train):
            for x,y in self.training_points:
                self.train(x[0], x[1], y)

    # Run this to test your neural network implementation for correctness after it is trained
    def test_neural_network(self):

        for point in self.testing_points:
            print("Point,", point, "Prediction,", self.predict(point[0], point[1]))
            if abs(self.predict(point[0], point[1]) - 7*point[0]) < 0.1:
                print("Test Passed")
            else:
                print("Point ", point[0], point[1], " failed to be predicted correctly.")
                return

x = NeuralNetwork()

x.train_neural_network()

# UNCOMMENT THE LINE BELOW TO TEST YOUR NEURAL NETWORK
x.test_neural_network()
