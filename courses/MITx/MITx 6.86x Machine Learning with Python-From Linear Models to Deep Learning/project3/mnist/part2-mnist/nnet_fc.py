#! /usr/bin/env python

import _pickle as cPickle, gzip
import numpy as np
from tqdm import tqdm
import torch
import torch.autograd as autograd
import torch.nn.functional as F
import torch.nn as nn
import sys
sys.path.append("..")
import utils
from utils import *
from train_utils import batchify_data, run_epoch, train_model

# Baseline = 0.9204727564102564
# Batch 64 = 0.9314903846153846
# lr 0.01 =  0.9206730769230769
# momentum 0.9 = 0.8855168269230769
# LeakyReLU = 0.9207732371794872
def main(hidden_size, batch_size, lr, momentum, activation):
    # Specify seed for deterministic behavior, then shuffle. Do not change seed for official submissions to edx
    np.random.seed(12321)  # for reproducibility
    torch.manual_seed(12321)  # for reproducibility
    print("---------------------------START",hidden_size,batch_size, lr, momentum, activation)
    # Load the dataset
    num_classes = 10
    X_train, y_train, X_test, y_test = get_MNIST_data()

    # Split into train and dev
    dev_split_index = int(9 * len(X_train) / 10)
    X_dev = X_train[dev_split_index:]
    y_dev = y_train[dev_split_index:]
    X_train = X_train[:dev_split_index]
    y_train = y_train[:dev_split_index]

    permutation = np.array([i for i in range(len(X_train))])
    np.random.shuffle(permutation)
    X_train = [X_train[i] for i in permutation]
    y_train = [y_train[i] for i in permutation]

    # Split dataset into batches
    train_batches = batchify_data(X_train, y_train, batch_size)
    dev_batches = batchify_data(X_dev, y_dev, batch_size)
    test_batches = batchify_data(X_test, y_test, batch_size)

    #################################
    ## Model specification TODO
    model = nn.Sequential(
        nn.Linear(784, hidden_size),
        activation,
        nn.Linear(hidden_size, 10),
    )
    ##################################

    accuracy = train_model(train_batches, dev_batches, model, lr=lr, momentum=momentum)
    print ("Accuracy: " + str(accuracy))

    ## Evaluate the model on test data
    loss, accuracy = run_epoch(test_batches, model.eval(), None)
    print ("Loss on test set:"  + str(loss) + " Accuracy on test set: " + str(accuracy))


if __name__ == '__main__':
    main(10, 32, 0.1, 0, nn.ReLU())
    main(10, 64, 0.1, 0, nn.ReLU())
    main(10, 32, 0.01, 0, nn.ReLU())
    main(10, 32, 0.1, 0.9, nn.ReLU())
    main(10, 32, 0.1, 0, nn.LeakyReLU())

    main(128, 32, 0.1, 0, nn.ReLU())
    main(128, 64, 0.1, 0, nn.ReLU())
    main(128, 32, 0.01, 0, nn.ReLU())
    main(128, 32, 0.1, 0.9, nn.ReLU())
    main(128, 32, 0.1, 0, nn.LeakyReLU())
