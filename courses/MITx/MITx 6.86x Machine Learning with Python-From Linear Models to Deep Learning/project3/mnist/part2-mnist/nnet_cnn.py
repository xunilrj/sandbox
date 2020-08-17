#! /usr/bin/env python

from onnx import version_converter
import onnx
import os.path
from train_utils import batchify_data, run_epoch, train_model, Flatten
from utils import *
import utils
import _pickle as c_pickle
import gzip
import numpy as np
from tqdm import tqdm
import torch
import torch.autograd as autograd
import torch.nn.functional as F
import torch.nn as nn
import sys
sys.path.append("..")


def main():
    #################################
    # Model specification TODO
    model = nn.Sequential(
        nn.Conv2d(1, 32, (3, 3)),
        nn.ReLU(),
        nn.MaxPool2d((2, 2)),
        nn.Conv2d(32, 64, (3, 3)),
        nn.ReLU(),
        nn.MaxPool2d((2, 2)),
        Flatten(),
        nn.Linear(1600, 128),
        nn.Dropout(p=0.5),
        nn.Linear(128, 10),
    )

    if os.path.isfile("digits.pth"):
        model.load_state_dict(torch.load("digits.pth"))
        model.eval()
    else:
        num_classes = 10
        X_train, y_train, X_test, y_test = get_MNIST_data()
        X_train = np.reshape(X_train, (X_train.shape[0], 1, 28, 28))
        X_test = np.reshape(X_test, (X_test.shape[0], 1, 28, 28))
        dev_split_index = int(9 * len(X_train) / 10)
        X_dev = X_train[dev_split_index:]
        y_dev = y_train[dev_split_index:]
        X_train = X_train[:dev_split_index]
        y_train = y_train[:dev_split_index]
        permutation = np.array([i for i in range(len(X_train))])
        np.random.shuffle(permutation)
        X_train = [X_train[i] for i in permutation]
        y_train = [y_train[i] for i in permutation]
        batch_size = 32
        train_batches = batchify_data(X_train, y_train, batch_size)
        dev_batches = batchify_data(X_dev, y_dev, batch_size)
        test_batches = batchify_data(X_test, y_test, batch_size)
        train_model(train_batches, dev_batches, model, nesterov=True)
        loss, accuracy = run_epoch(test_batches, model.eval(), None)
        print("Loss on test set:" + str(loss) +
              " Accuracy on test set: " + str(accuracy))
        torch.save(model.state_dict(), "digits.pth")

    print(model)
    onnxFilename = "digits.onnx"
    torch.onnx.export(model,
                      torch.rand(32, 1, 28, 28),
                      onnxFilename,
                      export_params=False,
                      verbose=True,
                      do_constant_folding=True,
                      keep_initializers_as_inputs=False)

    # target_version = 8
    # ir_version = 3
    model = onnx.load(onnxFilename)
    # converted_model = version_converter.convert_version(model, target_version)
    # converted_model.ir_version = ir_version
    # onnx.save(converted_model, onnxFilename)

    onnx.checker.check_model(model)
    onnx.helper.printable_graph(model.graph)


if __name__ == '__main__':
    # Specify seed for deterministic behavior, then shuffle. Do not change seed for official submissions to edx
    np.random.seed(12321)  # for reproducibility
    torch.manual_seed(12321)
    main()
