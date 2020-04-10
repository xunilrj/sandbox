import torch
import torch.nn as nn
import torch.optim as optim

print('Building model...')

model = nn.Sequential(
    nn.Linear(2,1)
)
optimizer = optim.SGD(model.parameters(), lr=0.1, momentum=0.9)
criterion = torch.nn.MSELoss() 

print('Loading training data...')

inputs = torch.tensor([[1.0,2.0],[2.0,3.0],[3.0,4.0]])
labels = torch.tensor([[3.0],[5.0],[7.0]])

print('Training...')

for epoch in range(1000): 
    optimizer.zero_grad()
    predictions = model(inputs)
    loss = criterion(predictions, labels)
    loss.backward()
    optimizer.step()

print('Saving model...')

dummy_input = torch.tensor([[1.0,2.0]])
input_names = ["input"]
output_names = ["output"]

torch.onnx.export(model, dummy_input, "model.onnx", verbose=True, input_names=input_names, output_names=output_names)

print('Done!')