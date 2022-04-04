from __future__ import print_function

import os
import sys
from argparse import ArgumentParser
from time import time

import pandas as pd
import tensorflow as tf
import numpy as np
import cv2
import matplotlib.pyplot as plt
import scipy.misc
from tensorflow.keras.optimizers import Adam

### import self-defined functions
from model import *
from image_reader import *
from loss import *


tf.keras.backend.set_floatx('float32')

def build_parser():
    parser = ArgumentParser()
    
    # Paths
    parser.add_argument('--image-dir', type=str, default='../denmark_rplc_pos_224_224/', help='path to foler of training images')
    parser.add_argument('--train-file', type=str, default='../denmark_rplc_pos_224_224/train_all_shifts.csv', help='path to csv file of training/testing examples')
    parser.add_argument('--trained-model-dir', type=str, default='../trained_models/', help='path to trained models folder')
    parser.add_argument('--trained-model-fn', type=str, default='model_224x224_fold5', help='trained model filename')
    parser.add_argument('--result-name', type=str, default='../trained_models/results_224x224_fold5.csv', help='directory to store registration results')
    # Optimization parameters 
    parser.add_argument('--lr', type=float, default=0.0001, help='learning rate')
    parser.add_argument('--num-epochs', type=int, default=100, help='number of training epochs')
    parser.add_argument('--batch-size', type=int, default=8, help='training batch size')
    parser.add_argument('--image-size', type=int, default=224, help='size of image used for training and testing')
    parser.add_argument('--gpu-id', type=int, default=0, help='training batch size')
    # Model parameters
    parser.add_argument('--feature-cnn', type=str, default='vgg16', help='Feature extraction network: vgg16/resnet101')
    
    return parser
   
   
def main():

    parser = build_parser()
    args = parser.parse_args()

    devices = tf.config.experimental.list_physical_devices('GPU')
    for device in devices:
        tf.config.experimental.set_memory_growth(device, True)
    tf.config.experimental.set_visible_devices(devices[args.gpu_id], 'GPU')
    
    
    train_losses = np.zeros(args.num_epochs)
    validation_losses = np.zeros(args.num_epochs)
    data = pd.read_csv(args.train_file)
    
    dataset = image_reader(args.train_file, args.image_dir, output_shape = (args.image_size,args.image_size))

    image = dataset['image']
    g_stages = dataset['g_stages']
    delivery = dataset['delivery']
    classes = dataset['classes']

    
    index_train = np.where(classes.reshape(classes.shape[0],)!=5)
    index_val = np.where(classes.reshape(classes.shape[0],)==5)

    imgae_train = image[index_train,:,:,:]
    g_stages_train = g_stages[index_train]
    image_test = image[index_val,:,:,:]
    g_stages_test = g_stages[index_val]
    
   
    num_of_train_images = g_stages_train.shape[0]
    num_of_validation_images = g_stages_test.shape[0]
    
    imgae_train = imgae_train.reshape((num_of_train_images,image.shape[1],image.shape[2],image.shape[3]))
    image_test = image_test.reshape((num_of_validation_images,image.shape[1],image.shape[2],image.shape[3]))
    
    
    #num_of_train_images = 2524
    #num_of_validation_images = 3000 - 2524
    #imgae_train = image[0:num_of_train_images,:,:,:]
    #g_stages_train = g_stages[0:num_of_train_images]
    #image_test = image[num_of_train_images:num_of_train_images + num_of_validation_images,:,:,:]
    #g_stages_test = g_stages[num_of_train_images:num_of_train_images + num_of_validation_images]
    

    
    print(imgae_train.shape)
    print(g_stages_train.shape)
    print(image_test.shape)
    print(g_stages_test.shape)
    
    input_shape = image.shape[1:4]
    model = reg_net(input_shape,feature_cnn=args.feature_cnn)
    model.summary()
    
    optimizer = tf.keras.optimizers.Adam(learning_rate=args.lr)

    for epoch in range(1,args.num_epochs+1):
        optimizer = tf.keras.optimizers.Adam(learning_rate=np.power(0.98,epoch)*args.lr)
        num_of_batches = int(num_of_train_images/args.batch_size)
        s = 0
        for idx in range(0,num_of_batches):
            
            batch_idx = np.random.randint(num_of_train_images, size=args.batch_size)
            
            image_batch = imgae_train[batch_idx, :]
            stage_batch = g_stages_train[batch_idx]


            with tf.GradientTape() as tape:
                    
                g_stage_predicted = model(image_batch)
                    
                loss = losses(stage_batch,g_stage_predicted)

            gradients = tape.gradient(loss, model.trainable_variables)
            optimizer.apply_gradients(zip(gradients,model.trainable_variables))
                
                ### sum up training loss
            s = s + loss.numpy()
            
            
        ### compute validationing loss
        loss_validation = 0
        g_stage_validation_predicted_array = np.zeros((num_of_validation_images,1))
        
        for i in range(0,num_of_validation_images):
            
            test_image = image_test[i,:,:,:]
            test_image = test_image.reshape((1,test_image.shape[0],test_image.shape[1],test_image.shape[2]))
            test_stage = g_stages_test[i]
        
            g_stage_validation_predicted = model(test_image)
        
    
            loss_validation = losses(test_stage,g_stage_validation_predicted) + loss_validation
            
            g_stage_validation_predicted_array[i] = g_stage_validation_predicted
            
        loss_validation = loss_validation/num_of_validation_images
        loss_validation = np.sqrt(loss_validation)
        
            
        print("epoch= " + str(epoch) + ",  train loss = " + str(format(np.sqrt(s/num_of_batches), '.3f')) +
        ",   validation loss = " + str(format(loss_validation, '.3f')))
            
        train_losses[epoch-1] = s/num_of_batches
        
        validation_losses[epoch-1] = loss_validation
            

    # save model for each image resolution
    model.save(args.trained_model_dir + args.trained_model_fn + '.h5')
    
    print('done!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
    array = np.empty((args.num_epochs + 10,3), dtype='U25')
    
    array[0,0] = "epoch"
    array[0,1] = "train_loss"
    array[0,2] = "validation_loss"
    
    for j in range(0,args.num_epochs):
        array[1 + j , 0] = str(j+1)
        array[1 + j , 1] = str(train_losses[j])
        array[1 + j , 2] = str(validation_losses[j])
    np.savetxt(args.result_name, array, delimiter=",", fmt='%s')
    

    
    np.savetxt('../trained_models/predicted_224x224_fold5.csv', np.concatenate((g_stages_test,g_stage_validation_predicted_array), axis=1), delimiter=",", fmt='%s')
    
    
if __name__ == '__main__':
    main()
