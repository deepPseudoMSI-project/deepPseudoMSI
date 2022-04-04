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
import glob

### import self-defined functions
from model import *
from image_reader import *
from loss import *

tf.keras.backend.set_floatx('float32')


def build_parser():
    parser = ArgumentParser()
    
    # Paths
    parser.add_argument('--image-dir', type=str, default='../validation200_224_224/', help='path to foler of training images')
    parser.add_argument('--trained-model-dir', type=str, default='../trained_models/', help='path to trained models folder')
    parser.add_argument('--trained-model-fn', type=str, default='model_224x224_fold5', help='trained model filename')
    parser.add_argument('--result-dir', type=str, default='../results/', help='directory to store registration results')
    # Optimization parameters 
    parser.add_argument('--gpu-id', type=int, default=0, help='which GPU to use')
    # Model parameters
    parser.add_argument('--feature-cnn', type=str, default='vgg16', help='Feature extraction network: vgg16/resnet101')
    parser.add_argument('--image-size', type=int, default=224, help='size of image used for training and testing')
    
    return parser
   
   
def main():

    parser = build_parser()
    args = parser.parse_args()
    
    devices = tf.config.experimental.list_physical_devices('GPU')
    print(devices)
    for device in devices:
        tf.config.experimental.set_memory_growth(device, True)
    tf.config.experimental.set_visible_devices(devices[args.gpu_id], 'GPU')
    
    image_names = glob.glob(args.image_dir + '*.png')
    num_of_test_images = len(image_names)
    
    model = tf.keras.models.load_model(args.trained_model_dir + args.trained_model_fn + '.h5')
        
    g_stage_test_predicted_array = np.zeros((num_of_test_images,1))
        
    for i in range(0,num_of_test_images):
        test_image = cv2.imread(image_names[i])
        test_image = test_image.astype('float32')
        test_image = test_image.reshape((1,test_image.shape[0],test_image.shape[1],test_image.shape[2]))
        g_stage_test_predicted_array[i] = model(test_image)

    print('done!')
    
    image_names = np.array(image_names)
    image_names = image_names.reshape((image_names.shape[0],1))
    
    np.savetxt(args.result_dir + 'external_validation_dataset2_fold5.csv', np.concatenate((image_names,g_stage_test_predicted_array),axis=1), delimiter="," ,fmt='%s')
    
if __name__ == '__main__':
    main()
