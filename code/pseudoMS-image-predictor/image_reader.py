#### we referenced code from: https://github.com/ignacio-rocco/cnngeometric_pytorch

import tensorflow as tf
import numpy as np
import cv2
import matplotlib.pyplot as plt
import scipy.misc
from skimage import io
import pandas as pd
import os
tf.keras.backend.set_floatx('float32')

### the output of each cost function is a tensor of shape TensorShape([batch_size])
def image_reader(csv_file, image_dir, output_shape = (224,224)):
    data = pd.read_csv(csv_file)
    image_names = data.iloc[:,0] 
    g_stages = data.iloc[:,1]
    weeks_to_dlvrys = data.iloc[:,2]
    classes = data.iloc[:,3]
    
  

    num_of_images = len(image_names)
    image = np.zeros([num_of_images,output_shape[0],output_shape[1],3])

    for idx in range(0,num_of_images):
        image_path = os.path.join(image_dir,image_names[idx])
        
        print(image_path)
        
        image_2d = cv2.imread(image_path)
        image_2d = cv2.resize(image_2d, output_shape)
        image[idx,:,:,:] = image_2d
        image = image.astype('float32')
        g_stages = g_stages.astype('float32')
        weeks_to_dlvrys = weeks_to_dlvrys.astype('float32')
        

        stages_array = np.zeros(g_stages.shape[0])
        for i in range(0,g_stages.shape[0]):
            stages_array[i] = g_stages[i]
            
        delivery_array = np.zeros(weeks_to_dlvrys.shape[0])
        for i in range(0,weeks_to_dlvrys.shape[0]):
            delivery_array[i] = weeks_to_dlvrys[i]
        
        
        classes_array = np.zeros(classes.shape[0])
        for i in range(0,classes.shape[0]):
            classes_array[i] = classes[i]
        
        stages_array = np.reshape(stages_array,(g_stages.shape[0],1))
        delivery_array = np.reshape(delivery_array,(weeks_to_dlvrys.shape[0],1))
        classes_array = np.reshape(classes_array,(classes.shape[0],1))

    
    dataset = {'image': image, 'g_stages': stages_array, 'delivery': delivery_array, 'classes': classes_array }
    
    return dataset
