import tensorflow as tf
import keras.backend as K
import numpy as np
import cv2
import matplotlib.pyplot as plt
import scipy.misc
tf.keras.backend.set_floatx('float32')

### the output of each cost function is a tensor of shape TensorShape([batch_size])
def losses(y_true,y_predicted):
    loss = tf.math.reduce_sum(tf.math.square(y_true - y_predicted))/y_true.shape[0]
    #loss = tf.math.sqrt(loss)
    return loss
