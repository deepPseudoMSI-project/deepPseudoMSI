import tensorflow as tf
import tensorflow.keras.layers as KL
from tensorflow.keras.layers import *
from tensorflow.keras.models import Model, load_model
from tensorflow.keras import regularizers
tf.keras.backend.set_floatx('float32')
 
def reg_net(input_shape, feature_cnn='vgg16'):
    x_in = Input(input_shape)
    
    if feature_cnn == 'vgg16' :
        model = tf.keras.Sequential()
        vgg16 = tf.keras.applications.VGG16(include_top=False, weights = 'imagenet', input_shape = input_shape)
            ### cropped at forth pooling layer, replace maximum pooling with global average pooling
        for i in range(0,13):
            vgg16.layers[i].trainable = False
            model.add(vgg16.layers[i])
        for i in range(13,14):
            model.add(vgg16.get_layer(index=i))
            #model.add(Conv2D(filters=512, kernel_size=(3,3), padding="same", activation="relu", kernel_regularizer=regularizers.l2(0.001)))
            model.add(tf.keras.layers.Dropout(0.5))
        model.add(tf.keras.layers.GlobalAveragePooling2D())
        
        x = model(x_in)
        
    size = x.shape[1]
    factor = (size)** (1. / 3)
        
    fcl_model =  tf.keras.Sequential()
    fcl_model.add(Dense(int(size/factor), input_shape=(size,), activation=tf.nn.relu,kernel_regularizer=regularizers.l2(1)))
    fcl_model.add(Dense(int(size/(factor*factor)), activation=tf.nn.relu, kernel_regularizer=regularizers.l2(1)))
    #fcl_model.add(Dense(int(size/factor), input_shape=(size,), activation=tf.nn.relu))
    #fcl_model.add(Dense(int(size/(factor*factor)), activation=tf.nn.relu))
    fcl_model.add(Dense(1))    
    
    y_out = fcl_model(x)
  
    
    return Model(inputs = x_in, outputs = y_out)
        
    
