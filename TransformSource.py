
import numpy as np
import scipy.ndimage as spnd
import cv2
import os

#system paths
image_path = 'c:/Users/RandyCocks/Desktop/R_Projects/sourceImages/'
transformed = 'c:/Users/RandyCocks/Desktop/R_Projects/TransformedImages/'



#mean affine matrix
affine = np.float32([[0.6102760448,0.0008837023,0.0000000000,-15.6382177708],
         [-0.0006794866,0.6121305464,0.0000000000,5.8742046245],
          [0.0000000000,0.0000000000,1.0000000000,0.0000000000],
          [0.0000000000,0.0000000000,0.0000000000,1.0000000000]])

#automate reading and transforming the images

for filename in os.listdir(image_path):
    if filename.endswith('.jpg'):
        img = cv2.imread(image_path + filename)
        #transpose image
        img = [[img[j][i] for j in range(len(img))] for i in range(len(img[0]))]
        newimg = spnd.affine_transform(img,affine)
        cv2.imwrite(transformed +'Transformed'+filename,newimg)

