# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import sys
import numpy as np
from PIL import Image

bruh = "./plane.png"




a = np.array(Image.open(bruh))


def convert(arr):
    res = np.zeros((len(arr),len(arr[0])),dtype=int)
    for i in range(len(arr)):
        for j in range(len(arr[0])):
            if arr[i][j][3]!=0:
                res[i][j] = int(arr[i][j][0]*65536+arr[i][j][1]*256+arr[i][j][2])
    return res


b = convert(a)


def python_to_ocaml_arr(arr):
    a = np.array2string(arr,separator=";",prefix="[|",suffix="|]",edgeitems=1000)
    a = a.replace(" ","")
    a = a.replace("[","[|")
    a = a.replace("]","|]")
    return a

print(c := python_to_ocaml_arr(b))