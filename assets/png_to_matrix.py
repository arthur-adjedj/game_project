# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import sys
import numpy as np
from PIL import Image

bruh = ["./star_0.png","./star_1.png"]


def convert(arr):
    res = np.zeros((len(arr),len(arr[0])),dtype=int)
    for i in range(len(arr)):
        for j in range(len(arr[0])):
            if arr[i][j][3] != 0:
                res[i][j] = int(arr[i][j][0]*65536+arr[i][j][1]*256+arr[i][j][2])
    return res


def python_to_ocaml_arr(arr):
    a = np.array2string(arr,separator=";",prefix="[|",suffix="|]",edgeitems=1000)
    a = a.replace(" ","")
    a = a.replace("[","[|")
    a = a.replace("]","|]")
    return a

def f(i):
    return python_to_ocaml_arr(convert(np.array(Image.open(bruh[i]))))
    

print(c := "[|" + f(0) + ";" + f(1) + "|]")