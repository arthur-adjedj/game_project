# -*- coding: utf-8 -*-
"""
Small script to convert png files to color matrices used in OCaml
/!\\ The plane and missile icons are not mine and are under licensing, I should probably source them or find 
accessible ones...
"""

import sys
import numpy as np
from PIL import Image

bruh = ["./boost_0.png","./boost_1.png","./boost_2.png"]


def convert(arr):
    res = np.zeros((len(arr),len(arr[0])),dtype=int)
    for i in range(len(arr)):
        for j in range(len(arr[0])):
            if arr[i][j][3] != 0:
                res[i][j] = int(arr[i][j][0]*65536+arr[i][j][1]*256+arr[i][j][2])
            else:
                res[i][j] = -1
    return res


def python_to_ocaml_arr(arr):
    a = np.array2string(arr,separator=";",prefix="[|",suffix="|]",edgeitems=1000)
    a = a.replace(" ","")
    a = a.replace("[","[|")
    a = a.replace("]","|]")
    return a

def f(i):
    return python_to_ocaml_arr(convert(np.array(Image.open(bruh[i]))))
    

print(c := "[|" + f(0) + ";" + f(1) + ";" + f(2) + "|]")