doubleMe x = x + x

doubleUs x y z = doubleMe x + doubleMe y + doubleMe z

doubleSmallNumber x = 
    if x > 100
    then x
    else doubleMe x