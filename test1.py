x = 10000;
d = 333333;
def global_variable():
    global x;
    global d;
    x = 10;
    d = 150;
    return 0;
    print(43);;

print(global_variable());
print(x, d);