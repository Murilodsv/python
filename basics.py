#--- Basic files to get intuition about python sytaxis

#--- Set lists
l = range(10);
l = [1,50]

#--- In order to print or "end" your loop you need to press "enter" when in console mode 
l = []
for i in range(9):
    l.append(i)
    print(l)
    
#--- Alternatively you can run it within exec()
l = []
exec("for i in range(9): l.append(i)")
print(l)
