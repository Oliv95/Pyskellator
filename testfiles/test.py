
x = 5
y = x*5
z = 0
for i in range(11):
    z += i

k = 0

while k + x <= 30:
    if k == 8:
        print("hej")
    elif k < 3:
        k += 1
    else:
        k += 10

print(x)
print(y)
print(z)
