
cond_l = 1 < 2 
cond_g = 3 > 2
cond_le = 3 <= 4
cond_ge = 8 >= 7
cond_eq = 4 == 4

l = [1,2,3,4]

x = 0
while x < 10:
    x += 1
else:
    x += 2
print(x)

y = 0
for a,b in zip(range(10),range(10)):
    y |= a*b
else:
    y -= 10
print(y)

def hello():
    print("hello!")
    return 8

class dog():
    def __init__(self,name):
        self.name = name

z = 0
if cond_l:
    z += 1

if not cond_l:
    z += 1
elif False and True:
    pass
else:
    z += 2

if not cond_g:
    z += 1
elif cond_eq:
    z ^= 10
print(z)

k = 0
try:
    a = int("hej")
except:
    k = 1
print(k)

a = 0
try:
    a = int("8")
except:
    pass
else:
    a += 1

print(a)

b = 0
try:
    a = int("hej")
except:
    pass
finally:
    print(b)

try:
    raise Exception("Exception!")
except Exception as e:
    print(e)

print(hello())

for x in range(len(l)):
    print(l[x])
    print(l[x:len(l)])

t = (1,2,3)

def gen():
    for e in t:
        yield e

comp = [a for a in t]
print(comp)

dic  = {a:a for a in t}
print(dic)
