class dog():
    def __init__(self,name):
        self.dog_name = name

    def bark(self):
        print("My name is {}".format(self.dog_name))


dog("Fiddo").bark()
