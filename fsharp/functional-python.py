# does python have closures?
# does python have first-class functions?

# first class functions in python
def f():
    print("I am f")
g = f
g()

# does python have closures?
def make_increment(n):
    def inc(x):
        return x + n
    return inc

count5 = make_increment(5)
count10 = make_increment(10)
print(count5(0))
print(count5(5))
print(count5(10))
print(count10(0))
print(count10(10))
print(count10(20))


#def puzzle():
    #counter = 0
    #def count():
        #counter += 1
        #print(counter)
    #return count
#
##counter1 = puzzle()
#counter1()
#counter1()
#counter1()
# The above does not work in python, but it does in F#!


# an anonymous function
(lambda x : print(f"Anonymous print of {x}"))(5)
