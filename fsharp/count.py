def count(n, limit):
    if n <= limit:
        print(n)
        count(n+1, limit)

count(1, 30000)