def fib(n: int) -> int:
    a = 0
    b = 1
    for _ in range(0, n):
        tmp = a
        a = b
        b = tmp + b

    return a

print(fib(40))
