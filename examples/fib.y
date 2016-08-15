


fact: func [n] [either [n > 1] [n * fact n - 1] [1]]

print fact 5
