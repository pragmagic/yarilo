

factorial: func [n] [either [n > 1] [n * factorial n - 1] [1]]
print factorial 10
