

factorial: func [n] [either [n > 1] [mul factorial n - 1 n] [1]]
print factorial 10
