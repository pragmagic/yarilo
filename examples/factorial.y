

factorial: func [n] [either [gt n 1] [mul factorial sub n 1 n] [1]]

print factorial 5


