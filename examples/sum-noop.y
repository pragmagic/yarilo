sum: 0
n: 1000000

while [gt n 0] [
  sum: add sum n
  n: sub n 1
]

print sum