# Язык программирования Ярило

Новый импортозамещающий язык программирования Ярило.

Чтобы скомпилировать Ярило-интерпретатор:
```nim c -d:release yar.nim```

Чтобы запустить Ярило-скрипт:
```./yar путь-к-Ярило-скрипту```

Пример Ярило-скрипта:

```
сумма: пусть изначально 0
число: же изначально 1000000
пока [число поболее чем 0] [
  пусть сумма: будетъ к сумма присовокупить число 
  число: же езъмь число отнять 1
]
ответствуй! сумма
```

## Ограничения

Рекурсия не работает, окромя хвостовой как в factorial.y
