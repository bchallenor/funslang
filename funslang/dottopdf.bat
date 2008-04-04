dot -Tps2 %1.dot > %1.eps
epstopdf %1.eps
del %1.eps