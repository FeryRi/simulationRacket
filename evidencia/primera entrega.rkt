#lang racket
;registro: (nombre-semaforo (luz1 (segs1) (#autos1) (#peatones1)) (luz2 (segs2) (#autos2) (#peatones2))...(luz5 (segs5)(#autos5)(#peatones5))     
(define BDcrucero '((sem1 (verde (20)(15)())(amarillo(3)()())(rojo(45)()(10))()()) ;semaforo de 3 luces
                     (sem2 (verde (20)(15)())(amarillo(3)()())(rojo(45)()(10))()())
                     (sem3 (rojo (20)()(15))(verde(45)(20)())(amarillo(3)()())()())
                     (sem4 (rojo (20)()(15))(verde(45)(20)())(amarillo(3)()())()())
                     (sem5 (rojo (50)()(35))(verde(15)(10)())(amarillo(3)()())()())
                     (sem6 (rojo (20)()(15))(verde(45)(20)())(felcha (10)(8)())(amarillo(3)()())()) ;semaforo con vuelta de 4 luces
                     (sem7 (verde (15)(9)())(amarillo(3)()())(rojo(45)()(10))()())
                     (sem8 (peatonal (40)()(15))(rojo(25)(20)())()()()) ;semaforo peatonal de 2 luces
                     (sem9 (verde (15)(9)())(flecha (15)(9)())(amarillo(3)()())(rojo(45)()(10))()()(peatonal(40)()(13)))));semaforo con las 5 luces




#|
Implementa una función en Scheme que simule el comportamiento de un semáforo para un carril de tráfico con el ciclo clásico: rojo -> verde -> amarillo
donde la duración del rojo es de 15 segundos,el verde 20 segundos yel amarillo 4 segundos.

La función recibirá como entrada una lista que contiene los tiempos de llegada de vehículos,
y generará como resultado una lista con la cantidad de tiempo que tardó cada vehículo en pasar.
Como prueba, utiliza la siguiente lista: (0 3 8 12 17 21 29 30 31 32 45 48 53 55 63 66 67 77 80 85 86 88 94 99 100 120 121 130 150 180) 
Adicionalmente, deberás implementar una función que usando el resultado anterior obtenga el tiempo promedio que tardó cada vehículo en pasar.
|#

;caso mas pequenio: lista vacia -> ()
;suponer que se tiene quien calcule el tiempo que tarda un vehiculo en pasar
;solucion: calcular el tiempo que tarda un vehiculo en pasar y agregarlo al final de una lista nueva

(define (sem-clasico lst)
  (cond ((null? lst)'())
        (else (cons (calcula-tmpo (car lst))
                    (sem-clasico (cdr lst))))))

(define (calcula-tmpo tmp)
  (cond
    ((= tmp 0)15)
    ((and(< tmp 16)(> tmp 0))(- 15 tmp))
    ((and(> tmp 15)(< tmp 39))0)
    (else(calcula-tmpo (- tmp 39)))))



;caso mas pequenio: esta vacia
;suponer que se tiene la longitud de la lista y tenemos quien sume cada elemento de la lista
;solucion: sumar cada elemento de la lista y dividirlo entre su longitud
(define(suma lst)
  (if (null? lst)
      0
      (+(car lst)(suma (cdr lst)))))

(define(average lst)
  (if (null? lst)
      0
  (/(suma lst)(length lst))))