#lang racket


(define arch(open-input-file "reporte2.txt")) ;abre el archivo creado previamente
(define tmp-autos (read arch)) ;lee la primera lista y la asocia con x
(close-input-port arch) ;cierra el archivo

;hace que el elemento se vuelva 1
(define haz-1
  (lambda(x)1))

;se define un semaforo
(define sem-clasico
  (lambda (lst)
       (apply list(map calcula-tmpo lst))))

;calcula el tiempo para poder cruzar tomando en cuenta si el semaforo esta en rojo o en verde o amarillo
;el ciclo del semaforo es de 39 s
(define (calcula-tmpo tmp)
  (cond
    ((= tmp 0)15)
    ((and(< tmp 16)(> tmp 0))(- 15 tmp))
    ((and(> tmp 15)(< tmp 39))0)
    (else(calcula-tmpo (- tmp 39)))))

;calcula el tiempo en el que un vehiculo esta en la linea
(define tmpo-linea
  (lambda(tmpSem)
    (if (null? tmpSem)
        '()
        (map + (calcula-sem-tot tmpSem) tmpSem))))

;calcula el tiempo total del semaforo dependiendo de la posicion del auto en la linea
(define calcula-sem-tot
  (lambda (tmp)
    (if(null? tmp)
       '()
       (map + (elem tmp)(sem-clasico tmp)))))

;cuneta los elementos dentro de la lista y hace una lista con el lugar que ocupa cada elemento
(define contar-el
  (lambda (lst n)
    (if(null? lst)
       '()
        (append (list(+ 1 n))(contar-el (cdr lst)(+ n 1)))
            )))

;se obtiene una lista con la posicion de los elementos en la lista
(define elem
  (lambda (lst)
    (map - (contar-el lst 0)(map haz-1 lst))))
    
;se obtiene una lista con la posicion de los elementos en la lista que va a servir para
;encontrar el elemnto que se encunetra previo en una segunda lista y as[i poder hacer 
;la comparacion que se hace en compara (elemento de una lista con predecesor de la otra)
(define obten-pos
  (lambda (lst)
    (if(null? lst)
       '()
       (map - (elem lst)(map haz-1 lst)))))

;accesa a la posicion que se indica y busca el elemento que se encuentre en ella
(define(accesa-n n lst)
  (cond
    ((empty? lst)'())
    ((= n 0)(car lst))
    (else(accesa-n (- n 1)(cdr lst)))))

;calcula si el vehiculo tiene delay y de cuanto es este
;compara el elemento de tmpCarro con su predecesor de tmpLinea y si es menor, tiene
(define compara
  (lambda (carro linea pos n)
    (cond ((null? carro) '())
          ((null? pos) '())
          ((or (equal? (car carro) 0) (equal? (length carro) (length linea))) ;se asegura de que el elemento se el primero en la lista
           (cons n (compara (cdr carro) linea (cdr pos) 0)))
          ((equal?(car pos)0) ;se asegura de que la posicion a buscar sea igual a 0
           (cons (+ n (compara-aux (car carro) (accesa-n (car pos) linea)))(compara (cdr carro) linea (cdr pos) 0)))
          ((and (not (null? (car pos)))(> (car pos)0)) ;se asegura de que el primer elemento no corresponda a una lista vacia y sea mayor a 0
          (cons (+ n (compara-aux (car carro) (accesa-n (car pos) linea))
                   (cond ((and(equal? 1 (compara-aux (car carro) (accesa-n (car pos) linea)))(> (car pos)1)) ;se asegura de que el resultado de la primera comparacion sea 1 y la posicion sea mayor a 1
                               (+(+ n (compara-aux (car carro) (accesa-n (- (car pos) 1) linea))) ;se hace una suma entre el resultado de la primera comparacion y una segunda, la cual toma en cuenta la posicion previa del elemento a comparar
                                 (+ n (compara-aux (car carro) (accesa-n (- (car pos) 1) linea)))))
                           ((and(equal? 1 (compara-aux (car carro) (accesa-n (car pos) linea)))(=(car pos)1))
                               (+ n (compara-aux (car carro) (accesa-n (- (car pos) 1) linea))))
                            (else   (+ n 0))))
                (compara (cdr carro) linea (cdr pos) 0)))         
          (else(compara (cdr carro) linea (cdr pos) 0)))))
           
;hace la comparacion
 (define compara-aux
          (lambda (a b)
            (if (<= a b) 1 0)))

;define el tiempo final del semaforo tomando en cuenta el delay
 (define tmpo-sem-fn
   (lambda (tmp)
     (map + (sem-clasico tmp)(compara tmp (tmpo-linea tmp)(obten-pos tmp)0))))

;crea una lista de 5s por elemento en la lista de entrada
;(segundos que tardan en cruzar la calle despues de pasar el semaforo)
(define five
  (lambda (lst)5))

;define el tiempo de llegada tomando en cuenta el tiempo del semaforo y los segundos que tardan en cruzar
(define tmpo-llegada
  (lambda (tmp)
    (map + (map five tmp)(map + (tmpo-sem-fn tmp)tmp))))

;calcula el tiempo total que tuvieron que esperar los autos para poder avanzar
(define tmpo-espera
  (lambda (tmp)
    (map - (tmpo-llegada tmp)tmp)))
                   
 ;obtiene el identificador y manda lo que resta de la lista a obtener el tiempo de espera   
(define (tmpo-aux lst)
      (if(null? lst)
       '()
       (append (list(car lst))(tmpo-espera (cdr lst)))))
             

(define arch2(open-output-file "tiempoEsperaAutos.txt" #:exists 'replace)) ;sobrescribe en un txt si este ya existe 
(write (map tmpo-aux  tmp-autos) arch2)
(close-output-port arch2) ;cierra el archivo

(define arch3(open-input-file "tiempoEsperaAutos.txt")) ;abre el archivo creado previamente
(define tmp-espera (read arch3)) ;lee la primera lista y la asocia con x
(close-input-port arch3) ;cierra el archivo



;saca la cantidad de vehiculos que pasan por semaforo
(define vehiculos
  (lambda (lst)
    (if(null? lst)
       '()
       (append (list(car lst)) ;se obtiene el identificador 
               (list(length (cdr lst))))))) ;evalua la funcion con el resto de los datos

;saca la cantidad total de vehiculos que pasan por el crucero
(define vehiculos-tot
  (lambda (lst)
            (list(apply + (apply append (map cdr(map vehiculos lst)))))))

;para la visualizacion de los datos
(define cantidad-vSem
  (lambda (lst)
  (map (lambda (lst)
         (display "La cantidad total de vehiculos en el semaforo ")
         (display (car lst))
         (display " es de: ")
         (display (cadr(vehiculos lst)))
         (newline))
       lst)))

;para la visualizacion de los datos
(define Vehiculos-sem-crucero
  (lambda (lst)
    (cantidad-vSem lst)
    (newline)
    (display "La cantidad total de vehiculos en el crucero es de: ")
    (display(car(vehiculos-tot lst)))
    (newline)
    (newline)))
              
         


;crea una lista que toma en cuenta solo los tiempos cuando el semaforo esta en verde
;y el primer tiempo de cuando el semaforo se pone en rojo
(define verde
  (lambda (sem carro)
    (cond((null? carro)'())
         ((= 0 (car sem))
          (cons (car carro)
                (if (and(not(null? (cdr sem)))(not(= 0 (cadr sem))))
                    (cons(cadr carro)(verde (cdr sem)(cdr carro)))
                    (verde (cdr sem)(cdr carro)))))
          (else (verde (cdr sem)(cdr carro))))))

;calcula la cantidad de veces que hubo tiempo muerto tomando en cuenta la lista de la funcion verde
(define tmp-muerto
  (lambda (lst n)
    (cond ((null? lst)'())
          ((not(null? (cdr lst)))
             (if(> (- (cadr lst)(car lst))1) ;si la diferencia entre el primer y segundo dato es mayor a1, se toma como vez que hubo tiempo muerto
                (tmp-muerto (cdr lst) (+ n 1))
                (tmp-muerto (cdr lst)  n)))
          (else n))))

;calcula la cantidad de veces que hubo tiempo muerto en un semaforo
(define tmp-muerto-sem
  (lambda (lst)
    (if (null? lst)
        '()
        (append(list(car lst))(list(tmp-muerto (cdr lst)0))))))

;calcula la cantidad de veces que hubo tiempo muerto en un crucero haciendo una suma de todos los elementos
(define tmp-m-crucero
  (lambda(lst)
   (apply + (muerto-crucero-aux (map tmp-muerto-sem lst)))))

;hace una lista con solo los elementos numericos de la lista ingresada
(define muerto-crucero-aux
  (lambda(lst)
    (if (null? lst)
        '()
        (append (list(cadar lst))(muerto-crucero-aux (cdr lst))))))  

;para la visualizacion de los datos
(define veces-m-sem
  (lambda (lst)
    (map (lambda (lst)
           (display "La cantidad total de veces que el semaforo ")
           (display (car lst))
           (display " estuvo en verde y no pasaron autos es de: ")
           (display (cadr(tmp-muerto-sem lst)))
           (newline))
         lst)))

;para la visualizacion de los datos
(define veces-m-crucero
  (lambda (lst)
    (veces-m-sem lst)
    (newline)
    (display "La cantidad total de veces que el crucero estuvo en verde y no pasaron autos es de: ")
    (display(tmp-m-crucero lst))
    (newline)))
                              
  

#|
Tiempo promedio (en segundos) que tardaron los vehículos en pasar el crucero,
específicando el promedio total del crucero, y el promedio por semáforo.
|#
(define prom
  (lambda (lst)
    (if (null? lst)
        '()
        (list (/ (apply + lst)
                  (+ 1(apply + (map haz-1 (cdr lst)))))))))

;prom por semaforo 
(define prom-sem
  (lambda (lst)
    (if (null? lst)
        '()
        (append
         (list (car lst))
               (prom (tmpo-espera (cdr lst)))))))

;prom por crucero 
(define prom-crucero
  (lambda (lst)
    (if(null? lst)
       '()
       (prom (lista lst)))))

;hace una lista simple con solo los promedios de los semaforos 
(define lista
  (lambda (lst)
    (if (null? lst)
        '()
        (map cadr(map prom-sem lst)))))

;para la visualizacion de los datos
(define promedio-sem
  (lambda (tmp-espera)
    (map (lambda (tmp-espera)
           (display "El promedio de tiempo que los autos tardaron en cruzar el semaforo ")
           (display (car tmp-espera))
           (display " es de: ")
           (display (cadr (prom-sem tmp-espera)))
           (display " segs")
           (newline))
         tmp-espera)))

;para la visualizacion de los datos
(define promedio-crucero
  (lambda (tmp-espera)
    (promedio-sem tmp-espera)
    (newline)
    (display "El promedio de tiempo que los autos tardaron en cruzar el crucero es de: ")
    (display (prom-crucero tmp-espera))
    (newline)))

;para la visualizacion final de los datos solicitados
(define muestra
  (lambda(tmp-espera tmp-autos)
    (Vehiculos-sem-crucero tmp-espera)
    (newline)
    (veces-m-crucero tmp-autos)
    (newline)
    (newline)
    (promedio-crucero tmp-espera)
    (newline)
    (newline)))


(define arch4(open-output-file "muestraSalida.txt" #:exists 'replace)) ;sobrescribe en un txt si este ya existe 
(write (muestra tmp-espera tmp-autos) arch4)
(close-output-port arch4) ;cierra el archivo




