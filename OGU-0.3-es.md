# El lenguaje de programación Ogu

## Introducción

Esta es la versión 0.3.x de Ogú. 
Esta versión se conoce también como Ñeclito.

## Características de Ñeclito

Ñeclito es una nueva versión del lenguaje, cuyo parser fue re escrito en Rust.
 
Esta versión tiene cambios mayores en la sintáxis de Ogú, aproximándolo aún más al paradigma funcional.

A continuación una descripción de lo que se puede hacer con el subconjunto de Ogú implementado por Ñeclito.

# Comentarios

En Ogú los comentarios empiezan con '--' y terminan con el fin de linea

    -- este es un comentario

# Espacios en blanco

En Ogú los espacios en blanco son relevantes en ciertas ocasiones

Por ejemplo, 

    1+1 
    
Es una expresión válida, pero se prefiere que se escriba así:
    
    1 + 1
        
Esto es muy importante, porque:

    a+1 
    
Es un identificador, en cambio

    a + 1
    
Es una expressión.

# Valores 

Los valores se almacenan en variables inmutables:

    c = 300000
    
Ejemplos:

    max_intentos = 10
    intentos-hasta-ahora  = 0 

## Identificadores

Los nombres de los identificacdores son los mismos de Clojure, 
y pueden contener los caracteres  *, +, !, -, _, ', ?, <, > y =.

Ejemplos:

    lost+found = True
    max-intentos = 10
    parametros* = 200
    _es-finalista? = False
    
Siempre se debe colocar su valor inicial usando el operador =.

### Fechas
    
Las fechas se pueden ingresar usando el símbolo #  seguido de la fecha expresada en un subconjunto del formato ISO8601.

    timestamp = #2016-01-19T20:30:01.245
    horazulu = #2016-01-19T16:40:01.s45Z
    horasantiago = #2016-01-19T16:40:01.s45-03:00
    
### Números    
    
Los números se expresan de la manera tradicional, incluyendo notación exponencial.

    pi = 3.14.15
    avogadro = 6.022e23
    bigint = 20N
    bigdec = 20M
     
El sufijo N indica un numero entero con cantidad arbitraria de dígitos.
El sufijo M indica un numero real de precision indefinida.

También se pueden expresar fracciones del siguiente modo:

    tres-cuartos = 3/4 -- notar la necesidad usar espacios

# Valores booleanos en Ogú

Los valores false y true son valores reservados para representar booleanos.

### Expresiones Regulares

Las expresiones regulares se expresan del siguiente modo

    patron = #/abc(.*)/#
    
Van encerradas por los delimitadores #/ /#
Opcionalmente se pueden usar los delimitadores #` "` (esto para poder parsear / dentro de una expresión regular).

    patron = #`abc/(.*)`#

(*) hay un bug en este release y patrón no puede tener la secuencia de escape \/ si usas #/ /# como delimitadores.
    
Se usan las convenciones definidas en Clojure y Java.

Los siguientes operadores se definen

    "abcxyz" =~ patron  -- hace match con todo el string, retorna ["abcxyz", "xyz"]

    "xyzabcxyz" ~ patron -- hace match con la parte del string, retorna ["abcxyz", "xyz"]
    
    "qweqwehkzabcsdada" ~~  #/qwe/# -- retorna todos los matches encontrados, retorna (qwe, qwe)

### Tuplas

En Ogú las tuplas son usadas en varios contextos. 

Por ejemplo, hay funciones que retornan tuplas. 

En ese caso si se quiere rescatar los valores de retorno de la tupla en forma separada se debe usar la siguiente notación:

    (p, q) = frac(0.4) -- x = 4, y = 10

(Acá suponemos que frac(x) retorna un número real como una fracción)


# Variables locales

En una expresión podemos declarar una declaración local de la siguiente manera

    let x = 10
    in x + 10
     
Acá x sólo tiene un valor en la expresión x + 10.

    let a = 1, b = 2 + 2, c = min 2 6 in a * b / c
     
Esta expresión tiene el valor final 2.

# Listas y Vectores

Las listas y vectores, o secuencias en general, se escriben entre corchetes:

    [1, 2, 3]
    
    ["a", "b", "c"]
    
Los rangos corresponden a listas donde se definen los inicios y terminos de una secuencia de números

    [1 .. 100] -- del 1 al 100 inclusive
    [1 ..< 100] -- del 1 al 99
    
Una forma especial de escribir un rango es defininiendo el paso entre los elementos
    
    [3, 6 .. 999] -- 3, 6, 9, 12, ... 999

Los rangos pueden ser infinitos:
    
    [10 ...]
    
Las listas se pueden escribir por comprension:
    
    [x * y | x <- [100 ..< 1000], y <- [100 ..< 1000] ]
    
Si tienes un vector puedes acceder al elemento i-esimo del siguiente modo:

    v = [100, 200, 300]
    
    (v 1) -- 200
    
# Mapas
    
Los mapas se escriben entre `#{}`:
    
    #{"nombre" => "Pedro", "edad" => "15"}
    
       
    
Si tienes un mapa, puedes acceder a sus elementos como una función
    
    mapa = #{"nombre" => "Pedro", "edad" =>  "15"}
    
    edad-de-pedro = mapa "edad"
    
Un mapa vacio se designa así
   
    #{}
    
# Conjuntos
         
Un conjunto se designa asi:
     
     ${elemento1, elemento2}
     
Un conjunto vacio se designa así:

    ${}


# Funciones

Ogú es un lenguaje funcional, con gran influencia de Haskell y Clojure. 

Consideremos la función max, que entrega el máximo entre dos número, en Ogú se puede invocar de la siguiente manera:

    max 10 4

El valor de retorno es 10. 

Otra manera de invocar esta función es:

    (max 10 4)

Como se hace en Lisp, esto es útil en contextos donde puede haber ambigüedad.

Consideremos los siguientes casos

    max 4 5 + 2
    2 + max 4 5
    max 4 + 5 2

¿Cómo interpreta esto el compilador Ñeclito de Ogú?

En el primer caso el resultado es 7. En el segundo caso también es 7. 

En el tercer caso es 9, tal como se puede esperar.

    max 4 5 + 2 -- (max 4 5) + 2 
    2 + max 4 5 -- (2 + (max 4 5))
    max 4 + 5 2 -- (max (4 + 5) 2)
    
La opcion -p del compilador permite ver el AST (Abstract Syntax Tree) que corresponde a S-Expressions en Clojure, con lo que puedes depurar si tienes dudas.

Ante la duda es bueno usar parentesis.

## Invocando funciones con tuplas

Supongamos ahora que existe otra función que llamaremos max’ que en este caso ha sido definida para recibir una tupla de dos elementos (dupla). 

En este caso para invocarla se deben usar paréntesis y comas en su invocación.

    max’ (4, 5)

Porque esta función recibe una dupla y retorna un valor.

Las funciones que hemos visto se declaran en Ogú de la siguiente manera:

    max a b = if a > b then a else b

en cambio la función max’ se declara en Ogú de esta manera

    max' (a, b) = if a > b then a else b

Aunque parecen similares, las dos funciones se evalúan de manera diferente. 

## Aplicaciones parciales

Podemos hacer aplicaciones parciales del siguiente modo:

    from5 = max 5

define una función parcial que retorna 5 o cualquier número mayor que 5.

Con lo anterior tendremos lo siguiente:

    from5 3 -- retorna 5
    from5 8 -- retorna 8

En Ogú se puede usar aplicaciones igual que en Clojure. No hay soporte de Currying.

Ejemplos:

    multiplicar x y = x * y
    doblar  = multiplicar 2
    diez  = doblar 5
    doce = doblar 6

El primer caso define una función multiply, que recibe dos números.

En segundo caso define una función que retorna otra función que multiplica por dos sus argumentos.

De este modo diez y doce son funciones que retornan el mismo valor (el compilador debería optimizar esto a valores fijos).

## Valores 

Notar que en la seciónn anterior hicimos lo siguiente

    doblar  = multiplicar 2
    
Acá doblar es una función parcial.
    
En Ogú las funciones son objetos de primera clase, es decir, las funciones pueden ser pasadas como argumentos a otras funciones:

    my-apply f x = f x
    
    my-apply upper "hola" -- "HOLA"
    
## Declaración de funciones

La forma de declarar una función es la siguiente

    nombre-de-la-funcion args = expresión

Ejemplos:


    factorial n = if n == 0 then 1 else n * factorial (n - 1)

Esto es similar a Haskell o a Elm.

El parámetro puede ser una tupla como en este ejemplo:

    min' (a, b) = if a < b then a else b

Por supuesto el valor de retorno puede también ser una tupla:

    swap' (a, b) = (b,a)


El uso de tuplas  permite hacer cosas interesantes como lo siguiente:

    sumar-vectores (a, b) (c, d) = (a + c, b + d)
    
    sumar-vectores (10, 10) (20, 20) -- produce (30,30)

Por supuesto lo habitual es declarar las funciones de este modo:

    sumar a b = a + b -- recordar los espacios
    
Con esto la función sumar se puede invocar:

    sumar 10 20
    
    sumar 1.0 2.0 
    
    
# Tipos de las  funciones

Una función define sus tipos de la siguiente manera:

    sumar : Int -> Int -> Int
    sumar a b = a + b
    
De este modo ocurre lo siguiente

    sumar 10 20
    
    sumar 1.0 2.0 -- produce un error 
    

Si no se especifica el tipo el compilador infiere lo siguiente:

    sumar a b = a + b
    
    sumar : Number -> Number -> Number
    
## Pattern Matching de Funciones

Esta es una característica tomada de Haskell, que permite definir funciones de manera bastante conveniente:

    factorial 0 = 1
    factorial 1 = 1
    factorial n = n * factorial (n - 1)


Otro ejemplo:

    radioAlfa ‘a’ = “Alfa”
    radioAlfa ‘b’ = “Bravo”
    radioAlfa ‘c’ = “Charlie”
    radioAlfa ‘d’ = “Delta”

En este caso estamos definiendo una función que retorna un string por cada carácter usando el alfabeto radiofónico.

Otros ejemplos:
    
    first (a, _, _) = a

    second (_,b,_) = b

el símbolo _ indica que no nos interesa el valor. 

En estos dos ejemplos hemos creado funciones para obtener elementos de una 3-tupla.


## Funciones con listas 

Veamos algunos ejemplos:

    head’ [] = error! “Lista vacía”
    head’ (x :: _) = x

    length’ [] = 0
    length’ (x :: xs) = 1 + length’ xs


## Guardias

A veces una función se puede expresar mejor en base a varias condiciones que deben cumplirse.

Por ejemplo, supongamos que queremos una función que nos clasifique según nuestro indice de masa corporal (imc).

    strIMC imc
      | imc <= 18.5 = "estas bajo el peso normal"
      | imc <= 25.0 = "tu peso es normal"
      | imc <= 30.0 = "estas con sobrepeso"
      | otherwise   = "estas obeso, cuidado!"

A diferencia del pattern matching, que sólo permite valores o formas de una expresión, los guardias permiten expresiones booleanas.
En este caso los guardias se separan por una barra vertical | y están antes del cuerpo de la función.

Otro ejemplo, en este caso calculamos el IMC en base a la estatura y el peso.

    strIMC’ peso altura 
        | peso / altura ^ 2 <= 18.5 = “estas bajo el peso normal”
        | peso / altura ^ 2 <= 25.0 = “tu peso es normal”
        | peso / altura ^ 2 <= 30.0 = “estas con sobrepeso”
        | otherwise = “estas obeso, cuidado!”


## **where** 

La función anterior calcula una y otra vez el IMC. Podemos simplificar esto usando  **where** :

    strIMC’ peso altura 
        | imc <= 18.5 = “estas bajo el peso normal”
        | imc <= 25.0 = “tu peso es normal”
        | imc <= 30.0 = “estas con sobrepeso”
        | otherwise = “estas obeso, cuidado!”
        where imc = peso / altura ^ 2

Si queremos documentar un poco más esta función podemos hacer lo siguiente

    strIMC’ peso altura 
        | imc <= delgado = “estas bajo el peso normal”
        | imc <= normal = “tu peso es normal”
        | imc <= gordo = “estas con sobrepeso”
        | otherwise = “estas obeso, cuidado!”
        where 
          imc = peso / altura ^ 2 
          delgado = 18.5
          normal = 25.0
          gordo = 30.0

Una forma más compacta es:

    strIMC''' peso altura
      | imc <= delgado = "estas bajo el peso normal"
      | imc <= normal = "tu peso es normal"
      | imc <= gordo = "estas con sobrepeso"
      | otherwise = "estas obeso, cuidado!"
      where imc = peso / altura ^ 2
            (delgado, normal, gordo) = (18.5, 25.0, 30.0)

La cláusula **where**  después del cuerpo de una función permite definir variables o funciones. 

Notar que se deben indentar tanto los guards como las declaraciones en el where.

Veamos otro ejemplo:

    
    calcIMCs lista = [imc p a | (p, a) <- lista]
       where imc peso altura = peso / altura ^ 2

Esta función recibe una lista de duplas con pesos y alturas retornando una lista de los indices de masa corporal respectivos.

(Notar que se parece mucho a Haskell)

La notación [imc p a | (p, a) <- xs] indica que se debe armar una lista por comprensión, donde cada elemento de la lista corresponde la aplicación de
la función imc para cada parámetro p y a, donde p y a son los elementos de la dupla en xs. 

El operador <- toma cada uno de los elementos de la lista. 

## Cuerpo de la función

El cuerpo de una función es una expresión.


## Funciones con efectos colaterales

Cuando el nombre de una función termina en ! significa que tiene un efecto colateral.

Ejemplo:

    hello! name = println! "hello " name
   
La función println! genera un efecto lateral y por lo tanto debe ser marcada con ! al final.

Si una función usa otra función con efectos laterales debe terminar en !
    
El tipo de una función de este tipo es unit que se expresa en Ogú así: ()

    hello! : String -> ()
    
Si quisieramos retornar el valor del nombre el tipo sería así:

    hello! : String -> IO String
    
    hello! name = do 
      println! "hello" name
      name
    
El bloque `do` permite agrupar varias expresiones y se usa normalmente en el contexto de una función con efectos laterales.

El bloque do debe ir seguido de una o más expresiones indentadas.

La expresión final es el valor del bloque `Do`

Se pueden encadenar varias expresiones usando `;`:

    hello! name = 
        println! "hello " ++ name;
        name
        
Y así no usar el `do`

# Recursividad

Para implementar recursion podemos invocar la función directamente o usar `recur`:


    siracusa n
        | n == 1 = 4
        | n == 2 = 1
        | n % 2 == 0 = siracusa (n / 2)
        | otherwise = siracusa (n * 3 + 1)
        
Esto es equivalente:
                
    siracusa n
       | n == 1 = 4
       | n == 2 = 1
       | n % 2 == 0 = recur (n / 2)
       | otherwise = recur (n * 3 + 1)


Existe una construcción **similar** a la implementada en Clojure para implementar loops:

    rev num =
      for reversed = 0, n = num 
      loop  
        if zero? n then
          reversed
       else
         recur let reversed = reversed * 10 + n % 10, let  n = n // 10

(*) El operador // es la división entera
Esto en realidad implementa una función recursiva, así que no hay side effects y las variables siguen
siendo inmutables.

Las variables se pueden omitir en el recur:

    rev num =
      for reversed = 0, n = num 
      loop  
        if zero? n then
          reversed
        else
          recur reversed * 10 + n % 10,  n // 10

Esto es lo mismo que:

    rev num = reverse 0 n
      where
         reverse reversed n = 
            if zero? n then 
              n
            else 
              reverse (reversed * 10 + n % 10) (n // 10)
        

Loop inicializa las variables, cuando invocas recur haces una llamada recursiva al loop con nuevos valores para las variables.

Hay varias diferencias con el loop de Clojure:

1. se inicializan las variables con for
2. puedes nombrar a las variables nuevamente en el `recur`, pero puedes capturar su valor temporalmente:
    
    calculo n = 
      for i = 1, salida = 0 loop
        if i == n then salida
        else repeat let i' = inc i, let salida = i' * 2
        
    calculo 10
    -- resultado es 20, si usaramos i en vez de i' el resultado seria 18

Otro ejemplo, la función minmax, que retorna una dupla con los valores máximos y mínimos de una lista.

    minmax : List Int -> Option (Int, Int)
    
    minmax [] = None
    
    minmax list =
        for
            cmin = head l
            cmax = head l
            x = head l
            xs = tail l
        loop
            if empty xs then 
              Some (cmin, cmax)
           else
              recur
                cmin = min cmin x
                cmax = max cmax x
                x = head xs
                xs = tail xs

(*) Notar que acá usamos un tipo `Option` que sería similar al tipo `Option` de Scala. 

(**) No se debe asumir que el tipo Option es parte de la biblioteca estándar e Ogú, esto se muestra sólo para efectos demostrativos.
Al momento de escribir esto no está definida la biblioteca estándar de Ogú.
        
Otra forma de escribirla es así:

        minmax : List Int -> Option (Int, Int)
        
        minmax [] = None
        
        minmax list =
            for
                cmin = head l
                cmax = head l
                x = head l
                xs = tail l
            while not (empty xs)
            loop
            recur
                cmin = min cmin x
                cmax = max cmax x
                x = head xs
                xs = tail xs
            return
                Some (cmin, cmax)
        
o así:

         minmax list =
            for
                cmin = head l
                cmax = head l
                x = head l
                xs = tail l
            until empty xs
            loop
                cmin = min cmin x
                cmax = max cmax x
                x = head xs
                xs = tail xs
            return
                Some (cmin, cmax)
                
                
Si se omite return el valor que retorna la expresión loop es ()

             bad-minmax! list =
                for
                    cmin = head l
                    cmax = head l
                    x = head l
                    xs = tail l
                until empty xs
                loop
                    cmin = min cmin x
                    cmax = max cmax x
                    x = head xs
                    xs = tail xs
                
          
Estas construcciones dan la sensación de estar usando un lenguaje imperativo.
    
        
# Tipos Algebraicos

Ogú soporta tipos algebraicos. Se usa una notación similar a Haskell.

Ejemplo:

    -- algebraic data types
    
    type Tree = Empty | Leaf value | Node left right
    
    tmax Node left right = 
      let ml = tmax left
          mr = tmax right
      in 
        if ml > mr then ml else mr
    
    tmax Leaf value = value
    
    tree = (Node (Leaf 10)  
                (Node (Node (Leaf4)  
                            (Node (Leaf 10) (Leaf 32))
                       )
                       (Node (Leaf 80) (Leaf 50))
                 )
          )
    
    tmax tree
    
    
(*) debido al uso de identación en Ogú usamos el paréntsis para construir la variable `tree`, sin esto
tendríamos extraños errores sobre indentación.    
 
# clases

Además de los tipos algebraicos en  Ogú hay clases.

Una clase se define así:

    type Circle =  Circle (x: Int, y: Int, radius: Int)
    
    type Rectanglle = Rectangle( x: Int,  y: Int, width: Int, height:Int)
      

Un record se define así:

    type Car = Car {company: String, model: String, year: String}

La diferencia son las llaves. 

Se usan de la siguiente manera:


    mustang56 = Car {company = "Ford", model = "Mustang", year = 1956}
    
    cir = Circle(10, 10, 10)

Los records son útiles para modelar entidades del dominio del negocio.
Las clases son usadas de manera preferente para implementar tipos de datos abstractos.

Los campos de un record o de una clase se acceden con la notación '.campo' como en otros lenguajes:

    mustang.company -- "Ford"

Tambien se pueden invocar como funciones aplicadas sobre la instancia:

    .company mustang -- "Ford"
   
    
# Traits 

Un trait es como los protocolos de Clojure.

    
    trait Shape where

        area : Shape -> Int
        
    trait Vehicle where

        move! : Vehicle -> ()    

Los traits definen listas de funciones que son soportadas por el trait.

Una clase o un record pueden implementar un trait 

    type Circle = Circle (x: Int, y : Int, radius : Int)
    extends Shape where
          area self = pi * (self.radius ^ 2)

    type Car = Car {company, model, year}
    extends Vehicle where
         move! this = println! "moving car " this.company this.model this.year
         

         
El primer argumento de un metodo trait puede llamarse como quiera el programador, por convencion se le llama self o this.
         
Una vez que tenemos definido un trait podemos extender un tipo que ya existe del siguiente modo:

    extends Rectangle as Shape where
            
        area self = (.width self) * (.height self)



