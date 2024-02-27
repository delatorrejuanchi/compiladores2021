Para poner a prueba a el compilador utilizaremos una técnica denominada
*Test de Caja Negra*. Es decir, trataremos al compilador como una función
de la cual no conocemos su implementación, y por lo que, lo trataremos
como una función `comp` que a dado un código fuente `A` nos retorna un
binario `comp = BA` como resultado. Ejecutaremos el binario resultante,
`BA`, y analizaremos el resultado. Si el resultado es el que esperamos,
el compilador ha *superado* esa prueba, y sino lo ha *fallado*.

Utilizaremos una libraría del compilador
[GHC](https://www.haskell.org/ghc/) para realizar tipo de pruebas
llamada [Haskell Test Framework
(HTF)](https://github.com/skogsbaer/HTF). En nuestro caso, utilizaremos
un fragmento de la librería donde haremos pruebas de caja negra de
código de fuente que se compilan y se ejecutan de manera unitaria.

Debido a que el compilador tiene varios modos de compilación definiremos
un archivo indicando como se ejecutan las pruebas, aunque las pruebas
**son las mismas para todos los modos**, y además, **todos los modos
esbozan el mismo comportamiento**.

Para sumar nuestra bateria de pruebas al proyecto de
[stack](https://docs.haskellstack.org/en/stable/README/) lo que haremos
es definir un nuevo modulo indicando cual será el archivo de cabecera
que se ejecutará para cada uno de los modos del compilador.

Definimos Test Suites:
- `Run`: para pruebas sobre el modo de ejecución original.
- `RunOptimized`: para pruebas sobre el modo de ejecución original con optimizaciones.
- `RunCEK`: para pruebas sobre el modo de ejecución usando la máquina CEK.
- `RunCEKOptimized`: para pruebas sobre el modo de ejecución usando la máquina CEK con optimizaciones.
- `Bytecode`: para pruebas sobre el modo de ejecución usando bytecode.
- `BytecodeOptimized`: para pruebas sobre el modo de ejecución usando bytecode con optimizaciones.
- `C`: para pruebas sobre el modo de ejecución mediante compilación a C.
- `COptimized`: para pruebas sobre el modo de ejecución mediante compilación a C con optimizaciones.

Los archivos que definen las baterías de prueba estarán ubicados en la carpeta `test`,
mientras que los casos de prueba estarán en `test/correctos/`.
Esto nos permite lanzar las diferentes baterías de prueba desde la terminal de
la siguiente forma:
``` bash
$ stack test :NombreDelTestSuite
```

NOTA: en algunos entornos, es posible que deban pasarse las opciones "--ghc-options -threaded" al ejecutar los tests.

Para listar todo las posibles baterías de prueba podemos utilizar el comando:
``` bash
$ stack ide targets
```

Donde los casos de prueba serán los que figuren como `compiladores:test:...`.

Dentro de cada archivo de prueba encontrarán como se define la ejecución de las pruebas.
Para esto se utiliza una función que provee la catedra definida en el archivo [file:Spec.hs](Spec.hs) :

``` haskell
runTestWith :: String -> IO ()
runTestWith script = do
  bbts <- blackBoxTests "test/correctos" script ".fd4" bbTArg
  htfMain ([makeTestSuite "bbts" bbts])
  where
    bbTArg = defaultBBTArgs
      { bbtArgs_stdoutSuffix = ".fd4.out"
      , bbtArgs_stderrSuffix = ".fd4.err"
      }
```

Donde lo que hace es simplemente ejecutar un script `S` cuya dirección
se la entrega como argumento a la función `runTestWith`. La función
`runTestWith` lo que hará es ejecutar el script `S` en cada uno de los
archivos terminados en `.fd4` que se encuentren en la carpeta
`test/correctos` y comparará el resultado de la salida estandar con los
archivos con mismo nombre pero terminados en `.fd4.out` y `.fd4.err`.

Se recomienda fuertemente incluir nuevos casos y en lo posible incluir
casos que sean erroneos, casos que se sepa que deban fallar.
