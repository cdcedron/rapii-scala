
###  Scala


## Instalación

Instalar Java 8

Instalar SBT:

* Comprobar que SBT no está efectivamente instalado.

* Descargarlo de la [página web](https://www.scala-sbt.org/download.html).

* Descomprimirlo en algún directorio.

* Agregar ese directorio en la variable PATH. Para que los cambio sean permanente, lo mejor es agregar esta línea en el archivo `~/.bashrc`

export PATH=$PATH:~/PATH_TO_SBT/bin/

y cerrar y abrir la consola o ejecutar

    source ~/.bashrc

## ¿Cómo correr el lab?

Para ejecutar el laboratorio, lo primero que deben hacer es comprobar que SBT funcione.

    cd rapy
    sbt
    sbt:rapy> run

Para salir, CTRL+C. Tengan en cuenta que el comando run está también compilando su proyecto. Además, la primera vez que ejecuten esto, si sbt no está en la versión definida por el archivo de configuración se descargará la versión nueva y puede llevar un tiempo. Además de ello, al ejecutar descargará las dependencias necesarias para el proyecto.

Si realizan un cambio en el código, SBT no recompila automáticamente.

# ¿Cómo testear la implementación?

La primera forma de comunicarse con la API es por consola usando el comando CURL.

En su primera versión, sólo tenemos los endpoints correspondientes a las direcciones, para requests de tipo GET y POST. Para las consultas GET con este *endpoint*, se puede usar el navegador pegando la URL en la barra de navegación. Otra herramienta por consola muy útil es `curl`:

    curl -X GET http://localhost:4000/api/locations/

Deberían ver la salida por pantalla, siempre y cuando el servidor esté corriendo. Al principio les dará error hasta que implementen los modelos.

Si el *request* GET tiene argumentos, se pasan en la misma URL.

    curl -X GET http://localhost:4000/api/orders/detail/2

Por otro lado, si el request es un POST o un PUT ya no se puede utilizar el navegador. Con curl deben usar:

    curl -d '{"name":"Centro", "coordX":0, "coordY": 0}' -H "Content-Type: application/json" -X POST http://localhost:4000/api/locations

También pueden poner el contenido del json dentro de un archivo y pasarlo con la opción -d "@file.json". Más info en muuuuchos lugares de internet!

Otra forma de testear la implementación es usando [Postman](https://www.getpostman.com/) o herramientas similares, que les permiten hacer consultas al localhost usando métodos POST.

