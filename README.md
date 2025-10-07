# Taller-1-Econom-a-Urbana-
En el  marco del curso **Economía Urbana** encontrara el siguiente repertorio con una serie de códigos completamente replicables de dos estudios empíricos que combinan análisis econométrico y herramientas espaciales realizados en **R**.con el objetivo de estimar índices de precios inmobiliarios y analizar la valoración espacial de amenidades urbanas.Teniendo en cuenta que estos códigos se desarrollaron en un entorno académico; se presentan como ejercicios cuyo propósito es extrapolar el aprendizaje de clase a contextos prácticos. 

### Ejercicio 1

- **Ejercicio 1:** Construcción de **índices de precios de vivienda** (Cook County, Illinois) mediante tres metodologías:
  - Modelo **Hedónico**
  - Estimador de **Ventas Repetidas (Repeat Sales)**
  - Modelo de **Efectos Fijos (FE)**
 
  
el primer punto refiriendoce a los codigos utilizados para crear como producto principal dos graficas que presentan de manera sencilla como se comportan los indices, tanto en niveles como en logaritmos. Por otro lado el pundo se refiere al codigo empleado para dar cuenta de la integración de datos espaciales de Bogotá con características inmobiliarias,, al análisis de gradientes de precios respecto al centro urbano, a la estimación de efectos de proximidad a parques y plazas. Y la Exportación de resultados y visualizaciones espaciales. 

Para replicar los codigos del punto 1 se necesita, en primer lugar, tener en la carpeta de su repertorio la base de datos "dataTaller01_PriceIndeces.Rds" y, en segundo, correr el codigo en su consola. Al terminar este proceso, tendra como resultado a su dispocición las graficas y las tablas empleadas en el informe del punto 1.



### Ejercicio 2 - Bogotá a Cielo Abierto — Índices de Precios y Amenidades Verdes

- **Ejercicio 2:** Estimación del efecto de **parques y plazas** como **amenidades públicas** sobre los precios de la vivienda en **Bogotá**, a través de:
  - Gradientes espaciales de precios, renta y densidad
  - Estimadores Hedónicos y de **Diferencias Espaciales (SFD)**
  - Corrección por autocorrelación espacial (**Errores de Conley**)
  - Análisis de la **Disposición a Pagar (WTP)**

El ejercicio integra análisis espacial y econométrico para estimar cómo los espacios abiertos (parques y plazas) influyen en los precios de vivienda en Bogotá. Se combinan datos catastrales, inmobiliarios y de OSM para medir cercanía al CBD y a amenidades, se analizan gradientes urbanos de precios y densidad mediante LOESS, y se estiman modelos hedónicos (OLS-FE), SFD y Conley, encontrando que los precios disminuyen cerca de 3,3 % por cada kilómetro adicional de distancia a un parque.

Asimismo, para la realización del punto 2, es necesario descargar dentro de su carpeta las siguientes carpetas adicionales: Primero, carpeta "Data" con las subcarpetas "Raw" y "Modified". Dentro de esta carpeta deberá estar el archivo "dataTaller01_Amenidades.Rds" junto con bases de datos complementarias necesarias para el análisis. Por último, deberá crear en su directorio la carpeta "Outputs", para que todas las exportaciones de gráficas y tablas del punto 2 terminen dentro de esta carpeta.


