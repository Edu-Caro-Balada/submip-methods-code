\# submip-methods-code



Código en R utilizado en la tesis de Edu Caro Balada "PERIODOS DE INTENSIDAD SUBMÁXIMA EN FÚTBOL PROFESIONAL" para el cálculo de variables y detección de periodos SubMIP (umbral 85%) a partir de archivos raw (GPS).



\## Estructura esperada

\- `data/raw/5/`  

&nbsp; Carpeta con los CSV raw (sin cabecera).

\- `data/reference/MIP FINAL.xlsx`  

&nbsp; Excel con MIPs por jugadora (`Player\_displ` + columnas `MIP\_\*`).



> \*\*Nota:\*\* por privacidad, los datos y el archivo de MIPs no se suben al repositorio.



\## Cómo ejecutar

1\. Coloca los archivos raw en `data/raw/5/`

2\. Coloca el Excel de MIPs en `data/reference/MIP.xlsx`

3\. Ejecuta el script:

&nbsp;  - `src/submip\_methods.R`



Salida:

\- `BBDD\_SubMIP.csv` (en el directorio de trabajo)



\## Reproducibilidad

Se recomienda fijar versiones de paquetes con `renv` (opcional):

```r

install.packages("renv")

renv::init()

renv::snapshot()



