# 0 Notas ----
#
# Para este ejercicio no modifique el directorio de trabajando usando setwd()
#
# Reiniciar R:
# Session ->  Restart R
# Ctrl + Shift + F10 (Windows),
# Cmd + Shift + 0 / Cmd + Shift + F10 (Mac)

# 1 Rutas absolutas ----
# Modifica el código para importar los datos usando rutas absolutas.
datos_01 <-
  read.csv(
    "C:/Users/alex/Documents/r/etnobotanica-workshop/reproducibilidad/ejercicio/data/raw_data.csv"
  )

# Ver datos
View(datos_01)


# 2 Rutas relativas (directorio de trabajo) ----
# Reiniciar R
# Modificar el código para importar los datos usando rutas relativas
# (No use el paquete `here`)
datos_02 <-
  read.csv(
    "raw_data.csv"
  )

# Contar el número de filas
nrow(datos_02)


# 3 Rutas relativas (directorio del proyecto) ----
# Reiniciar R
# Modificar este código usando el paquete here para importar los datos.
# (Use el paquete `here`)
datos_03 <-
  read.csv(
    "raw_data.csv"
  )

# Ver las primeras seis observaciones
head(datos_03)
