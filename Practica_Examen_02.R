#PRACTICA EXAMEN 2
#YELENA HERNANDEZ B

#Librerias que seguro voy a ocupar
library(readr)
library(readxl)
library(dplyr) 
library(tidyr) 
library(stringr)
library(lubridate)
library(magrittr)

#EJERCICIO 1
#2
clientes <-  readr::read_csv("clientes.csv")
metas <- readr::read_csv("metas_mensuales.csv") 
productos <- readr::read_csv("productos.csv") 
sucursales <- readr::read_csv("sucursales.csv")
ventas <- readr::read_csv("ventas_eventos.csv")

#3
head(clientes)
str(clientes) 
glimpse(clientes)
summary(clientes)
names(clientes)

head(metas)
str(metas) 
glimpse(metas)
summary(metas)
names(metas)

head(productos)
str(productos) 
glimpse(productos)
summary(productos)
names(productos)

head(sucursales)
str(sucursales) 
glimpse(sucursales)
summary(sucursales)
names(sucursales)

head(ventas)
str(ventas) 
glimpse(ventas)
summary(ventas)
names(ventas)

#4, 5, 6, 7
ventas2 <- ventas %>% 
  mutate( total_bruto = unidades * precio_unitario , 
          monto_descuento = total_bruto * descuento,
          total_neto = total_bruto - monto_descuento) %>% 
  select( id_venta, id_cliente, id_producto, 
          id_sucursal, fecha_venta, metodo_pago, 
          unidades, total_bruto, monto_descuento,
          total_neto) %>% 
  filter(total_neto >= 50000, monto_descuento >= 0) %>% 
  arrange(desc(total_neto))

head(ventas2, 15)

#8
ventas2 %>% 
  group_by(metodo_pago) %>% 
  summarise(cantidad_ventas = n(),
            
            unidades_vendidas = sum(unidades),
            
            ingreso_neto_total = sum(total_neto),
            
            ingreso_neto_promedio = mean(total_neto))

#9 
ventas2 %>% 
  group_by(metodo_pago) %>% 
  summarise(id_sucursal = n())

#10
write_csv(ventas2, "ventas2.csv")


#EJERCICIO 2
#1
glimpse(clientes)
glimpse(ventas)
glimpse(sucursales)

ventas <- ventas %>% 
  mutate(fecha_venta = ymd(fecha_venta))

clientes <- clientes %>% 
  mutate(fecha_nacimiento = ymd(fecha_nacimiento),
         fecha_registro = ymd(fecha_nacimiento))

sucursales <- sucursales %>% 
  mutate(fecha_apertura = ymd(fecha_apertura))

#Otra forma, no se si esta bien
ymd(ventas$fecha_venta)
ymd(clientes$fecha_registro)
ymd(clientes$fecha_nacimiento)
ymd(sucursales$fecha_apertura)

#2
anio <- year(ventas$fecha_venta)
mes <- month(ventas$fecha_venta)
dia <- day(ventas$fecha_venta)

#3
fecha_referencia <- as.Date("2026-05-19")

clientes <- clientes %>% 
  
  mutate( edad = year(fecha_referencia) - year(fecha_nacimiento))

#4
ingreso_mensual <- ventas2 %>%
  mutate(mes_venta = floor_date(fecha_venta, "month")) %>%
  group_by(mes_venta) %>%
  summarise(ingreso_neto_mensual = sum(total_neto, na.rm = TRUE),
            .groups = "drop")

#5
metas_largo <- metas %>%
  pivot_longer(
    cols = starts_with("meta_"),
    names_to = "periodo_meta",
    values_to = "meta_mensual"
  ) 

#6
metas_largo <- metas_largo %>%
  separate(periodo_meta, into = c("anio_meta", "mes_meta"), sep = "-") %>% 
  mutate(
    anio_meta = as.integer(anio_meta),
    mes_meta = as.integer(mes_meta),
    fecha_meta = make_date(anio_meta, mes_meta, 1) 
  )

#7
metas_ancho <- metas_largo %>% 
  pivot_wider(
    id_cols = meses,
    names_from = metodo_pago,
    values_from = ingreso_neto_total)

#8
metas2 <- read_csv("metas_mensuales.csv") %>% 
  mutate( meta_Ene_Feb = meta_2026_01 + meta_2026_02,
          meta_Mar_Abr = meta_2026_03 + meta_2026_04) %>% 
  select( sede, categoria_producto, 
          meta_Ene_Feb, meta_Mar_Abr) %>% 
  filter(meta_Ene_Feb >= 100000, meta_Mar_Abr >= 100000 ) %>% 
  arrange(desc(meta_Ene_Feb))

metas2 %>% slice(1:5)

#9
ventas2 %>% 
  filter(total_neto >= 50000) %T>% 
  glimpse() %>% 
  group_by(metodo_pago) %>% 
  summarise(ingreso_total = sum(total_neto),
            cantidad_ventas = n())

#10
ventas2 %$% 
  cor(unidades, total_neto)

#11
ventas2 %<>%
  arrange(fecha_venta) %>%
  mutate(
    clasificacion_venta = case_when(
      total_neto >= 100000 ~ "Alta",
      total_neto >= 70000 ~ "Media",
      TRUE ~ "Baja"))


#EJERCICIO 3
#1
count(productos) %>% 
filter(n > 1)

count(clientes) %>% 
  filter(n > 1)

count(sucursales) %>% 
  filter(n > 1)

#2
#Arroga los valores que esten en ventas que no esten en productos
join1 <- ventas %>% anti_join(productos)

#3
join2 <- ventas %>% anti_join(clientes)

#4
join3 <- ventas %>% anti_join(sucursales)

#5
ventas_completas <- ventas2%>%
  left_join(productos, by = "id_producto") %>%
  left_join(clientes, by = "id_cliente") %>%
  left_join(sucursales, by = "id_sucursal")

#6
ventas_completas %>% 
  summarise(
    na_producto = sum(is.na(producto)),
    na_categoria = sum(is.na(categoria_producto)),
    na_nombre = sum(is.na(nombre)),
    na_apellido = sum(is.na(apellido)),
    na_sede = sum(is.na(sede))
  )
#el left_join devuelve todas las filas que estaban en la tabla de la 
#izquierda y si no hay coincidencia con la tabla de la derecha se rellena
#el espacio con un NA

#7
ventas_validas <- ventas2 %>%
  inner_join(productos, by = "id_producto") %>%
  inner_join(clientes, by = "id_cliente") %>%
  inner_join(sucursales, by = "id_sucursal")

#Usando filter seria algo asi?
ventas_validas <- ventas_completas %>%
  filter(
    !is.na(producto),
    !is.na(nombre),
    !is.na(sede))

#8
ventas_validas <- ventas_validas %>% 
  mutate(mes_venta = floor_date(fecha_venta, "month"))

resumen <- ventas_validas %>% 
  group_by(sede, categoria_producto, mes_venta) %>%
  summarise(
    cantidad_ventas = n(),
    unidades_totales = sum(unidades, na.rm = TRUE),
    ingreso_neto_total = sum(total_neto, na.rm = TRUE),
    .groups = "drop"
  )

#9
join4<-resumen %>% inner_join(metas_largo, 
                              by = c("sede", "categoria_producto")) %>% 
  mutate(diferencia_meta = ingreso_neto_total - meta_mensual,
         porcentaje_cumplimiento = ingreso_neto_total /  meta_mensual)

#10
join5 <- resumen %>% 
  full_join(metas_largo, by = c("sede", "categoria_producto")) 

#11
top_10 <- ventas_validas %>% 
  group_by(producto) %>% 
  summarise(
    ingreso_neto_total = sum(total_neto)
  ) %>% 
  arrange(desc(ingreso_neto_total)) %>% 
slice(1:10)

#12
lista_final <- split(resumen, resumen$sede)
resumen_final <- do.call(rbind, lista_final)

#EJERCICIO 4
#Aprendiendo a usar github
#Crear carpeta de la practica mkdir y cd con el nombre que le quiero poner
#Iniciar repositorio = git init
#Revisar estado del repositorio = git status
#Agregar archivos al repositorio = git add .
#Hacer commit de los cambios = git commit -m "mensaje del commit"
#Crear rama limpieza-datos = git checkout -b limpieza-datos
#Regresar a la rama principal = git checkout main
#Revisar el historial de commits = git log
