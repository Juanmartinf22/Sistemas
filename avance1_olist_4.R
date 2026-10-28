# ============================================================
# Pregunta: ¿Cuánto empeora la calificación cuando el pedido llega tarde?
# ============================================================

library(tidyverse)  

# ---- 1. CARGAR --------------------------------------------------
orders      <- read_csv("olist_orders_dataset.csv")
reviews     <- read_csv("olist_order_reviews_dataset.csv")
order_items <- read_csv("olist_order_items_dataset.csv")

# ---- 2. UNIR ----------------------------------------------------
# Algunas órdenes tienen más de una reseña: nos quedamos con una por orden
# para no contar la misma orden dos veces.
reviews_unicas <- reviews %>%
  distinct(order_id, .keep_all = TRUE) %>%
  select(order_id, review_score)

cat("Reseñas repetidas descartadas:", nrow(reviews) - nrow(reviews_unicas), "\n")

# Solo órdenes entregadas, con fecha de entrega y con reseña.
# Trabajamos a nivel ORDEN (una fila = una orden), porque la reseña
# se hace sobre la orden, no sobre cada ítem.
entregadas <- orders %>%
  filter(order_status == "delivered",
         !is.na(order_delivered_customer_date))

df <- entregadas %>%
  inner_join(reviews_unicas, by = "order_id")

cat("Órdenes en el dataset:", nrow(orders), "\n")
cat("Entregadas con fecha:", nrow(entregadas), "\n")
cat("Entregadas con fecha y con reseña (base del análisis):", nrow(df), "\n")

# ---- 3. CONSTRUIR VARIABLES --------------------------------------
# Comparamos solo el DÍA (as.Date) porque la fecha prometida viene
# sin hora (00:00:00). Si comparáramos con hora, un pedido entregado
# el mismo día prometido figuraría como "tarde".
df <- df %>%
  mutate(
    dias_atraso  = as.numeric(as.Date(order_delivered_customer_date) -
                              as.Date(order_estimated_delivery_date)),
    tiempo_total = as.numeric(as.Date(order_delivered_customer_date) -
                              as.Date(order_purchase_timestamp)),
    llego_tarde  = dias_atraso > 0
  )

cat("Entregadas el mismo día prometido:", sum(df$dias_atraso == 0), "\n")

# ---- 4. RESULTADO 1: a tiempo vs. tarde -------------------------
# Puntaje promedio y cantidad de órdenes según si llegó tarde o no
resumen <- df %>%
  group_by(llego_tarde) %>%
  summarise(ordenes = n(),
            puntaje_promedio = mean(review_score))
print(resumen)

# Porcentaje de cada puntaje (1 a 5) dentro de cada grupo
distribucion <- df %>%
  count(llego_tarde, review_score) %>%
  group_by(llego_tarde) %>%
  mutate(porcentaje = round(100 * n / sum(n), 1))
print(distribucion)

# ---- 5. RESULTADO 2: puntaje por tramo de atraso ----------------
# case_when asigna a cada orden una etiqueta según la primera condición
# que cumple. Solo se usan las órdenes entregadas el día prometido o después.
por_tramo_atraso <- df %>%
  filter(dias_atraso >= 0) %>%
  mutate(tramo = case_when(
    dias_atraso == 0  ~ "a) el día prometido",
    dias_atraso <= 3  ~ "b) 1 a 3 días tarde",
    dias_atraso <= 7  ~ "c) 4 a 7 días tarde",
    dias_atraso <= 14 ~ "d) 8 a 14 días tarde",
    TRUE              ~ "e) 15 días o más"
  )) %>%
  group_by(tramo) %>%
  summarise(ordenes = n(),
            puntaje_promedio = round(mean(review_score), 2))
print(por_tramo_atraso)

# ---- 6. RESULTADO 3: ¿pesa más tardar o incumplir? -------------
# Agrupamos por tiempo total de entrega y, dentro de cada grupo,
# comparamos las órdenes a tiempo con las que llegaron tarde.
tardar_vs_incumplir <- df %>%
  mutate(tramo_total = case_when(
    tiempo_total <= 7  ~ "a) 0 a 7 días",
    tiempo_total <= 14 ~ "b) 8 a 14 días",
    tiempo_total <= 21 ~ "c) 15 a 21 días",
    tiempo_total <= 30 ~ "d) 22 a 30 días",
    TRUE               ~ "e) 31 días o más"
  )) %>%
  group_by(tramo_total, llego_tarde) %>%
  summarise(ordenes = n(),
            puntaje_promedio = round(mean(review_score), 2),
            .groups = "drop")
print(tardar_vs_incumplir)

# ---- PUNTOS ABIERTOS ----------------------------------------------

# Abierto 1: órdenes sin fecha de entrega
sin_fecha <- orders %>%
  filter(is.na(order_delivered_customer_date))

print(count(sin_fecha, order_status))        # cuántas hay en cada estado

sin_fecha %>%                                # cuántas tienen reseña y su promedio
  inner_join(reviews_unicas, by = "order_id") %>%
  summarise(con_resena = n(),
            puntaje_promedio = round(mean(review_score), 2)) %>%
  print()

# Abierto 2: órdenes con más de un ítem o más de un vendedor
items_por_orden <- order_items %>%
  group_by(order_id) %>%
  summarise(items = n(),
            vendedores = n_distinct(seller_id))

cat("Órdenes con más de un ítem:", sum(items_por_orden$items > 1), "\n")
cat("Órdenes con más de un vendedor:", sum(items_por_orden$vendedores > 1), "\n")

# ---- GRÁFICOS -------------------------------------------------------

# Gráfico 1: distribución de puntajes, a tiempo vs. tarde (barras al 100%)
grafico_barras <- df %>%
  mutate(estado  = ifelse(llego_tarde, "Llegó tarde", "A tiempo"),
         puntaje = factor(review_score)) %>%
  ggplot(aes(x = estado, fill = puntaje)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "RdYlGn") +
  labs(title = "Impacto del atraso en la calificación",
       subtitle = "Proporción de cada puntaje (1 a 5) según cumplimiento de la fecha prometida",
       x = NULL, y = "Porcentaje de reseñas", fill = "Puntaje") +
  theme_minimal()

print(grafico_barras)

# Gráfico 2: puntaje promedio según días de atraso
# Valores negativos = llegó antes de lo prometido. La línea punteada marca
# la fecha prometida. Se omiten los días con menos de 30 órdenes para que
# un par de casos sueltos no muevan la línea.
grafico_linea <- df %>%
  filter(dias_atraso >= -20, dias_atraso <= 30) %>%
  group_by(dias_atraso) %>%
  summarise(puntaje_promedio = mean(review_score),
            ordenes = n()) %>%
  filter(ordenes >= 30) %>%
  ggplot(aes(x = dias_atraso, y = puntaje_promedio)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(color = "#d35400", linewidth = 1) +
  geom_point(color = "#d35400", size = 2) +
  scale_y_continuous(limits = c(1, 5)) +
  labs(title = "Puntaje promedio según días de atraso",
       subtitle = "Negativo = llegó antes de la fecha prometida; positivo = llegó tarde",
       x = "Días de atraso (fecha real - fecha prometida)",
       y = "Puntaje promedio (1 a 5)") +
  theme_minimal()

print(grafico_linea)

# Gráfico 3: puntaje según tiempo total, a tiempo vs. tarde (barras agrupadas)
grafico_tramos <- tardar_vs_incumplir %>%
  mutate(estado = ifelse(llego_tarde, "Llegó tarde", "A tiempo")) %>%
  ggplot(aes(x = tramo_total, y = puntaje_promedio, fill = estado)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("A tiempo" = "grey55", "Llegó tarde" = "#d35400")) +
  labs(title = "¿Pesa más tardar o incumplir la fecha?",
       subtitle = "Puntaje promedio según tiempo total de entrega",
       x = "Tiempo total entre compra y entrega", y = "Puntaje promedio (1 a 5)",
       fill = NULL) +
  theme_minimal()

print(grafico_tramos)

# Guardar los gráficos como imagen para el informe
ggsave("grafico_barras.png", grafico_barras, width = 7, height = 5, bg = "white")
ggsave("grafico_linea.png",  grafico_linea,  width = 7, height = 5, bg = "white")
ggsave("grafico_tramos.png", grafico_tramos, width = 8, height = 5, bg = "white")
