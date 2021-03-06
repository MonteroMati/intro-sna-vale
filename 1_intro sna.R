##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                                ~~
##             Introducción al análisis de redes sociales                       ----
##     Ciclo de talleres: “Aprendiendo metodología por Valentina”                 ~~
## Alejandro Plaza (agplaza@uc.cl) y Matías Montero (matias.montero@ug.uchile.cl) ~~
##                                                                                ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### 1. Set up ----
getwd()
setwd("G:/My Drive/Teaching/Taller SNA Valentina/intro-sna-vale")
pacman::p_load(igraph, viridis)

### 2. Estructura de datos de redes ----
# 2.1 Matriz de adyacencia
first_matrix = matrix(
  c(0,1,0,0, 
    0,0,0,0, 
    0,0,0,1, 
    0,1,0,0), 
  nrow = 4, ncol = 4, byrow = TRUE)

first_net = graph.adjacency(first_matrix)
net_undirect = graph.adjacency(first_matrix, mode = 'undirected') 
plot(first_net)
plot(net_undirect)

# 2.2 Lista de enlaces
ego = c(1,3,3,2,4)
alter = c(2,4,1,3,2)
elist = data.frame(ego, alter)
elist
my_net = graph.data.frame(elist)

# Grafo no dirigido: agregar el argumento "directed"
my_net_ud = graph.data.frame(elist, directed = FALSE)
plot(my_net)
plot(my_net_ud)

# 2.3 Matriz de incidencia (redes modo 2)
e1 = c(1,1,1,0,0,0)
e2 = c(0,1,1,1,0,0)
e3 = c(0,0,1,1,1,0)
e4 = c(0,0,0,0,1,1)
protest_df = data.frame(e1,e2,e3,e4) # Eventos de protesta
row.names(protest_df) = c("Alejandro", "Matías", "Valentina", # Nombre de nodos
                           "Daniela", "Agnes", "Hoshi")

protest_events_net = graph.incidence(protest_df)

summary(protest_events_net)

# Graficar red modo 2
plt.x = c(rep(2,6), rep(4,4))
plt.y = c(7:2, 6:3)
lay = as.matrix(cbind(plt.x, plt.y))

shapes = c("circle", "square")
colors = c("blue", "red")
plot(protest_events_net, vertex.color = colors[V(protest_events_net)$type+1],
      vertex.shape = shapes[V(protest_events_net)$type+1],
      vertex.size = 10, vertex.label.degree = -pi/2,
      vertex.label.dist = 1.2, vertex.label.cex = 0.9,
      layout = lay)

# Transformar a red modo 1 (co-partipación en evento de protesta)
projections = bipartite.projection(protest_events_net)
projections
projection.individuals = projections$proj1

protest_events_net_adj = get.adjacency(projection.individuals, sparse=FALSE, attr="weight")
protest_events_net_adj = graph.adjacency(protest_events_net_adj)
plot(protest_events_net_adj)

# 2.4 Bonus: contruir una red desde función make_graph (igraph)
make_graph(c("A","B", "X","Y", "A","X", "B","Z"), directed = FALSE)

# No dirigida
g1 = make_graph(~ A - X, X - Y:Z, B - A)
g1

# Dirigida
g2 = make_graph(~ A -+ X, B -+ Z, A +- X:Y, B -+ A, Y +- Z) 
g2

plot(g1) 
plot(g2)

# Guardar en formato imagen (archivo .jpeg)
jpeg(filename = "guardando mi primera red.jpeg", width = 500, height = 500, units = "px",
     quality = 100, res = 100)
plot(g1)
dev.off()

### 3. Visualización usando datos reales ----
# 3.1. Datos reales: redes de una escuela (crédito a Prof. Lorena Ortega)
load("curso_vertices.Rda")
load("curso_enlaces.Rda")

red_curso = graph_from_data_frame(curso_enlaces, vertices = curso_vertices, directed = TRUE)

# 3.2. Seleccionar la red de juego
red_juego = delete_edges(red_curso, E(red_curso)[question != "play"])
red_juego
plot(red_juego, edge.arrow.size = 0.3)

# 3.3. Modificar las características por default del gráfico
# Uso de argumentos (escalares o vectoriales/secuencias)

plot(red_juego,
     edge.color = "indianred4", # Color del enlace (usa hex codes o etiquetas de colores de R)
     edge.arrow.size = 1, # Tamaño de la flecha
     vertex.color = V(red_juego)$female + 1) # Crea una secuencia de vértices para todos los vértices del grafo
                                             # y, con ello, distingue color a cada vértice según género.

# También podemos operar sobre el objeto igraph de forma secuencial (función V() y E())
game_net = red_juego
V(game_net)$color = ifelse(V(game_net)$female, "#FCFF8F", "#C0FF6A") 
E(game_net)$color = "black"
E(game_net)$arrow.size = 0.2
plot(game_net)

# Color de vértices y enlaces
plot(red_juego, vertex.color = viridis(vcount(red_juego)), # para cada vértice asigna un color de la paleta viridis.
     edge.color = "black", edge.arrow.size = 0.4)
plot(red_juego, edge.color = viridis(ecount(red_juego)), vertex.color = "white", edge.arrow.size = 0.5)

# Estilo y ancho de los enlaces
plot(red_juego, edge.width = 2, edge.arrow.size = 0.2, edge.curved = 0.2,
     edge.color = "black", vertex.color = "red", vertex.frame.color = "white", vertex.label = NA)

# Forma y tamaño de los vértices
plot(red_juego,
     vertex.shape = ifelse(V(red_juego)$female, "circle", "square"), # Distingue hombres y mujeres por forma
     vertex.size = scales::rescale(degree(red_juego), c(5,15)), # Tamaño según degree (n enlaces adyacentes)
     vertex.label = NA, edge.arrow.size = 0.3, edge.color = "black")

# Etiquetas de texto
plot(red_juego, vertex.label.cex = 0.7, vertex.label.color = "white", vertex.color = "blue", 
     vertex.label = seq(1, vcount(red_juego)), # Etiqueta los nodos contando desde 1. Default = etiqueta en variable "name"
     vertex.size = 15,
     edge.arrow.size = 0.2, edge.color = "black")

plot(red_juego, vertex.color = "blue", vertex.label = NA, vertex.size = 5, edge.arrow.size = 0.2,
     edge.label = seq(1, ecount(red_juego)), edge.label.cex= 0.7, # Etiqueta los enlaces
     edge.color="black")

# Marcando grupos
set.seed(270122)
plot(red_juego, mark.groups = list(
  V(red_juego)[female], V(red_juego)[!female] ), # Genera una lista por género del nodo
  mark.col = adjustcolor(c("red", "blue"), 0.1), mark.border = c("red", "blue"), 
  vertex.shape = ifelse(V(red_juego)$female, "circle", "square"), vertex.label = NA, 
  edge.arrow.size = 0.2, edge.color = "black")

## Force-directed graph design: algoritmos de diseño
# Posicionan los nodos de un grafo en un espacio bidimensional o tridimensional de forma que
# los enlaces tengan más o menos la misma longitud y exista el menor número posible de enlaces cruzados
# Asigna fuerzas de atracción entre el conjunto de enlaces y el conjunto de nodos,
# en función de sus posiciones relativas.

# Fruchterman-Reingold
plot(red_juego, layout = layout_with_fr, vertex.label = NA, 
     vertex.size = 8,
     edge.arrow.size = 0.3,
     vertex.color=ifelse(V(red_juego)$female, "red", "lightblue"))

# Kamada-Kawai
plot(red_juego, layout = layout_with_kk, vertex.label = NA,
     vertex.size = 8,
     edge.arrow.size = 0.3,
     vertex.color=ifelse(V(red_juego)$female, "red", "lightblue"))

# Multidimensional scaling
plot(red_juego, layout = layout_with_mds, vertex.label = NA,
     vertex.size = 8,
     edge.arrow.size = 0.3,
     vertex.color = ifelse(V(red_juego)$female, "red", "lightblue"))

# Circle
plot(red_juego, layout = layout_in_circle, vertex.label = NA,
     vertex.size = 8,
     edge.arrow.size = 0.3,
     vertex.color = ifelse(V(red_juego)$female, "red", "lightblue"))

### 4. Díadas y tríadas ----
# - Densidad
graph.density(red_juego) # d = 0.135.

# - Reciprocidad
reciprocity(red_juego) # la reciprocidad es igual a 0.5; ¿cómo interpretamos?

# Estrategia de interpretación (1)
# Contextual: revisar literatura especializada de redes en el campo de estudio en particular

# Estrategia de interpretación (2)
# Comparar con el valor que se obtiene en una red aleatoria de densidad similar.
# En grafos aleatorios, la probabilidad de que dos díadas estén enlazadas se deberá al azar.
# La probabilidad de observar un vínculo dado es independiente de observar un vínculo entre cualquier otra díada.
# Es semejante a comparar con un modelo nulo en regresión.

# La función erdos.renyi.game genera una red del mismo tamaño y densidad, pero con enlaces emitidos al azar
random_graph = erdos.renyi.game(n = vcount(red_juego), # Número de nodos
                                p.or.m = graph.density(red_juego), # Probabilidad de generar un enlace entre dos nodos
                                directed = TRUE) # La red es dirigida

# Comprobación visual de ambos grafos
plot(red_juego,  
     vertex.size = 10, 
     vertex.label = NA, 
     edge.curved = .1, 
     vertex.color = "tomato", 
     edge.arrow.size = .1, 
     edge.width = .5, 
     edge.color = "grey60")

plot(random_graph,  
     vertex.size = 10, 
     vertex.label = NA, 
     edge.curved = .1, 
     vertex.color = "tomato", 
     edge.arrow.size = .1, 
     edge.width = .5, 
     edge.color = "grey60")

# - ¿Cuál es la reciprocidad del grafo aleatorio?
reciprocity(random_graph)

# - Homofilia
assortativity_nominal(red_juego, types = as.numeric(V(red_juego)$female) + 1)

# - Tríadas: transitividad
transitivity(red_juego)

# - Tríadas: censo de tríadas
triad.census(red_juego)
