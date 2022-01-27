##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##             Introducción al análisis de redes sociales                   ----
##     Ciclo de talleres: “Aprendiendo metodología por Valentina”             ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### 1. Set up ----
getwd()
setwd("G:/My Drive/Teaching/Taller SNA Valentina")
pacman::p_load(igraph, statnet)

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
# Redes de una escuela (crédito a Prof. Lorena Ortega)
load("curso_vertices.Rda")
load("curso_enlaces.Rda")

# Asignar color a atributos
# Color, ancho, estilo, formas, tamaños, etiquetas a nodos y enlaces
# Algoritmos de diseño (quizás no)

### 4. Díadas y tríadas (seleccionar?)
# - Díadas: Reciprocidad y Homofilia
# - Díadas: Reglas de comportamiento y poder*
# - Tríadas: Cierre triádico*
# - Tríadas: Transitividad
# - Triadas: Balance Estructural*