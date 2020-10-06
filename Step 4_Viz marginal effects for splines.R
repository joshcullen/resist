
### Generate marginal effects plot of dist2rd splines

#Load data
path.N<- read.csv('N Armadillo Resistance Data.csv', as.is=T)
path.S<- read.csv('S Armadillo Resistance Data.csv', as.is=T)

path.N$dt<- path.N$dt/60  #convert to min from sec
path.S$dt<- path.S$dt/60

# Filter data for only steps with 3 >= dt >= 7 min
cond.N<- path.N[path.N$dt >= 3 & path.N$dt <= 7 & !is.na(path.N$dt), "seg.id"]
path.N<- path.N[path.N$seg.id %in% cond.N,]

cond.S<- path.S[path.S$dt >= 3 & path.S$dt <= 7 & !is.na(path.S$dt), "seg.id"]
path.S<- path.S[path.S$seg.id %in% cond.S,]


# Filter data by behavior (foraging or transit)
path.N.forage<- path.N %>% 
  filter(state == "Foraging")
path.N.transit<- path.N %>% 
  filter(state == "Transit")

path.S.forage<- path.S %>% 
  filter(state == "Foraging")
path.S.transit<- path.S %>% 
  filter(state == "Transit")


## For N Forage ##
#Generate sequence along dist2rd
rango1<- range(path.N.forage$dist2rd)
seq.dist<- seq(rango1[1], rango1[2], length.out = 100)
seq.dist2<- scale(seq.dist, center = T, scale = T)

#Run splines on standardized sequence
rango2<- range(seq.dist2)
knot.locs<- seq(rango2[1], rango2[2], length.out = 4)[2:3]
spline.dist<- bs(seq.dist2, degree=2, intercept = FALSE, knots = knot.locs)

#Create design matrix where 0s added for all other non-spline vars
design.mat<- cbind(1, matrix(0, nrow = 100, ncol = 4), spline.dist)
betas.forage_N<- colMeans(store.betas_Nforage)

# Take cross-product of design matrix with betas and exponentiate to calc response
y<- exp(design.mat %*% t(store.betas_Nforage))  #whole posterior
y.mu<- exp(design.mat %*% betas.forage_N)

# Add results to data frame to plot all lines from posterior
y.df<- cbind(rep(seq.dist2, nrow(store.betas_Nforage)), as.vector(y),
             paste0("Sample", rep(1:nrow(store.betas_Nforage), each = 100)))
y.df<- data.frame(y.df) %>% 
  mutate_at(1:2, as.character) %>% 
  mutate_at(1:2, as.numeric)
names(y.df)<- c("x", "y", "sample")

y.mu.df<- data.frame(x = seq.dist2, y = y.mu, sample = "Mean")

# Plot relationship
ggplot(data = y.df) +
  geom_line(aes(x, y, group = sample), alpha = 0.2) +
  geom_line(data = y.mu.df, aes(x, y), color = "red", size = 1) +
  labs(x = "Standardized Distance to Road", y = "Time Spent per Cell (min)") +
  theme_bw()







## For N Transit ##
#Generate sequence along dist2rd
rango1<- range(path.N.transit$dist2rd)
seq.dist<- seq(rango1[1], rango1[2], length.out = 100)
seq.dist2<- scale(seq.dist, center = T, scale = T)

#Run splines on standardized sequence
rango2<- range(seq.dist2)
knot.locs<- seq(rango2[1], rango2[2], length.out = 4)[2:3]
spline.dist<- bs(seq.dist2, degree=2, intercept = FALSE, knots = knot.locs)

#Create design matrix where 0s added for all other non-spline vars
design.mat<- cbind(1, matrix(0, nrow = 100, ncol = 4), spline.dist)
betas.transit_N<- colMeans(store.betas_Ntransit)

# Take cross-product of design matrix with betas and exponentiate to calc response
y<- exp(design.mat %*% t(store.betas_Ntransit))  #whole posterior
y.mu<- exp(design.mat %*% betas.transit_N)

# Add results to data frame to plot all lines from posterior
y.df<- cbind(rep(seq.dist2, nrow(store.betas_Ntransit)), as.vector(y),
             paste0("Sample", rep(1:nrow(store.betas_Ntransit), each = 100)))
y.df<- data.frame(y.df) %>% 
  mutate_at(1:2, as.character) %>% 
  mutate_at(1:2, as.numeric)
names(y.df)<- c("x", "y", "sample")

y.mu.df<- data.frame(x = seq.dist2, y = y.mu, sample = "Mean")

# Plot relationship
ggplot(data = y.df) +
  geom_line(aes(x, y, group = sample), alpha = 0.2) +
  geom_line(data = y.mu.df, aes(x, y), color = "red", size = 1) +
  labs(x = "Standardized Distance to Road", y = "Time Spent per Cell (min)") +
  theme_bw()











## For S Forage ##
#Generate sequence along dist2rd
rango1<- range(path.S.forage$dist2rd)
seq.dist<- seq(rango1[1], rango1[2], length.out = 100)
seq.dist2<- scale(seq.dist, center = T, scale = T)

#Run splines on standardized sequence
rango2<- range(seq.dist2)
knot.locs<- seq(rango2[1], rango2[2], length.out = 4)[2:3]
spline.dist<- bs(seq.dist2, degree=2, intercept = FALSE, knots = knot.locs)

#Create design matrix where 0s added for all other non-spline vars
design.mat<- cbind(1, matrix(0, nrow = 100, ncol = 4), spline.dist)
betas.forage_S<- colMeans(store.betas_Sforage)

# Take cross-product of design matrix with betas and exponentiate to calc response
y<- exp(design.mat %*% t(store.betas_Sforage))  #whole posterior
y.mu<- exp(design.mat %*% betas.forage_S)

# Add results to data frame to plot all lines from posterior
y.df<- cbind(rep(seq.dist2, nrow(store.betas_Sforage)), as.vector(y),
             paste0("Sample", rep(1:nrow(store.betas_Sforage), each = 100)))
y.df<- data.frame(y.df) %>% 
  mutate_at(1:2, as.character) %>% 
  mutate_at(1:2, as.numeric)
names(y.df)<- c("x", "y", "sample")

y.mu.df<- data.frame(x = seq.dist2, y = y.mu, sample = "Mean")

# Plot relationship
ggplot(data = y.df) +
  geom_line(aes(x, y, group = sample), alpha = 0.2) +
  geom_line(data = y.mu.df, aes(x, y), color = "red", size = 1) +
  labs(x = "Standardized Distance to Road", y = "Time Spent per Cell (min)") +
  theme_bw()







## For S Transit ##
#Generate sequence along dist2rd
rango1<- range(path.S.transit$dist2rd)
seq.dist<- seq(rango1[1], rango1[2], length.out = 100)
seq.dist2<- scale(seq.dist, center = T, scale = T)

#Run splines on standardized sequence
rango2<- range(seq.dist2)
knot.locs<- seq(rango2[1], rango2[2], length.out = 4)[2:3]
spline.dist<- bs(seq.dist2, degree=2, intercept = FALSE, knots = knot.locs)

#Create design matrix where 0s added for all other non-spline vars
design.mat<- cbind(1, matrix(0, nrow = 100, ncol = 4), spline.dist)
betas.transit_S<- colMeans(store.betas_Stransit)

# Take cross-product of design matrix with betas and exponentiate to calc response
y<- exp(design.mat %*% t(store.betas_Stransit))  #whole posterior
y.mu<- exp(design.mat %*% betas.transit_S)

# Add results to data frame to plot all lines from posterior
y.df<- cbind(rep(seq.dist2, nrow(store.betas_Stransit)), as.vector(y),
             paste0("Sample", rep(1:nrow(store.betas_Stransit), each = 100)))
y.df<- data.frame(y.df) %>% 
  mutate_at(1:2, as.character) %>% 
  mutate_at(1:2, as.numeric)
names(y.df)<- c("x", "y", "sample")

y.mu.df<- data.frame(x = seq.dist2, y = y.mu, sample = "Mean")

# Plot relationship
ggplot(data = y.df) +
  geom_line(aes(x, y, group = sample), alpha = 0.2) +
  geom_line(data = y.mu.df, aes(x, y), color = "red", size = 1) +
  labs(x = "Standardized Distance to Road", y = "Time Spent per Cell (min)") +
  theme_bw()
