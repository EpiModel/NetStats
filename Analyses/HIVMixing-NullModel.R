
# Partner
# Ego   HIV-        HIV+       Unk
# HIV-  8752 (68.4)	432 (3.4)	 3604 (28.2)
# HIV+  581 (39.6)	367 (25.0) 518 (35.3)
# Unk   1028 (53.4)	33 (1.7)	 863 (44.9)

ae <- c(8752, 432, 3604, 581, 367, 518, 1028, 33, 863)
sum(ae)

3726 (76.0)
428 (8.7)
750 (15.3)

library("ergm")

n <- 10000
hiv.props <- c(0.760, 0.087, 0.153)
nw <- network.initialize(n, directed = FALSE)
hiv <- sample(rep(1:3, times = n*hiv.props))
nw <- set.vertex.attribute(nw, "hiv", hiv)
table(nw %v% "hiv")

md <- c(1.24, 1.59, 0.75)

wt.md <- md[1]*hiv.props[1] + md[2]*hiv.props[2] + md[3]*hiv.props[3]
wt.edges <- wt.md*n/2
nf <- c(md[1]*sum(hiv == 1), md[2]*sum(hiv == 2), md[3]*sum(hiv == 3))

# edges = md * n/2
# md = edges/(n/2) = 2*edges/n

fit <- ergm(nw ~ edges, target.stats = wt.edges, eval.loglik = FALSE)
sim <- simulate(fit, nsim = 10000,
                monitor = ~nodefactor("hiv", levels = NULL) + nodemix("hiv", levels = NULL),
                output = "stats")
head(sim)
(cms <- colMeans(sim))
round(cms)

cms[2:4]/table(hiv)

sum(cms[5:7])/cms[1]


fit <- ergm(nw ~ edges + nodefactor("hiv"),
            target.stats = c(wt.edges, nf[2:3]), eval.loglik = FALSE)
sim <- simulate(fit, nsim = 10000,
                monitor = ~nodefactor("hiv", levels = NULL) + nodemix("hiv", levels = NULL),
                output = "stats")
head(sim)
(cms <- colMeans(sim))
round(cms)

cms[4:6]/table(hiv)

sum(cms[5:7])/cms[1]
