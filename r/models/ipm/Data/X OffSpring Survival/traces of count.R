temp2 <- melt((outPre$sigmaBeta[]))

ggplot(temp2, aes(X3,(value)
                  #ggplot(temp2, aes((value)
                  #, colour=(X3)  # turn on when >1 chain
)) + 
  facet_grid(X1~X2, scales='free') + 
  geom_point() +
  #  geom_density(size=1.5) +
  #  geom_hline(yintercept=0) +
  scale_colour_continuous()


temp2 <- melt((outPre$sigmaInt[]))

ggplot(temp2, aes(X2,(value)
                  #ggplot(temp2, aes((value)
                  #, colour=(X3)  # turn on when >1 chain
)) + 
  facet_grid(.~X1, scales='free') + 
  geom_point() +
  #  geom_density(size=1.5) +
  #  geom_hline(yintercept=0) +
  scale_colour_continuous()





temp2 <- melt((outPre$meanInt[]))

ggplot(temp2, aes(X2,(value)
                  #ggplot(temp2, aes((value)
                  #, colour=(X3)  # turn on when >1 chain
)) + 
  facet_grid(.~X1, scales='free') + 
  geom_point() +
  #  geom_density(size=1.5) +
  #  geom_hline(yintercept=0) +
  scale_colour_continuous()


temp2 <- melt((outPre$meanBeta[]))

ggplot(temp2, aes(X3,(value)
                  #ggplot(temp2, aes((value)
                  #, colour=(X3)  # turn on when >1 chain
)) + 
  facet_grid(X1~X2, scales='free') + 
  geom_point() +
  #  geom_density(size=1.5) +
  #  geom_hline(yintercept=0) +
  scale_colour_continuous()