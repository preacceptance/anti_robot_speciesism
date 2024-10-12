## clear workspace
rm(list = ls()) 

## install packages

if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('pwr')

## Perfect likeness study 1 "Robots"
## N is obtained from 332 participants / 6 conditions ~= 60 participants per condition
x <- pwr.chisq.test(N = 60, df = 1, sig.level = .05, power = .8)
x
x$w

## Perfect likeness study 2 "Avatar"
## N is obtained from 211 participants / 2 conditions ~= 100 participants per condition
x <- pwr.chisq.test(N = 100, df = 1, sig.level = .05, power = .8)
x
x$w

## Study 1
## n is obtained from 204 participants / 2 conditions ~= 100 participants per condition
x <- pwr.t.test( n = 100, sig.level = .05, power = .8, type = c("two.sample"))
x
x$d
