#### Tvtable
# put in error for 1 model for wide

t1 <- tvtable_tvcure(at1, format = "long")
t2 <- tvtable_tvcure(at2, format = "long")
t3 <- tvtable_tvcure(at3, format = "long")
t4 <- tvtable_tvcure(at4, format = "long")
tvtable_combine(c("t1", "t2", "t3", "t4"), format = "long")
t <- tvtable_combine(c("t1", "t2", "t3", "t4"), format = "long")

"t3", "t4"