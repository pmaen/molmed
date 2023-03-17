value <- c(1.657001,3.521643,1.740506,1.351351,1.654846,1.325723,1.978081,1.190103,1.058431,1.28942,0.939261,1.271586,1.433889,1.257466,1.106521,0.951444,1.192879,1.014637,1.0907,0.900761,0.832925,1.823233,1.14371,1.377832,8.909677,9.022202,10.738817,5.367379,7.039338,5.54213,8.175062,4.71955,6.484809,8.063704,6.451613,5.035247,64.705879,62.5,53.768219,59.755299,63.248875,47.314049,2.978083,2.658662,1.544991,1.479776,1.336811,4.555382)#quality values are enclosed in the brackets
group <- c("none","none","none","none","none","none","none + NSA","none + NSA","none + NSA","none + NSA","none + NSA","none + NSA","T","T","T","T","T","T","T + NSA","T + NSA","T + NSA","T + NSA","T + NSA","T + NSA","TB","TB","TB","TB","TB","TB","TB + NSA","TB + NSA","TB + NSA","TB + NSA","TB + NSA","TB + NSA","TBZ","TBZ","TBZ","TBZ","TBZ","TBZ","TBZ + NSA","TBZ + NSA","TBZ + NSA","TBZ + NSA","TBZ + NSA","TBZ + NSA") #sample values are enclosed in the brackets

my_data = data.frame(value, group)

#subsequent pairwise wilcox test to identify which groups are different
pairwise.t.test(value, group, p.adjust="bonferroni", pool.sd=FALSE, paired=FALSE)
