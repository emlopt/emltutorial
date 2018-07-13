source('sim.R')

# ============================================================================
# Simulation code
# ============================================================================

# edge.ratio = 0.003 # 0.001 basically enough to solve
# inf.prob = 0.7 # 0.2 rather small, but not enough to solve
# act.rate = 3.0 # 1 very small, but not enough to solve
# rec.rate = 0.05 # 0.1 very high, but not enough to solve
# ds.rate = 0.05 # even 0.0 by itself does not solve
# di.rate = 0.03 # 0.2 very large but unreliable
# dr.rate = ds.rate

gen.data <- function(out.file='results.csv',
		     stop.after=NULL,
		     verbose=TRUE) {

  iter.num <- 30
  # Write header
  line = "edge_ratio,inf_prob,act_rate,rec_rate,ds_rate,di_rate,dr_rate,time,s_num,i_num,r_num,num"
  # Append to the results file
  write(line, file=out.file, append=FALSE)
  # Counter
  cnt <- 0

  for (edge.ratio in c(0.001, 0.002, 0.003, 0.004, 0.005)) {
    for (inf.prob in c(0.3, 0.5, 0.7)) {
      for (act.rate in c(1.0, 2.0, 3.0)) {
	for (rec.rate in c(0.00, 0.05, 0.075, 0.1)) {
	  for (ds.rate in c(0.00, 0.025, 0.05)) {
	    for (di.rate in c(0.03, 0.13, 0.23)) {
	      dr.rate = ds.rate
	      for (iter in 1:iter.num) {
		here.they.come(edge.ratio, inf.prob, act.rate, rec.rate,
			       ds.rate, di.rate,
			       plot.figures=FALSE, out.file, verbose)
		cnt <- cnt +1
		if (!is.null(stop.after) && cnt >= stop.after) {
		  return('done')
		}
	      }
	    }
	  }
	}
      }
    }
  }

}
