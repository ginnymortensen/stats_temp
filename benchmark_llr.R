# Assuming the french_fries dataset is loaded in your environment:
french_fries <- french_fries[complete.cases(french_fries), ]
z <- seq(0, 15, length.out = 100)

# Using microbenchmark
microbenchmark_results <- microbenchmark(
  llr_results <- llr(x = french_fries$potato, y = french_fries$buttery, z = z, omega = 2),
  times = 100  # Number of times the function is run; adjust as needed
)

cat("Microbenchmark results:\n")
print(microbenchmark_results)

# Using bench::mark
bench_results <- bench::mark(
  llr_results <- llr(x = french_fries$potato, y = french_fries$buttery, z = z, omega = 2),
  iterations = 100  # Number of times the function is run; adjust as needed
)

cat("\nBench results:\n")
print(bench_results)
