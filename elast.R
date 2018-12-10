# e_domestic <- matrix(data = c(
#   -0.696,  -0.289, -0.172, -0.024, -0.094,
#   -0.382,  -0.536, -0.046, -0.021,	0.031,
#   -0.191,	  0.108, -0.235, -0.008,  0.127,
#   -0.511,  -0.323, -0.159, -0.157,   0.04,
#   -1.665,	  0.498,  0.591,  0.056, -0.202
# ),nrow = length(crops),ncol = length(crops),byrow = TRUE,
# dimnames = list(crops,crops))

e_domestic <- -1 * diag(5)
dimnames(e_domestic) = list(crops,crops)

e_Corn_Belt<- matrix(data = c( 					
	0.201,	-0.108,	-0.004,	0,	0,
	-0.167,	0.153,	-0.005,	-0.001,	0,
	-0.155,	0.201,	0.201,	-0.001,	0,
	-0.164,	-0.117,	-0.006,	0.238,	0,
	0,	0,	0,	0,	0
),nrow = length(crops),ncol = length(crops),byrow = TRUE,
dimnames = list(crops,crops))

e_Corn_Belt <-  diag(5)
dimnames(e_Corn_Belt) = list(crops,crops)


e_Central_Plains  <- matrix(data = c(					
	0.262,	-0.064,	-0.059,	0,	0,
	-0.144,	0.256,	-0.059,	0,	0,
	-0.085,	-0.039,	0.151,	0,	0,
	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0
),nrow = length(crops),ncol = length(crops),byrow = TRUE,
dimnames = list(crops,crops))

e_Central_Plains <-  diag(5)
dimnames(e_Central_Plains) = list(crops,crops)


e_Delta_States <- matrix(data = c(					
	0.326,	-0.036,	-0.003,	-0.034,	0,
	-0.031,	0.191,	-0.008,	-0.095,	0,
	-0.016,	-0.047,	0.331,	-0.045,	0,
	-0.015,	-0.05,	-0.004,	0.473,	0,
	0,	0,	0,	0,	0
),nrow = length(crops),ncol = length(crops),byrow = TRUE,
dimnames = list(crops,crops))

e_Delta_States <-  diag(5)
dimnames(e_Delta_States) = list(crops,crops)


e_Far_West <- matrix(data = c(					
	0.474,	0,	-0.094,	-0.015,	0,
	0,	0,	0,	0,	0,
	-0.057,	0,	0.12,	-0.026,	0,
	-0.035,	0,	-0.098,	0.513,	0,
	0,	0,	0,	0,	0
),nrow = length(crops),ncol = length(crops),byrow = TRUE,
dimnames = list(crops,crops))

e_Far_West <-  diag(5)
dimnames(e_Far_West) = list(crops,crops)


e_Lake_States <- matrix(data = c(					
	0.19,	-0.081,	-0.016,	0,0,
	-0.14,	0.156,	-0.017,	0,	0,
	-0.149,	-0.087,	0.21,	0,	0,
	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0
),nrow = length(crops),ncol = length(crops),byrow = TRUE,
dimnames = list(crops,crops))

e_Lake_States <-  diag(5)
dimnames(e_Lake_States) = list(crops,crops)


e_Northeast <- matrix(data = c(					
	0.214,	-0.05,	-0.017,	0,	0,
	-0.281,	0.257,	-0.015,	0,	0,
	-0.367,	-0.057,	0.461,	0,	0,
	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0
),nrow = length(crops),ncol = length(crops),byrow = TRUE,
dimnames = list(crops,crops))

e_Northeast <-  diag(5)
dimnames(e_Northeast) = list(crops,crops)


e_Northern_Plains <- matrix(data = c(					
	0.477,	-0.029,	-0.1,	0,	0,
	-0.03,	0.36,	-0.103,	0,	0,
	-0.081,	-0.08,	0.335,	0,	0,
	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0
),nrow = length(crops),ncol = length(crops),byrow = TRUE,
dimnames = list(crops,crops))

e_Northern_Plains <-  diag(5)
dimnames(e_Northern_Plains) = list(crops,crops)


e_Southeast <- matrix(data = c(					
	0.253,	-0.013,	-0.004,	0,	-0.01,
	-0.042,	0.129,	-0.005,	0,	-0.013,
	-0.047,	-0.019,	0.238,	0,	-0.007,
	0,	0,	0,	0,	0,
	-0.042,	-0.017,	-0.002,	0,	0.169
),nrow = length(crops),ncol = length(crops),byrow = TRUE,
dimnames = list(crops,crops))

e_Southeast <-  diag(5)
dimnames(e_Southeast) = list(crops,crops)


e_Southern_Plains <- matrix(data = c(					
	0.482,	-0.001,	-0.082,	-0.002,	-0.004,
	-0.034,	0.199,	-0.117,	-0.005,	-0.006,
	-0.043,	-0.002,	0.157,	-0.004,	-0.008,
	-0.029,	-0.002,	-0.102,	0.624,	0,
	-0.017,	-0.001,	-0.059,	0,	0.605
),nrow = length(crops),ncol = length(crops),byrow = TRUE,
dimnames = list(crops,crops))

e_Southern_Plains <-  diag(5)
dimnames(e_Southern_Plains) = list(crops,crops)


e_export <- diag(x=1,nrow = length(crops),ncol = length(crops))
dimnames(e_export) <- list(crops,crops)

e_imports <- diag(x=1,nrow = length(crops),ncol = length(crops))
dimnames(e_imports) <- list(crops,crops)

#consumption weights

w_domestic <- matrix(1,nrow = length(crops),ncol = length(crops))
dimnames(w_domestic) = list(crops,crops)

w_export <- matrix(data = c(
  0.14,	0,	0,	0,	0,
  0,	0.83,	0,	0,	0,
  0,	0,	0.80,	0,	0,
  0,	0,	0,	0.84,	0,
  0,	0,	0,	0,	0.23
),nrow = length(crops),ncol = length(crops),byrow = TRUE,
dimnames = list(crops,crops))

#supply weights
ss_AOS <- matrix(data = c(
  0.94,	0,	0,	0,	0,
  0,	0.88,	0,	0,	0,
  0,	0,	0.90,	0,	0,
  0,	0,	0,	0.24,	0,
  0,	0,	0,	0,	0.03
),nrow = length(crops),ncol = length(crops),byrow = TRUE,
dimnames = list(crops,crops))

ss_FRS <- matrix(data = c(
  0.06,	0,	0,	0,	0,
  0,	0.12,	0,	0,	0,
  0,	0,	0.10,	0,	0,
  0,	0,	0,	0.76,	0,
  0,	0,	0,	0,	0.97
),nrow = length(crops),ncol = length(crops),byrow = TRUE,
dimnames = list(crops,crops))

ss_imports <- matrix(data = c(
  0.01,	0,	0,	0,	0,
  0,	0.02,	0,	0,	0,
  0,	0,	0.12,	0,	0,
  0,	0,	0,	0.17,	0,
  0,	0,	0,	0,	0.03
),nrow = length(crops),ncol = length(crops),byrow = TRUE,
dimnames = list(crops,crops))