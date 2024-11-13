### forwardHMM.R ##############################################################
#
#  This is an R implementation of the forward algorithm for inference on HMMs 
#  It takes as input 
#		y				a vector describing the observation sequence
#		a_transition	a matrix of transition probabilities 
#		b_emission		a matrix of emission probabilities 
#		pi_initial		a vector of initial probabilities 
# 
#  Note that this implementation takes advantage of recursive relationships 
#  between time points but isn't a true example, where we would expect the 
#  forward function to be repeatedly called on the subproblem
#

forwardHMM = function(y, a_transition, b_emission, pi_initial){
	sequence_length = length(y)
	num_hidden = nrow(a_transition)
	forward_probabilities = matrix(0, sequence_length, num_hidden)

	# base case 
	forward_probabilities[1, ] = pi_initial*b_emission[, y[1]]

	for (time_t in 2:sequence_length) {
		tmp = forward_probabilities[time_t - 1, ] %*% a_transition
		forward_probabilities[time_t, ] = tmp * b_emission[, y[time_t]]
	}
	return(rowSums(forward_probabilities)[sequence_length])
}

# Example running the forward algorithm using our weather example. 
# Note that we encode the state names (sun, rain) and emission 
# names (sunglasses, umbrella, hate) as numbers. 

weather_a <- matrix(c(0.8, 0.4, 0.2, 0.6), ncol=2, nrow=2)
#colnames(weather_a) <- rownames(weather_a) <- c('sun', 'rain')
weather_b <- matrix(c(0.6, 0, 0.1, 0.8, 0.3, 0.2), ncol=3, nrow=2)
# rownames(weather_b) <- c('sun', 'rain')
# colnames(weather_b) <- c('sunglasses', 'umbrella', 'hat')
weather_pi <- c(0.5, 0.5)
# names(weather_pi) <- c('sun', 'rain')

observed_sequence <- c('sunglasses', 'hat', 'umbrella')
observed_sequence <- c(1, 3, 2)

forwardHMM(
	y = observed_sequence,
	a_transition = weather_a,
	b_emission = weather_b,
	pi_initial = weather_pi
	)
