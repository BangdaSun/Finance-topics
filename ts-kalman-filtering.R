# Kalman filtering example 
#
# Reference:
#   http://scipy.github.io/old-wiki/pages/Cookbook/KalmanFiltering
#   http://www.cs.unc.edu/~welch/kalman/kalmanIntro.html

# For fixed internal value

# x in R^n (x[k] doesn't mean kth element in x)
#     x[k] = A * x[k-1] + B * u[k] + w[k-1] 
# z in R^m
#     z[k] = H * x[k] + v[k]
# 
# time update:
#   (1) project the state ahead
#       xhat_[k] = A * xhat[k] + B * u[k]
#   (2) project the error covariance ahead
#       P_[k] = A * P[k-1] * A^T + Q
#  
# measuremenet update:
#   (1) compute kalman gain
#       K[k] = P_[k] * H^T * (H * P_[k] * H^T + R)^(-1)
#   (2) update the estimate with measurement z[k]
#       xhat[k] = xhat_[k] + K[k] * (z[k] - H * xhat_[k])
#   (3) update error covariance
#       P[k] = (1 - K[k] * H) * P_[k]


# set parameters
n_iter    = 50
true_val = 5.234324344                              # actual value
z        = rnorm(n_iter, mean = true_val, sd = 3)  # observation

Q     = 1e-5            # process variance
xhat  = rep(0, n_iter)  # a posterior estimate
P     = rep(0, n_iter)  # a posterior error estimate
xhat_ = rep(0, n_iter)  # a priori esimate
P_    = rep(0, n_iter)  # a priori error estimate
K     = rep(0, n_iter)  # gain

R     = .1 ^ 2          # estimate of measurement variance 

# initial guess
xhat[1] = 0
P[1]    = 1

# iteration
for (k in seq(2, n_iter)) {
  # time update
  xhat_[k] = xhat[k-1]
  P_[k]    = P[k-1] + Q
  
  # measurement update
  K[k]    = P_[k] / (P_[k] + R)
  xhat[k] = xhat_[k] + K[k] * (z[k] - xhat_[k])
  P[k]    = (1 - K[k]) * P_[k]
}

library(ggplot2)
df = data.frame(iter = 1:50, obs = z, filtered = xhat)
ggplot(df) + 
  geom_point(aes(x = iter, y = z), col = 'red') +
  geom_line(aes(x = iter, y = xhat), size = 1, linetype = 'dashed', col = 'blue') + 
  geom_abline(slope = 0, intercept = true_val, col = 'black')
  labs(x = 'iterations', y = 'values')
