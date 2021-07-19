pZr <- function(z, b = 1, n = 10, r = 5, lower.tail = TRUE) {
    # return the P(Z_r < z | H_r)
    # z : the argument of cdf of Z_r, has range 0 < z <1/r
    # n : total number of observation in the sample
    # r : integer, number of contamination in the observation
    # b : the parameter for contaminated exponential sample
    #     the contamination assumed to have Exp(theta/b) disribution,
    #    assuming  b <= 1
    # lower.tail: logical, if it's false then return 1 - P(Z_r < z | H_r)
    if (!lower.tail) {
        return(1 - pZr(z, b, n, r))
    }
    if (z > 1 / r) {
        return(1)
    } else if (z < 0) {
        return(0)
    }
    factor_1 <- b^(r) * gamma(r * b + n - r) / gamma(r * b + 1)

    j <- 2:(n - r)
    a_j <- r * b + n - r - j + 1

    factor_2 <- (-1)^(n - j - r) *
        (b^(-r) - (a_j * z / (1 - r * z) + b)^(-r)) / (gamma(j - 1) * gamma(n - j - r + 1) * a_j)

    return(sum(factor_1 * factor_2))
}