
pRk <- function(r, n, k, beta, lower.tail = TRUE) {
    # the function calculate the cdf of Rk
    # r : argument of the cdf
    # n: total number of observations in a sample
    # k: number of contaminations under slippage alternative
    # beta: the parameter of the slippage alternative, assumed to be greater than 1
    if (!lower.tail) {
        return(1 - pRk(r, n, k, beta))
    }
    factor_1 <- gamma(k) * beta * gamma(k * beta + n - k) / (gamma(k * beta + 1))
    summand <- function(i, j) {
        # the function calculate the summation
        factor_2 <- (-1)^(n + i + j - 2) / (gamma(n - k - j + 1) * gamma(j - 1) * gamma(k - i + 1) * gamma(i - 1))
        m_i <- beta * (k - i + 1)
        n_i <- k * beta + n - k - j + 1
        factor_3 <- (1 / m_i - 1 / (m_i + r * n_i)) / n_i

        return(sum(factor_3 * factor_2))
    }

    mat_ind <- matrix(nrow = k - 1, ncol = n - k - 1)
    i <- row(mat_ind) + 1
    j <- col(mat_ind) + 1
    return(factor_1 * summand(i, j))
}