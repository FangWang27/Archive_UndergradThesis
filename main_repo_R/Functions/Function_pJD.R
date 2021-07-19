
pJD <- function(x, n, k, beta, lower.tail = TRUE) {
    # the function calculate the cdf of JD_r
    # x : argument of the cdf
    # n: total number of observations in a sample
    # k: number of contaminations under slippage alternative
    # beta: the parameter of the slippage alternative, assumed to be greater than 1
    if (!lower.tail) {
        return(1 - pJD(x, n, k, beta))
    }
    pR <- function(r) {
        # the internal function used to calculate the cdf of R_k
        # r : the argument of cdf
        factor_1 <- beta * gamma(k * beta + n - k) / (gamma(k * beta + 1) * gamma(k + 2))
        summand <- function(i, j) {
            # the function calculate the summation
            factor_2 <- (-1)^(n + i + j - 2) / (gamma(n - k - j + 1) * gamma(j - 1) * gamma(k - i + 1) * gamma(i - 1))
            m_i <- beta * (k - i + 1)
            n_i <- k * beta + n - k - j + 1
            factor_3 <- (1 / m_i - 1 / (beta * m_i + r * n_i)) / n_i

            return(sum(factor_3 * factor_2))
        }

        mat_ind <- matrix(nrow = k - 1, ncol = n - k - 1)
        i <- row(mat_ind) + 1
        j <- col(mat_ind) + 1

        return(factor_1 * summand(i, j))
    }

    return(1 - pR(1 - x))
}