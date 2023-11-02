library(ggplot2)

plotEmpiricalRule <- function(mu, sigma, n = 100, N = 1000000) {
    population <- rnorm(n = N, mean = mu, sd = sigma)
    mu <- mean(population)
    sigma <- sd(population)
    ySample <- sample(x = population, replace = F, size = n)
    ybar <- mean(ySample)
    s <- sd(ySample)
    
    y <- rbind(
        data.frame(type = "Population", y = population),
        data.frame(type = "Sample", y = ySample)
    )
    
    g <- ggplot(data = y, aes(x = y, fill = type)) +
        geom_histogram(aes(y=..density..), alpha=0.2, position="identity", color = '#737373') +
        geom_density(alpha = 0.3, color = '#737373') +
        facet_grid(rows = vars(type)) +
        geom_vline(xintercept = mu, color = '#F8766D', linetype = 'dashed', size = 0.5) +
        geom_vline(xintercept = ybar, color = '#00BFC4', linetype = 'dashed', size = 0.5) +
        geom_vline(xintercept = ybar - s, color = '#2A6637', size = 1) +
        geom_vline(xintercept = ybar + s, color = '#2A6637', size = 1) +
        geom_vline(xintercept = ybar - 2*s, color = '#36C488', size = 1) +
        geom_vline(xintercept = ybar + 2*s, color = '#36C488', size = 1) +
        geom_vline(xintercept = ybar - 3*s, color = '#7BE19A', size = 1) +
        geom_vline(xintercept = ybar + 3*s, color = '#7BE19A', size = 1) +
        labs(
            title = sprintf(
                '\U003BC: %s, \U0233: %s, \u03C3: %s, s: %s',
                round(mu, digits = 2),
                round(ybar, digits = 2),
                round(sigma, digits = 2),
                round(s, digits = 2)
            )
        ) +
        theme(plot.title = element_text(hjust = 0.5))
    
    return(g)
}

plotEmpiricalRule(100, 10) +
    xlim(25, 175) +
    ylim(0, 0.1)
