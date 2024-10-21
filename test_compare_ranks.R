let <- sample(c(letters, LETTERS), length(letters))
let2 <- sample(c(letters, LETTERS), length(letters))

swapn <- function(x, swaps) {
    for (i in 1:swaps) {
        one <- sample.int(length(x), 1)
        two <- sample.int(length(x), 1)
        
        while (one == two) {
            one <- sample.int(length(x), 1)
            two <- sample.int(length(x), 1)
        }
        
        t <- x[one]
        x[one] <- x[two]
        x[two] <- t
    }
    x
}

cong <- continuous_congruency(
    let,
    let
)

for (i in 1:100) {
    cong[[paste0("swap_", i)]] <- {
        continuous_congruency(
            let,
            swapn(let, i)
        )$value
    }
}

cong2 <- continuous_congruency(
    let,
    let2
)

for (i in 1:100) {
    cong2[[paste0("swap_", i)]] <- {
        continuous_congruency(
            let,
            swapn(let2, i)
        )$value
    }
}

v <- plot_continuous_congruency(cong) + ggplot2::theme(legend.position = "none") + ggplot2::ggtitle("New, same sets")
p  <- plot_continuous_congruency(cong2) + ggplot2::theme(legend.position = "none") + ggplot2::ggtitle("New, diff sets")

# with old impl
cong3 <- compare.ranks::continuous_congruency(
    let,
    let
)

for (i in 1:100) {
    cong3[[paste0("swap_", i)]] <- {
        compare.ranks::continuous_congruency(
            let,
            swapn(let, i)
        )$value
    }
}

k <- plot_continuous_congruency(cong3) + ggplot2::theme(legend.position = "none")  + ggplot2::ggtitle("Old, same sets")

cong4 <- compare.ranks::continuous_congruency(
    let,
    let2
)

for (i in 1:100) {
    cong4[[paste0("swap_", i)]] <- {
        compare.ranks::continuous_congruency(
            let,
            swapn(let2, i)
        )$value
    }
}

n<-plot_continuous_congruency(cong4) + ggplot2::theme(legend.position = "none") + ggplot2::ggtitle("Old, diff sets")

ggpubr::ggarrange(v,p,k,n)

