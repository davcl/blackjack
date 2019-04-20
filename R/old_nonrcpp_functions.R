
#' @title nextCardProbs2
#' @description not only do we have to adjust the \code{dealer} function when he's
#' showing 1 or 10, we have to slightly modify the player next card probs
#' @author David Clement
#' @param dealer_card the card the dealer has. if 1 then they can't get 10 next,
#' if 10 then they can't get 1 next. NA is treated like 2:9
#' @param cards_remaining vector with number of each type of card remaining
#' @return vector of adjusted player next card probabilities
#' @export
nextCardProbs2 <- function(cards_remaining, dealer_card = NA) {

  next_card_probs <- cards_remaining / sum(cards_remaining)

  if(is.na(dealer_card) | dealer_card %in% 2:9) {
    return(next_card_probs)
  }

  not_dealer_card <- ifelse(dealer_card == 1,
                            10,
                            1)
  other_cards <- (1:10)[-not_dealer_card]

  # increase probability of getting the card dealer can't have
  next_card_probs[not_dealer_card] <- next_card_probs[not_dealer_card] *
    sum(cards_remaining) / (sum(cards_remaining) - 1)

  # decrease probabilities of all other cards by the same multiplicative amount
  # while maintaining the sum to 1 constraint
  next_card_probs[other_cards] <- next_card_probs[other_cards] *
    (1 - next_card_probs[not_dealer_card])/sum(next_card_probs[other_cards])

  return(next_card_probs)
}


