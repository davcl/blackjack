
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



#' @title dealer2
#' @description recursive function to find dealer pmf
#' @author David Clement
#' @param dealer_total what the dealer total is so far
#' @param cards_remaining vector of counts for each of 1 through 10
#' @param is_ace boolean indicating if one of dealer cards is an ace
#' @param hit_soft_17 defaults to \code{TRUE}
#' @param is_first if dealer is showing only one card so far, we can be sure
#' it's not a blackjack because that would have been revealed
#' @return a named vector of length 6, with names 17:22 (the possible
#' results for dealer)
#' @export
dealer2 <- function(dealer_total, cards_remaining, is_ace, hit_soft_17 = TRUE, is_first = FALSE) {

  cards_remaining_tmp <- cards_remaining

  # if dealer total is 1 they can't have a 10
  # if dealer total is 10 they can't have a 1
  if(is_first & dealer_total == 1) {
    cards_remaining_tmp[10] <- 0
  } else if(is_first & dealer_total == 10) {
    cards_remaining_tmp[1] <- 0
  }

  next_card_probs <- nextCardProbs(cards_remaining = cards_remaining_tmp,
                                   dealer_card = NA)

  max_without_bust <- 21 - dealer_total

  possible_dealer_results <- rep(0, 6)
  names(possible_dealer_results) <- 17:22

  # modify the dealer total by 10 (if they have an ace and are going to stand)
  dealer_total <- ifelse(is_ace &
                           (dealer_total + 10 < 22) &
                           (dealer_total >= 7 + hit_soft_17),
                         dealer_total + 10,
                         dealer_total)

  if(dealer_total >= 17) {
    possible_dealer_results[as.character(dealer_total)] <- 1
  } else {

    for(i in 1:min(max_without_bust, 10)) {

      cards_remaining_tmp <- cards_remaining
      cards_remaining_tmp[i] <- max(0, cards_remaining_tmp[i] - 1)
      is_ace_now <- is_ace | (i == 1)
      possible_dealer_results <- possible_dealer_results +
        next_card_probs[i] * dealer(dealer_total = dealer_total + i,
                                    cards_remaining = cards_remaining_tmp,
                                    is_ace = is_ace_now,
                                    hit_soft_17 = 1,
                                    is_first = TRUE)

    }
    possible_dealer_results["22"] <- 1 - sum(possible_dealer_results[1:5])
  }

  return(possible_dealer_results)
}


