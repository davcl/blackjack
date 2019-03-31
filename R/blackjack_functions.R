

#' @title hitStandProbs
#' @description compares the hand win probability for hit and stand given the
#' player and dealer starting cards
#' @author David Clement
#' @param p1 first player card (order with p2 doesn't matter)
#' @param p2 second player card
#' @param d dealer card
#' @param cards_gone will be modified to include p1, p2, d. But could also
#' pass initial vector here for card counting purposes
#' @param n_decks default to 6
#' @return data frame with p_win_if_stand, p_win_if_hit, player_card_1,
#' player_card_2, dealer_card
#' @export
hitStandProbs <- function(p1, p2, d, cards_gone = c(), n_decks = 6) {

  cards_gone <- c(cards_gone, p1, p2, d)
  cards_remaining <- findRemainingCards(cards_gone, n_decks)

  p_win_if_stand <- stand(player_total = p1 + p2,
                          dealer_total = d,
                          cards_remaining = cards_remaining)

  p_win_if_hit <- hit(player_total = p1 + p2,
                      dealer_total = d,
                      cards_remaining = cards_remaining)

  return(data.frame(p_win_if_stand = p_win_if_stand,
                    p_win_if_hit = p_win_if_hit,
                    player_card_1 = p1,
                    player_card_2 = p2,
                    dealer_card = d))

}

#' @title adjNextCardProbsForNoDealerBlackjack
#' @description not only do we have to adjust the \code{dealer} function when he's
#' showing 1 or 10, we have to slightly modify the player next card probs
#' @author David Clement
#' @param next_card_probs before accounting for "not dealer card"
#' @param dealer_card the card the dealer has. if 1 then they can't get 10 next,
#' if 10 then they can't get 1 next
#' @param cards_remaining vector with number of each type of card remaining
#' @return vector of adjusted player next card probabilities
#' @export
adjNextCardProbsForNoDealerBlackjack <- function(next_card_probs, dealer_card,
                                                 cards_remaining) {

  if(dealer_card %in% 2:9) {
    return(next_card_probs)
  }

  not_dealer_card <- ifelse(dealer_card == 1,
                            10,
                            1)
  other_cards <- (1:10)[-not_dealer_card]

  # increase probability of getting the card dealer can't have
  next_card_probs[not_dealer_card] <- next_card_probs[not_dealer_card] *
    sum(cards_remaining) / (sum(cards_remaining) - cards_remaining[not_dealer_card])

  # decrease probabilities of all other cards by the same multiplicative amount
  # while maintaining the sum to 1 constraint
  next_card_probs[other_cards] <- next_card_probs[other_cards] *
    (1 - next_card_probs[not_dealer_card])/sum(next_card_probs[other_cards])

  return(next_card_probs)
}


#' @title hit
#' @description recursive function to find player win probability when they hit
#' @author David Clement
#' @param player_total sum of player cards so far in the hand
#' @param dealer_total what dealer card is showing
#' @param cards_remaining vector of counts for each of 1 through 10
#' @param is_ace boolean indicating whether one of player cards is an ace
#' @return a player win probability
#' @export
hit <- function(player_total, dealer_total, cards_remaining, is_ace) {

  if(player_total < 12 & !is_ace) {
    stop("player total too low to stand, so comparison not needed")
  }

  next_card_probs <- cards_remaining / sum(cards_remaining)
  next_card_probs <- adjNextCardProbsForNoDealerBlackjack(next_card_probs = next_card_probs,
                                                          dealer_card = dealer_total,
                                                          cards_remaining = cards_remaining)


  if(dealer_total == 1) {
    next_card_probs[10] <- next_card_probs[10] * sum(cards_remaining)/
      (sum(cards_remaining) - 1)
    next_card_probs[1:9] <- next_card_probs[1:9] *
      (1 - next_card_probs[10])/sum(next_card_probs[1:9])
  } else if(dealer_total == 10) {
    next_card_probs[1] <- next_card_probs[1] * sum(cards_remaining)/
      (sum(cards_remaining) - 1)
    next_card_probs[2:10] <- next_card_probs[2:10] *
      (1 - next_card_probs[1])/sum(next_card_probs[2:10])
  }

  # player total must be at least 12; otherwise we'd hit for sure
  max_without_bust <- 21 - player_total

  to_return <- 0
  if(max_without_bust > 0) {

    poss_cards <- intersect(1:min(max_without_bust, 10),
                            which(next_card_probs > 0))

    for(i in poss_cards) {

      cards_remaining_tmp <- cards_remaining
      cards_remaining_tmp[i] <- max(0, cards_remaining_tmp[i] - 1)
      is_ace_now <- is_ace | (i == 1)


      to_return <- to_return +
        next_card_probs[i] * max(hit(player_total = player_total + i,
                                     dealer_total = dealer_total,
                                     cards_remaining = cards_remaining_tmp,
                                     is_ace = is_ace_now),
                                 stand(player_total = ifelse(player_total + i <= 11 &
                                                               is_ace_now,
                                                             player_total + i + 10,
                                                             player_total + i),
                                       dealer_total = dealer_total,
                                       cards_remaining = cards_remaining_tmp))

    }


  } else {

    to_return <- stand(player_total = 21,
                       dealer_total = dealer_total,
                       cards_remaining = cards_remaining)
  }

  return(to_return)
}


#' @title stand
#' @description function to find player win probability if they stand
#' @author David Clement
#' @param player_total sum of player cards so far in the hand
#' @param dealer_total what dealer card is showing
#' @param cards_remaining vector of counts for each of 1 through 10
#' @return a player win probability
#' @export
stand <- function(player_total, dealer_total, cards_remaining) {

  dealer_pmf <- dealer(dealer_total = dealer_total,
                       cards_remaining = cards_remaining,
                       is_ace = dealer_total == 1,
                       is_first = TRUE)

  p_tie <- ifelse(player_total >= 17,
                  dealer_pmf[as.character(player_total)],
                  0)

  p_player_wins <- (sum(dealer_pmf[as.numeric(names(dealer_pmf)) < player_total]) +
                      dealer_pmf["22"]) + 0.5 * p_tie

  return(as.numeric(p_player_wins))

}



#' @title dealer
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
dealer <- function(dealer_total, cards_remaining, is_ace, hit_soft_17 = TRUE, is_first = FALSE) {

  cards_remaining_tmp <- cards_remaining

  # if dealer total is 1 they can't have a 10
  # if dealer total is 10 they can't have a 1
  if(is_first & dealer_total == 1) {
    cards_remaining_tmp[10] <- 0
  } else if(is_first & dealer_total == 10) {
    cards_remaining_tmp[1] <- 0
  }

  next_card_probs <- cards_remaining_tmp / sum(cards_remaining_tmp)

  max_without_bust <- 21 - dealer_total
  #if(max_without_bust < 10) {
  #  p_bust <- sum(next_card_probs[(max_without_bust + 1):10])
  #} else {
  #  p_bust <- 0
  #}

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
                                    is_ace = is_ace_now)

    }
    possible_dealer_results["22"] <- 1 - sum(possible_dealer_results[1:5])
  }

  return(possible_dealer_results)
}


#' @title findAllCards
#' @description find a vector of length 10 with card counts at start of game
#' @author David Clement
#' @param n_decks number of decks in play
#' @return a named vector of length 10 with card counts at start of game
#' @export
findAllCards <- function(n_decks) {
  return(c(rep(4 * n_decks, 9), 16 * n_decks))
}


#' @title findRemainingCards
#' @description vector of length 10 with counts of remaining cards
#' @author David Clement
#' @param cards_gone vector of cards gone, e.g. c(1,10,5) would be one ace,
#' one 10/face card, and one 5 already showing
#' @param n_decks number of decks in play
#' @return a named vector of length 10 with card counts at the current state
#' of the game
#' @export
findRemainingCards <- function(cards_gone, n_decks) {

  all_cards <- findAllCards(n_decks)

  cards_gone_counts <- rep(0, 10)
  tmp <- sort(table(cards_gone))
  cards_gone_counts[as.numeric(names(tmp))] <- tmp

  return(all_cards - cards_gone_counts)
}

