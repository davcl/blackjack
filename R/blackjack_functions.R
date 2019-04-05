

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
#' @param blackjack_payout defaults to 6/5 (as at New York New York, where I played)
#' @return data frame with p_win_if_stand, p_win_if_hit, player_card_1,
#' player_card_2, dealer_card
#' @export
hitStandProbs <- function(p1, p2, d, cards_gone = c(), n_decks = 1,
                          blackjack_payout = 6/5) {

  cards_gone <- c(cards_gone, p1, p2, d)
  cards_remaining <- findRemainingCards(cards_gone, n_decks)

  stand_roi <- stand(player_total = p1 + p2,
                          dealer_card = d,
                          cards_remaining = cards_remaining)

  hit_roi <- hit(player_total = p1 + p2,
                 dealer_card = d,
                 cards_remaining = cards_remaining,
                 is_ace = any(c(p1, p2) == 2))

  split_roi <- ifelse(p1 == p2,
                      split(p = p1,
                            cards_remaining = cards_remaining,
                            dealer_card = d,
                            blackjack_payout = blackjack_payout),
                      NA)

  double_down_roi <- doubleDown(player_total = p1 + p2,
                                cards_remaining = cards_remaining,
                                dealer_card = d,
                                is_ace = any(c(p1, p2) == 2))


  return(data.frame(stand_roi = stand_roi,
                    hit_roi = hit_roi,
                    split_roi = split_roi,
                    double_down_roi = double_down_roi,
                    player_card_1 = p1,
                    player_card_2 = p2,
                    dealer_card = d))

}



#' @title split
#' @description returns unit ROI for a split
#' @author David Clement
#' @param p the value of the card being split
#' @param cards_remaining vector with number of each type of card remaining
#' @param dealer_card dealer card
#' @param blackjack_payout this is the one place in hit/stand/split/double down
#' decision process (without card counting) where blackjack odds are needed
#' @param n_hands I think this function can be written faster without this argument
#' but as a first pass I found it clearer this way. note also I am allowing for a
#' situation where you split once but then don't want to split again - I would
#' theorize this would never actually happen. You wouldn't split the first time.
#' @return expected winnings of the split
#' @export
split <- function(p, cards_remaining, dealer_card, blackjack_payout, n_hands = 2) {

  next_card_probs <- nextCardProbs(cards_remaining = cards_remaining,
                                   dealer_card = dealer_card)
  poss_cards <- which(next_card_probs > 0)

  r <- 0
  for(i in poss_cards) {

    # remove this card from the deck but only in this iteration of for loop
    cards_remaining_tmp <- cards_remaining
    cards_remaining_tmp[i] <- max(0, cards_remaining_tmp[i] - 1)
    is_ace = p == 1 | i == 1

    # if i == p, you can split again
    if(i == p) {

      # but take the max in case you don't want to split again
      hand1 <- max(split(p = p,
                         dealer_card = dealer_card,
                         cards_remaining = cards_remaining_tmp,
                         blackjack_payout = blackjack_payout),
                   hit(player_total = 2 * p,
                       dealer_card = dealer_card,
                       cards_remaining = cards_remaining_tmp,
                       is_ace = is_ace),
                   stand(player_total = 2 * p + 10 * is_ace,
                         dealer_card = dealer_card,
                         cards_remaining = cards_remaining_tmp))


    } else {

      hand1 <- max(hit(player_total = p + i,
                       dealer_card = dealer_card,
                       cards_remaining = cards_remaining_tmp,
                       is_ace = is_ace),
                   stand(player_total = p + i + 10 * is_ace,
                         dealer_card = dealer_card,
                         cards_remaining = cards_remaining_tmp,
                         payout_for_21 = blackjack_payout))

    }

    # if there was a second hand still in play (that hadn't received its 2nd card)
    # then call the split function on it regardless of whether "hand1" was able to split
    hand2 <- ifelse(n_hands == 2,
                    split(p = p,
                          dealer_card = dealer_card,
                          cards_remaining = cards_remaining_tmp,
                          blackjack_payout = blackjack_payout,
                          n_hands = 1),
                    0)

    r <- r + next_card_probs[i] * (hand1 + hand2)


  }

  return(r)


}



#' @title doubleDown
#' @description returns unit ROI for a double down (hit exactly one more card)
#' @author David Clement
#' @param player_total sum of the two cards so far
#' @param cards_remaining vector with number of each type of card remaining
#' @param dealer_card dealer card
#' @param is_ace \code{TRUE} if the hand contains an ace
#' @return expected winnings of the double down
#' @export
doubleDown <- function(player_total, cards_remaining, dealer_card, is_ace) {

  next_card_probs <- nextCardProbs(cards_remaining = cards_remaining,
                                   dealer_card = dealer_card)
  poss_cards <- which(next_card_probs > 0)

  r <- 0
  for(i in poss_cards) {

    cards_remaining_tmp <- cards_remaining
    cards_remaining_tmp[i] <- cards_remaining_tmp[i] - 1

    r_hand <- stand(player_total = player_total + i,
                    dealer_card = dealer_card,
                    cards_remaining = cards_remaining_tmp)


    if(is_ace & (player_total + 10 + i <= 21)) {
      r_soft_hand <- stand(player_total = player_total + 10 + i,
                               dealer_card = dealer_card,
                          cards_remaining = cards_remaining_tmp)

      r_hand <- max(r_hand, r_soft_hand)
    }

    r <- r + next_card_probs[i] * r_hand

  }

  # double the stake
  r <- 2 * r

  return(r)


}

#' @title nextCardProbs
#' @description not only do we have to adjust the \code{dealer} function when he's
#' showing 1 or 10, we have to slightly modify the player next card probs
#' @author David Clement
#' @param dealer_card the card the dealer has. if 1 then they can't get 10 next,
#' if 10 then they can't get 1 next. NA is treated like 2:9
#' @param cards_remaining vector with number of each type of card remaining
#' @return vector of adjusted player next card probabilities
#' @export
nextCardProbs <- function(cards_remaining, dealer_card = NA) {

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





#' @title hit
#' @description recursive function to find player win probability when they hit
#' @author David Clement
#' @param player_total sum of player cards so far in the hand
#' @param dealer_card what dealer card is showing
#' @param cards_remaining vector of counts for each of 1 through 10
#' @param is_ace boolean indicating whether one of player cards is an ace
#' @return expected return for player
#' @export
hit <- function(player_total, dealer_card, cards_remaining, is_ace) {

  next_card_probs <- nextCardProbs(cards_remaining = cards_remaining,
                                   dealer_card = dealer_card)

  # player total must be at least 12; otherwise we'd hit for sure
  max_without_bust <- 21 - player_total

  r <- 0
  if(max_without_bust > 0) {

    # possible cards that don't bust the player
    poss_cards <- intersect(1:min(max_without_bust, 10),
                            which(next_card_probs > 0))

    for(i in poss_cards) {

      cards_remaining_tmp <- cards_remaining
      cards_remaining_tmp[i] <- max(0, cards_remaining_tmp[i] - 1)
      is_ace_now <- is_ace | (i == 1)


      r <- r + next_card_probs[i] * max(hit(player_total = player_total + i,
                                            dealer_card = dealer_card,
                                            cards_remaining = cards_remaining_tmp,
                                            is_ace = is_ace_now),
                                        stand(player_total = ifelse(player_total + i <= 11 &
                                                                      is_ace_now,
                                                                    player_total + i + 10,
                                                                    player_total + i),
                                              dealer_card = dealer_card,
                                              cards_remaining = cards_remaining_tmp))


    }

    p_bust <- 1 - sum(next_card_probs[poss_cards])
    r <- r - p_bust


  } else {

    r <- stand(player_total = 21,
               dealer_card = dealer_card,
               cards_remaining = cards_remaining)
  }

  return(r)
}



#' @title stand
#' @description function to find player win probability if they stand
#' @author David Clement
#' @param player_total sum of player cards so far in the hand
#' @param dealer_card what dealer card is showing
#' @param cards_remaining vector of counts for each of 1 through 10
#' @param payout_for_21 defaults to 1 because 21 can't be blackjack usually
#' (only for splits, at least when we are considering hand only after player
#' see their first two cards)
#' @return expected return for player
#' @export
stand <- function(player_total, dealer_card, cards_remaining, payout_for_21 = 1) {

  dealer_pmf <- dealer(dealer_total = dealer_card,
                       cards_remaining = cards_remaining,
                       is_ace = dealer_card == 1,
                       is_first = TRUE)

  p_tie <- ifelse(player_total >= 17,
                  dealer_pmf[as.character(player_total)],
                  0)

  p_lose <- sum(dealer_pmf[as.numeric(names(dealer_pmf)) > player_total &
                             names(dealer_pmf) != "22"])

  p_win <- 1 - (p_tie + p_lose)

  r <- p_win * (1 + (player_total == 21) * (payout_for_21 - 1)) - p_lose

  return(as.numeric(r))

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

  next_card_probs <- nextCardProbs(cards_remaining = cards_remaining_tmp)

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

