require(testthat)
context("exact_probabilities")


test_that("test.remaining_cards", {

  x <- findRemainingCards(c(1, 2, 3), n_decks = 1)
  expect_equal(x, c(3, 3, 3, 4, 4, 4, 4, 4, 4, 16))

})

test_that("test.dealer", {

  # test that the two dealer possibilities are both correct
  cards_remaining <- rep(0, 10)
  cards_remaining[c(2,7)] <- 1
  dealer_pmf <- dealer(dealer_total = 15,
                       cards_remaining = cards_remaining,
                       is_ace = FALSE)
  expect_equal(as.numeric(dealer_pmf["17"]), 0.5)
  expect_equal(as.numeric(dealer_pmf["22"]), 0.5)


  # test that the dealer blackjack is properly removed
  cards_remaining <- rep(0, 10)
  cards_remaining[c(1,9,10)] <- 1
  dealer_pmf <- dealer(dealer_total = 10,
                       cards_remaining = cards_remaining,
                       is_ace = FALSE,
                       is_first = TRUE)
  expect_equal(as.numeric(dealer_pmf["19"]), 0.5)
  expect_equal(as.numeric(dealer_pmf["20"]), 0.5)


  # test one recursive call
  cards_remaining <- rep(0, 10)
  cards_remaining[c(5, 6, 10)] <- 1
  dealer_pmf <- dealer(dealer_total = 10,
                       cards_remaining = cards_remaining,
                       is_ace = FALSE)
  expect_equal(as.numeric(dealer_pmf), c(rep(0,3), rep(1/3,3)))

})


test_that("test.stand", {

  cards_remaining <- rep(0, 10)
  cards_remaining[c(5, 6, 10)] <- 1
  r_stand <- stand(player_total = 15,
                   dealer_card = 10,
                   cards_remaining = cards_remaining)
  expect_equal(r_stand, -1/3)


  # 1/3 chance of tying at 21
  cards_remaining <- rep(0, 10)
  cards_remaining[c(5, 6, 10)] <- 1
  r_stand <- stand(player_total = 21,
                   dealer_card = 10,
                   cards_remaining = cards_remaining)
  expect_equal(r_stand, 5/6 - (1 - 5/6))


})


test_that("test.hit", {

  # 1/3 win with 21 (dealer can't get exactly 21 unless player busts),
  # 1/3 bust, 1/6 tie at 20, 1/6 win with 20 by dealer bust
  # => 1/2 win, 1/3 lose, 1/6 tie
  # => 1/2 + 1/12 win, 1/3 + 1/12 lose
  cards_remaining <- rep(0, 10)
  cards_remaining[c(5, 6, 10)] <- 1
  r_hit <- hit(player_total = 15,
               dealer_card = 10,
               cards_remaining = cards_remaining,
               is_ace = FALSE)
  expect_equal(r_hit, 7/12 - (1 - 7/12))

})



test_that("test.soft_hands", {

  # dealer
  cards_remaining <- rep(0, 10)
  cards_remaining[c(7, 10)] <- 1
  dealer_pmf <- dealer(dealer_total = 7,
                       cards_remaining = cards_remaining,
                       is_ace = TRUE)
  expect_equal(as.numeric(dealer_pmf["17"]), 0.5)
  expect_equal(as.numeric(dealer_pmf["22"]), 0.5)

  cards_remaining <- rep(0, 10)
  cards_remaining[c(7, 10)] <- 1
  dealer_pmf <- dealer(dealer_total = 8,
                       cards_remaining = cards_remaining,
                       is_ace = TRUE)
  expect_equal(as.numeric(dealer_pmf["18"]), 1)

  # hit
  # will win as long as player doesn't get both tens
  cards_remaining <- rep(0, 10)
  cards_remaining[c(5, 6)] <- 1
  cards_remaining[10] <- 2
  r_hit <- hit(player_total = 5,
               dealer_card = 8,
               cards_remaining = cards_remaining,
               is_ace = TRUE)
  expect_equal(r_hit, 5/6 - (1 - 5/6))

  # dealer must have the 9
  cards_remaining <- rep(0, 10)
  cards_remaining[9] <- 1
  cards_remaining[10] <- 6
  r_hit <- hit(player_total = 12,
               dealer_card = 1,
               cards_remaining = cards_remaining,
               is_ace = FALSE)
  expect_equal(r_hit, -1)

  # dealer must have the 9. player has 1/7 chance of getting 21
  cards_remaining <- rep(0, 10)
  cards_remaining[9] <- 2
  cards_remaining[10] <- 6
  r_hit <- hit(player_total = 12,
               dealer_card = 1,
               cards_remaining = cards_remaining,
               is_ace = FALSE)
  expect_equal(r_hit, 1/7 - (1 - 1/7))

})




test_that("test.split", {

  cards_remaining <- rep(0, 10)
  cards_remaining[10] <- 6

  s <- split(p = 1,
             cards_remaining = cards_remaining,
             dealer_card = 5,
             blackjack_payout = 6/5)
  expect_equal(s, 2*6/5)


  cards_remaining <- rep(0, 10)
  cards_remaining[1] <- 1
  cards_remaining[10] <- 6

  s <- split(p = 1,
             cards_remaining = cards_remaining,
             dealer_card = 5,
             blackjack_payout = 6/5)
  expect_equal(s, 1/7*3*6/5 + 6/7*(1/6*3*6/5 + 5/6*2*6/5))


  cards_remaining <- rep(0, 10)
  cards_remaining[9] <- 6
  cards_remaining[5] <- 3

  s <- split(p = 10,
             cards_remaining = cards_remaining,
             dealer_card = 4,
             blackjack_payout = 100)
  expect_equal(s, 1/12*(18/42)*2 + 5/12*2*(1 - 3/7*2/6*1/5) + 1/2*(1 + 10/21 + 2/42 - 5/21 - 10/42))



})



test_that("test.doubleDown", {

  # make sure expected profit can be 2
  cards_remaining <- rep(0, 10)
  cards_remaining[8] <- 6

  s <- doubleDown(player_total = 12,
                  cards_remaining = cards_remaining,
                  dealer_card = 6,
                  is_ace = FALSE)
  expect_equal(s, 2)

  # make sure expected profit can be -2
  cards_remaining <- rep(0, 10)
  cards_remaining[4] <- 6

  s <- doubleDown(player_total = 12,
                  cards_remaining = cards_remaining,
                  dealer_card = 6,
                  is_ace = FALSE)
  expect_equal(s, -2)

  # one with a soft hand (3/5 probability of 21)
  # and 2/5 probability we go to a 3/4 chance of winning (win if dealer doesn't get the 10)
  cards_remaining <- rep(0, 10)
  cards_remaining[2] <- 3
  cards_remaining[10] <- 2

  s <- doubleDown(player_total = 9,
                  cards_remaining = cards_remaining,
                  dealer_card = 10,
                  is_ace = TRUE)
  expect_equal(s, 2*(3/5 + 2/5*3/4 - 2/5*1/4))


  # make sure it's twice the return from hit when you for sure don't hit twice
  cards_remaining <- rep(0, 10)
  cards_remaining[6] <- 6
  cards_remaining[7] <- 3

  s <- doubleDown(player_total = 12,
                  cards_remaining = cards_remaining,
                  dealer_card = 9,
                  is_ace = FALSE)
  h <- hit(player_total = 12,
           cards_remaining = cards_remaining,
           dealer_card = 9,
           is_ace = FALSE)
  expect_equal(s, 2 * h)

})



test_that("test.nextCardProbs", {

  cards_remaining <- rep(0, 10)
  cards_remaining[9] <- 1
  cards_remaining[10] <- 6

  p <- nextCardProbs(cards_remaining = cards_remaining,
                     dealer_card = 1)
  expect_equal(p[10], 1)

  p <- nextCardProbs(cards_remaining = cards_remaining,
                     dealer_card = 5)
  expect_equal(p[10], 6/7)

  cards_remaining <- rep(0, 10)
  cards_remaining[9] <- 1
  cards_remaining[1] <- 6
  p <- nextCardProbs(cards_remaining = cards_remaining,
                     dealer_card = 10)
  expect_equal(p[1], 1)


})


