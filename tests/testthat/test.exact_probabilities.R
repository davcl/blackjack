require(testthat)
context("exact_probabilities")


test_that("test.remaining_cards", {

  x <- findRemainingCards(c(1,2,3), n_decks = 1)
  expect_equal(x, c(3,3,3,4,4,4,4,4,4,16))

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
  p_stand <- stand(player_total = 15,
                   dealer_total = 10,
                   cards_remaining = cards_remaining)
  expect_equal(p_stand, 1/3)


  # 1/3 chance of tying at 21
  cards_remaining <- rep(0, 10)
  cards_remaining[c(5, 6, 10)] <- 1
  p_stand <- stand(player_total = 21,
                   dealer_total = 10,
                   cards_remaining = cards_remaining)
  expect_equal(p_stand, 1 - 1/6)


})


test_that("test.hit", {

  # 1/3 win with 21 (dealer can't get exactly 21),
  # 1/3 bust, 1/6 tie at 20, 1/6 win with 20 by dealer bust
  # => 1/2 win, 1/3 lose, 1/6 tie
  # => 1/2 + 1/12 win, 1/3 + 1/12 lose
  cards_remaining <- rep(0, 10)
  cards_remaining[c(5, 6, 10)] <- 1
  p_hit <- hit(player_total = 15,
               dealer_total = 10,
               cards_remaining = cards_remaining,
               is_ace = FALSE)
  expect_equal(p_hit, 1/2 + 1/12)

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
  p_hit <- hit(player_total = 5,
               dealer_total = 8,
               cards_remaining = cards_remaining,
               is_ace = TRUE)
  expect_equal(p_hit, 1 - 1/6)

  # dealer must have the 9
  cards_remaining <- rep(0, 10)
  cards_remaining[9] <- 1
  cards_remaining[10] <- 6
  p_hit <- hit(player_total = 12,
               dealer_total = 1,
               cards_remaining = cards_remaining,
               is_ace = FALSE)
  expect_equal(p_hit, 0)

  # dealer must have the 9. player has 1/7 chance of getting 21
  cards_remaining <- rep(0, 10)
  cards_remaining[9] <- 2
  cards_remaining[10] <- 6
  p_hit <- hit(player_total = 12,
               dealer_total = 1,
               cards_remaining = cards_remaining,
               is_ace = FALSE)
  expect_equal(p_hit, 0)

})




