#include <Rcpp.h>
using namespace Rcpp;

//' The probabilities of each card coming next
//'
//' @param cards_remaining vector count of each type of card
//' @param dealer_card value of dealer's card showing
//' @return vector of probabilities
//' @export
// [[Rcpp::export]]
std::vector<double> nextCardProbs(std::vector<int> cards_remaining, int dealer_card) {

  std::vector<double> next_card_probs(10);
  double tot = std::accumulate(cards_remaining.begin(), cards_remaining.end(), 0.0);


  for(int i = 0; i < 10; ++i) {
    next_card_probs[i] = cards_remaining[i] / tot;
  }

  if(std::isnan(dealer_card)  || ( (dealer_card != 1) && (dealer_card != 10) ) ) {
    return next_card_probs;
  }


  int not_dealer_card;
  if(dealer_card == 1) {
    not_dealer_card = 10;
  } else {
    not_dealer_card = 1;
  }

  // find the sum of other card probs before modifying not_dealer_card
  double sum_other_card_probs = 1 - next_card_probs[not_dealer_card - 1];

  // increase probability of getting the card dealer can't have
  next_card_probs[not_dealer_card - 1] = next_card_probs[not_dealer_card - 1] * tot / (tot - 1);

  // decrease probabilities of all other cards by the same multiplicative amount
  // while maintaining the sum to 1 constraint
  for( int i = 1; i <= 10; ++i ) {
    if(i != not_dealer_card) {
      next_card_probs[i - 1] = next_card_probs[i - 1] * (1 - next_card_probs[not_dealer_card - 1]) / sum_other_card_probs;
    }
  }

  return next_card_probs;

}



//' vector of probabilities for the 6 different dealer possibilities
//'
//' @param dealer_total dealer's total so far
//' @param cards_remaining vector count of each type of card
//' @param is_ace boolean indicating whether hand is soft
//' @param hit_soft_17 int (0 or 1) indicating dealer behaviour; defaults to true
//' @param is_first boolean indicating whether this is dealer's first unknown card
//' @return vector of probabilities (length 6)
//' @export
// [[Rcpp::export]]
std::vector<double> dealer(int dealer_total, std::vector<int> cards_remaining, bool is_ace, int hit_soft_17, bool is_first) {

  std::vector<int> cards_remaining_tmp;
  cards_remaining_tmp = cards_remaining;

  // if dealer total is 1 they can't have a 10
  // if dealer total is 10 they can't have a 1
  if( (is_first == true) && (dealer_total == 1) ) {
    cards_remaining_tmp[9] = 0;
  } else if( (is_first == true) && (dealer_total == 10) ) {
    cards_remaining_tmp[0] = 0;
  }


  std::vector<double> next_card_probs = nextCardProbs(cards_remaining_tmp, dealer_total);

  int max_without_bust = 21 - dealer_total;

  std::vector<double> dealer_results(6);
  std::fill(dealer_results.begin(), dealer_results.end(), 0.0);

  // modify the dealer total by 10 (if they have an ace and are going to stand)
  if(is_ace == true && (dealer_total + 10 < 22) & (dealer_total >= 7 + hit_soft_17) ) {
    dealer_total += 10;
  }


  if(dealer_total >= 17) {
    dealer_results[dealer_total - 17] = 1;
  } else {

    for(int i = 1; i <= std::min(max_without_bust, 10); ++i) {

      std::vector<int> cards_remaining_tmp;
      cards_remaining_tmp = cards_remaining;

      cards_remaining_tmp[i] = std::max(0, cards_remaining_tmp[i] - 1);
      int is_ace_now = is_ace || (i == 1);

      std::vector<double> recursive_dealer_results = dealer(dealer_total + i, cards_remaining_tmp, is_ace_now, hit_soft_17, FALSE);

      for(int j = 17; j <= 21; ++j) {
        dealer_results[j - 17] = next_card_probs[i - 1] * recursive_dealer_results[j - 17];
      }
    }

  }

  dealer_results[5] = 1 - std::accumulate(dealer_results.begin(), dealer_results.begin() + 5, 0.0);

  return dealer_results;

}




