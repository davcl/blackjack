

import numpy as np

# there are 400 card states:
# - 20 totals for player
# - 10 cards for dealer
# - 2 for is_ace or not for player
# - up to 4 possible actions
# and there are 10 different type of card probabilities we want to tally
# start the value at -2 so that impossible combinations are not chosen
values = [np.ones([21, 10, 2, 4]) - 2, np.zeros[10]]

# learning rate
alpha = 0.001

# state counts for epsilon greedy algorithm
epsilon = 1
state_counts = np.zeros([21, 10, 2])

# dictionary of actions
actions = {"hit":0, "stand":1, "doubledown":2, "split":3}


# randomize the order of the cards to be dealt in this episode
def setCardOrder(n_decks):
    cards = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10]
    remaining_cards = []

    for i in range(4 * n_decks):
        for j in cards:
            remaining_cards.append(j)

    np.random.shuffle(remaining_cards)
    return remaining_cards


# pop off the next card from the list
def dealNextCard(remaining_cards):
    return remaining_cards.pop(0)


# determine the values of the 410 different features
# only one of the first 400 will be 1. the rest are zero
# then the final 10 states are probabilities of each card
def findState(player_cards, dealer_card, remaining_cards):
    is_ace = int(1 in player_cards)
    player_total = sum(player_cards)

    n_cards_remaining = len(remaining_cards)

    counts = np.zeros(10)
    for i in range(0, 10):
        counts[i] = remaining_cards.count(i + 1)
    probs = counts/n_cards_remaining

    return [player_total - 2, dealer_card - 1, is_ace, probs]


def findPossibleActions(player):
    if len(player) > 2:
        return ["hit", "stand"]
    elif player[0] != player[1]:
        return ["hit", "stand", "doubledown"]
    else:
        return ["hit", "stand", "doubledown", "split"]


def chooseAction(state, player, epsilon):
    possible_actions = findPossibleActions(player)
    possible_indices = np.asarray([actions.get(key) for key in possible_actions])

    #find the action that leads to maximum value
    best_index = np.argmax(values[state[0], state[1], state[2], possible_indices])

    # with probabilty epsilon choose an action at random from possible_indices
    random_index = np.random.choice(possible_indices)

    if np.random.rand(1) < epsilon:
        chosen_index = random_index
    else:
        chosen_index = best_index

    return chosen_index


def changeValues(actions_this_hand, value_this_hand, probs):
    values[0] += actions_this_hand * alpha * (value_this_hand - values[0])
    values[1] += probs * alpha * (value_this_hand - values[1])


def isDealerHit(dealer):
  return (sum(dealer) < 17 & (1 not in dealer | sum(dealer) < 8))


def sumCards(cards):
  if 1 in cards and sum(cards) <= 11:
    return sum(cards) + 10
  else:
    return sum(cards)


def playHandAfterDeal(player, dealer, remaining_cards, remaining_cards_start_of_hand):

      stake = 1

      # while the hand is not over keeping dealing
      while True:

          # count the number of times through this state for the GLIE
          # (Greedy in the Limit with infinite expectation)
          state = findState(player, dealer, remaining_cards_start_of_hand)
          state_counts[state[0], state[1], state[2]] += 1

          # choose the next action
          action_index = chooseAction(state, player, epsilon/state_counts[state[0], state[1], state[2]])
          action_name = list(actions.keys())[list(actions.values()).index(action_index)]
          actions_this_hand[state[0], state[1], state[2], action_index] += 1

          if action_name == "stand":
              while isDealerHit(dealer):
                  dealer.append(dealNextCard(remaining_cards))
              break

          elif action_name == "hit":
              player.append(dealNextCard(remaining_cards))
              if sumCards(player) > 21:
                  break
              else:
                  continue

          elif action_name == "doubledown":
              player.append(dealNextCard(remaining_cards))
              stake = 2
              if sumCards(player) > 21:
                  break
              else:
                  while isDealerHit(dealer) < 17:
                      dealer.append(dealNextCard(remaining_cards))
                  break

          elif action_name == "split":
              player1 = player[0]
              player1.append(dealNextCard(remaining_cards))
              playHandAfterDeal(player1, dealer, remaining_cards,
                          remaining_cards_start_of_hand)
              player2 = player[1]
              player2.append(dealNextCard(remaining_cards))
              playHandAfterDeal(player2, dealer, remaining_cards,
                          remaining_cards_start_of_hand)


      # judge the winner of the hand and assign value
      if sumCards(player) > 21:
          value_this_hand = -stake
      elif sumCards(dealer) > 21:
          value_this_hand = stake
      elif sumCards(player) == sumCards(dealer):
          value_this_hand = 0
      elif sumCards(player) > sumCards(dealer):
          value_this_hand = stake
      else:
          value_this_hand = -stake

      changeValues(actions_this_hand, value_this_hand, state[3])



def playSeries(n_decks, min_num_cards):

    remaining_cards = setCardOrder(n_decks)

    # deal a new hand if we still have enough cards left in the deck
    while len(remaining_cards) > min_num_cards:

        player = []
        dealer = []
        actions_this_hand = np.zeros([21, 10, 2, 4])

        # this is the state with what we know before the hand
        # I put this before dealing because we actually want to figure out
        # pre-hand when there is an edge
        remaining_cards_start_of_hand = remaining_cards

        # deal the cards for this hand
        player.append(dealNextCard(remaining_cards))
        dealer.append(dealNextCard(remaining_cards))
        player.append(dealNextCard(remaining_cards))
        dealer.append(dealNextCard(remaining_cards))

        playHandAfterDeal(player, dealer, remaining_cards,
                          remaining_cards_start_of_hand)


for x in range(10):
    playSeries(6, 52)

print(values)

# for each state, find the argmax action
# this will show us

for i in range(21):
  for j in range(10):
    for k in range(2):


# values[1] can dot product with probs to show value
# values[0] can be ranked at each i,j,k combination to show the best strategy
