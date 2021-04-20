// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

// Corresponds to Proposal.hs module

#include "types.mligo"
#include "common.mligo"
#include "token/fa2.mligo"
#include "token.mligo"
#include "permit.mligo"

// -----------------------------------------------------------------
// Helper
// -----------------------------------------------------------------

[@inline]
let to_proposal_key (propose_params, sender_addr : propose_params * address): proposal_key =
  Crypto.blake2b (Bytes.pack (propose_params, sender_addr))

[@inline]
let check_if_proposal_exist (proposal_key, store : proposal_key * storage): proposal =
  match Map.find_opt proposal_key store.proposals with
    Some p -> p
  | None ->
      (failwith("PROPOSAL_NOT_EXIST") : proposal)

// gets the current (baking) cycle
let get_current_cycle(voting_period_params: voting_period_params) : nat =
  match is_nat((Tezos.level - voting_period_params.levels_per_cycle_change_at.1) : int) with
        | Some (elapsed_levels) -> voting_period_params.levels_per_cycle_change_at.0 + (elapsed_levels/voting_period_params.levels_per_cycle)
        | None -> ([%Michelson ({| { FAILWITH } |} : string * unit -> nat)]
            ("STARTED_ON_IN_FUTURE", ()))

let get_current_period_num(voting_period_params: voting_period_params) : nat =
  match is_nat(get_current_cycle(voting_period_params) - voting_period_params.cycles_per_period_change_at.1) with
    | Some (elapsed_cycles) -> voting_period_params.cycles_per_period_change_at.0 + elapsed_cycles/voting_period_params.cycles_per_period
    | None -> ([%Michelson ({| { FAILWITH } |} : string * unit -> nat)]
        ("STARTED_ON_IN_FUTURE", ()))

// If a period consists of `n` cycles, this function returns a value between 0 and n
let get_current_period_cycle(voting_period_params: voting_period_params) : nat =
  match is_nat(get_current_cycle(voting_period_params) - voting_period_params.cycles_per_period_change_at.1) with
    | Some (elapsed_cycles) -> elapsed_cycles mod voting_period_params.cycles_per_period
    | None -> ([%Michelson ({| { FAILWITH } |} : string * unit -> nat)]
        ("STARTED_ON_IN_FUTURE", ()))

// Checks that we are in proposing period. We split each period of 5 cycles (as of now)
// into two segments. First one for proposing, and second one for voting. The length of
// voting_period, in number of cycles, is configured in store.voting_period.
let is_proposing_segment(store : storage): bool =
  // Compute cycles available for proposing
  let proposing_cycles = match is_nat(store.voting_period_params.cycles_per_period - store.voting_period) with
    | Some (pc) -> pc
    | None -> (failwith("BAD_VOTING_PERIOD") : nat)
  // Check if we are past that many cycles into the period, because then we wont by in proposing period.
  in (get_current_period_cycle(store.voting_period_params) < proposing_cycles)

[@inline]
let ensure_proposal_voting_segment (proposal, store : proposal * storage): storage =
  let current_period = get_current_period_num(store.voting_period_params) in
  if current_period = (proposal.period_num) && not is_proposing_segment(store)
  then store
  else (failwith("VOTING_PERIOD_OVER") : storage)

let ensure_proposing_segment(store : storage): storage =
  if (is_proposing_segment(store))
  then store
  else (failwith("NOT_PROPOSING_PERIOD") : storage)

[@inline]
let ensure_proposal_is_unique (propose_params, store : propose_params * storage): proposal_key =
  let proposal_key = to_proposal_key(propose_params, Tezos.sender) in
  if Map.mem proposal_key store.proposals
    then (failwith("PROPOSAL_NOT_UNIQUE") : proposal_key)
    else proposal_key

// Utility function for quorum_threshold comparison.
// Returns two nats, which can be compared to check for the desired condition.
// e.g. nat_1 > nat_2 implies that qt_1 > qt_2
let cmp_qt(qt_1, qt_2 : quorum_threshold * quorum_threshold): (nat * nat) =
  // Calculated this way because there is no support for floating point operations
  (qt_1.numerator * qt_2.denominator, qt_1.denominator * qt_2.numerator)

[@inline]
// Returns true iff the first quorum_threshold is strictly bigger than the second.
let is_gt_qt(qt_1, qt_2 : quorum_threshold * quorum_threshold): bool =
  let (nat_1, nat_2) = cmp_qt (qt_1, qt_2) in
  nat_1 > nat_2

[@inline]
// Returns true iff the first quorum_threshold is bigger or equal than the second.
let is_ge_qt(qt_1, qt_2 : quorum_threshold * quorum_threshold): bool =
  let (nat_1, nat_2) = cmp_qt (qt_1, qt_2) in
  nat_1 >= nat_2

[@inline]
// Returns true iff the first quorum_threshold is strictly less than the second.
let is_le_qt(qt_1, qt_2 : quorum_threshold * quorum_threshold): bool =
  let (nat_1, nat_2) = cmp_qt (qt_1, qt_2) in
  nat_1 <= nat_2

// -----------------------------------------------------------------
// Freeze history operations
// -----------------------------------------------------------------

let sub_frozen_fh (amt, fh : nat * address_freeze_history)
    : address_freeze_history =
  match is_nat(fh.unstaked - amt) with
  | None ->
      ([%Michelson ({| { FAILWITH } |} : (string * unit) -> address_freeze_history)]
        ( "NOT_ENOUGH_FROZEN_TOKENS", ()) : address_freeze_history)
  | Some new_amt ->
      { fh with unstaked = new_amt }

let stake_frozen_fh (amt, fh : nat * address_freeze_history): address_freeze_history =
  let fh = sub_frozen_fh(amt, fh) in
  { fh with staked = fh.staked + amt }

let unstake_frozen_fh (amt, fh : nat * address_freeze_history)
    : address_freeze_history =
  match is_nat(fh.staked - amt) with
  | None ->
      ([%Michelson ({| { FAILWITH } |} : (string * unit) -> address_freeze_history)]
        ("NOT_ENOUGH_STAKED_TOKENS", ()) : address_freeze_history)
  | Some new_amt ->
      { fh with staked = new_amt; unstaked = fh.unstaked + amt }

// -----------------------------------------------------------------
// Propose
// -----------------------------------------------------------------

[@inline]
let check_is_proposal_valid (config, propose_params, store : config * propose_params * storage): storage =
  if config.proposal_check (propose_params, store.extra)
    then store
    else (failwith("FAIL_PROPOSAL_CHECK") : storage)

[@inline]
let check_proposal_limit_reached (config, store : config * storage): storage =
  if config.max_proposals <= List.length store.proposal_key_list_sort_by_date
    then (failwith("MAX_PROPOSALS_REACHED") : storage)
    else store

let ensure_sender_key_hash(key_hash, addr : key_hash * address) : address =
  if (Tezos.address(Tezos.implicit_account(key_hash)) = addr)
    then addr
    else (failwith("BAD_SENDER_KEY_HASH") : address)

let get_frozen_tokens_for_current_period(curr_period, addr, store: nat * address * storage) : nat =
  match Big_map.find_opt addr store.freeze_history with
    | Some fh -> if fh.current_period_num = curr_period
        then fh.staked + fh.unstaked
        else 0n
    | None -> 0n

// Ensures that the total number of frozen tokens for the current period for caller does not exceed the
// VOTING_POWER for them in the current period.
let freeze_on_ledger (tokens, addr, key_hash, store: nat * address * key_hash * storage)
    : storage =
  let sender_ = ensure_sender_key_hash(key_hash, addr) in
  let voting_power (n : key_hash) : nat = [%Michelson ({| { VOTING_POWER } |} : key_hash -> nat) ] n in
  let sender_voting_power = voting_power(key_hash) in
  let current_period = get_current_period_num(store.voting_period_params) in
  let current_frozen_tokens = get_frozen_tokens_for_current_period(current_period, sender, store) in
  let (ledger, total_supply) = if (current_frozen_tokens + tokens <= sender_voting_power)
    then credit_to (tokens, sender_, frozen_token_id, store.ledger, store.total_supply)
    else (failwith "CANT_EXCEED_VOTING_POWER" : (ledger * total_supply))
  in {store with ledger = ledger; total_supply = total_supply}

let stake_tk(token_amount, addr, store : nat * address * storage): storage =
  let current_period = get_current_period_num(store.voting_period_params) in
  let new_freeze_history = match Big_map.find_opt addr store.freeze_history with
    | Some fh ->
        let fh = stake_frozen_fh(token_amount, fh) in
        Big_map.update addr (Some(fh)) store.freeze_history
    | None ->
      if token_amount = 0n
      then store.freeze_history
      else ([%Michelson ({| { FAILWITH } |} : (string * unit) -> freeze_history)]
              ("NOT_ENOUGH_FROZEN_TOKENS", ()) : freeze_history)
  in { store with freeze_history = new_freeze_history }

[@inline]
let unfreeze_on_ledger (tokens, addr, ledger, total_supply, frozen_token_id, governance_token : nat * address * ledger * total_supply * token_id * governance_token): (ledger * total_supply) =
  let (ledger, total_supply) = debit_from (tokens, addr, frozen_token_id, ledger, total_supply) in
  (ledger, total_supply)

let add_proposal (propose_params, store : propose_params * storage): storage =
  let proposal_key = ensure_proposal_is_unique (propose_params, store) in
  let current_period = get_current_period_num(store.voting_period_params) in
  let store = ensure_proposing_segment(store) in
  let timestamp = Tezos.now in
  let proposal : proposal =
    { upvotes = 0n
    ; downvotes = 0n
    ; start_date = timestamp
    ; period_num = current_period // TODO@SRAS Check if we should change this.
    ; metadata = propose_params.proposal_metadata
    ; proposer = Tezos.sender
    ; proposer_frozen_token = propose_params.frozen_token
    ; proposer_fixed_fee_in_token = store.fixed_proposal_fee_in_token
    ; voters = ([] : voter list)
    } in
  { store with
    proposals =
      Map.add proposal_key proposal store.proposals
  ; proposal_key_list_sort_by_date =
      Set.add (timestamp, proposal_key) store.proposal_key_list_sort_by_date
  }

let propose (param, config, store : propose_params * config * storage): return =
  let store = check_is_proposal_valid (config, param, store) in
  let store = check_proposal_limit_reached (config, store) in
  let amount_to_freeze = param.frozen_token + store.fixed_proposal_fee_in_token in
  let store = stake_tk(amount_to_freeze, Tezos.sender, store) in
  let store = add_proposal (param, store) in
  ( ([] : operation list)
  , store
  )

// -----------------------------------------------------------------
// Vote
// -----------------------------------------------------------------

let submit_vote (proposal, vote_param, author, store : proposal * vote_param * address * storage): storage =
  let proposal_key = vote_param.proposal_key in

  let proposal =
    if vote_param.vote_type
      then { proposal with upvotes = proposal.upvotes + vote_param.vote_amount }
      else { proposal with downvotes = proposal.downvotes + vote_param.vote_amount }
    in
  let voter =
    { voter_address = author
    ; vote_amount = vote_param.vote_amount
    ; vote_type = vote_param.vote_type
    } in
  let proposal =
    { proposal with
      voters = voter :: proposal.voters
    } in
  let store = stake_tk(vote_param.vote_amount, author, store) in
  { store with proposals = Map.add proposal_key proposal store.proposals }

[@inline]
let check_vote_limit_reached
    (config, proposal, vote_param : config * proposal * vote_param): vote_param =
  if config.max_votes < proposal.upvotes + proposal.downvotes + vote_param.vote_amount
    then (failwith("MAX_VOTES_REACHED") : vote_param)
    else vote_param

let vote(votes, config, store : vote_param_permited list * config * storage): return =
  let accept_vote = fun (store, pp : storage * vote_param_permited) ->
    let (param, author, store) = verify_permit_protected_vote (pp, store) in
    let proposal = check_if_proposal_exist (param.proposal_key, store) in
    let vote_param = check_vote_limit_reached (config, proposal, param) in
    let store = ensure_proposal_voting_segment (proposal, store) in
    let store = submit_vote (proposal, vote_param, author, store) in
    store
    in
  ( ([] : operation list)
  , List.fold accept_vote votes store
  )

// -----------------------------------------------------------------
// Admin entrypoints
// -----------------------------------------------------------------

// Update a fixed fee in the native token for submitting
// a proposal. The new fee affects only those proposals
// submitted after the call.
[@inline]
let set_fixed_fee_in_token(new_fee, store : nat * storage): return =
  let store = authorize_admin store in
  let store = { store with fixed_proposal_fee_in_token = new_fee } in
  (([] : operation list), store)

// Update voting period of all ongoing and new proposals.
[@inline]
let set_voting_period(new_period, config, store : voting_period * config * storage): return =
  let store = authorize_admin store in

  let store =
    if new_period >= store.voting_period_params.cycles_per_period // there should be atleast one cycle for proposing.
      then (failwith("OUT_OF_BOUND_VOTING_PERIOD") : storage)
      else store
    in
  let store = { store with voting_period = new_period;} in
  (([] : operation list), store)

// Update quorum_threshold. The new quorum_threshold affects
// all ongoing and new proposals.
[@inline]
let set_quorum_threshold(new_threshold, config, store : quorum_threshold * config * storage): return =
  let store = authorize_admin store in
  if   is_le_qt (new_threshold, config.max_quorum_threshold)
    && is_ge_qt (new_threshold, config.min_quorum_threshold)
    && (new_threshold.numerator < new_threshold.denominator)
  then (nil_op, { store with quorum_threshold = new_threshold })
  else (failwith("OUT_OF_BOUND_QUORUM_THRESHOLD") : (operation list * storage))

[@inline]
let burn_frozen_token (tokens, addr, store : nat * address * storage): storage =
  let (ledger, total_supply) = debit_from(tokens, addr, store.frozen_token_id, store.ledger, store.total_supply)
  in {store with ledger = ledger; total_supply = total_supply}

// Burn up to desired_burn_amount of tokens. The desired burn amount comprises
// slash amount calculated by "config.rejected_proposal_return_value" and
// the fixed fee payed for the proposal. The case when the desired burn amount
// is larger than the available frozen tokens is possible because the contract
// administrator can transfer the proposer's frozen tokens.
[@inline]
let burn_what_possible (desired_burn_amount, frozen_tokens, addr, store : nat * nat * address * storage): storage =
  let to_burn =
    if frozen_tokens >= desired_burn_amount
    then desired_burn_amount
    else frozen_tokens
  in burn_frozen_token (to_burn, addr, store)

let unstake_tk(token_amount, addr, store : nat * address * storage): storage =
  let current_period = get_current_period_num(store.voting_period_params) in
  match Big_map.find_opt addr store.freeze_history with
    | Some(fh) ->
        let fh = unstake_frozen_fh(token_amount, fh) in
        let new_freze_history = Big_map.update addr (Some(fh)) store.freeze_history in
        { store with freeze_history = new_freze_history }
    | None -> ([%Michelson ({| { FAILWITH } |} : (string * unit) -> storage)]
          ("NOT_ENOUGH_STAKED_TOKENS", ()) : storage)

let unfreeze_proposer_and_voter_token
  (rejected_proposal_return, is_accepted, proposal, store :
    (proposal * contract_extra -> nat) * bool * proposal * storage): storage =
  // unfreeze_proposer_token
  let (tokens, store) =
    if is_accepted
    then (proposal.proposer_frozen_token + proposal.proposer_fixed_fee_in_token, store)
    else
      let slash_amount = rejected_proposal_return (proposal, store.extra) in
      let fee = proposal.proposer_fixed_fee_in_token in
      let frozen_tokens = proposal.proposer_frozen_token + fee in
      let desired_burn_amount = slash_amount + fee in
      let store =
        burn_what_possible
          (desired_burn_amount, frozen_tokens, proposal.proposer, store) in
      let tokens =
            match Michelson.is_nat(frozen_tokens - desired_burn_amount) with
              Some value -> value
            | None -> 0n
            in
      (tokens, store)
    in
  let store = unstake_tk(tokens, proposal.proposer, store) in

  // unfreeze_voter_token
  let do_unfreeze = fun
        ( store, voter
        : storage * voter
        ) -> unstake_tk(voter.vote_amount, voter.voter_address, store) in

  List.fold do_unfreeze proposal.voters store

[@inline]
let is_voting_period_over (proposal, store : proposal * storage): bool =
  let current_period = get_current_period_num(store.voting_period_params)
  in (current_period = proposal.period_num && (not(is_proposing_segment(store))))

[@inline]
let do_total_vote_meet_quorum_threshold (proposal, store : proposal * storage): bool =
  let votes_placed = proposal.upvotes + proposal.downvotes in
  // The following appear to cause an error when the embedded Michelson expression is not a function.
  // Hence it is made as a function that accept a unit argument.
  let total_voting_power : nat = [%Michelson ({| { DROP; TOTAL_VOTING_POWER } |} : unit -> nat) ] unit in
  let total_supply =
        match Map.find_opt store.frozen_token_id store.total_supply with
        | Some v -> v
        | None -> 0n
  in
  // Note: this is equivalent to checking that the number of votes placed is
  // bigger or equal than the total supply of frozen tokens multiplied by the
  // quorum_threshold proportion.
  let reached_quorum = {numerator = votes_placed; denominator = total_voting_power} in
  is_ge_qt(reached_quorum, store.quorum_threshold)

// Delete a proposal from 'sProposalKeyListSortByDate'
[@inline]
let delete_proposal
    (start_date, proposal_key, store : timestamp * proposal_key * storage): storage =
  { store with proposal_key_list_sort_by_date =
    Set.remove (start_date, proposal_key) store.proposal_key_list_sort_by_date
  }

[@inline]
let handle_proposal_is_over
    (config, start_date, proposal_key, store, ops, counter
      : config * timestamp * proposal_key * storage * operation list * counter
    )
    : (operation list * storage * counter) =
  let proposal = check_if_proposal_exist (proposal_key, store) in
  if    is_voting_period_over (proposal, store)
     && counter.current < counter.total // not finished
  then
    let counter = { counter with current = counter.current + 1n } in
    let cond =    do_total_vote_meet_quorum_threshold(proposal, store)
               && proposal.upvotes > proposal.downvotes
    in
    let store = unfreeze_proposer_and_voter_token
          (config.rejected_proposal_return_value, cond, proposal, store) in
    let (new_ops, store) =
      if cond
      then
        let (ops, (new_voting_period_params_opt, new_extra)) = config.decision_lambda (proposal, store.extra)
        in match new_voting_period_params_opt with
            | Some(new_voting_period_params) -> (ops, { store with extra = new_extra; voting_period_params = new_voting_period_params })
            | None -> (ops, { store with extra = new_extra; })
      else (([] : operation list), store)
    in
    let cons = fun (l, e : operation list * operation) -> e :: l in
    let ops = List.fold cons ops new_ops in
    let store = delete_proposal (start_date, proposal_key, store) in
    (ops, store, counter)
  else (ops, store, counter)

// Flush all proposals that passed their voting period.
let flush(n, config, store : nat * config * storage): return =
  let store =
    if n = 0n
      then
        (failwith("BAD_ENTRYPOINT_PARAMETER") : storage)
      else store
    in

  let counter : counter =
    { current = 0n
    ; total = n
    } in
  let flush_one
      (acc, e: (operation list * storage * counter) * (timestamp * proposal_key)) =
        let (ops, store, counter) = acc in
        let (start_date, proposal_key) = e in
        handle_proposal_is_over (config, start_date, proposal_key, store, ops, counter)
      in
  let (ops, store, counter) =
    Set.fold flush_one store.proposal_key_list_sort_by_date (([] : operation list), store, counter) in
  (ops, store)

// Removes an accepted and finished proposal by key.
let drop_proposal (proposal_key, config, store : proposal_key * config * storage): return =
  let store = authorize_admin store in

  let proposal = check_if_proposal_exist (proposal_key, store) in
  if is_voting_period_over(proposal, store)
  then
    if   do_total_vote_meet_quorum_threshold(proposal, store)
      && proposal.upvotes > proposal.downvotes
    then
      let store = unfreeze_proposer_and_voter_token
            (config.rejected_proposal_return_value, true, proposal, store) in
      let store = delete_proposal (proposal.start_date, proposal_key, store) in
      (([] : operation list), store)
    else
      (failwith("FAIL_DROP_PROPOSAL_NOT_ACCEPTED") : return)
  else
    (failwith("FAIL_DROP_PROPOSAL_NOT_OVER") : return)

let freeze (freeze_param, store : freeze_param * storage) : return =
  let (tokens, sender_key_hash) = freeze_param in
  let sender_ = ensure_sender_key_hash(sender_key_hash, Tezos.sender) in

  let voting_power (n : key_hash) : nat = [%Michelson ({| { VOTING_POWER } |} : key_hash -> nat) ] n in
  let sender_voting_power = voting_power(sender_key_hash) in
  let current_period = get_current_period_num(store.voting_period_params) in
  let current_frozen_tokens = get_frozen_tokens_for_current_period(current_period, sender, store) in
  let (ledger, total_supply) = if (current_frozen_tokens + tokens <= sender_voting_power)
    then credit_to (tokens, sender_, frozen_token_id, store.ledger, store.total_supply)
    else (failwith "CANT_EXCEED_VOTING_POWER" : (ledger * total_supply)) in

  // Add the `amt` to the current period frozen token count of the freeze-history.
  let new_freeze_history_for_address = match Big_map.find_opt sender_ store.freeze_history with
    | Some fh ->
      { fh with unstaked = fh.unstaked + freeze_param.0 }
    | None -> { current_period_num = current_period; staked = 0n; unstaked = freeze_param.0}
  in
  (([] : operation list),
    { store with
      freeze_history = Big_map.update sender_ (Some(new_freeze_history_for_address)) store.freeze_history
    ; ledger = ledger
    ; total_supply = total_supply
    })

let unfreeze (amt, store : unfreeze_param * storage) : return =
  let addr = Tezos.sender in
  let current_period = get_current_period_num(store.voting_period_params) in

  let new_freeze_history =
    match Big_map.find_opt addr store.freeze_history with
    | Some fh ->
        let fh = sub_frozen_fh(amt, fh) in
        Big_map.update addr (Some(fh)) store.freeze_history
    | None ->
        ([%Michelson ({| { FAILWITH } |} : (string * unit) -> freeze_history)]
          ("NOT_ENOUGH_FROZEN_TOKENS", ()) : freeze_history)
  in

  let (ledger, total_supply) = unfreeze_on_ledger (amt, Tezos.sender, store.ledger, store.total_supply, store.frozen_token_id, store.governance_token) in

    (([] : operation list), { store with
        ledger = ledger
      ; total_supply = total_supply
      ; freeze_history = new_freeze_history
    })
