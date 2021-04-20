// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#include "types.mligo"

let default_config : config = {
    proposal_check = (fun (_, _ : propose_params * contract_extra) -> true);
    rejected_proposal_return_value = (fun (_, _ : proposal * contract_extra) -> 0n);
    decision_lambda = (fun (_, extras : proposal * contract_extra) -> (([] : (operation list))
      , ((None : voting_period_params option), extras)));
    max_proposals = 500n;
    max_votes = 1000n;
    max_quorum_threshold = {numerator = 99n; denominator = 100n}; // 99%
    min_quorum_threshold = {numerator = 1n; denominator = 100n}; // 1%
    max_voting_period = 2592000n; // 60 * 60 * 24 * 30
    min_voting_period = 1n;
    custom_entrypoints = (Map.empty : custom_entrypoints);
    }

let default_storage (admin , governance_token, now_val, metadata : address * governance_token * timestamp * metadata_map) : storage = {
    ledger = (Big_map.empty : ledger);
    operators = (Big_map.empty : operators);
    governance_token = governance_token;
    admin = admin;
    pending_owner = admin;
    metadata = metadata;
    voting_period = 11n;
    quorum_threshold = {numerator = 1n; denominator = 10n}; // 10%
    extra = (Map.empty : (string, bytes) map);
    proposals = (Big_map.empty : (proposal_key, proposal) big_map);
    proposal_key_list_sort_by_date = (Set.empty : (timestamp * proposal_key) set);
    permits_counter = 0n;
    freeze_history = (Big_map.empty : freeze_history);
    total_supply = Map.literal
        [ (frozen_token_id, 0n)
        ];
    fixed_proposal_fee_in_token = 0n;
    frozen_token_id = frozen_token_id;
    voting_period_params =
      { levels_per_cycle_change_at = (0n, 0n)
      ; cycles_per_period_change_at = (0n, 0n)
      ; levels_per_cycle = 4096n
      ; cycles_per_period = 5n
      }
}

let default_full_storage (admin, governance_token, now_val, metadata_map : address * governance_token * timestamp * metadata_map) : full_storage =
  (default_storage (admin, governance_token, now_val, metadata_map), default_config)
