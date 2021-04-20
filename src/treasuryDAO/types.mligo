// SPDX-FileCopyrightText: 2021 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

#include "../types.mligo"

(*
 * Proposal Metadata
 *)

// For reference only
// ```
// type proposal_metadata =
//   [@layout:comb]
//   { agora_post_id : nat
//   ; transfers : transfer_type list
//   }
// ```

(*
 * Contract Extra
 *)

type treasury_contract_extra =
  ( nat // frozen_scale_value
  * nat // frozen_extra_value
  * nat // max_proposal_size
  * nat // slash_scale_value
  * nat // slash_division_value
  * tez // min_xtz_amount
  * tez // max_xtz_amount
  )

type proposal_type =
  | Transfer_proposal of transfer_type list
  | VotingPeriodConstantsUpdate of voting_period_params
