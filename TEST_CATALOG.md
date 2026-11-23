# Test catalogue — tagged-urn/tagged-urn-rs

Generated from the test catalogue. Edit the tests, not this file.

112 tests: 112 numbered, 0 unnumbered.

## Numbered

| Number | Repository | Language | Test | Location | Description |
|---|---|---|---|---|---|
| TEST1 | tagged-urn/tagged-urn-rs | rust | `test0001_tag_order_normalization` | src/tagged_urn.rs:4051 | TEST0001: Tag order normalization |
| TEST392 | tagged-urn/tagged-urn-rs | rust | `test392_least_upper_bound` | src/tagged_urn.rs:3751 | TEST392: the bound of several URNs: the values they share, a key every one sets to differing values as the set of those values, the coarser of two comparable URNs, nothing for none, and another prefix refused |
| TEST501 | tagged-urn/tagged-urn-rs | rust | `test0501_tagged_urn_creation` | src/tagged_urn.rs:1724 | TEST0501: Create tagged URN from string and verify prefix and tag values |
| TEST502 | tagged-urn/tagged-urn-rs | rust | `test0502_custom_prefix` | src/tagged_urn.rs:1736 | TEST0502: Parse URN with custom prefix and verify serialization |
| TEST503 | tagged-urn/tagged-urn-rs | rust | `test0503_prefix_case_insensitive` | src/tagged_urn.rs:1745 | TEST0503: Normalize prefix to lowercase regardless of input case |
| TEST504 | tagged-urn/tagged-urn-rs | rust | `test0504_prefix_mismatch_error` | src/tagged_urn.rs:1762 | TEST0504: Return PrefixMismatch error when comparing URNs with different prefixes |
| TEST505 | tagged-urn/tagged-urn-rs | rust | `test0505_builder_with_prefix` | src/tagged_urn.rs:1780 | TEST0505: Build URN with custom prefix using TaggedUrnBuilder |
| TEST506 | tagged-urn/tagged-urn-rs | rust | `test0506_unquoted_values_lowercased` | src/tagged_urn.rs:1794 | TEST0506: Normalize unquoted keys and values to lowercase |
| TEST507 | tagged-urn/tagged-urn-rs | rust | `test0507_quoted_values_preserve_case` | src/tagged_urn.rs:1820 | TEST0507: Preserve original case for quoted values while lowercasing keys |
| TEST508 | tagged-urn/tagged-urn-rs | rust | `test0508_quoted_value_special_chars` | src/tagged_urn.rs:1839 | TEST0508: Parse quoted values containing semicolons, equals signs, and spaces |
| TEST509 | tagged-urn/tagged-urn-rs | rust | `test0509_quoted_value_escape_sequences` | src/tagged_urn.rs:1855 | TEST0509: Parse escape sequences for quotes and backslashes in quoted values |
| TEST510 | tagged-urn/tagged-urn-rs | rust | `test0510_mixed_quoted_unquoted` | src/tagged_urn.rs:1871 | TEST0510: Parse URN with both quoted and unquoted tag values |
| TEST511 | tagged-urn/tagged-urn-rs | rust | `test0511_unterminated_quote_error` | src/tagged_urn.rs:1879 | TEST0511: Reject unterminated quoted value with appropriate error |
| TEST512 | tagged-urn/tagged-urn-rs | rust | `test0512_invalid_escape_sequence_error` | src/tagged_urn.rs:1889 | TEST0512: Reject invalid escape sequences in quoted values |
| TEST513 | tagged-urn/tagged-urn-rs | rust | `test0513_serialization_smart_quoting` | src/tagged_urn.rs:1906 | TEST0513: Apply smart quoting during serialization based on value content |
| TEST514 | tagged-urn/tagged-urn-rs | rust | `test0514_round_trip_simple` | src/tagged_urn.rs:1964 | TEST0514: Round-trip parse and serialize a simple URN |
| TEST515 | tagged-urn/tagged-urn-rs | rust | `test0515_round_trip_quoted` | src/tagged_urn.rs:1974 | TEST0515: Round-trip parse and serialize a URN with quoted values |
| TEST516 | tagged-urn/tagged-urn-rs | rust | `test0516_round_trip_escapes` | src/tagged_urn.rs:1985 | TEST0516: Round-trip parse and serialize a URN with escape sequences |
| TEST517 | tagged-urn/tagged-urn-rs | rust | `test0517_prefix_required` | src/tagged_urn.rs:1996 | TEST0517: Require a prefix in URN string and reject missing prefix |
| TEST518 | tagged-urn/tagged-urn-rs | rust | `test0518_trailing_semicolon_equivalence` | src/tagged_urn.rs:2011 | TEST0518: Treat trailing semicolon as equivalent to no trailing semicolon |
| TEST519 | tagged-urn/tagged-urn-rs | rust | `test0519_canonical_string_format` | src/tagged_urn.rs:2043 | TEST0519: Serialize tags in alphabetical order as canonical string format |
| TEST520 | tagged-urn/tagged-urn-rs | rust | `test0520_tag_matching` | src/tagged_urn.rs:2057 | TEST0520: Match tags with exact values, subsets, wildcards, and mismatches |
| TEST521 | tagged-urn/tagged-urn-rs | rust | `test0521_matching_case_sensitive_values` | src/tagged_urn.rs:2083 | TEST0521: Enforce case-sensitive matching for quoted tag values |
| TEST522 | tagged-urn/tagged-urn-rs | rust | `test0522_missing_tag_handling` | src/tagged_urn.rs:2097 | TEST0522: Handle missing tags in instance vs pattern matching semantics |
| TEST523 | tagged-urn/tagged-urn-rs | rust | `test0523_specificity` | src/tagged_urn.rs:2125 | TEST0523: Compute graded specificity scores and tuples for URN tags |
| TEST524 | tagged-urn/tagged-urn-rs | rust | `test0524_builder` | src/tagged_urn.rs:2162 | TEST0524: Build URN with multiple tags using TaggedUrnBuilder |
| TEST525 | tagged-urn/tagged-urn-rs | rust | `test0525_builder_preserves_case` | src/tagged_urn.rs:2182 | TEST0525: Preserve value case in builder while lowercasing keys |
| TEST526 | tagged-urn/tagged-urn-rs | rust | `test0526_directional_accepts_with_tag_overlap` | src/tagged_urn.rs:2198 | TEST0526: Verify directional accepts between patterns with shared and disjoint tags |
| TEST527 | tagged-urn/tagged-urn-rs | rust | `test0527_best_match` | src/tagged_urn.rs:2227 | TEST0527: Find best matching URN by specificity from a list of candidates |
| TEST528 | tagged-urn/tagged-urn-rs | rust | `test0528_merge_and_subset` | src/tagged_urn.rs:2249 | TEST0528: Merge two URNs and extract a subset of tags |
| TEST529 | tagged-urn/tagged-urn-rs | rust | `test0529_merge_prefix_mismatch` | src/tagged_urn.rs:2266 | TEST0529: Reject merge of URNs with different prefixes |
| TEST530 | tagged-urn/tagged-urn-rs | rust | `test0530_wildcard_tag` | src/tagged_urn.rs:2280 | TEST0530: Convert specific tag value to wildcard and verify matching behavior |
| TEST531 | tagged-urn/tagged-urn-rs | rust | `test0531_empty_tagged_urn` | src/tagged_urn.rs:2297 | TEST0531: Handle empty tagged URN with no tags in matching and serialization |
| TEST532 | tagged-urn/tagged-urn-rs | rust | `test0532_empty_with_custom_prefix` | src/tagged_urn.rs:2328 | TEST0532: Create empty URN with custom prefix |
| TEST533 | tagged-urn/tagged-urn-rs | rust | `test0533_extended_character_support` | src/tagged_urn.rs:2337 | TEST0533: Parse forward slashes and colons in unquoted tag values |
| TEST534 | tagged-urn/tagged-urn-rs | rust | `test0534_wildcard_restrictions` | src/tagged_urn.rs:2361 | TEST0534: Reject wildcard in keys but accept wildcard in values |
| TEST535 | tagged-urn/tagged-urn-rs | rust | `test0535_duplicate_key_rejection` | src/tagged_urn.rs:2372 | TEST0535: Reject duplicate keys in URN string |
| TEST536 | tagged-urn/tagged-urn-rs | rust | `test0536_numeric_key_restriction` | src/tagged_urn.rs:2382 | TEST0536: Reject keys that begin with a digit but allow digits after the first letter |
| TEST537 | tagged-urn/tagged-urn-rs | rust | `test0537_empty_value_error` | src/tagged_urn.rs:2403 | TEST0537: Reject empty value after equals sign |
| TEST538 | tagged-urn/tagged-urn-rs | rust | `test0538_has_tag_case_sensitive` | src/tagged_urn.rs:2410 | TEST0538: Verify has_tag uses case-sensitive value comparison and case-insensitive key lookup |
| TEST539 | tagged-urn/tagged-urn-rs | rust | `test0539_with_tag_preserves_value` | src/tagged_urn.rs:2427 | TEST0539: Preserve value case when adding tag with with_tag method |
| TEST540 | tagged-urn/tagged-urn-rs | rust | `test0540_with_tag_rejects_empty_value` | src/tagged_urn.rs:2437 | TEST0540: Reject empty value string in with_tag method |
| TEST541 | tagged-urn/tagged-urn-rs | rust | `test0541_builder_rejects_empty_value` | src/tagged_urn.rs:2449 | TEST0541: Reject empty value string in builder tag method |
| TEST542 | tagged-urn/tagged-urn-rs | rust | `test0542_semantic_equivalence` | src/tagged_urn.rs:2461 | TEST0542: Treat quoted and unquoted simple lowercase values as semantically equivalent |
| TEST543 | tagged-urn/tagged-urn-rs | rust | `test0543_matching_semantics_test1_exact_match` | src/tagged_urn.rs:2480 | TEST0543: Verify exact match when instance and pattern have identical tags |
| TEST544 | tagged-urn/tagged-urn-rs | rust | `test0544_matching_semantics_test2_instance_missing_tag` | src/tagged_urn.rs:2495 | TEST0544: Reject match when instance is missing a tag required by pattern |
| TEST545 | tagged-urn/tagged-urn-rs | rust | `test0545_matching_semantics_test3_urn_has_extra_tag` | src/tagged_urn.rs:2521 | TEST0545: Match when instance has extra tags not constrained by pattern |
| TEST546 | tagged-urn/tagged-urn-rs | rust | `test0546_matching_semantics_test4_request_has_wildcard` | src/tagged_urn.rs:2537 | TEST0546: Match when pattern has wildcard accepting any value for a tag |
| TEST547 | tagged-urn/tagged-urn-rs | rust | `test0547_matching_semantics_test5_urn_has_wildcard` | src/tagged_urn.rs:2556 | TEST0547: An instance's wildcard promises presence, not the value asked for `ext` is "some ext". It used to satisfy a pattern asking for `ext=pdf` — "decided later" — which let a cap promising some ext stand in for one that produces a pdf. A pdf satisfies "some ext"; "some ext" does not satisfy pdf. |
| TEST548 | tagged-urn/tagged-urn-rs | rust | `test0548_matching_semantics_test6_value_mismatch` | src/tagged_urn.rs:2575 | TEST0548: Reject match when tag values conflict between instance and pattern |
| TEST549 | tagged-urn/tagged-urn-rs | rust | `test0549_matching_semantics_test7_pattern_has_extra_tag` | src/tagged_urn.rs:2590 | TEST0549: Reject match when pattern requires a tag absent from instance |
| TEST550 | tagged-urn/tagged-urn-rs | rust | `test0550_matching_semantics_test8_empty_pattern_matches_anything` | src/tagged_urn.rs:2615 | TEST0550: Match any instance against empty pattern with no constraints |
| TEST551 | tagged-urn/tagged-urn-rs | rust | `test0551_matching_semantics_test9_cross_dimension_constraints` | src/tagged_urn.rs:2641 | TEST0551: Reject match when instance and pattern have non-overlapping tag dimensions |
| TEST552 | tagged-urn/tagged-urn-rs | rust | `test0552_matching_different_prefixes_error` | src/tagged_urn.rs:2667 | TEST0552: Return error for conforms_to, accepts, and is_more_specific_than with different prefixes |
| TEST553 | tagged-urn/tagged-urn-rs | rust | `test0553_valueless_tag_parsing_single` | src/tagged_urn.rs:2689 | TEST0553: Parse single value-less tag as wildcard |
| TEST554 | tagged-urn/tagged-urn-rs | rust | `test0554_valueless_tag_parsing_multiple` | src/tagged_urn.rs:2699 | TEST0554: Parse multiple value-less tags and serialize alphabetically |
| TEST555 | tagged-urn/tagged-urn-rs | rust | `test0555_valueless_tag_mixed_with_valued` | src/tagged_urn.rs:2711 | TEST0555: Parse mix of value-less and valued tags together |
| TEST556 | tagged-urn/tagged-urn-rs | rust | `test0556_valueless_tag_at_end` | src/tagged_urn.rs:2729 | TEST0556: Parse value-less tag at end of URN without trailing semicolon |
| TEST557 | tagged-urn/tagged-urn-rs | rust | `test0557_valueless_tag_equivalence_to_wildcard` | src/tagged_urn.rs:2742 | TEST0557: Verify value-less tag is equivalent to explicit wildcard (key=*) |
| TEST558 | tagged-urn/tagged-urn-rs | rust | `test0558_valueless_tag_matching` | src/tagged_urn.rs:2758 | TEST0558: A valueless tag promises presence, not a value Reading `ext` as "whatever the pattern wants" made `ext` and `ext=pdf` refine each other — equivalent — and refinement non-transitive. Refinement is inclusion of what each form allows: every pdf is some ext. |
| TEST559 | tagged-urn/tagged-urn-rs | rust | `test0559_valueless_tag_in_pattern` | src/tagged_urn.rs:2796 | TEST0559: Require value-less tag in pattern to be present in instance |
| TEST560 | tagged-urn/tagged-urn-rs | rust | `test0560_valueless_tag_specificity` | src/tagged_urn.rs:2819 | TEST0560: Score value-less wildcard tags with graded specificity |
| TEST561 | tagged-urn/tagged-urn-rs | rust | `test0561_valueless_tag_roundtrip` | src/tagged_urn.rs:2832 | TEST0561: Round-trip value-less tags through parse and serialize |
| TEST562 | tagged-urn/tagged-urn-rs | rust | `test0562_valueless_tag_case_normalization` | src/tagged_urn.rs:2844 | TEST0562: Normalize value-less tag keys to lowercase |
| TEST563 | tagged-urn/tagged-urn-rs | rust | `test0563_empty_value_still_error` | src/tagged_urn.rs:2855 | TEST0563: Reject empty value with equals sign as distinct from value-less tag |
| TEST564 | tagged-urn/tagged-urn-rs | rust | `test0564_valueless_tag_directional_accepts` | src/tagged_urn.rs:2863 | TEST0564: Verify directional accepts of value-less wildcard tags with specific values |
| TEST565 | tagged-urn/tagged-urn-rs | rust | `test0565_valueless_numeric_key_still_rejected` | src/tagged_urn.rs:2883 | TEST0565: Reject purely numeric keys for value-less tags |
| TEST566 | tagged-urn/tagged-urn-rs | rust | `test0566_whitespace_in_input_rejected` | src/tagged_urn.rs:2897 | TEST0566: Reject leading, trailing, and embedded whitespace in URN input |
| TEST567 | tagged-urn/tagged-urn-rs | rust | `test0567_unspecified_question_mark_parsing` | src/tagged_urn.rs:2942 | TEST0567: Parse question mark as unspecified value and verify serialization. All three input aliases (?x, x?, x=?) parse to the same constraint, unconstrained, and serialize as the canonical prefix form `?x`. |
| TEST568 | tagged-urn/tagged-urn-rs | rust | `test0568_must_not_have_exclamation_parsing` | src/tagged_urn.rs:2953 | TEST0568: Parse exclamation mark as must-not-have value and verify serialization. All three input aliases (!x, x!, x=!) parse to the constraint absent and serialize as canonical `!x`. |
| TEST569 | tagged-urn/tagged-urn-rs | rust | `test0569_question_mark_pattern_matches_anything` | src/tagged_urn.rs:2962 | TEST0569: Match any instance against pattern with unspecified (?) tag value |
| TEST570 | tagged-urn/tagged-urn-rs | rust | `test0570_question_mark_in_instance` | src/tagged_urn.rs:3001 | TEST0570: An instance with K=? promises nothing about K `?` is "no constraint" on either side. As an instance it used to satisfy every pattern, which made refinement non-transitive: missing ⪯ ?k ⪯ k=v, yet missing ⋠ k=v. It satisfies exactly the patterns that ask for nothing. |
| TEST571 | tagged-urn/tagged-urn-rs | rust | `test0571_must_not_have_pattern_requires_absent` | src/tagged_urn.rs:3044 | TEST0571: Pattern with K=! requires the instance to SAY K is absent A key an instance does not mention is not a promise that it is absent: as a pattern the same omission means "anything", and one form cannot mean two things. `media:pdf` satisfied `media:pdf;!compressed` while `media:pdf;compressed` satisfied `media:pdf` and not the `!compressed` pattern, so refinement was not transitive. |
| TEST572 | tagged-urn/tagged-urn-rs | rust | `test0572_must_not_have_in_instance` | src/tagged_urn.rs:3080 | TEST0572: Reject instance with must-not-have (!) tag against patterns requiring that tag |
| TEST573 | tagged-urn/tagged-urn-rs | rust | `test0573_full_cross_product_matching` | src/tagged_urn.rs:3114 | TEST0573: Verify full cross-product truth table for all instance/pattern value combinations |
| TEST574 | tagged-urn/tagged-urn-rs | rust | `test0574_mixed_special_values` | src/tagged_urn.rs:3172 | TEST0574: Match URN with mixed required, optional, forbidden, and exact tags |
| TEST575 | tagged-urn/tagged-urn-rs | rust | `test0575_serialization_round_trip_special_values` | src/tagged_urn.rs:3206 | TEST0575: Round-trip all special values (?, !, *, exact) through parse and serialize |
| TEST576 | tagged-urn/tagged-urn-rs | rust | `test0576_bidirectional_accepts_with_special_values` | src/tagged_urn.rs:3225 | TEST0576: Check bidirectional accepts between !, *, ?, and specific value tags |
| TEST577 | tagged-urn/tagged-urn-rs | rust | `test0577_specificity_with_special_values` | src/tagged_urn.rs:3389 | TEST0577: Verify graded specificity scores and tuples for special value types under the six-form ladder. |
| TEST578 | tagged-urn/tagged-urn-rs | rust | `test578_equivalent_identical_tags` | src/tagged_urn.rs:3262 | TEST578: Equivalent URNs with identical tag sets |
| TEST579 | tagged-urn/tagged-urn-rs | rust | `test579_not_equivalent_when_one_more_specific` | src/tagged_urn.rs:3271 | TEST579: Non-equivalent URNs where one is more specific |
| TEST580 | tagged-urn/tagged-urn-rs | rust | `test580_comparable_specialization_chain` | src/tagged_urn.rs:3280 | TEST580: Comparable URNs on the same specialization chain |
| TEST581 | tagged-urn/tagged-urn-rs | rust | `test581_incomparable_different_branches` | src/tagged_urn.rs:3292 | TEST581: Incomparable URNs in different branches of the lattice |
| TEST582 | tagged-urn/tagged-urn-rs | rust | `test582_equivalent_implies_comparable` | src/tagged_urn.rs:3304 | TEST582: Equivalent implies comparable but not vice versa |
| TEST583 | tagged-urn/tagged-urn-rs | rust | `test583_prefix_mismatch_errors` | src/tagged_urn.rs:3320 | TEST583: Prefix mismatch returns error for both relations |
| TEST584 | tagged-urn/tagged-urn-rs | rust | `test584_empty_tags_comparable_to_all` | src/tagged_urn.rs:3329 | TEST584: Empty tag set is comparable to everything with same prefix |
| TEST585 | tagged-urn/tagged-urn-rs | rust | `test585_string_variants` | src/tagged_urn.rs:3343 | TEST585: String variants of is_equivalent and is_comparable |
| TEST586 | tagged-urn/tagged-urn-rs | rust | `test586_special_values` | src/tagged_urn.rs:3353 | TEST586: Special values (*, !, ?) with is_equivalent and is_comparable |
| TEST587 | tagged-urn/tagged-urn-rs | rust | `test587_builder_fluent_api` | src/tagged_urn.rs:3539 | TEST587: Builder fluent API for tag manipulation |
| TEST588 | tagged-urn/tagged-urn-rs | rust | `test588_builder_custom_tags` | src/tagged_urn.rs:3561 | TEST588: Builder with custom tags |
| TEST589 | tagged-urn/tagged-urn-rs | rust | `test589_builder_tag_overrides` | src/tagged_urn.rs:3580 | TEST589: Builder tag overrides (last value wins) |
| TEST590 | tagged-urn/tagged-urn-rs | rust | `test590_builder_empty_build` | src/tagged_urn.rs:3596 | TEST590: Builder empty build returns error (tags required) |
| TEST591 | tagged-urn/tagged-urn-rs | rust | `test591_builder_single_tag` | src/tagged_urn.rs:3607 | TEST591: Builder with single tag |
| TEST592 | tagged-urn/tagged-urn-rs | rust | `test592_builder_complex` | src/tagged_urn.rs:3623 | TEST592: Builder with complex multi-tag URN |
| TEST593 | tagged-urn/tagged-urn-rs | rust | `test593_builder_wildcards` | src/tagged_urn.rs:3660 | TEST593: Builder with wildcards |
| TEST594 | tagged-urn/tagged-urn-rs | rust | `test594_builder_custom_prefix` | src/tagged_urn.rs:3684 | TEST594: Builder with custom prefix |
| TEST595 | tagged-urn/tagged-urn-rs | rust | `test595_builder_matching_with_built_urn` | src/tagged_urn.rs:3698 | TEST595: Builder matching with built URN |
| TEST599 | tagged-urn/tagged-urn-rs | rust | `test599_every_row_of_the_model_s_table` | tests/formal_conformance.rs:16 |  |
| TEST600 | tagged-urn/tagged-urn-rs | rust | `test600_every_model_function_carries_a_proved_claim` | tests/formal_assurance.rs:11 |  |
| TEST6752 | tagged-urn/tagged-urn-rs | rust | `test6752_qualifier_value_alias` | src/tagged_urn.rs:3784 | TEST6752: An unquoted value that begins with ? or ! is that qualifier (x=?v is x?=v, x=!v is x!=v); a quoted one is literal |
| TEST6753 | tagged-urn/tagged-urn-rs | rust | `test6753_prefix_and_key_are_identifiers` | src/tagged_urn.rs:3825 | TEST6753: The prefix and every key are identifiers (an ASCII letter, then ASCII letters, digits and -), lowered, in parsing and in every constructor |
| TEST6754 | tagged-urn/tagged-urn-rs | rust | `test6754_every_value_reads_back` | src/tagged_urn.rs:3895 | TEST6754: Every value a URN holds prints so that it reads back as itself |
| TEST6755 | tagged-urn/tagged-urn-rs | rust | `test6755_unquoted_value_takes_any_non_separator` | src/tagged_urn.rs:3921 | TEST6755: An unquoted value takes any character but ; = \| " \ and whitespace |
| TEST6756 | tagged-urn/tagged-urn-rs | rust | `test6756_quoted_value_is_literal` | src/tagged_urn.rs:3950 | TEST6756: A quoted value is the literal value: a quoted *, ? or ! is never a sigil, and a quoted ?=v is never a qualifier |
| TEST7210 | tagged-urn/tagged-urn-rs | rust | `test7210_admits_absence_by_form` | src/tagged_urn.rs:3520 | TEST7210: A written tag lets its key be absent exactly when it is !k, ?k or k?=v; a key the URN does not mention says nothing of it, so it allows absence too. |
| TEST7223 | tagged-urn/tagged-urn-rs | rust | `test7223_urn_read_back_from_its_handle` | src/tagged_urn.rs:3416 | TEST7223: A URN read back from its model handle is the URN: its prefix and every tag, each form and a quoted value included. |
| TEST7224 | tagged-urn/tagged-urn-rs | rust | `test7224_set_forms_parse_and_print` | src/tagged_urn.rs:3429 | TEST7224: Each set form parses to the set it writes and prints canonically — values once, in increasing order, `!` last — reading back as itself; the qualifier aliases write the same sets. |
| TEST7225 | tagged-urn/tagged-urn-rs | rust | `test7225_set_forms_refused` | src/tagged_urn.rs:3460 | TEST7225: A set of values refuses a value given twice, an empty member, a bare sigil or qualifier among its values, and `!` under a qualifier; a literal `\|` is quoted. |
| TEST7226 | tagged-urn/tagged-urn-rs | rust | `test7226_bound_is_exact_per_key` | src/tagged_urn.rs:3486 | TEST7226: The bound of several URNs is exact per key: png and jpeg are bound by ext=jpeg\|png, a value and its absence by ext=v\|!, and a value and any value by ext. |
| TEST7227 | tagged-urn/tagged-urn-rs | rust | `test7227_set_scores` | src/tagged_urn.rs:3503 | TEST7227: A set scores by what it allows — two or more values 3, absence and some values 1 — and narrowing never lowers a score, though two on one chain can tie. |
| TEST7241 | tagged-urn/tagged-urn-rs | rust | `test7241_urns_are_ordered_by_their_parts` | src/tagged_urn.rs:4021 | TEST7241: URNs are ordered by their parts, never their text: the prefix, then each tag in key order by key, then by its constraint's form (unconstrained, optionalNot, present, presentNot, exact, absent), then by its value |
| TEST8025 | tagged-urn/tagged-urn-rs | rust | `test8025_documented_parse_error_names` | src/tagged_urn.rs:3988 | TEST8025: Each parse error the domain doc names is the error the parser raises for it |

