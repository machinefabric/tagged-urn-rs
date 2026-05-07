# Rust Test Catalog

**Total Tests:** 96

**Numbered Tests:** 18

**Unnumbered Tests:** 78

**Numbered Tests Missing Descriptions:** 0

**Numbering Mismatches:** 77

All numbered test numbers are unique.

This catalog lists all tests in the Rust codebase.

| Test # | Function Name | Description | File |
|--------|---------------|-------------|------|
| test578 | `test578_equivalent_identical_tags` | TEST578: Equivalent URNs with identical tag sets | src/tagged_urn.rs:2605 |
| test579 | `test579_not_equivalent_when_one_more_specific` | TEST579: Non-equivalent URNs where one is more specific | src/tagged_urn.rs:2614 |
| test580 | `test580_comparable_specialization_chain` | TEST580: Comparable URNs on the same specialization chain | src/tagged_urn.rs:2623 |
| test581 | `test581_incomparable_different_branches` | TEST581: Incomparable URNs in different branches of the lattice | src/tagged_urn.rs:2635 |
| test582 | `test582_equivalent_implies_comparable` | TEST582: Equivalent implies comparable but not vice versa | src/tagged_urn.rs:2647 |
| test583 | `test583_prefix_mismatch_errors` | TEST583: Prefix mismatch returns error for both relations | src/tagged_urn.rs:2663 |
| test584 | `test584_empty_tags_comparable_to_all` | TEST584: Empty tag set is comparable to everything with same prefix | src/tagged_urn.rs:2672 |
| test585 | `test585_string_variants` | TEST585: String variants of is_equivalent and is_comparable | src/tagged_urn.rs:2686 |
| test586 | `test586_special_values` | TEST586: Special values (*, !, ?) with is_equivalent and is_comparable | src/tagged_urn.rs:2696 |
| test587 | `test587_builder_fluent_api` | TEST587: Builder fluent API for tag manipulation | src/tagged_urn.rs:2754 |
| test588 | `test588_builder_custom_tags` | TEST588: Builder with custom tags | src/tagged_urn.rs:2771 |
| test589 | `test589_builder_tag_overrides` | TEST589: Builder tag overrides (last value wins) | src/tagged_urn.rs:2786 |
| test590 | `test590_builder_empty_build` | TEST590: Builder empty build returns error (tags required) | src/tagged_urn.rs:2799 |
| test591 | `test591_builder_single_tag` | TEST591: Builder with single tag | src/tagged_urn.rs:2810 |
| test592 | `test592_builder_complex` | TEST592: Builder with complex multi-tag URN | src/tagged_urn.rs:2824 |
| test593 | `test593_builder_wildcards` | TEST593: Builder with wildcards | src/tagged_urn.rs:2852 |
| test594 | `test594_builder_custom_prefix` | TEST594: Builder with custom prefix | src/tagged_urn.rs:2872 |
| test595 | `test595_builder_matching_with_built_urn` | TEST595: Builder matching with built URN | src/tagged_urn.rs:2884 |
| | | | |
| unnumbered | `test_best_match` | TEST527: Find best matching URN by specificity from a list of candidates | src/tagged_urn.rs:1794 |
| unnumbered | `test_bidirectional_accepts_with_special_values` | TEST576: Check bidirectional accepts between !, *, ?, and specific value tags | src/tagged_urn.rs:2573 |
| unnumbered | `test_builder` | TEST524: Build URN with multiple tags using TaggedUrnBuilder | src/tagged_urn.rs:1737 |
| unnumbered | `test_builder_preserves_case` | TEST525: Preserve value case in builder while lowercasing keys | src/tagged_urn.rs:1752 |
| unnumbered | `test_builder_rejects_empty_value` | TEST541: Reject empty value string in builder tag method | src/tagged_urn.rs:1983 |
| unnumbered | `test_builder_with_prefix` | TEST505: Build URN with custom prefix using TaggedUrnBuilder | src/tagged_urn.rs:1381 |
| unnumbered | `test_canonical_string_format` | TEST519: Serialize tags in alphabetical order as canonical string format | src/tagged_urn.rs:1623 |
| unnumbered | `test_custom_prefix` | TEST502: Parse URN with custom prefix and verify serialization | src/tagged_urn.rs:1337 |
| unnumbered | `test_directional_accepts_with_tag_overlap` | TEST526: Verify directional accepts between patterns with shared and disjoint tags | src/tagged_urn.rs:1766 |
| unnumbered | `test_duplicate_key_rejection` | TEST535: Reject duplicate keys in URN string | src/tagged_urn.rs:1916 |
| unnumbered | `test_empty_tagged_urn` | TEST531: Handle empty tagged URN with no tags in matching and serialization | src/tagged_urn.rs:1854 |
| unnumbered | `test_empty_value_error` | TEST537: Reject empty value after equals sign | src/tagged_urn.rs:1940 |
| unnumbered | `test_empty_value_still_error` | TEST563: Reject empty value with equals sign as distinct from value-less tag | src/tagged_urn.rs:2300 |
| unnumbered | `test_empty_with_custom_prefix` | TEST532: Create empty URN with custom prefix | src/tagged_urn.rs:1884 |
| unnumbered | `test_extended_character_support` | TEST533: Parse forward slashes and colons in unquoted tag values | src/tagged_urn.rs:1893 |
| unnumbered | `test_full_cross_product_matching` | TEST573: Verify full cross-product truth table for all instance/pattern value combinations | src/tagged_urn.rs:2474 |
| unnumbered | `test_has_tag_case_sensitive` | TEST538: Verify has_tag uses case-sensitive value comparison and case-insensitive key lookup | src/tagged_urn.rs:1947 |
| unnumbered | `test_invalid_escape_sequence_error` | TEST512: Reject invalid escape sequences in quoted values | src/tagged_urn.rs:1484 |
| unnumbered | `test_matching_case_sensitive_values` | TEST521: Enforce case-sensitive matching for quoted tag values | src/tagged_urn.rs:1658 |
| unnumbered | `test_matching_different_prefixes_error` | TEST552: Return error for conforms_to, accepts, and is_more_specific_than with different prefixes | src/tagged_urn.rs:2150 |
| unnumbered | `test_matching_semantics_test1_exact_match` | TEST543: Verify exact match when instance and pattern have identical tags | src/tagged_urn.rs:2014 |
| unnumbered | `test_matching_semantics_test2_instance_missing_tag` | TEST544: Reject match when instance is missing a tag required by pattern | src/tagged_urn.rs:2026 |
| unnumbered | `test_matching_semantics_test3_urn_has_extra_tag` | TEST545: Match when instance has extra tags not constrained by pattern | src/tagged_urn.rs:2045 |
| unnumbered | `test_matching_semantics_test4_request_has_wildcard` | TEST546: Match when pattern has wildcard accepting any value for a tag | src/tagged_urn.rs:2057 |
| unnumbered | `test_matching_semantics_test5_urn_has_wildcard` | TEST547: Match when instance has wildcard satisfying pattern's specific value | src/tagged_urn.rs:2069 |
| unnumbered | `test_matching_semantics_test6_value_mismatch` | TEST548: Reject match when tag values conflict between instance and pattern | src/tagged_urn.rs:2081 |
| unnumbered | `test_matching_semantics_test7_pattern_has_extra_tag` | TEST549: Reject match when pattern requires a tag absent from instance | src/tagged_urn.rs:2093 |
| unnumbered | `test_matching_semantics_test8_empty_pattern_matches_anything` | TEST550: Match any instance against empty pattern with no constraints | src/tagged_urn.rs:2111 |
| unnumbered | `test_matching_semantics_test9_cross_dimension_constraints` | TEST551: Reject match when instance and pattern have non-overlapping tag dimensions | src/tagged_urn.rs:2131 |
| unnumbered | `test_merge_and_subset` | TEST528: Merge two URNs and extract a subset of tags | src/tagged_urn.rs:1811 |
| unnumbered | `test_merge_prefix_mismatch` | TEST529: Reject merge of URNs with different prefixes | src/tagged_urn.rs:1828 |
| unnumbered | `test_missing_tag_handling` | TEST522: Handle missing tags in instance vs pattern matching semantics | src/tagged_urn.rs:1672 |
| unnumbered | `test_mixed_quoted_unquoted` | TEST510: Parse URN with both quoted and unquoted tag values | src/tagged_urn.rs:1466 |
| unnumbered | `test_mixed_special_values` | TEST574: Match URN with mixed required, optional, forbidden, and exact tags | src/tagged_urn.rs:2531 |
| unnumbered | `test_must_not_have_exclamation_parsing` | TEST568: Parse exclamation mark as must-not-have value and verify serialization. All three input aliases (!x, x!, x=!) parse to stored value `"!"` and serialize as canonical `!x`. | src/tagged_urn.rs:2391 |
| unnumbered | `test_must_not_have_in_instance` | TEST572: Reject instance with must-not-have (!) tag against patterns requiring that tag | src/tagged_urn.rs:2455 |
| unnumbered | `test_must_not_have_pattern_requires_absent` | TEST571: Require tag to be absent when pattern uses must-not-have (!) value | src/tagged_urn.rs:2438 |
| unnumbered | `test_numeric_key_restriction` | TEST536: Reject purely numeric keys but allow mixed alphanumeric keys | src/tagged_urn.rs:1926 |
| unnumbered | `test_prefix_case_insensitive` | TEST503: Normalize prefix to lowercase regardless of input case | src/tagged_urn.rs:1346 |
| unnumbered | `test_prefix_mismatch_error` | TEST504: Return PrefixMismatch error when comparing URNs with different prefixes | src/tagged_urn.rs:1363 |
| unnumbered | `test_prefix_required` | TEST517: Require a prefix in URN string and reject missing prefix | src/tagged_urn.rs:1576 |
| unnumbered | `test_question_mark_in_instance` | TEST570: Match instance with unspecified (?) tag against any pattern constraint | src/tagged_urn.rs:2419 |
| unnumbered | `test_question_mark_pattern_matches_anything` | TEST569: Match any instance against pattern with unspecified (?) tag value | src/tagged_urn.rs:2400 |
| unnumbered | `test_quoted_value_escape_sequences` | TEST509: Parse escape sequences for quotes and backslashes in quoted values | src/tagged_urn.rs:1450 |
| unnumbered | `test_quoted_value_special_chars` | TEST508: Parse quoted values containing semicolons, equals signs, and spaces | src/tagged_urn.rs:1434 |
| unnumbered | `test_quoted_values_preserve_case` | TEST507: Preserve original case for quoted values while lowercasing keys | src/tagged_urn.rs:1415 |
| unnumbered | `test_round_trip_escapes` | TEST516: Round-trip parse and serialize a URN with escape sequences | src/tagged_urn.rs:1565 |
| unnumbered | `test_round_trip_quoted` | TEST515: Round-trip parse and serialize a URN with quoted values | src/tagged_urn.rs:1554 |
| unnumbered | `test_round_trip_simple` | TEST514: Round-trip parse and serialize a simple URN | src/tagged_urn.rs:1544 |
| unnumbered | `test_semantic_equivalence` | TEST542: Treat quoted and unquoted simple lowercase values as semantically equivalent | src/tagged_urn.rs:1995 |
| unnumbered | `test_serialization_round_trip_special_values` | TEST575: Round-trip all special values (?, !, *, exact) through parse and serialize | src/tagged_urn.rs:2554 |
| unnumbered | `test_serialization_smart_quoting` | TEST513: Apply smart quoting during serialization based on value content | src/tagged_urn.rs:1501 |
| unnumbered | `test_specificity` | TEST523: Compute graded specificity scores and tuples for URN tags | src/tagged_urn.rs:1700 |
| unnumbered | `test_specificity_with_special_values` | TEST577: Verify graded specificity scores and tuples for special value types under the six-form ladder. | src/tagged_urn.rs:2724 |
| unnumbered | `test_tag_matching` | TEST520: Match tags with exact values, subsets, wildcards, and mismatches | src/tagged_urn.rs:1635 |
| unnumbered | `test_tag_order_normalization` |  | src/tagged_urn.rs:2925 |
| unnumbered | `test_tagged_urn_creation` | TEST501: Create tagged URN from string and verify prefix and tag values | src/tagged_urn.rs:1327 |
| unnumbered | `test_trailing_semicolon_equivalence` | TEST518: Treat trailing semicolon as equivalent to no trailing semicolon | src/tagged_urn.rs:1591 |
| unnumbered | `test_unquoted_values_lowercased` | TEST506: Normalize unquoted keys and values to lowercase | src/tagged_urn.rs:1393 |
| unnumbered | `test_unspecified_question_mark_parsing` | TEST567: Parse question mark as unspecified value and verify serialization. All three input aliases (?x, x?, x=?) parse to the same stored value `"?"` and serialize as the canonical prefix form `?x`. | src/tagged_urn.rs:2380 |
| unnumbered | `test_unterminated_quote_error` | TEST511: Reject unterminated quoted value with appropriate error | src/tagged_urn.rs:1474 |
| unnumbered | `test_valueless_numeric_key_still_rejected` | TEST565: Reject purely numeric keys for value-less tags | src/tagged_urn.rs:2327 |
| unnumbered | `test_valueless_tag_at_end` | TEST556: Parse value-less tag at end of URN without trailing semicolon | src/tagged_urn.rs:2207 |
| unnumbered | `test_valueless_tag_case_normalization` | TEST562: Normalize value-less tag keys to lowercase | src/tagged_urn.rs:2289 |
| unnumbered | `test_valueless_tag_directional_accepts` | TEST564: Verify directional accepts of value-less wildcard tags with specific values | src/tagged_urn.rs:2308 |
| unnumbered | `test_valueless_tag_equivalence_to_wildcard` | TEST557: Verify value-less tag is equivalent to explicit wildcard (key=*) | src/tagged_urn.rs:2217 |
| unnumbered | `test_valueless_tag_in_pattern` | TEST559: Require value-less tag in pattern to be present in instance | src/tagged_urn.rs:2244 |
| unnumbered | `test_valueless_tag_matching` | TEST558: Match value-less wildcard tag against any specific value | src/tagged_urn.rs:2229 |
| unnumbered | `test_valueless_tag_mixed_with_valued` | TEST555: Parse mix of value-less and valued tags together | src/tagged_urn.rs:2194 |
| unnumbered | `test_valueless_tag_parsing_multiple` | TEST554: Parse multiple value-less tags and serialize alphabetically | src/tagged_urn.rs:2182 |
| unnumbered | `test_valueless_tag_parsing_single` | TEST553: Parse single value-less tag as wildcard | src/tagged_urn.rs:2172 |
| unnumbered | `test_valueless_tag_roundtrip` | TEST561: Round-trip value-less tags through parse and serialize | src/tagged_urn.rs:2277 |
| unnumbered | `test_valueless_tag_specificity` | TEST560: Score value-less wildcard tags with graded specificity | src/tagged_urn.rs:2264 |
| unnumbered | `test_whitespace_in_input_rejected` | TEST566: Reject leading, trailing, and embedded whitespace in URN input | src/tagged_urn.rs:2335 |
| unnumbered | `test_wildcard_restrictions` | TEST534: Reject wildcard in keys but accept wildcard in values | src/tagged_urn.rs:1905 |
| unnumbered | `test_wildcard_tag` | TEST530: Convert specific tag value to wildcard and verify matching behavior | src/tagged_urn.rs:1839 |
| unnumbered | `test_with_tag_preserves_value` | TEST539: Preserve value case when adding tag with with_tag method | src/tagged_urn.rs:1964 |
| unnumbered | `test_with_tag_rejects_empty_value` | TEST540: Reject empty value string in with_tag method | src/tagged_urn.rs:1971 |
---

## Unnumbered Tests

The following tests are cataloged but do not currently participate in numeric test indexing.

- `test_best_match` — src/tagged_urn.rs:1794
- `test_bidirectional_accepts_with_special_values` — src/tagged_urn.rs:2573
- `test_builder` — src/tagged_urn.rs:1737
- `test_builder_preserves_case` — src/tagged_urn.rs:1752
- `test_builder_rejects_empty_value` — src/tagged_urn.rs:1983
- `test_builder_with_prefix` — src/tagged_urn.rs:1381
- `test_canonical_string_format` — src/tagged_urn.rs:1623
- `test_custom_prefix` — src/tagged_urn.rs:1337
- `test_directional_accepts_with_tag_overlap` — src/tagged_urn.rs:1766
- `test_duplicate_key_rejection` — src/tagged_urn.rs:1916
- `test_empty_tagged_urn` — src/tagged_urn.rs:1854
- `test_empty_value_error` — src/tagged_urn.rs:1940
- `test_empty_value_still_error` — src/tagged_urn.rs:2300
- `test_empty_with_custom_prefix` — src/tagged_urn.rs:1884
- `test_extended_character_support` — src/tagged_urn.rs:1893
- `test_full_cross_product_matching` — src/tagged_urn.rs:2474
- `test_has_tag_case_sensitive` — src/tagged_urn.rs:1947
- `test_invalid_escape_sequence_error` — src/tagged_urn.rs:1484
- `test_matching_case_sensitive_values` — src/tagged_urn.rs:1658
- `test_matching_different_prefixes_error` — src/tagged_urn.rs:2150
- `test_matching_semantics_test1_exact_match` — src/tagged_urn.rs:2014
- `test_matching_semantics_test2_instance_missing_tag` — src/tagged_urn.rs:2026
- `test_matching_semantics_test3_urn_has_extra_tag` — src/tagged_urn.rs:2045
- `test_matching_semantics_test4_request_has_wildcard` — src/tagged_urn.rs:2057
- `test_matching_semantics_test5_urn_has_wildcard` — src/tagged_urn.rs:2069
- `test_matching_semantics_test6_value_mismatch` — src/tagged_urn.rs:2081
- `test_matching_semantics_test7_pattern_has_extra_tag` — src/tagged_urn.rs:2093
- `test_matching_semantics_test8_empty_pattern_matches_anything` — src/tagged_urn.rs:2111
- `test_matching_semantics_test9_cross_dimension_constraints` — src/tagged_urn.rs:2131
- `test_merge_and_subset` — src/tagged_urn.rs:1811
- `test_merge_prefix_mismatch` — src/tagged_urn.rs:1828
- `test_missing_tag_handling` — src/tagged_urn.rs:1672
- `test_mixed_quoted_unquoted` — src/tagged_urn.rs:1466
- `test_mixed_special_values` — src/tagged_urn.rs:2531
- `test_must_not_have_exclamation_parsing` — src/tagged_urn.rs:2391
- `test_must_not_have_in_instance` — src/tagged_urn.rs:2455
- `test_must_not_have_pattern_requires_absent` — src/tagged_urn.rs:2438
- `test_numeric_key_restriction` — src/tagged_urn.rs:1926
- `test_prefix_case_insensitive` — src/tagged_urn.rs:1346
- `test_prefix_mismatch_error` — src/tagged_urn.rs:1363
- `test_prefix_required` — src/tagged_urn.rs:1576
- `test_question_mark_in_instance` — src/tagged_urn.rs:2419
- `test_question_mark_pattern_matches_anything` — src/tagged_urn.rs:2400
- `test_quoted_value_escape_sequences` — src/tagged_urn.rs:1450
- `test_quoted_value_special_chars` — src/tagged_urn.rs:1434
- `test_quoted_values_preserve_case` — src/tagged_urn.rs:1415
- `test_round_trip_escapes` — src/tagged_urn.rs:1565
- `test_round_trip_quoted` — src/tagged_urn.rs:1554
- `test_round_trip_simple` — src/tagged_urn.rs:1544
- `test_semantic_equivalence` — src/tagged_urn.rs:1995
- `test_serialization_round_trip_special_values` — src/tagged_urn.rs:2554
- `test_serialization_smart_quoting` — src/tagged_urn.rs:1501
- `test_specificity` — src/tagged_urn.rs:1700
- `test_specificity_with_special_values` — src/tagged_urn.rs:2724
- `test_tag_matching` — src/tagged_urn.rs:1635
- `test_tag_order_normalization` — src/tagged_urn.rs:2925
- `test_tagged_urn_creation` — src/tagged_urn.rs:1327
- `test_trailing_semicolon_equivalence` — src/tagged_urn.rs:1591
- `test_unquoted_values_lowercased` — src/tagged_urn.rs:1393
- `test_unspecified_question_mark_parsing` — src/tagged_urn.rs:2380
- `test_unterminated_quote_error` — src/tagged_urn.rs:1474
- `test_valueless_numeric_key_still_rejected` — src/tagged_urn.rs:2327
- `test_valueless_tag_at_end` — src/tagged_urn.rs:2207
- `test_valueless_tag_case_normalization` — src/tagged_urn.rs:2289
- `test_valueless_tag_directional_accepts` — src/tagged_urn.rs:2308
- `test_valueless_tag_equivalence_to_wildcard` — src/tagged_urn.rs:2217
- `test_valueless_tag_in_pattern` — src/tagged_urn.rs:2244
- `test_valueless_tag_matching` — src/tagged_urn.rs:2229
- `test_valueless_tag_mixed_with_valued` — src/tagged_urn.rs:2194
- `test_valueless_tag_parsing_multiple` — src/tagged_urn.rs:2182
- `test_valueless_tag_parsing_single` — src/tagged_urn.rs:2172
- `test_valueless_tag_roundtrip` — src/tagged_urn.rs:2277
- `test_valueless_tag_specificity` — src/tagged_urn.rs:2264
- `test_whitespace_in_input_rejected` — src/tagged_urn.rs:2335
- `test_wildcard_restrictions` — src/tagged_urn.rs:1905
- `test_wildcard_tag` — src/tagged_urn.rs:1839
- `test_with_tag_preserves_value` — src/tagged_urn.rs:1964
- `test_with_tag_rejects_empty_value` — src/tagged_urn.rs:1971

---

## Numbering Mismatches

These tests have a numbering disagreement between the function name and the authoritative immediate TEST comment/docstring above the test. This is reported explicitly so comment sync does not silently overwrite a misnumbered test.

- `unnumbered` / `test527` / `test_best_match` — src/tagged_urn.rs:1794
- `unnumbered` / `test576` / `test_bidirectional_accepts_with_special_values` — src/tagged_urn.rs:2573
- `unnumbered` / `test524` / `test_builder` — src/tagged_urn.rs:1737
- `unnumbered` / `test525` / `test_builder_preserves_case` — src/tagged_urn.rs:1752
- `unnumbered` / `test541` / `test_builder_rejects_empty_value` — src/tagged_urn.rs:1983
- `unnumbered` / `test505` / `test_builder_with_prefix` — src/tagged_urn.rs:1381
- `unnumbered` / `test519` / `test_canonical_string_format` — src/tagged_urn.rs:1623
- `unnumbered` / `test502` / `test_custom_prefix` — src/tagged_urn.rs:1337
- `unnumbered` / `test526` / `test_directional_accepts_with_tag_overlap` — src/tagged_urn.rs:1766
- `unnumbered` / `test535` / `test_duplicate_key_rejection` — src/tagged_urn.rs:1916
- `unnumbered` / `test531` / `test_empty_tagged_urn` — src/tagged_urn.rs:1854
- `unnumbered` / `test537` / `test_empty_value_error` — src/tagged_urn.rs:1940
- `unnumbered` / `test563` / `test_empty_value_still_error` — src/tagged_urn.rs:2300
- `unnumbered` / `test532` / `test_empty_with_custom_prefix` — src/tagged_urn.rs:1884
- `unnumbered` / `test533` / `test_extended_character_support` — src/tagged_urn.rs:1893
- `unnumbered` / `test573` / `test_full_cross_product_matching` — src/tagged_urn.rs:2474
- `unnumbered` / `test538` / `test_has_tag_case_sensitive` — src/tagged_urn.rs:1947
- `unnumbered` / `test512` / `test_invalid_escape_sequence_error` — src/tagged_urn.rs:1484
- `unnumbered` / `test521` / `test_matching_case_sensitive_values` — src/tagged_urn.rs:1658
- `unnumbered` / `test552` / `test_matching_different_prefixes_error` — src/tagged_urn.rs:2150
- `unnumbered` / `test543` / `test_matching_semantics_test1_exact_match` — src/tagged_urn.rs:2014
- `unnumbered` / `test544` / `test_matching_semantics_test2_instance_missing_tag` — src/tagged_urn.rs:2026
- `unnumbered` / `test545` / `test_matching_semantics_test3_urn_has_extra_tag` — src/tagged_urn.rs:2045
- `unnumbered` / `test546` / `test_matching_semantics_test4_request_has_wildcard` — src/tagged_urn.rs:2057
- `unnumbered` / `test547` / `test_matching_semantics_test5_urn_has_wildcard` — src/tagged_urn.rs:2069
- `unnumbered` / `test548` / `test_matching_semantics_test6_value_mismatch` — src/tagged_urn.rs:2081
- `unnumbered` / `test549` / `test_matching_semantics_test7_pattern_has_extra_tag` — src/tagged_urn.rs:2093
- `unnumbered` / `test550` / `test_matching_semantics_test8_empty_pattern_matches_anything` — src/tagged_urn.rs:2111
- `unnumbered` / `test551` / `test_matching_semantics_test9_cross_dimension_constraints` — src/tagged_urn.rs:2131
- `unnumbered` / `test528` / `test_merge_and_subset` — src/tagged_urn.rs:1811
- `unnumbered` / `test529` / `test_merge_prefix_mismatch` — src/tagged_urn.rs:1828
- `unnumbered` / `test522` / `test_missing_tag_handling` — src/tagged_urn.rs:1672
- `unnumbered` / `test510` / `test_mixed_quoted_unquoted` — src/tagged_urn.rs:1466
- `unnumbered` / `test574` / `test_mixed_special_values` — src/tagged_urn.rs:2531
- `unnumbered` / `test568` / `test_must_not_have_exclamation_parsing` — src/tagged_urn.rs:2391
- `unnumbered` / `test572` / `test_must_not_have_in_instance` — src/tagged_urn.rs:2455
- `unnumbered` / `test571` / `test_must_not_have_pattern_requires_absent` — src/tagged_urn.rs:2438
- `unnumbered` / `test536` / `test_numeric_key_restriction` — src/tagged_urn.rs:1926
- `unnumbered` / `test503` / `test_prefix_case_insensitive` — src/tagged_urn.rs:1346
- `unnumbered` / `test504` / `test_prefix_mismatch_error` — src/tagged_urn.rs:1363
- `unnumbered` / `test517` / `test_prefix_required` — src/tagged_urn.rs:1576
- `unnumbered` / `test570` / `test_question_mark_in_instance` — src/tagged_urn.rs:2419
- `unnumbered` / `test569` / `test_question_mark_pattern_matches_anything` — src/tagged_urn.rs:2400
- `unnumbered` / `test509` / `test_quoted_value_escape_sequences` — src/tagged_urn.rs:1450
- `unnumbered` / `test508` / `test_quoted_value_special_chars` — src/tagged_urn.rs:1434
- `unnumbered` / `test507` / `test_quoted_values_preserve_case` — src/tagged_urn.rs:1415
- `unnumbered` / `test516` / `test_round_trip_escapes` — src/tagged_urn.rs:1565
- `unnumbered` / `test515` / `test_round_trip_quoted` — src/tagged_urn.rs:1554
- `unnumbered` / `test514` / `test_round_trip_simple` — src/tagged_urn.rs:1544
- `unnumbered` / `test542` / `test_semantic_equivalence` — src/tagged_urn.rs:1995
- `unnumbered` / `test575` / `test_serialization_round_trip_special_values` — src/tagged_urn.rs:2554
- `unnumbered` / `test513` / `test_serialization_smart_quoting` — src/tagged_urn.rs:1501
- `unnumbered` / `test523` / `test_specificity` — src/tagged_urn.rs:1700
- `unnumbered` / `test577` / `test_specificity_with_special_values` — src/tagged_urn.rs:2724
- `unnumbered` / `test520` / `test_tag_matching` — src/tagged_urn.rs:1635
- `unnumbered` / `test501` / `test_tagged_urn_creation` — src/tagged_urn.rs:1327
- `unnumbered` / `test518` / `test_trailing_semicolon_equivalence` — src/tagged_urn.rs:1591
- `unnumbered` / `test506` / `test_unquoted_values_lowercased` — src/tagged_urn.rs:1393
- `unnumbered` / `test567` / `test_unspecified_question_mark_parsing` — src/tagged_urn.rs:2380
- `unnumbered` / `test511` / `test_unterminated_quote_error` — src/tagged_urn.rs:1474
- `unnumbered` / `test565` / `test_valueless_numeric_key_still_rejected` — src/tagged_urn.rs:2327
- `unnumbered` / `test556` / `test_valueless_tag_at_end` — src/tagged_urn.rs:2207
- `unnumbered` / `test562` / `test_valueless_tag_case_normalization` — src/tagged_urn.rs:2289
- `unnumbered` / `test564` / `test_valueless_tag_directional_accepts` — src/tagged_urn.rs:2308
- `unnumbered` / `test557` / `test_valueless_tag_equivalence_to_wildcard` — src/tagged_urn.rs:2217
- `unnumbered` / `test559` / `test_valueless_tag_in_pattern` — src/tagged_urn.rs:2244
- `unnumbered` / `test558` / `test_valueless_tag_matching` — src/tagged_urn.rs:2229
- `unnumbered` / `test555` / `test_valueless_tag_mixed_with_valued` — src/tagged_urn.rs:2194
- `unnumbered` / `test554` / `test_valueless_tag_parsing_multiple` — src/tagged_urn.rs:2182
- `unnumbered` / `test553` / `test_valueless_tag_parsing_single` — src/tagged_urn.rs:2172
- `unnumbered` / `test561` / `test_valueless_tag_roundtrip` — src/tagged_urn.rs:2277
- `unnumbered` / `test560` / `test_valueless_tag_specificity` — src/tagged_urn.rs:2264
- `unnumbered` / `test566` / `test_whitespace_in_input_rejected` — src/tagged_urn.rs:2335
- `unnumbered` / `test534` / `test_wildcard_restrictions` — src/tagged_urn.rs:1905
- `unnumbered` / `test530` / `test_wildcard_tag` — src/tagged_urn.rs:1839
- `unnumbered` / `test539` / `test_with_tag_preserves_value` — src/tagged_urn.rs:1964
- `unnumbered` / `test540` / `test_with_tag_rejects_empty_value` — src/tagged_urn.rs:1971

---

*Generated from Rust source tree*
*Total tests: 96*
*Total numbered tests: 18*
*Total unnumbered tests: 78*
*Total numbered tests missing descriptions: 0*
*Total numbering mismatches: 77*
