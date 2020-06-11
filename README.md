# morley-sop (beta)

Example commands:

```bash
$ morley-sop "$(cat fa2ext_stub.tz | tr -d '\n')" --help 

Available commands:

Usage: morley-sop %_2%balance_of%(*) --callback (contract (list (pair nat (pair address nat))))
                                     --requests (list (pair address nat))

Usage: morley-sop %_2%is_operator%(***(All_tokens%)) --All_tokens unit
                                                     --callback_1 (contract (pair bool (pair address (pair address (or unit (set nat))))))
                                                     --operator_1 address
                                                     --owner address

Usage: morley-sop %_2%is_operator%(***(Some_tokens%)) --Some_tokens (set nat)
                                                      --callback_1 (contract (pair bool (pair address (pair address (or unit (set nat))))))
                                                      --operator_1 address
                                                      --owner address

Usage: morley-sop %_3%_5%permissions_descriptor% --permissions_descriptor (contract (pair (pair (option (pair (option address) string)) (or unit unit)) (pair (or unit (or unit unit)) (pair (or unit unit) (or unit (or unit unit))))))

Usage: morley-sop %_3%_5%set_administrator% --set_administrator address

Usage: morley-sop %_3%mint%(***) --address address --amount nat --symbol string
                                 --token_id nat

Usage: morley-sop _1%_8%set_pause% --set_pause bool

Usage: morley-sop _1%_8%token_metadata%(*) --callback_2 (contract (list (pair nat (pair string (pair string (pair nat (map string string)))))))
                                           --token_ids (list nat)

Usage: morley-sop _1%_9%_10%_11%single_transfer%(***) --amount_1 nat
                                                      --from_ address
                                                      --to_ address
                                                      --token_id_1 nat

Usage: morley-sop _1%_9%_10%_11%transfer% --transfer (list (pair address (pair address (pair nat nat))))

Usage: morley-sop _1%_9%_10%_12%_15%Add_operator%(**(All_tokens_1%)) --All_tokens_1 unit
                                                                     --operator_2 address
                                                                     --owner_1 address

Usage: morley-sop _1%_9%_10%_12%_15%Add_operator%(**(Some_tokens_1%)) --Some_tokens_1 (set nat)
                                                                      --operator_2 address
                                                                      --owner_1 address

Usage: morley-sop _1%_9%_10%_12%_15%Remove_operator%(**(All_tokens_2%)) --All_tokens_2 unit
                                                                        --operator_3 address
                                                                        --owner_2 address

Usage: morley-sop _1%_9%_10%_12%_15%Remove_operator%(**(Some_tokens_2%)) --Some_tokens_2 (set nat)
                                                                         --operator_3 address
                                                                         --owner_2 address

Usage: morley-sop _1%_9%_10%_12%update_operators% --update_operators (list (or (pair address (pair address (or unit (set nat)))) (pair address (pair address (or unit (set nat))))))

Usage: morley-sop _1%_9%total_supply%(*) --callback_3 (contract (list (pair nat nat)))
                                         --token_ids_1 (list nat)
```

```bash
$ morley-sop "$(cat fa2ext_stub.tz | tr -d '\n')" '_1%_9%_10%_11%single_transfer%(***)' --help 

Usage: morley-sop _1%_9%_10%_11%single_transfer%(***) --amount_1 nat
                                                      --from_ address
                                                      --to_ address
                                                      --token_id_1 nat

Available options:
  -h,--help                Show this help text
```

```bash
$ stack build --fast && stack exec -- morley-sop "$(cat fa2ext_stub.tz | tr -d '\n')" '_1%_9%_10%_11%single_transfer%(***)' --amount_1 42 --from_ "\"$ALICE_ADDRESS\"" --to_ "\"$BOB_ADDRESS\"" --token_id_1 0

Right (Right (Right (Left (Right (Pair "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr" (Pair "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm" (Pair 0 42)))))))
```

