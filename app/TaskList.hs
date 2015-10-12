module TaskList
    ( dispatch
    ) where

    import Euclid
    import FastDegree
    import GeneratePrimeNumber
    import qualified RSA as R
    import Factorization
    import CustomCodes

    dispatch :: String -> [String] -> IO ()
    dispatch "-extend-euclid" params = case params of
                                         [a, b] -> print $ euc (read a) (read b)
                                         _      -> printHelper
    dispatch "-fast-degree" params = case params of
                                       [n, a, e] -> print $ deg (read n) (read a) (read e)
                                       _         -> printHelper
    dispatch "-prime-factor" params = case params of
                                       [n] -> do
                                         prime <- genPrime (read n)
                                         print prime
                                       _  -> printHelper
    dispatch "-rsa-params-factor" params = case params of
                                       [n, k_name] -> do
                                         params@(pub_k, pr_k) <- R.genParams (read n)
                                         writeFile ("./.keys/" ++ k_name ++ "_rsa.pub") (show pub_k)
                                         writeFile ("./.keys/" ++ k_name ++ "_rsa") (show pr_k)
                                         print params
                                       _   -> printHelper
    dispatch "-encrypt" params = case params of
                                      [k_name, text] -> do
                                        pub_k <- readFile ("./.keys/" ++ k_name ++ "_rsa.pub")
                                        print $ R.encrypt (read pub_k) text
                                      _              -> printHelper
    dispatch "-decrypt" params = case params of
                                      [k_name, code] -> do
                                        pr_k <- readFile ("./.keys/" ++ k_name ++ "_rsa")
                                        print $ R.decrypt (read pr_k) (read code)
                                      _              -> printHelper
    dispatch "-second-task" params = case params of
                                      [n, e, sw] -> do
                                        pq <- fact (read n)
                                        print pq
                                        let p_key = R.calcPrivateKey pq (read e)
                                        print $ customDecrypt p_key (read sw)
                                      _ -> printHelper
    dispatch "-help" _ = printHelper
    dispatch _ _ =  printHelper

    printHelper :: IO ()
    printHelper = putStr $  "cmd = [-extend-euclid | -fast-degree | -prime-factor | -rsa-params-facor | -encrypt | -decrypt | -second-task | -help]\n"
                         ++ "in/out values = -extend-euclid {A} {B} -> {x} {y}\n"
                         ++ "                -fast-degree {N - modulo} {A - number} {E - degree} -> {r - result}\n"
                         ++ "                -prime-factor {N - bit count} -> {p - prime number}\n"
                         ++ "                -rsa-params-factor {N - bit count} {KEYS_NAME} -> {(pub_key, pr_key) - public and private keys with modulo}\n"
                         ++ "                -encrypt {KEYS_NAME} {TEXT} -> {code}\n"
                         ++ "                -decrypt {KEYS_NAME} {CODE} -> {text}\n"
                         ++ "                -second-task {N, E, SW} -> {(p, q) - factorization result} {text}\n"
