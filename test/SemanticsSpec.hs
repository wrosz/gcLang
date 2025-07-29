{-# LANGUAGE OverloadedStrings #-}

module SemanticsSpec (tests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import Parser
import Semantics (runProgram, GCState(..))
import qualified Data.Map as Map

-- Test 1: GC usuwa nieosiągalne obiekty (cykl)
test_gc_removes_unreachable_cycle :: Assertion
test_gc_removes_unreachable_cycle = do
  let prog = Program
        [ FuncDef "createCycle" [] (
            Seq
              (Let "a" (New ["ref"] [Null]) (
                Let "b" (New ["ref"] [Null]) (
                  Seq
                    (FieldAssign (Var "a") "ref" (Var "b"))
                    (FieldAssign (Var "b") "ref" (Var "a"))
                )))
              (Var "a")
          )
        , FuncDef "main" [] (
            Let "y" (New ["val"] [IntLit 42]) (Var "y")
         )
        ]
        (Call "main" [])

  let (_, finalState) = runProgram prog
  let heapSize = Map.size (heap finalState)
  assertEqual "Heap should contain only one live object after GC" 1 heapSize


-- Test 2: GC nie usuwa żywego obiektu
test_gc_keeps_reachable :: Assertion
test_gc_keeps_reachable = do
  let prog = Program [] (Let "x" (New ["val"] [IntLit 10]) (Var "x"))
  let (_, finalState) = runProgram prog
  let heapSize = Map.size (heap finalState)
  assertEqual "Heap should contain 1 reachable object" 1 heapSize

-- Test 3: GC usuwa porzucone tablice
test_gc_removes_unreachable_array :: Assertion
test_gc_removes_unreachable_array = do
  let prog =
        Program []
          (Seq
            (NewArray (IntLit 5) (IntLit 99))  -- tworzymy tablicę, ale jej nie zapisujemy
            (New ["val"] [IntLit 42])          -- to jedyny żywy obiekt
          )
  let (_, finalState) = runProgram prog
  let heapSize = Map.size (heap finalState)
  assertEqual
    "Heap should contain only one reachable object (array removed)"
    1
    heapSize


-- Test 4: GC zachowuje tablicę przypisaną do zmiennej
test_gc_keeps_reachable_array :: Assertion
test_gc_keeps_reachable_array = do
  let prog =
        Program []
          (Let "arr"
            (NewArray (IntLit 3) (IntLit 0))
            (Var "arr")
          )
  let (_, finalState) = runProgram prog
  let heapSize = Map.size (heap finalState)
  assertEqual "Heap should contain 1 reachable array" 1 heapSize


-- Wszystkie testy
tests =
  [ testGroup "Semantics - Garbage Collector"
    [ testCase "GC removes unreachable cycles" test_gc_removes_unreachable_cycle
    , testCase "GC keeps reachable objects" test_gc_keeps_reachable
    , testCase "GC removes unreachable arrays" test_gc_removes_unreachable_array
    , testCase "GC keeps reachable arrays" test_gc_keeps_reachable_array
    ]
  ]

