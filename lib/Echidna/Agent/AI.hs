module Echidna.Agent.AI where

import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Text.Printf (printf)

import Echidna.Types.Agent
import Echidna.Types.InterWorker

data AIAgent = AIAgent
  { agentId :: AgentId
  }

instance Show AIAgent where
  show (AIAgent aid) = "AIAgent " ++ show aid

instance Agent AIAgent where
  getAgentId (AIAgent aid) = aid

  runAgent _ bus _ = do
    inbox <- atomically $ dupTChan bus

    let loop = do
          msg <- atomically $ tryReadTChan inbox
          case msg of
            Just (WrappedMessage sender payload) -> do
               case payload of
                  Broadcast (FoundBug _test) ->
                     printf "[AI] Noticed a bug found by %s. Analyzing...\n" (show sender)
                  _ -> return ()
               loop
            Nothing -> do
               threadDelay 1000000 -- 1s
               -- printf "[AI] Monitoring campaign...\n"
               loop

    loop
