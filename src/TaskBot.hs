module TaskBot
  ( runBot
  ) where

import Consts
import System.IO

type Task = String
type UserName = String
type BotName = String

-- | Run task bot.
-- Commands:
-- /list -- show task list
-- /complete -- complete the last task
-- /exit -- stop bot
-- Any other input is considered as new task.
runBot :: IO ()
runBot = do
  -- disable buffering for stdout
  hSetBuffering stdout NoBuffering
  putStrLn namePrompt
  name <- getLine
  putStrLn botNamePrompt
  nameBot <- getLine
  go name nameBot []
  where
    -- Helper function to interact with user and update tasks list
    go :: UserName -> BotName -> [Task] -> IO ()
    go name nameBot taskList = do
      putStr $ name ++ "> "
      str <- getLine
      if (str == "/exit")
        then putStrLn goodbyeMsg
        else do
          -- process input unless it is an "/exit" command
          let (output, newTaskList) = processCommand str taskList
          putStrLn (nameBot ++ "> " ++ output)
          go name nameBot newTaskList

-- | Process user input. Returns output string to be printed by bot and
-- updated list of tasks in a tuple.
processCommand :: String -> [Task] -> (String, [Task])
processCommand cmd prevTaskList = case cmd of
  "/list" -> cmdList prevTaskList 1
  "/complete" -> cmdComplete prevTaskList
  "/delete" -> cmdDelete prevTaskList
  _ -> addTask cmd prevTaskList

-- | Command to show tasks list.


cmdList :: [Task] -> Int -> (String, [Task])
cmdList tasks y = ("\n" ++ vyvod tasks y, tasks)

vyvod :: [Task] -> Int -> String
vyvod [] _ = emptyList 
vyvod (x:xs) y = (show y ++ ". " ++ x ++ if xs == [] then "" else "\n" ++ (vyvod xs (y + 1))) 

-- | Command to complete the last task.
cmdComplete :: [Task] -> (String, [Task])
cmdComplete [] = (noTasksMsg, [])
cmdComplete (_:xs) = (completeMsg, xs)

cmdDelete :: [Task] -> (String, [Task])
cmdDelete [] = (clearAlready, [])
cmdDelete _ = (clearList, [])

-- | Add new task to tasks list.
addTask :: String -> [Task] -> (String, [Task])
addTask task l = (newTaskMsg, task:l)
