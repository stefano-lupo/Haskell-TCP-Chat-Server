import System.IO
import Control.Concurrent.STM -- software transactional memory
import Control.Concurrent     -- forkIO


type Account = TVar Int

-- Note STM here is an action just like IO
-- Only the range of side effects it can cause are much smaller
-- The Main things we can do are read / write transaction variables TV
-- Note we CANT run IO actions inside of STM actions
-- Only allowed mutate TVars
withdraw :: Account -> Int -> STM ()
withdraw account amount = do
  bal <- readTVar account
  writeTVar account (bal - amount)

deposit :: Account -> Int -> STM ()
deposit account amount = withdraw account (-amount)

transfer :: Account -> Account -> Int -> IO ()
transfer from to amount
  = atomically (do
    deposit to amount
    withdraw from amount)


showAccount name account = do
  balance <- atomically (readTVar account)
  hPutStr stdout (name ++ ": $")
  hPutStr stdout (show balance ++ "\n")


-- Using blocking with retry
limitedWithDraw :: Account -> Int -> STM ()
limitedWithDraw acc amount = do
  bal <- readTVar acc
  -- Retry throws out the transaction and starts over when it gets processor time
{-- Abstracted into check function
  if amount > 0 && amount > bal
  then retry
  else writeTVar acc (bal-amount)
--}
  check (amount < bal)    -- Execution only drops past here once condition met
  writeTVar acc (bal - amount)


-- Deposits amount after 3 sec
delayDeposit :: Account -> Int -> IO ()
delayDeposit acc amount = do
  putStrLn "About to deposit some money..."
  threadDelay 3000000
  putStrLn "Depositing now.."
  atomically (do  bal <- readTVar acc
                  writeTVar acc (bal + amount))


-- there is also support for choice
-- We can specify two STM actions as parameters
-- if the first one retrys then it will try 2nd, then will retry this entire STM (ie try first again..)
limitedWithDrawMeOrParent :: Account -> Account -> Int -> STM ()
limitedWithDrawMeOrParent myAccount parentAccount amount = do
  orElse (limitedWithDraw myAccount amount) (limitedWithDraw parentAccount amount)

main = do
  fromAcc <- atomically (newTVar 200)
  toAcc <- atomically (newTVar 100)
  transfer fromAcc toAcc 50
  showAccount "Account 1" fromAcc
  showAccount "Account 2" toAcc

  acc3 <- atomically (newTVar 100)
  forkIO(delayDeposit acc3 100)
  putStrLn "About to try withdraw 120"
  atomically (limitedWithDraw acc3 120)
  putStrLn "Successfully withdrew 120"

  myAcc <- atomically (newTVar 1)
  parentAcc <- atomically (newTVar 1000)

  atomically (limitedWithDrawMeOrParent myAcc parentAcc 500)

  showAccount "Parent Account" parentAcc
  showAccount "My Account" myAcc