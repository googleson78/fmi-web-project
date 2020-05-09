% Servant in 5(?) minutes
% Georgi Lyubenov

---
aspectratio: 1610
colortheme: crane
mainfont: NotoSans-Regular.ttf

---
# Ceremonial rites
Включване на нужни разширения и различни модули
```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Example where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.IORef
import Data.Proxy
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API
import Servant.Client
import Servant.Server
import System.IO.Unsafe
```

# Данни
Това което ще пазим и пращаме
```haskell
newtype CounterVal = CounterVal {getCounterVal :: Int}
```

# Данни
...с нужните функционалности за (де)кодиране
```haskell
newtype CounterVal = CounterVal {getCounterVal :: Int}
  deriving (Show, Num, FromJSON, ToJSON)
```

# Дефиниция на API
Главното ни API - кобминация от две други
```haskell
type Counter = GetCounter :<|> StepCounter
```

# `GetCounter`
API-то за взимане взимане на брояч
```haskell
type GetCounter
```

# `GetCounter`
Отговаряме с JSON-кодирана версия на `CounterVal`
```haskell
type GetCounter = Get '[JSON] CounterVal
```

# `StepCounter`
API-то за инкрементиране на брояч
```haskell
type StepCounter
```

# `StepCounter`
Ще е на под-адрес "step"
```haskell
type StepCounter = "step" :> _
```

# `StepCounter`
Ще взима header "User"
```haskell
type StepCounter = "step" :> Header "User" String :> _
```

# `StepCounter`
Ще е на няколко реда, защото ми е голям фонтът
```haskell
type StepCounter =
  "step" :>
  Header "User" String :>
  _
```

# `StepCounter`
Ще взима тяло - с колко да увеличим брояча
```haskell
type StepCounter =
  "step" :>
  Header "User" String :>
  ReqBody '[JSON] CounterVal :>
  _
```

# `StepCounter`
Ще отговаря на `POST` и няма какво да връща
```haskell
type StepCounter =
  "step" :>
  Header "User" String :>
  ReqBody '[JSON] CounterVal :>
  PostNoContent
```

# Имплементация на сървър
Огледално на типа - главния сървър
```haskell
counter :: Server Counter
counter = getCounter :<|> stepCounter
```

# `getCounter`
```haskell
getCounter :: Server GetCounter
getCounter = readCounter
```

# `readCounter`
Не е интересно, имплементирано с глобална променлива.

# `stepCounter`
Получаваме два аргумента, благодарение на типа `StepCounter`
```haskell
stepCounter :: Server StepCounter
stepCounter maybeUser amount = _
```

# `stepCounter`
"Истинската" сигнатура на функцията, пресметната е от типовата функция `Server`
```haskell
stepCounter :: Maybe String -> CounterVal -> Handler NoContent
```

# `stepCounter`
"Истинската" сигнатура на функцията, пресметната е от типовата функция `Server`
```haskell

                    -- type StepCounter = "step" :>
stepCounter ::      --   "step" :>
  Maybe String ->   --   Header "user" String :>
  CounterVal ->     --   ReqBody '[JSON] CounterVal :>
  Handler NoContent --   PostNoContent
```

# `stepCounter`
Връщайки се на "скритата" сигнатура, да видим имплементацията
```haskell
stepCounter :: Server StepCounter
stepCounter maybeUser amount = _
```

# `stepCounter`
Проверяваме дали ни е даден "User" header-ът
```haskell
stepCounter :: Server StepCounter
stepCounter maybeUser amount =
  case maybeUser of
    Nothing -> _
    Just user -> _
```

# `stepCounter`
Ако не, връщаме `401` и казваме на потребителя, че "User" е нужно
```haskell
stepCounter :: Server StepCounter
stepCounter maybeUser amount =
  case maybeUser of
    Nothing -> throwError err401 { errBody = userRequired }
    Just user -> _
  where
    userRequired :: String
    userRequired = "You must identify yourself via the \"User\" header."
```

# `stepCounter`
Ако да, позволяваме само на "Pesho" да променя брояча
```haskell
stepCounter :: Server StepCounter
stepCounter maybeUser amount =
  case maybeUser of
    Nothing -> throwError err401 { errBody = userRequired }
    Just user ->
      if user == "Pesho"
      then do
        incrementCounter amount
        pure NoContent
      else throwError err403 { errBody = onlyPesho }
  where
    userRequired :: String
    userRequired = "You must identify yourself via the \"User\" header."
    onlyPesho :: String
    onlyPesho = "Only Pesho can modify the counter."
```

# `incrementCounter`
Не е интересно, инкрементира глобална променлива по thread-safe начин.

# Proxy
За да можем да реферираме към API-то ни на ниво "стойност"
```haskell
api :: Proxy Counter
api = Proxy
```

# Пускане на сървъра
Използваме `warp` за `HTTP` имплементация, подавайки порт и `counter` от по-горе

# Пускане на сървъра
Използваме `warp` за `HTTP` имплементация, подавайки порт и `counter` от по-горе

Чрез `serve api counter` получаваме подходящо представяне (`Application`), което да дадем на `run` функцията от `warp`.
```haskell
startServer :: IO ()
startServer = do
  putStrLn "Starting a server"
  run 8080 (serve api counter)
```

```bash
<long ghci command-line with the right packages>
> startServer
Starting a server
```

# `curl` заявки
Взимане на брояча в началото
```bash
> curl localhost:8080
0
```

# `curl` заявки
Опит за инкрементиране v0
```bash
> curl \
    -X POST \
    -H 'Content-Type: application/json' \
    -d '13' \
    localhost:8080/step
You must identify yourself via the "User" header.
```

# `curl` заявки
Опит за инкрементиране v1
```bash
> curl \
    -X POST \
    -H 'Content-Type: application/json' \
    -d '13' \
    -H 'User: Gosho' \
    localhost:8080/step
Only Pesho can modify the counter.
```

# `curl` заявки
Успешно инкрементиране, с проверка след това
```bash
> curl \
    -X POST \
    -H 'Content-Type: application/json' \
    -d '13' \
    -H 'User: Pesho' \
    localhost:8080/step
> curl localhost:8080
13
```

# `servant-client` заявки
Много по-лесно!

# `servant-client` заявки
Без:

# `servant-client` заявки
Без:

* ръчно слагане на

# `servant-client` заявки
Без:

* ръчно слагане на

  * метод

# `servant-client` заявки
Без:

* ръчно слагане на

  * метод
  * тяло

# `servant-client` заявки
Без:

* ръчно слагане на

  * метод
  * тяло
  * параметри

# `servant-client` заявки
Без:

* ръчно слагане на

  * метод
  * тяло
  * параметри
  * header-и

# `servant-client` заявки
Без:

* ръчно слагане на

  * метод
  * тяло
  * параметри
  * header-и

* ръчно (де)кодиране от/в JSON

# `servant-client` заявки
Без:

* ръчно слагане на

  * метод
  * тяло
  * параметри
  * header-и

* ръчно (де)кодиране от/в JSON

  **Не може да сбъркаш типовете!!**

# `servant-cleint` заявки
Както чрез `server api counter` се навръзва сървър от по-малки парчета,
така чрез `client api` се генерират функции за заявки.

# `servant-cleint` заявки
На нас ни трябват две отделни функции, затова поотделно ги свързваме с имена.

```haskell
reqGetCounter :: Client ClientM GetCounter
reqGetCounter = let f :<|> _ = client api in f

reqStepCounter :: Client ClientM StepCounter
reqStepCounter = let _ :<|> f = client api in f
```

# `ClientM a`
`ClientM` е "среда" в която можем да правим заявки и да получаваме грешки.

# `ClientM a`
`ClientM` е "среда" в която можем да правим заявки и да получаваме грешки.

"Пази в себе си" стойност от тип `a` - реално резултат на заявка в нашия случай.

# `ClientM a`
`ClientM` е "среда" в която можем да правим заявки и да получаваме грешки.

"Пази в себе си" стойност от тип `a` - реално резултат на заявка в нашия случай.

Можем да си го мислим като заявка, чакащата да се случи. Изпълнява се с `runClientM`

# `Client`
Отново `Client` е функция на типово ниво, която пресмята какъв тип трябва да има нашата функция за заявки.

# `reqGetCounter`
"Истинския" тип на `reqGetCounter`
```haskell
reqGetCounter :: ClientM CounterVal
```
Не взима никакви аргументи, евентуално връща `CounterVal` когато се изпълни.

# `reqGetCounter`
Извикване
```haskell
> runClientM reqGetCounter defaultClientEnv
Right (CounterVal {getCounterVal = 39})
```
Връща `Right` (индикация за "успех"), който съдържа сегашната стойност на брояча.

# `reqStepCounter`
"Истински" тип на `reqStepCounter`
```haskell
reqStepCounter :: Maybe User -> CounterVal -> ClientM NoContent
```

# `reqStepCounter`
"Истински" тип на `reqStepCounter`
```haskell
reqStepCounter :: Maybe User -> CounterVal -> ClientM NoContent
```
Обикновена `Haskell`-ска функция която взима два аргумента - потребител (header-а) и стойност с която да инкрементираме (тялото).

Връща специален тип `NoContent`, защото това сме специфицирали в API-то

# `reqStepCounter`
"Истински" тип на `reqStepCounter`
```haskell
reqStepCounter :: Maybe User -> CounterVal -> ClientM NoContent
```
Обикновена `Haskell`-ска функция която взима два аргумента - потребител (header-а) и стойност с която да инкрементираме (тялото).

Връща специален тип `NoContent`, защото това сме специфицирали в API-то

`servant-client` се грижи вътрешно да вземе тези данни и да ги сложи по правилния начин,
вместо ръчната работа която извършихме с `curl`.

# `reqStepCounter`
Извикване
```haskell
> runClientM (reqStepCounter (Just "Pesho") 13) defaultClientEnv
Right NoContent
> runClientM reqGetCounter defaultClientEnv
Right (CounterVal {getCounterVal = 26})
```

# Бонус неща

# Бонус неща

* Генериране на документация
    Трябва само да дадем описание на "основните" ни данни - в този случай `CounterVal`

# Бонус неща

* Генериране на документация
    Трябва само да дадем описание на "основните" ни данни - в този случай `CounterVal`
* Генериране на функции за извикване в други езици

# Бонус неща

* Генериране на документация
    Трябва само да дадем описание на "основните" ни данни - в този случай `CounterVal`
* Генериране на функции за извикване в други езици
    * `js`

# Бонус неща

* Генериране на документация
    Трябва само да дадем описание на "основните" ни данни - в този случай `CounterVal`
* Генериране на функции за извикване в други езици
    * `js`
    * `elm`

# Бонус неща

* Генериране на документация
    Трябва само да дадем описание на "основните" ни данни - в този случай `CounterVal`
* Генериране на функции за извикване в други езици
    * `js`
    * `elm`
    * `purescript`

# Бонус неща

* Генериране на документация
    Трябва само да дадем описание на "основните" ни данни - в този случай `CounterVal`
* Генериране на функции за извикване в други езици
    * `js`
    * `elm`
    * `purescript`
    * имплементирането за нов език е лесно и псевдо-автоматизирано - оставена е само генерацията на код!

# Заключение

# Заключение

Като цяло утвърдяването на API-то на формален език позволява и неговата програмна манипулация.

# Заключение

Като цяло утвърдяването на API-то на формален език позволява и неговата програмна манипулация.

Допълнителен бонус е че езикът е програмен език - можеш да се възползваш от всичките средства за
програмиране с типове, които Haskell има.

# Заключение

Като цяло утвърдяването на API-то на формален език позволява и неговата програмна манипулация.

Допълнителен бонус е че езикът е програмен език - можеш да се възползваш от всичките средства за
програмиране с типове, които Haskell има.

Избягват се **всички** грешки свързани със ръчен труд при слагане на параметри на заявки и (де)кодирания,
като получаваш и типизирани функции на езика на който програмираш като бонус!

# cya

![bye](raccoon-goodbye.jpeg)

# скучните неща
Не са директно свързани със `servant`, затова са най-долу

```haskell
incrementCounter :: CounterVal -> Handler ()
incrementCounter n =
  liftIO $ atomicModifyIORef' globalCounter $ \old -> (old + n, ())

readCounter :: Handler CounterVal
readCounter = liftIO $ readIORef globalCounter

globalCounter :: IORef CounterVal
globalCounter = unsafePerformIO $ newIORef $ CounterVal 0
{-# NOINLINE globalCounter #-}

defaultClientEnv :: ClientEnv
defaultClientEnv = unsafePerformIO $ do
  manager <- newManager defaultManagerSettings
  pure $ mkClientEnv manager $ (BaseUrl Http "localhost" 8080 "")
{-# NOINLINE defaultClientEnv #-}
```
