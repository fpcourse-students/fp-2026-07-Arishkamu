{-
  '.  `~~`~`~~^`~~`~^~^`~~`~~`~^~`.   .`     /
    `> ' . ' ". _ '-"`  .  ` ' .   | .    ' /
   .' ,'~^~^~^`^~~^`^~`~~~^~~~^;       ' ' |
   .-'                          \ ` : ` . "|_
                                | | ,  | : | \
                                | "   `    |  \
                                | , :   |  |  /~--,__
                                |     `  , |  \__,--~'
                        ("-,_   |" `  | `, |   /
                         "-,_"-,|, _    ,  |__/
                             "-,|   \\______/ \
                                |,  | / _\___/ \
                               /| "  \ \/\  ||   \
                              / |     \   \,||    \
                        __.--~\ |`  `  \__/ ||     ),_
                       `~--.__/ | , |    :/  \   /-,_"-,_    __
                             \  |   `,   /=,'/ ('    `-,_"-,/_ /~=,_
                              \_| |   __/=/ |   \        "-/-,//~=,_~=,_
                              / |`   (__ / ('    \         \_//_~=,_~=,
                             / \\______//   \     )         '=,_~=,_~=
                            /   ___/_ \ | .| `\__(              ~=,_~
                           /    |   /\/ /" |
                        _.(     | ./   /   |
              __    _.-"_.-\   /|_ \__/ ' '|
         _.=~\ _\.-"_.-'    `) \  \:   | . |
     _.=~_.=~\\.-\-"        /   `.=\  .'   |
      .=~_.=~_\\_/         /    | \=\__  | |
       =~_.=~_.=`         (     `) \ __)  '|
        ~_.=~              )__/'| '   "   '|
                                | . " | ' .|
 '-"_'"-'_"'-_'"-_''_"-"-_"-\ \/'   '   . '`\/"/- '"-_-"'_ _-"'__-"'_
-_ --"-"_---_""-_'"--"'_-"-'\\/.\\' / /".\,//\//'-"-_'"-"'_'"-"''"-"'_
   -"-'_-"_-"-_"-"'_'-"-"_``"-`"_`'""-`''""'-_'"-"-_"'"-   '"- _'"- _
-}

-- | К сессии. На доске может быть вариация одной из этих задач.
module Level2 where

import Data.List (unfoldr)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord (Down (..))
import MetaUtils (todo, todoImpl)


-- 2.1. Специализированные свёртки.

data Nat = Z | S Nat deriving (Eq, Show, Ord)

foldNat :: a -> (a -> a) -> Nat -> a
foldNat z s = \case
  Z -> z
  S n -> s (foldNat z s n)

-- Для Left верните сумму двух Nat, для Right — длину строки как Nat.
-- В решении и вспомогательных функциях используйте только foldr,
-- uncurry, either, foldNat, Z, S и комбинаторы const, id, (.), ($).
-- Без сопоставления с образцом и явной рекурсии.

sumOrLength :: Either (Nat, Nat) String -> Nat
sumOrLength = either leftHandler rightHandler
  where
  leftHandler = uncurry $ \a -> foldNat a S
  rightHandler = foldr (\_ -> S) Z


-- 2.2. Реализуйте map через unfoldr, не конструируя результат через [] и (:).
-- Сопоставлять входной список с образцом можно. Сохраните продуктивность:
-- take 3 (map' (+1) [1..]) == [2,3,4].

map' :: (a -> b) -> [a] -> [b]
map' f = unfoldr $ \case
  []   -> Nothing
  x:xs -> Just (f x, xs)


-- 2.3. Разное: словарь как частичная функция.
-- Выберите способ решения сами. Новая запись заменяет прежнее значение.
-- myInsertMany обрабатывает контейнер слева направо: последний дубликат
-- побеждает. Остальные ключи сохраняются. Удаление отсутствующего безопасно.

newtype MyMap k v = MyMap { myLookup :: k -> Maybe v }

myEmpty :: MyMap k v
myEmpty = MyMap { myLookup = (\_ -> Nothing) }

myInsert :: Eq k => k -> v -> MyMap k v -> MyMap k v
myInsert k v (MyMap mpLookUp) = MyMap $
  \k_look -> if k == k_look then Just v else mpLookUp k_look

myInsertMany :: (Eq k, Foldable t) => MyMap k v -> t (k, v) -> MyMap k v
myInsertMany mp = foldl (\r (k, v) -> myInsert k v r) mp

myDelete :: Eq k => k -> MyMap k v -> MyMap k v
myDelete k (MyMap mpLookUp) = MyMap $
  \k_look -> if k == k_look then Nothing else mpLookUp k_look

myDeleteMany :: (Eq k, Foldable t) => MyMap k v -> t k -> MyMap k v
myDeleteMany = foldr myDelete


-- 2.4. Напишите полные инстансы Semigroup и Monoid.
-- При объединении общий ключ берётся из ЛЕВОГО словаря, иначе — из правого.
-- Ограничения на тип значения не нужны: значения не объединяются.
-- Нейтральный элемент — словарь, возвращающий Nothing для любого ключа.

instance Semigroup (MyMap k v) where
  (MyMap lup1) <> (MyMap lup2) = MyMap $
    \k -> maybe (lup2 k) Just (lup1 k)

instance Monoid (MyMap k v) where
  mempty = myEmpty


-- 2.5. Напишите полный Foldable (MyMap k) для (Enum k, Bounded k).
-- Обходите ключи в порядке [minBound .. maxBound], пропуская Nothing.
-- Каждый существующий ключ даёт одно значение, даже если значения равны.

instance (Eq k, Enum k, Bounded k) => Foldable (MyMap k) where
  foldMap f (MyMap lup1) = foldMapK minBound
    where
    foldMapK key = ( case lup1 key of
      Nothing -> mempty
      Just x  -> f x ) <> (if key == maxBound then mempty else foldMapK (succ key))


-- 2.6. Разное: отформатируйте результат wordCount.
-- Сначала исключите слова длиной <= smallWordsSize, затем сортируйте
-- по убыванию числа вхождений, при равенстве — лексикографически.
-- После фильтрации и сортировки возьмите topSize слов.
-- Строка имеет вид "слово : N\n". Неположительный topSize даёт "".
-- Вместе с решением 1.6 проверьте вывод на «Алисе в Стране чудес».
-- Из корня репозитория выполните:
--   make run ARGS='wc text/alice.txt'
-- Приложение выведет 30 самых частых слов длиной больше трёх букв.

data WordCountShow = WordCountShow
  { topSize :: Int
  , smallWordsSize :: Int
  } deriving (Eq, Show)

stdWcShow :: WordCountShow
stdWcShow = WordCountShow { topSize = 10, smallWordsSize = 3 }

wordCountShow :: WordCountShow -> Map String Int -> String
wordCountShow WordCountShow{ topSize = ts, smallWordsSize = sws } =
  toString . take ts . List.sortBy (\(k1, n) (k2, m) -> if n == m then compare k1 k2 else compare m n) . filter (\(k,_) -> length k > sws) . Map.toList
  where
  toString [] = ""
  toString ((k,n):xs) = k ++ " : " ++ show n ++ "\n" ++ toString xs

-- 2.7. Разное: сохраните значения Right в исходном порядке,
-- пропустите Left. Метод решения выберите сами.
-- collectRights [Right 2, Left "ошибка", Right 1] == [2,1].
-- Конечный префикс результата должен работать на бесконечном входе.

collectRights :: [Either e a] -> [a]
collectRights = foldr (\ex l -> case ex of
  Right x -> x : l
  _       -> l) []
