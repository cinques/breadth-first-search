type Node = Int
type Edge = (Node, Node)
type Graph = ([Node], [Edge])

-- Получить все вершины графа
nodes :: Graph -> [Node]
nodes g = fst g

-- Получить все ребра графа
edges :: Graph -> [Edge]
edges  g = snd g

-- Проверить принадлежит ли вершина графу
existsNode :: Node -> Graph -> Bool
existsNode _ ([], _) = False
existsNode n g = any (\x -> x == n) (nodes g)

-- Получить список потомков вершины
adjacents :: Node -> Graph -> [Node]
adjacents n g
  | existsNode n g = adjacentNodes n (edges g)
  | otherwise = []
  where adjacentNodes n [] = []
        adjacentNodes n (x : xs)
          | fst x == n = snd x : adjacentNodes n xs
          | otherwise = adjacentNodes n xs

-- Поиск в ширину
breadthFirstSearch :: Node -> Graph -> [Node]
breadthFirstSearch n g
  | existsNode n g = if (null(adjacents n g)) then [n] else bfs [] [n] g
  | otherwise = []
    where bfs _ [] _ = []
          bfs visited (f:fs) g = f : bfs (visited ++ [f]) (inQueueNodes f (visited ++ [f]) (adjacents f g) fs) g
            where inQueueNodes f visited [] fs = fs
                  inQueueNodes f visited childs fs = fs ++ (filter (\x -> (not (any (\y->x == y || f == x) fs))) (filter (\c-> not (elem c visited)) childs))