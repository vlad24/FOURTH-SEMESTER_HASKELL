class PaperProduct a where
	getPrice :: a -> Int

data Book = Book { bookName :: String, bookAuthor :: String, bookPrice :: Int }
data Magazine = Magazine { magName :: String, magYear :: Int, magNumber :: Int, magPrice :: Int }

instance PaperProduct Book where
	getPrice b = bookPrice b
instance PaperProduct Magazine where
	getPrice b = magPrice b


sumPrice :: [PaperProduct] -> Int
sumPrice [] sum = sum
sumPrice (x:xs) sum = sumPrice xs (sum + getPrice x)

