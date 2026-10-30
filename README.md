# hs-bundler-naive

素朴な実装によるHaskellコード結合器

**素朴な**、というのは用する以下のような方針のことです。

- ソースコードを構文解析**しない**
- ソースコードは特定の書き方していることを仮定**する**
- ユースケースは狭く限定**する**

エクスポート元のモジュールの配置に関して、以下を前提としとます。

- ビルドツール stack を使うものとし
    - package.yaml に以下の指定がある
      ```
      library:
        source-dirs: src
      ```

## 概要

特定のモジュールをエクスポート元とし、そこからインポートされているエンティティのソースをインポート側のモジュールで展開します。


`Sample.Bonsai`モジュール（ファイルとしては`` `pwd`/src/Sample/Bonsai.hs `` ）をエクスポート元とし、``` `pwd`/app/samplecase/Main.hs ```にバンドルするものとすると、以下のように使います。

```console
hs-bundle Sample.Bonsai app/samplecase/Main.hs
```

バンドル済のコードは標準出力にでますので、必要に応じてリダイレクトしてください。

## エクスポート元のモジュール（例：`Sample.Bonsai`）の書式

エンティティの宣言直前にブロックコメント書式で以下の2つを書きます。

- エンティティの宣言に関する情報
    - **必ず一行**で、その行の先頭部は、`{- DECL `で、末尾は` -}`であること
    - エンティティ名、内部依存エンティティリスト、外部依存モジュールリスト
        - 内部依存エンティティリストの要素は**クォーテーションマークで囲んではいけない**
        - 外部依存モジュールリスト（暗黙でインポートされる`"Prelude"`は含めない）の要素は**必ずダブルクォートで囲む**
- エンティティの概要説明
    - 先頭部が`{- | `である行ではじまり、`-}`だけを含む行で終了していること

以下は説明のためにモジュールファイルの一部を抜粋したものです。

```haskell
{- DECL MultiSet [] ["Data.Map qualified as M"] -}
{- | MultiSet
-}
type MultiSet a = M.Map a Int

{- DECL msEmpty [MultiSet] ["Data.Map qualified as M"] -}
{- | empty MultiSet
>>> msEmpty :: MultiSet Int
fromList []
-}
msEmpty :: MultiSet a
msEmpty = M.empty

{- DECL msSize [MultiSet] ["Data.Map qualified as M"] -}
{- | number of kinds of elements in MultiSet
-}
msSize :: Ord a => MultiSet a -> Int
msSize = M.size

{- DECL msInsert [MultiSet] ["Data.Map qualified as M"] -}
{- | insert a element into MultiSet
>>> msInsert (1 :: Int) msEmpty == msSingleton (1 :: Int)
True
>>> msInsert (1 :: Int) (msSingleton 0)
fromList [(0,1),(1,1)]
>>> msInsert (0 :: Int) (msSingleton 0)
fromList [(0,2)]
-}
msInsert :: Ord a => a -> MultiSet a -> MultiSet a
msInsert x = M.insertWith (+) x 1

{- DECL msFromList [MultiSet, msEmpty, msInsert] ["Data.List","Data.Map qualified as M"] -}
{- | converts from List
>>> msFromList [3,1,4,1,5,9,2,6,5 :: Int]
fromList [(1,2),(2,1),(3,1),(4,1),(5,2),(6,1),(9,1)]
-}
msFromList :: Ord a => [a] -> MultiSet a
msFromList = foldl' (flip msInsert) msEmpty

{- DECL yesno [] ["Data.Bool","Data.String"] -}
{- | displays judge with "Yes" or "No"
>>> yesno (() /= ()) :: String
"No"
-}
yesno :: IsString a
      => Bool -> a
yesno = bool "No" "Yes"
```

### バンドル対象ファイル（例：`app/samplecase/Main.hs`）の書式

`msFromList`、`msSize`、`yesno` を使いたいなら、以下のインポート宣言を含める。

```haskell
import Sample.Bonsai (msFromList, msSize, yesno)
```

この例では、このインポート宣言の行を`hs-bundle`が

```haskell
import Data.Bool
import Data.List
import Data.Map qualified as M
import Data.String
```

に書き換え、このファイルの末尾に以下を追加する（または、`{- Bonsai code -}`だけを含む行を以下に書き換える）。

```haskell
type MultiSet a = M.Map a Int

msEmpty :: MultiSet a
msEmpty = M.empty

msFromList :: Ord a => [a] -> MultiSet a
msFromList = foldl' (flip msInsert) msEmpty

msInsert :: Ord a => a -> MultiSet a -> MultiSet a
msInsert x = M.insertWith (+) x 1

msSize :: Ord a => MultiSet a -> Int
msSize = M.size

yesno :: IsString a
      => Bool -> a
yesno = bool "No" "Yes"

```
