# [Cabal 3.4 Users Guide](https://cabal.readthedocs.io/en/3.4/) 仮訳

## 1. Haskell と Cabal を使い始める

### 1.1. Haskell のツール群を<ruby><rb>設置</rb><rt>インストール</rt></ruby>する
`Haskell`のツール群を<ruby><rb>設置</rb><rt>インストール</rt></ruby>するためには，Linux や Mac を使っているなら [ghcup](https://www.haskell.org/ghcup/) を使って導入すればよい。Windows であれば，[こちらの導入手引](https://hub.zhox.com/posts/introducing-haskell-dev/)を参照のこと。

### 1.2. 新しいアプリケーションを作成する
Haskell のパッケージのディレクトリ構造や外部の依存モジュールをどのように加えるかを学ぶために，まず簡単な Haskell のアプリケーションを一から作ってみよう。

#### 1.2.1. アプリケーションの初期化
プロジェクトファイルを保管する`myfirstapp`ディレクトリをつくることから始めよう。以下の方法は，Unix のシェルでも動くし，Windows の PowerShell でも動く。
```shell
$ mkdir myfirstapp
$ cd myfirstapp
```
からのディレクトリが作成できたら，パッケージを初期化しよう。
```shell
$ cabal init --cabal-version=2.4 --license=NONE -p myfirstapp
```
これにより，以下のファイルが作成される。
```shell
$ ls
CHANGELOG.md
Main.hs
myfirstapp.cabal
Setup.hs
```
`Main.hs`は，パッケージのコードを格納するファイルである。初期設定では，`cabal init` は初期設定ではパッケージ名と同じ名前，この例では`myfirstapp`を使って実行可能ファイルを作成する。`cabal init`でライブラリだけを作成するなら`--lib`を，ライブラリと実行ファイルを作成するなら`--libandexe`をつける。他の<ruby><rb>選択肢</rb><rt>オプション</rt></ruby>については`cabal init --help`を参照されたい。

`myfirstapp.cabal`は，Cabal のメタデータファイルであり，作成しようとしているパッケージや，そのパッケージを作成する際に依存しているパッケージについての説明が記述されている。作成するパッケージに新たな外部依存パッケージが必要となったら，このファイルに少し手を加えることになる。

### 1.2.2. アプリケーションを走らせる
上述のように，`cabal init`と引数なしで動作させると，端末に「Hello, Haskell!」と表示するだけの実行可能プログラムを一つ作成する。この実行可能プログラムを稼働させるには，以下のコマンドを入力する：
```shell
cabal run :myfirstapp
```
すると，端末には以下のように表示されるであろう。
```shell
$ cabal run :myfirstapp
...
Hello, Haskell!
Note
```
「`:myfirstapp`」の<ruby><rb>接頭辞</rb><rt>プリフィックス</rt></ruby>「`：`」は，`myfirstapp`のターゲットが現在のパッケージの一部であることを意味している。

`cabal run`を実行する際に，事前に`cabal build`を実行していないことに気づかれたであろうか。これは，`cabal run`の実行過程で，まず先に実行可能プログラムを走らせる前に，コードを再<ruby><rb>構築</rb><rt>ビルド</rt></ruby>する必要があるかどうかを調べるためである。単にターゲットを<ruby><rb>構築</rb><rt>ビルド</rt></ruby>したいだけなら，以下のように<ruby><rb>構築</rb><rt>ビルド</rt></ruby>だけすればよい。
```shell
cabal build :myfirstapp
```
### 1.2.3. 依存パッケージを加える
次に，作成したアプリケーションに外部依存パッケージを加えよう。[Hackage](http://hackage.haskell.org/) は，Haskell コミュニティの中でオープンソース<ruby><rb>軟件</rb><rt>ソフトウェア</rt></ruby>の中心位置にあるパッケージ<ruby><rb>庫</rb><rt>アーカイブ</rt></ruby>である。

作成しているアプリケーションでは，haskell-say というパッケージを使う。これは，端末に，ちょっとした装飾を付けて文言を表示するためのパッケージである。
In our application, we’ll use a package called haskell-say to print text to the terminal with some embellishment.

> 注記
> もし`cabal`を<ruby><rb>設置</rb><rt>インストール</rt></ruby>してから少し時間がたっていて，しかもその間使ったことがなかったら，パッケージの索引を更新しなければならないかもしれない。更新には`cabal update`を実行する必要がある。

myfirstapp.cabal には，executable myfirstapp 節に build-depends 属性を加え，そこに haskell-say を追記する。
```yaml
executable myfirstapp
    main-is: Main.hs
    build-depends:
        base >=4.11 && <4.12,
        haskell-say ^>=1.0.0.0
```
> 注記
> `^>=1.0.0.0` は，ライブラリの第 1.0.0.0 以降の，メジャーバージョンが同じでよりあたらしい，マイナー改良版を使うことを意味している。

次に，`Main.hs`を更新して，HaskellSay ライブラリを使うようにしよう。
```Haskell
module Main where

import HaskellSay (haskellSay)

main :: IO ()
main =
  haskellSay "Hello, Haskell! You're using a function from another package!"
```
`import HaskellSay (haskellSay) `は，`HaskellSay`という名前のモジュールにある`haskellSay`関数を見えるようにする。`HaskellSay`モジュールは，`haskell-say`パッケージで定義されており，このパッケージは先程，依存していると追記した。

それでは，`cabal build`を実行し，再度，コードを走らせて新しい出力がどうなるかを見てみよう。
```
$ cabal run
    ________________________________________________________
   /                                                        \
  | Hello, Haskell! You're using a function from another     |
  | package!                                                 |
   \____       _____________________________________________/
        \    /
         \  /
          \/
    _____   _____
    \    \  \    \
     \    \  \    \
      \    \  \    \
       \    \  \    \  \-----------|
        \    \  \    \  \          |
         \    \  \    \  \---------|
         /    /  /     \
        /    /  /       \  \-------|
       /    /  /    ^    \  \      |
      /    /  /    / \    \  \ ----|
     /    /  /    /   \    \
    /____/  /____/     \____\
```
### 1.3. 次は?
これで，Cabal を使って，簡単な Haskell のパッケージを作り上げる方法をご理解いただけたであろうか。引き続き，Haskell のウェブサイトにある説明ページにある使えそうな情報を見たり，導入ページの Cabal やその他のパッケージに関する説明を読んでみてはどうだろう。

---
Cabal は，[Haskell](http://www.haskell.org/) の標準パッケージシステムである。Cabal によって，Haskell のソフトウェアを設定したり，<ruby><rb>構築</rb><rt>ビルド</rt></ruby>したり，<ruby><rb>設置</rb><rt>インストール</rt></ruby>することができ，その上，他のユーザーや開発者に配布するときにも使用する。

`cabal`は，Cabal パッケージという仕組みの上で動作するコマンドラインツールである。cabal によって，すでに存在しているパッケージの<ruby><rb>設置</rb><rt>インストール</rt></ruby>ができる他，独自のパッケージの開発も行うことができる。<ruby><rb>手元</rb><rt>ローカル</rt></ruby>にあるパッケージを使うこともできるし，<ruby><rb>遠隔地</rb><rt>リモート</rt></ruby>のパッケージ<ruby><rb>庫</rb><rt>アーカイブ</rt></ruby>からもってきたパッケージを，依存するパッケージをも含めて自動的に<ruby><rb>設置</rb><rt>インストール</rt></ruby>することもできる。初期設定では，[Hackage](http://hackage.haskell.org/) という，数千のライブラリやアプリケーションを Cabal パッケージ<ruby><rb>形式</rb><rt>フォーマット</rt></ruby>で掲載している Haskell の標準パッケージ<ruby><rb>庫</rb><rt>アーカイブ</rt></ruby>を使うように設定されている。

## 2. はじめに
Cabal は，Haskell ソフトウエアのパッケージシステムである。パッケージシステムのポイントは，ソフトウェアの開発者やユーザーが簡単にソフトウェアを配布したり使用したり再利用したりすることができるようにする点である。パッケージシステムは，ユーザーの手にソフトウェアを届けるのを簡単にする。これに負けず劣らず重要なのは，ソフトウェア開発者にとっては，他の開発者が作成したソフトウェア部品を再利用できるようになることである。

パッケージシステムは，パッケージを扱う。Cabal が扱うパッケージは，**Cabal パッケージ**である。Cabal パッケージ は，配布の単位となる。すべての Cabal パッケージは，パッケージ名と<ruby><rb>版番号</rb><rt>バージョンナンバー</rt></ruby>を与えられており，パッケージの識別のもとになる（例：`filepath-1.0`)。

Cabal パッケージは，他の Cabal パッケージに依存することができる。パッケージ管理を自動で行うためのツールもある。これの意味するところは，開発者やユーザーは，パッケージををインストールする際，依存するパッケージも含めて自動的にインストールできるということである。また，多くの開発者によって書かれたコードを再利用したたくさんのパッケージを使って，きわめて独立性の高いシステムを実用的に作成することができるという意味でもある。

Cabal パッケージは，ソースコード形式が基本であり，典型的には（必須ではないが），多くのプラットフォームや Haskell の実行環境に移植可能である。
Cabal パッケージの<ruby><rb>形式</rb><rt>フォーマット</rt></ruby>は，他の形式，例えば様々なシステムのバイナリパッケージに変換できるよう設計されている。

配布の際，Cabal パッケージは標準圧縮 tarball <ruby><rb>形式</rb><rt>フォーマット</rt></ruby>を用い，拡張子は.tar.gzとなる。（例：`filepath-1.0.tar.gz`）

パッケージは，Haskell言語の一部ではなく，CabalとGHC（あるいはいくつかのHaskellの他のコンパイラ）の組み合わせで機能が実現されている。

### 2.1 パッケージを扱う道具
cabal という名前のコマンドラインツールは，ユーザーや開発者が Cabal パッケージをビルドしたり<ruby><rb>設置</rb><rt>インストール</rt></ruby>したりできるようにする。このツールは，<ruby><rb>手元</rb><rt>ローカル</rt></ruby>なパッケージにも使えるし，ネットワーク越しにリモートでアクセスするパッケージも扱うことができる。自動的に Cabal パッケージや，そのパッケージが依存している他の Cabal パッケージををインストールすることができる。

開発者は，このツールを<ruby><rb>手元</rb><rt>ローカル</rt></ruby>のディレクトリに入れるパッケージで使うことができる。例えば，

```shell
$ cd foo/
$ cabal install
```
<ruby><rb>手元</rb><rt>ローカル</rt></ruby>のディレクトリにあるパッケージを扱う際は，開発者はそれぞれ単独で<ruby><rb>構成</rb><rt>コンフィギュア</rt></ruby>や<ruby><rb>構築</rb><rt>ビルド</rt></ruby>を行うこともできるし，<ruby><rb>説明文書</rb><rt>ドキュメント</rt></ruby>を生成したり，テストを走らせたり，ベンチマークをとったりすることもできる。

一度にいくつかのパッケージを<ruby><rb>手元</rb><rt>ローカル</rt></ruby>に<ruby><rb>設置</rb><rt>インストール</rt></ruby>することもできる。例えば，

```shell
$ cabal install foo/ bar/
```
開発者やユーザは，cabal を使って，<ruby><rb>遠隔地</rb><rt>リモート</rt></ruby>にある Cabal パッケージ<ruby><rb>庫</rb><rt>アーカイブ</rt></ruby>から<ruby><rb>手元</rb><rt>ローカル</rt></ruby>にパッケージを<ruby><rb>設置</rb><rt>インストール</rt></ruby>することもできる。`cabal`は，何もしなければ [Hackage](http://hackage.haskell.org/) という名前の，Haskell パッケージ<ruby><rb>庫</rb><rt>アーカイブ</rt></ruby>を利用するように設定されているが，対応している他の<ruby><rb>庫</rb><rt>アーカイブ</rt></ruby>を利用することも可能である。

```shell
$ cabal install xmonad
```
これは，`xmonad`パッケージと，すべての依存パッケージを設置するコマンドである。

庫に格納されたパッケージに加えて，開発者はローカルまたは<ruby><rb>遠隔地</rb><rt>リモート</rt></ruby>にある tarball ファイルからもパッケージを設置することができる。例えば，

```shell
$ cabal install foo-1.0.tar.gz
$ cabal install http://example.com/foo-1.0.tar.gz
```
Cabal は，パッケージをどこにどのように<ruby><rb>設置</rb><rt>インストール</rt></ruby>するかについて多様な方法を提供する。<ruby><rb>設置</rb><rt>インストール</rt></ruby>する場所をどこにするか，使用する Haskell の<ruby><rb>実装</rb><rt>インプリメンテーション</rt></ruby>をどれにするのか，できるだけ最適化するのか，それともプロファイルを取るための情報を埋め込むのかなどを選択することができる。ユーザーは，通常，`.cabal`ファイルを編集する必要はない。

詳細については，パッケージの<ruby><rb>構築</rb><rt>ビルド</rt></ruby>および<ruby><rb>設置</rb><rt>インストール</rt></ruby>の章を参照されたい。

Cabal は，Cabalパッケージを扱うための唯一のツールというわけではない。`.cabal`ファイルは標準的な形式とライブラリを利用しているので，特定の目的のための特別ツールがいくつか存在する。

### 2.2. パッケージの中身

Cabal パッケージは以下の要素から成る：

- ライブラリや実行可能ファイル，パッケージのテスト用メタデータなどが入った，人間にも機械にも可読な形式（`.cabal`ファイル）から成る Haskell のソフトウエア
- パッケージを<ruby><rb>構築</rb><rt>ビルド</rt></ruby>するための標準インタフェース（`"Setup.hs"`ファイル)

`.cabal`ファイルは，作者が作成したパッケージに関する情報を含んでいる。典型的には，パッケージが依存している他の Cabal パッケージの一覧が含まれている。

`.cabal`ファイルや`"Setup.hs"`ファイルが保持している情報の詳細についてや，<ruby><rb>構築</rb><rt>ビルド</rt></ruby>システムが提供する他の機能については，[パッケージの開発](#developingPackages)章を参照のこと。

### 2.3. Cabal の機能

Cabal および関係ツールや関係ウェブサイトは，以下の機能を提供する：

- ソフトウェアの<ruby><rb>構築</rb><rt>ビルド</rt></ruby>システム
- ソフトウェアの<ruby><rb>構成</rb><rt>コンフィギュレーション</rt></ruby>指定
- 配布のためのパッケージ生成
- 自動パッケージ管理
  - `cabal`コマンドラインツールを直接使うことで実現
  - RPM や deb のような，ネイティブパッケージに変換することで実現
- Web および<ruby><rb>手元</rb><rt>ローカル</rt></ruby>の Cabal パッケージ<ruby><rb>庫</rb><rt>アーカイブ</rt></ruby>
  - 1,000以上の Cabal パッケージを格納した中央 Hackage サイト

このシステムの一部は，他の要素がなくとも動作する。典型的には，簡素なパッケージを<ruby><rb>構築</rb><rt>ビルド</rt></ruby>するために最初から組み込まれている<ruby><rb>構築</rb><rt>ビルド</rt></ruby>システムは<ruby><rb>選択肢</rb><rt>オプション</rt></ruby>である：カスタム<ruby><rb>構築</rb><rt>ビルド</rt></ruby>システムを使用することも可能である。Some parts of the system can be used without others. In particular the built-in build system for simple packages is optional: it is possible to use custom build systems.

### 2.4. 類似のシステム

Cabal システムは，大雑把に言えば，Python の Eggs や，Ruby の Gem，Perl ディストリビューションに該当する。それぞれのシステムは，配布可能なパッケージの概念があり，パッケージの配布過程を管理したり，<ruby><rb>設置</rb><rt>インストール</rt></ruby>を管理するためのツールを持つ。

Hackageは，Cabal パッケージのオンライン<ruby><rb>庫</rb><rt>アーカイブ</rt></ruby>である。これは，大雑把に言えば，CPANに相当すが，掲載されているパッケージ数は少ない（概ね5,000対28,000)。

Cabalは，autoconf や automake によく例えられる。最も似ているのは，実際に<ruby><rb>構成</rb><rt>コンフィギュア</rt></ruby>したり<ruby><rb>構築</rb><rt>ビルド</rt></ruby>したりする際に，あらかじめ設定しておいた同じ<ruby><rb>構成</rb><rt>コンフィギュア</rt></ruby>が使われるという点である。

```shell
$ ./configure --prefix=...
$ make
$ make install
```
対応するのは，

```shell
$ cabal configure --prefix=...
$ cabal build
$ cabal install
```
簡単なパッケージを<ruby><rb>構築</rb><rt>ビルド</rt></ruby>する際のCabal の<ruby><rb>構築</rb><rt>ビルド</rt></ruby>システムは，make や automake よりも柔軟性はないが，Haskell のコードをどのように<ruby><rb>構築</rb><rt>ビルド</rt></ruby>するかという仕組みが内部に組み込まれているので，手作業で<ruby><rb>構成</rb><rt>コンフィギュア</rt></ruby>を調整する部分はほとんどない。Cabal の簡素な<ruby><rb>構築</rb><rt>ビルド</rt></ruby>システムは可搬性が高く，Windows においても cygwin や mingwin のような，疑似 Unix 環境は不要である。

autoconf と比べ，Cabal はパッケージ<ruby><rb>構成</rb><rt>コンフィギュア</rt></ruby>について，別の方法を採用している。Cabal の手法は，自動パッケージ管理を重視する考えに基づいている。依存している要素の存否を調べたりする<ruby><rb>簡易言語</rb><rt>スクリプト</rt></ruby>を備える替わりに，Cabal パッケージは依存関係を明示している。<ruby><rb>選択可能</rb><rt>オプション</rt></ruby>，あるいは条件依存の場合はこの限りではない。パッケージ作者が依存関係を明示することにより，ツールによって，依存しているものも含めて自動的にパッケージを<ruby><rb>設置</rb><rt>インストール</rt></ruby>することが可能となっている。また，（ほぼ自動で）別のパッケージ<ruby><rb>形式</rb><rt>フォーマット</rt></ruby>，例えば RPM や deb のような依存関係を適切に扱える<ruby><rb>形式</rb><rt>フォーマット</rt></ruby>にも，変換することができる。

## 3.<ruby><rb>構成</rb><rt>コンフィギュレーション</rt></ruby>とパッケージの<ruby><rb>設置</rb><rt>インストール</rt></ruby>
### 3.1. <ruby><rb>構成</rb><rt>コンフィギュレーション</rt></ruby>
#### 3.1.1. 概観
`cabal-install` を使う上で，<ruby><rb>全処通用</rb><rt>グローバル</rt></ruby>な<ruby><rb>構成</rb><rt>コンフィギュレーション</rt></ruby>ファイルは，`~/.cabal/config`である。もし，このファイルが存在しなければ，cabal は最初に`cabal update`が使われた際に作成する。他に，以下のように明示的に cabal にこのファイルを作成するよう指示することもできる。
```shell
$ cabal user-config update
```
<ruby><rb>全処通用</rb><rt>グローバル</rt></ruby>な<ruby><rb>構成</rb><rt>コンフィギュレーション</rt></ruby>ファイルの場所を変更するには，コマンド発行の際に`--config-file=FILE `を指定するか，環境変数`CABAL_CONFIG`に指定する。

<ruby><rb>構成</rb><rt>コンフィギュレーション</rt></ruby>ファイルの<ruby><rb>選択肢</rb><rt>オプション</rt></ruby>のほとんどは，コマンド発行時の<ruby><rb>選択肢</rb><rt>オプション</rt></ruby>としても指定可能であるので，該当する（コマンド発行時の<ruby><rb>選択肢</rb><rt>オプション</rt></ruby>の）説明文を参照すれば，意味が理解できる。作成された<ruby><rb>構成</rb><rt>コンフィギュレーション</rt></ruby>ファイルは，便利な<ruby><rb>選択肢</rb><rt>オプション</rt></ruby>のみ値を設定している。多くの<ruby><rb>選択肢</rb><rt>オプション</rt></ruby>は，妥当な初期値のままになっている。例えば，

```yaml
-- executable-stripping: True
```
が意味するところは，<ruby><rb>構成</rb><rt>コンフィギュレーション</rt></ruby>ファイルは現在`executable-stripping` の選択肢を指定していないということを意味する（この行がコメント化されているので）。また，初期設定値は`True`ということである。初期設定として，実行可能な？？のストライピングを不可としたければ，この行を烏賊のように変えれば良い。
```yaml
executable-stripping: False
```
また，`cabal user-config update`コマンドを発行して，旧版の`cabal`で作成された<ruby><rb>構成</rb><rt>コンフィギュレーション</rt></ruby>ファイルを移行させることもできる。

#### 3.1.2. 環境変数
多様な環境変数が`cabal-install`で使われる。

```yaml
CABAL_CONFIG
```
<ruby><rb>全処通用</rb><rt>グローバル</rt></ruby>な<ruby><rb>構成</rb><rt>コンフィギュレーション</rt></ruby>ファイルを見つけるための変数。
```yaml
CABAL_DIR
```
`cabal-install`ファイルを収容するための初期設定ディレクトリ。初期設定値は`getAppUserDataDirectory "cabal"`であり，これの意味するところは，Unix システムでは`$HOME/.cabal`であり，Windows では`%APPDATA%\cabal`である。
> 注記
> `CABAL_DIR`は，将来`cabal-install`が`XDG Directory`機能を備えた時点で削除される予定である。
```yaml
CABAL_BUILDDIR
```
<ruby><rb>構築</rb><rt>ビルド</rt></ruby>作業を行う場所の初期設定値を上書きする。ただし，`nix-style`の<ruby><rb>構築</rb><rt>ビルド</rt></ruby>での，<ruby><rb>構築</rb><rt>ビルド</rt></ruby>ディレクトリ（`dist-newstyle`)は，この環境変数の値によらない。

#### 3.1.2.1. <ruby><rb>構成</rb><rt>コンフィギュレーション</rt></ruby>ファイルの探索
もし`$CABAL_CONFIG`が設定されているなら，それを使う。

そうでなければ，もし`$CABAL_DIR`が設定されているなら`$CABAL_DIR/config`を使う。

そうでなければ，`getAppUserDirectory "cabal"`を使う。

もし<ruby><rb>構成</rb><rt>コンフィギュレーション</rt></ruby>ファイルがそれでも見つからなければ，`cabal-install`は，初期設定値を使ったファイルを作成し，ディレクトリの起点は，もし設定されているなら`$CABAL_DIR` を，そうでなければ`getAppUserDirectory "cabal"`をプリフィックスとして使う。

#### 3.1.3.  <ruby><rb>保管庫</rb><rt>リポジトリ</rt></ruby>の詳述
<ruby><rb>構成</rb><rt>コンフィギュレーション</rt></ruby>の重要な要素の一つに，<ruby><rb>保管庫</rb><rt>リポジトリ</rt></ruby>の詳述がある。`cabal`が初期設定の<ruby><rb>構成</rb><rt>コンフィギュレーション</rt></ruby>ファイルを作成する際は，中央 [Hackage](http://hackage.haskell.org/) サーバを使用するように設定する。

```yaml
repository hackage.haskell.org
  url: http://hackage.haskell.org/
```

<ruby><rb>保管庫</rb><rt>リポジトリ</rt></ruby>の名前は，最初の行で与えられている。そして，これは何を設定しても良い。この<ruby><rb>保管庫</rb><rt>リポジトリ</rt></ruby>からダウンロードされたパッケージは，` ~/.cabal/packages/hackage.haskell.org`という場所に<ruby><rb>仮置き</rb><rt>キャッシュ</rt></ruby>される（あるいは，自分で指定したファイル名；`remoto-repo-cache`の値を変更すれば，プリフィックスを変更することもできる）。もし望むなら，複数の<ruby><rb>保管庫</rb><rt>リポジトリ</rt></ruby>を指定することも可能であり，`cabal`はこれらを組み合わせて使い，これらのいずれからもパッケージをダウンロードすることができる。

#### 3.1.3.1. 安全な<ruby><rb>保管庫</rb><rt>リポジトリ</rt></ruby>の利用
TUF セキュリティ基盤を利用可能な<ruby><rb>保管庫</rb><rt>リポジトリ</rt></ruby>([Hackage](http://hackage.haskell.org/) も含まれるを利用する場合は，以下の設定をすることで安全なアクセスが可能となる。

```yaml
repository hackage.haskell.org
  url: http://hackage.haskell.org/
  secure: True
  root-keys: <root-key-IDs>
  key-threshold: <key-threshold>
```
`<root-key-IDs>`と`<key-threshold>`の値は，<ruby><rb>初回の起動</rb><rt>ブートストラップ</rt></ruby>の際に使われる。

TUF 基盤の一部として，<ruby><rb>保管庫</rb><rt>リポジトリ</rt></ruby>は`root.json`というファイル（例えば，[`http://hackage.haskell.org/root.json`](http://hackage.haskell.org/root.json)）を保有している。これは，利用者が認証をする際に必要になるファイルである。しかしながら，`cabal`はどうやって`root.json`ファイル自体を認証するのだろうか？これは，<ruby><rb>初回の起動</rb><rt>ブートストラップ</rt></ruby>の問題である。もし，<ruby><rb>根</rb><rt>ルート</rt></ruby>となるキー ID や対応する<ruby><rb>基準線</rb><rt>スレッショルドのリスト</rt></ruby>を特定するなら，`cabal`は少なくともあなたが`<root-key-IDs>`の中で設定したキー`<key-threshold>`で署名された`root.json`をダウンロードする。

おすすめというわけではないが，この２つの欄は未記入のままにもできる。その場合，cabal は`root.json`ファイルをダウンロードし，認証無しでそれを使用する。このような<ruby><rb>起動</rb><rt>ブートストラップ</rt></ruby>手順は安全ではないが，引き続き行われるアクセスは安全である（ダウンロードされた`root.json`が改変されていない限り）。もちろん，`root-keys`や`key-threshold`を，あなたの<ruby><rb>保管庫</rb><rt>リポジトリ</rt></ruby>の記述に追加しても，問題を先送りするだけである。なぜなら，こんどはそこで受け取った key ID が正しいものであることを確かなものにしなければならないからだ。これについては，cabal  自身では解決できない。

安全基盤についての情報は，[hackageの安全について](https://github.com/haskell/hackage-security) を参照のこと.

##### 3.1.3.2. 手元にある索引なしの<ruby><rb>保管庫</rb><rt>リポジトリ</rt></ruby>

任意の場所にあるディレクトリ内の`.tar.gz`パッケージファイルを，<ruby><rb>手元にある</rb><rt>ローカル</rt></ruby><ruby><rb>保管庫</rb><rt>リポジトリ</rt></ruby>として使うこともできる。

```cabal
repository my-local-repository
  url: file+noindex:///absolute/path/to/directory
```
cabal は，自動的にディレクトリ内に`package-name-version.tar.gz`ファイルから索引を作成し，`package-name-version.cabal`ファイルを新しい版用に対応する選択肢の一つとして使用する。

例えば，以下のようなファイルが`/absolute/path/to/directory`にあるとき，
```shell
/absolute/path/to/directory/
    foo-0.1.0.0.tar.gz
    bar-0.2.0.0.tar.gz
    bar-0.2.0.0.cabal
```
cabal は，ふたつのパッケージについて索引を作成する。

情報源を使用して，または`.cabal`ファイルの中の`foo-0.1.0.0.tar.gz`を使用して，`foo-0.1.0.0`を作成

`bar-0.2.0.0.tar.gz`内の情報源を利用して，あるいは`bar-0.2.0.2.cabal`を利用して`bar-0.2.0.0`を作成

作成した索引は，与えられたディレクトリ内に<ruby><rb>一時保管</rb><rt>キャッシュ</rt></ruby>される。もしそのディレクトリが書込み可能でなければ，`#shared-cache`<ruby><rb>形式</rb><rt>フォーマット</rt></ruby>をURIに追記することもできる。その場合，<ruby><rb>一時保管</rb><rt>キャッシュ</rt></ruby>は，<ruby><rb>遠隔地の</rb><rt>リモート</rt></ruby>`remoto-repo-cache`ディレクトリに保管される。（`remoto-repo-cache`の）`path`の部分が，<ruby><rb>一時保管</rb><rt>キャッシュ</rt></ruby>保管場所の特定に使用される。

> 注記
> `cabal-install`は，`.cache`ファイルを作成し，存在するなら積極的にそこに保管された内容を活用する。それ故，もしこのディレクトリを変更したときは，<ruby><rb>一時保管</rb><rt>キャッシュ</rt></ruby>の消去をお忘れなく。

> 注記
> `URI scheme file:`は，前節で説明したように，<ruby><rb>遠隔地</rb><rt>リモート</rt></ruby>の<ruby><rb>保管庫</rb><rt>リポジトリ</rt></ruby>として翻訳される。それ故，`01-index.tar`ファイルを手作業で構築することが必要になる。

##### 3.1.3.3. <ruby><rb>旧形式</rb><rt>レガシー</rt></ruby><ruby><rb>保管庫</rb><rt>リポジトリ</rt></ruby>
現在，`cabal`は一種類の「 <ruby><rb>旧形式</rb><rt>レガシー</rt></ruby><ruby><rb>保管庫</rb><rt>リポジトリ</rt></ruby>」に対応している。これは，以下のように指定する。

```cabal
remote-repo: hackage.haskell.org:http://hackage.haskell.org/packages/archive
```
これは，以下の形式の<ruby><rb>糖衣構文</rb><rt>シンタックスシュガー</rt></ruby>である。
```
repository hackage.haskell.org
  url: http://hackage.haskell.org/packages/archive
```
ただし，特定の Hackage でのみ, [http://hackage.haskell.org/packages/archive](http://hackage.haskell.org/packages/archive) は水面下で [http://hackage.haskell.org/](http://hackage.haskell.org/) に置き換えられる。.

##### 3.1.3.4. 安全な<ruby><rb>手元</rb><rt>ローカル</rt></ruby><ruby><rb>保管庫</rb><rt>リポジトリ</rt></ruby>
もし<ruby><rb>手元</rb><rt>ローカル</rt></ruby>のファイルシステムにある<ruby><rb>保管庫</rb><rt>リポジトリ</rt></ruby>を使いたいなら，安全な<ruby><rb>手元</rb><rt>ローカル</rt></ruby><ruby><rb>保管庫</rb><rt>リポジトリ</rt></ruby>を使うことをおすすめする。
```cabal
repository my-local-repo
  url: file:/path/to/local/repo
  secure: True
  root-keys: <root-key-IDs>
  key-threshold: <key-threshold>
```
これら<ruby><rb>手元</rb><rt>ローカル</rt></ruby>な<ruby><rb>保管庫</rb><rt>リポジトリ</rt></ruby>の配置は，<ruby><rb>遠隔地</rb><rt>リモート</rt></ruby>の<ruby><rb>保管庫</rb><rt>リポジトリ</rt></ruby>の配置と厳密に一致していなければならない。`hackage-pero-tool`は，このような<ruby><rb>保管庫</rb><rt>リポジトリ</rt></ruby>の作成や管理に使われる。

### 3.2. パッケージの<ruby><rb>構築</rb><rt>ビルド</rt></ruby>や<ruby><rb>設置</rb><rt>インストール</rt></ruby>

後日追記

#### 3.2.1. Hackage からパッケージを<ruby><rb>設置</rb><rt>インストール</rt></ruby>する
`cabal`ツールは，Hackage のパッケージやその依存先をダウンロードしたり<ruby><rb>構成</rb><rt>コンフィギュア</rt></ruby>したり，<ruby><rb>構築</rb><rt>ビルド</rt></ruby>したり，<ruby><rb>設置</rb><rt>インストール</rt></ruby>することを一操作でできる。これを行うには，以下のようにする。

```shell
$ cabal install [PACKAGE...]
```
利用可能なパッケージの一覧表については，[Hackage のウェブサイト](http://hackage.haskell.org/) を参照されたい。

### 4.1. Quickstart
> 注記
> もし cabal の初心者であれば，まず「はじめに（Getting Started guide）」を読むと良い。

プロジェクトのディレクトリは作成済みで，Haskell モジュールもひとつふたつすでに利用可能であることを仮定しよう。

すべてのプロジェクトは名前が必要である。ここでは，“proglet”という名前を使おう。
```shell
$ cd proglet/
$ ls
Proglet.hs
```
（外部の依存を除き，）パッケージを構成するすべてのファイルは`common project`の<ruby><rb>根</rb><rt>ルート</rt></ruby>ディレクトリの中にあると仮定しよう。この単純な例では，すべてのプロジェクトファイルは一箇所のディレクトリ内に格納されているが，実際には多くのパッケージは，一つ以上のサブディレクトリを使っていることが多いが。

Cabal パッケージにするため，他に２つのファイルがプロジェクトの<ruby><rb>根</rb><rt>ルート</rt></ruby>ディレクトリに存在しなければならない。
```cabal
proglet.cabal: パッケージのメタデータおよび<ruby><rb>構築</rb><rt>ビルド</rt></ruby>情報を含む

Setup.hs: 通常は，標準的な数行のコードを含んでいるが，必要ならこれを改変しても良い。
```
この２つのファイルは手作業で作成しても良いし，`cabal init`で自動作成しても良い。

#### 4.1.1. “cabal init” を使う
`cabal init --interactive`コマンドは，対話的に使う。もし，"sensible defaults" に "no" と答えれば，質問がいくつもされる。
```shell
$ cabal init --interactive
Should I generate a simple project with sensible defaults? [default: y] n
What does the package build:
   1) Executable
   2) Library
   3) Library and Executable
Your choice?
...
```
重要な質問の一つに，パッケージの構成がある。ライブラリのみか，実行可能ファイルのみか，あるいはその両方か，というものである。ライブラリは，Haskell のモジュールの集まりで，他の Haskell ライブラリやプログラムから再利用される。実行可能ファイルとは，単独で実行可能なプログラムのことである。

この時点では，これらは単なる選択でしかない。もっと複雑なパッケージ（例えばライブラリと複数の実行可能プログラムと試験一式からなるようなパッケージ）については，後で，.cabal ファイルを編集することができる。

（実行可能ファイル，ライブラリ，ライブラリと実行可能ファイルの両方の）選択を行った後，cabal は，使用する cabal の<ruby><rb>版</rb><rt>バージョン</rt></ruby>はいずれかといった質問から始まり，パッケージの名前（例えば "proglet"），パッケージの<ruby><rb>版番号</rb><rt>バージョンナンバー</rt></ruby>を尋ねられたりする。

また，その他多くのパッケージのメタデータに関する質問がある。外部に公開するつもりがないなら，そうした質問の解答欄は空欄で良い。

最後に，`cabal init --interactive`は，最初の（<ruby><rb>版</rb><rt>バージョン</rt></ruby>の） proglet.cabal と Setup.hs ファイル，ライセンスの質問の回答に応じた LICENCE ファイルを作成する。

```shell
Generating LICENSE...
Generating Setup.hs...
Generating proglet.cabal...
```

.cabal ファイルを編集することもできるし，Description フィールドを追加することもできる。この段階では，proglet.cabal ファイルは完成しているわけではない。パッケージを<ruby><rb>構築</rb><rt>ビルド</rt></ruby>する前に，ファイルを編集したり，ライブラリや実行可能プログラムの<ruby><rb>構築</rb><rt>ビルド</rt></ruby>情報を追記する必要がある。

#### 4.1.2. .cabal ファイルを編集する

テキストエディタに .cabal ファイルを読み込もう。.cabal ファイルの最初にかかれているのは，パッケージのメタデータであり，その後に<ruby><rb>実行可能ファイル</rb><rt>executable</rt></ruby>及び<ruby><rb>ライブラリ</rb><rt>library</rt></ruby>に関する部分が続く。

内容が記述されていないフィールドについてはコメント化してあることに気が付かれただろうか。Cabal は，Haskell 様式の`--`を使ったコメント文法を使う（コメント化は行またぎできず，その行のみ有効である。別行にコメントを続けることは，プログラムの<ruby><rb>選択肢</rb><rt>オプション</rt></ruby>で混乱を引き起こす可能性があるので，許されていない）。

前節でライブラリパッケージの作成を選択した場合，.cabal ファイルはこのような体裁のセクションを持つ。

```yaml
library
  exposed-modules:     Proglet
  -- other-modules:
  -- build-depends:
```
選択肢として，実行可能ファイルの作成を選択した場合，以下のような部分があるであろう。
```yaml
executable proglet
  -- main-is:
  -- other-modules:
  -- build-depends:
```
ここでは<ruby><rb>構築</rb><rt>ビルド</rt></ruby>情報フィールドは一覧に出ているのは，コメント化されているも含め，よく使われているもののみある。その他に，本書で後述するような多くの記述要素がある。

<ruby><rb>構築</rb><rt>ビルド</rt></ruby>情報フィールドの多くは，ライブラリと実行可能ファイルで同じである。違いは，ライブラリには「<ruby><rb>外部に見せる</rb><rt>exposed</rt></ruby>モジュールの記述欄があり，これがライブラリのインタフェースとなるが，実行可能ファイルの場合は「Mainモジュール」を含むファイルの記述欄がある点である。

ライブラリ名は，パッケージ名と一致させなければならない。それゆえ，ライブラリ記述部にはライブラリ名を記述する部分はない。実行可能ファイルも多くの場合はパッケージ名と同一名とするが，必須ではないので，名前は明示的に与えられる。

#### 4.1.3. パッケージ中に含まれるモジュール

ライブラリについては，`cabal init`はプロジェクトディレクトリ内でファイルを探し，Haskell のモジュールであれば，これを`library:exposed-modules`フィールドに追加していく。パッケージの外部とのインタフェースに不要なモジュールの場合，これを`other-modules`フィールドに移動してよい。いずれにしても，ライブラリが必要なすべてのモジュールがいずれかに記述されている必要がある。

実行可能プログラムについては，`cabal init`はプログラムの Main モジュールがいずれかということを推測しない。`executable:main-is`フィールドに，Main モジュールとなるファイル名（`.hs`または`.lhs`拡張子の記述が必要）を記述する必要がある。実行可能プログラム中のその他のモジュールは，`other-modules`フィールドに記述する必要がある。

#### 4.1.4. 他のパッケージからインポートするモジュール

作成するライブラリや実行可能プログラムのモジュールは，おそらく標準ライブラリあるいは<ruby><rb>あらかじめかためてある</rb><rt>pre-packaged</rt></ruby>ライブラリに含まれているたくさんの外部モジュールを使用するであろう。（もちろん，こうしたライブラリは，Cabalパッケージとしてある。）

ライブラリや実行可能プログラムがインポートするモジュールは，すべて一覧に記述しなければならない。あるいは，別の記述を行うこともある：作成するパッケージが依存するすべてのパッケージを記述するという方法だ。

例えば，例としている Proglet モジュールは，`Data.Map`モジュールをインポートしている。`Data.Map`モジュールは，`containers`パッケージの中にあるので，これを記述する。
```yaml
library
  exposed-modules:     Proglet
  other-modules:
  build-depends:       containers, base == 4.*
```
加えて，ほとんどすべてのパッケージは，標準`Prelude`モジュールや`Data.List`など基本的なモジュールを外部公開している`base`ライブラリパッケージに依存しているであろう。

以下のような記述に気づかれたであろうか：`==4.*`。これは，作成しているパッケージが必要とする`base`パッケージの<ruby><rb>版番号</rb><rt>バージョンナンバー</rt></ruby>の制約を表現している。よく使われる制約の表現は以下の通り：
```yaml
pkgname >= n

pkgname ^>= n (Cabal 2.0 以降)

pkgname >= n && < m

pkgname == n.* (Cabal 1.6 以降)
```
最後の例は，短縮形で，例えば，`base == 4.*`という記述は，`base >= 4 && < 5`という表現と全く同一のことを意味する。詳細については，`build-depends`フィールドの説明を参照されたい。

また，`build-depends`（や`ghc-options`のような他のフィールド）を，`common`部にまとめて記述し，`libraries`部や`executable`部で共用することもできる。例えば，
```yaml
common shared-properties
  default-language: Haskell2010
  build-depends:
    base == 4.*
  ghc-options:
    -Wall

library
  import: shared-properties
  exposed-modules:
    Proglet
```
共用部品のインポートは，各部の冒頭でなければならないことに注意してほしい。詳細については，`Common`部の節を参照されたい。

#### 4.1.5. パッケージを<ruby><rb>構築</rb><rt>ビルド</rt></ruby>する

簡単なパッケージについては，これでおしまいである。次に，パッケージの<ruby><rb>構成</rb><rt>コンフィギュア</rt></ruby>や<ruby><rb>構築</rb><rt>ビルド</rt></ruby>を行っていこう。

```shell
$ cabal configure
$ cabal build
```
この２段階が無事終了したら，パッケージの<ruby><rb>設置</rb><rt>インストール</rt></ruby>をすることも可能である。

```shell
$ cabal install
```
ライブラリは，これらの作業をすることで，`GHCi`の中や，他のライブラリで使えるようになる。実行可能プログラムについては，プログラムが<ruby><rb>設置</rb><rt>インストール</rt></ruby>されることで，実行可能となる（ただし，まずシステムの`$PATH`を調整する必要があるかもしれない）。

#### 4.1.6. 次の段階
これまでの記述内容が理解できれば，簡単なパッケージを操作するには十分である。

引き続く数節では，さらに複雑なパッケージや，パッケージを他の人に配布する際に必要となる詳細な内容を扱う。

本章では，自分が作成したり他人が開発したパッケージの<ruby><rb>構築</rb><rt>ビルド</rt></ruby>や<ruby><rb>設置</rb><rt>インストール</rt>を扱った。

### 4.2. パッケージの概念

パッケージの書き方を学ぶ前に，Haskell の世界でのパッケージの概念や，Cabal がいかにこれを扱うかについて少し学んでいこう。

#### 4.2.1. パッケージの要点
パッケージは，コードを整理し，配布するための仕組みである。パッケージは，典型的には「大きく<ruby><rb>作譜</rb><rt>プログラミング</rt></ruby>する」ために編み出されたもので，多くの人が別々のタイミングで記述したコードの利用を重ねてより大きなシステムを<ruby><rb>構築</rb><rt>ビルド</rt></ruby>するために使われる。

コードをパッケージにまとめるにあたっては，機能と依存をもとに整理する。社会的要素も重要である：通常，パッケージは一人の作者か，相対的に小さな集団が関わっている。

パッケージはまた，配布の際にも使用される：要は，パッケージさえ作成すれば，別のコンピュータに移動でき，異なった環境でも利用できる。こうした事を正しく行うために設定すべきことは非常にたくさんあるが，正しく作られていれば，この移動の過程は簡単かつ信頼の置けるものになる。

パッケージは，二種類に分かれる：再使用可能なコードのライブラリと，完全な，実行可能プログラムである。ライブラリは，コードのインタフェース，API を提供し，プログラムはそれ自身単独で実行可能である。Haskell の世界では，ライブラリパッケージは公開インタフェースとして Haskell のモジュール集を外部に公開している。Cabal パッケージは，ライブラリ，実行可能プログラム，あるいはその両方を含めることができる。

他の<ruby><rb>作譜</rb><rt>プログラミング</rt></ruby>言語の中には，パッケージの概念を言語仕様に内包しているものがある。例えば，JAVA では，パッケージは型などの定義の有効範囲を規定する。Haskell の世界では，パッケージ自体は言語そのものではない。Haskell の<ruby><rb>算譜</rb><rt>プログラム</rt></ruby>は，モジュールおよび機能に関連して切り分けされたモジュールの集合であるパッケージからなる。それ故，Haskell におけるモジュール名の選択は，パッケージを利用する際はやはり重要である。

#### 4.2.2. パッケージ名と<ruby><rb>版番号</rb><rt>バージョンナンバー</rt></ruby>
すべてのパッケージは，"HUnit"のような，名前を持つ。パッケージ名は，他（のパッケージ）と重複してはならない。Cabal パッケージの名前は，文字，数字，ハイフンからなり，空白を含んではならず，ハイフンの後に数字のみがある部分も名前とはならない。Cabal パッケージの名前の有効範囲は全域であり，階層構造ではない。

パッケージは，"1.1"のような<ruby><rb>版番号</rb><rt>バージョンナンバー</rt></ruby>を持つ。これは，パッケージが開発される典型的なやり方に適合する。厳密に述べると，パッケージのそれぞれの<ruby><rb>版</rb><rt>バージョン</rt></ruby>は独立しているが，通常は互いに似通っている。Cabal パッケージの<ruby><rb>版</rb><rt>バージョン</rt></ruby>は，数字のみの構成，すなわち，"1.0.1"や"2.0"のように，数字の並び列で表現する。「`SemiVer`」のようなパッケージの変更に応じてつけられる<ruby><rb>版番号</rb><rt>バージョンナンバー</rt></ruby>に意味を与える"versioning"パッケージには様々な規則があるが，[Hackage](http://hackage.haskell.org/) 経由で配布されるパッケージは，<ruby><rb>版番号</rb><rt>バージョンナンバー</rt></ruby>について Haskell のパッケージ<ruby><rb>版</rb><rt>バージョン</rt></ruby>作成方針（PVP/SemVer FAQ章を参照）が適用される。

パッケージ名と<ruby><rb>版</rb><rt>バージョン</rt></ruby>の組み合わせは，パッケージ ID と呼ばれ，ハイフンで名前と<ruby><rb>版</rb><rt>バージョン</rt></ruby>をつなぐ（例：`"HUnit-1.1"`)。

Cabal パッケージについては，パッケージ名と<ruby><rb>版</rb><rt>バージョン</rt></ruby>の組み合わせはパッケージごとに一意である。あるいは，別の言い方をすれば，同じ名前と<ruby><rb>版</rb><rt>バージョン</rt></ruby>のパッケージは同一とみなされる。

厳密に言えば，パッケージ ID は，Cabal ソースパッケージを区別しているに過ぎない。同じ Cabal ソースパッケージでも，別々の方法で<ruby><rb>構成</rb><rt>コンフィギュア</rt></ruby>や<ruby><rb>構築</rb><rt>ビルド</rt></ruby>されることがある。別々に<ruby><rb>設置</rb><rt>インストール</rt></ruby>された同じパッケージの<ruby><rb>実体</rb><rt>インスタンス</rt></ruby>が，互いに異なったパッケージ ID で別のものと識別されることもある。しかし，多くの場合，ユーザーはこうした詳細を季にする必要はない。
#### 4.2.3. パッケージの種類：Cabal・GHC・システム

最初は，パッケージに様々な種類があるので少しややこしいと思えるかもしれない。幸い，実際にはそれほど複雑な話ではない。

- Cabal パッケージ
  　Cabal パッケージは，本当のソースパッケージである。つまり，Haskell (あるいは時々 C )のソースコードを含んでいる。
  　Cabal パッケージは GHC パッケージを作成するためにコンパイルされることもある。これらは，OS のパッケージにも変換可能である。
- GHC packages
  　これは，GHC から見たパッケージである。GHC は，ライブラリパッケージのみ関わり，実行可能プログラムには関与しない。ライブラリパッケージは，GHCi でや，コンパイル時に他のプログラムやパッケージから使えるようにするためには，GHC に登録しなければならない。
  　パッケージの登録には，基盤的なツール，`ghc-pkg`が使われ，現在どんなパッケージが登録されているかなどの情報を得ることができる。
  　GHC パッケージを手作業で作成する必要はない。ライブラリを含む Cabal ライブラリの<ruby><rb>構築</rb><rt>ビルド</rt></ruby>や<ruby><rb>設置</rb><rt>インストール</rt></ruby>が実行されると，これらは自動的に GHC に登録される。
  　GHC 以外の Haskell の<ruby><rb>実装</rb><rt>インプリメンテーション</rt></ruby>でも，基本的には同じようなパッケージ登録の概念がある。Cabal は，こうした違いを吸収し，仔細について気にしなくても良いようにしてくれる。

- <ruby><rb>操作系</rb><rt>OS</rt></ruby>のパッケージ
  　Linux や ~~Mac OS X~~ (macOS) のような<ruby><rb>（個々の）操作系</rb><rt>OS</rt></ruby>には，パッケージについて，固有の概念があり，パッケージを<ruby><rb>設置</rb><rt>インストール</rt></ruby>したり管理するツールを備えている。

  　Cabal パッケージ<ruby><rb>形式</rb><rt>フォーマット</rt></ruby>は，Cabal パッケージをほぼ自動的に<ruby><rb>操作系</rb><rt>OS</rt></ruby>パッケージに変換できるよう設計されている。通常は，こうした変換は1:1の関係にあり，ひとつの Cabal パッケージがひとつの <ruby><rb>操作系</rb><rt>OS</rt></ruby>パッケージに変換される。
  　また，Cabal パッケージから Windows用<ruby><rb>設置プログラム</rb><rt>インストーラ</rt></ruby>を生成することも可能である。もっとも，この場合，依存ライブラリすべてを含んだ状態で扱われ，ライブラリは別々には扱われない。

#### 4.2.4. 配布単位

Cabalパッケージは，配布単位となる。これが意味するところは，Cabal パッケージはそれぞれ独立してソースコードまたはバイナリ形式で配布することが可能ということである。もちろん，パッケージごとの依存はあるだろうが，一緒に利用できるパッケージの<ruby><rb>版</rb><rt>バージョン</rt></ruby>にはある程度の融通が効くのことが多いので，パッケージを個別に配布できるということには意味がある。


It is perhaps easiest to see what being “the unit of distribution” means by contrast to an alternative approach. Many projects are made up of several interdependent packages and during development these might all be kept under one common directory tree and be built and tested together. When it comes to distribution however, rather than distributing them all together in a single tarball, it is required that they each be distributed independently in their own tarballs.

Cabal’s approach is to say that if you can specify a dependency on a package then that package should be able to be distributed independently. Or to put it the other way round, if you want to distribute it as a single unit, then it should be a single package.

#### 4.2.5. Explicit dependencies and automatic package management
Cabal takes the approach that all packages dependencies are specified explicitly and specified in a declarative way. The point is to enable automatic package management. This means tools like cabal can resolve dependencies and install a package plus all of its dependencies automatically. Alternatively, it is possible to mechanically (or mostly mechanically) translate Cabal packages into system packages and let the system package manager install dependencies automatically.

It is important to track dependencies accurately so that packages can reliably be moved from one system to another system and still be able to build it there. Cabal is therefore relatively strict about specifying dependencies. For example Cabal’s default build system will not even let code build if it tries to import a module from a package that isn’t listed in the .cabal file, even if that package is actually installed. This helps to ensure that there are no “untracked dependencies” that could cause the code to fail to build on some other system.

The explicit dependency approach is in contrast to the traditional “./configure” approach where instead of specifying dependencies declaratively, the ./configure script checks if the dependencies are present on the system. Some manual work is required to transform a ./configure based package into a Linux distribution package (or similar). This conversion work is usually done by people other than the package author(s). The practical effect of this is that only the most popular packages will benefit from automatic package management. Instead, Cabal forces the original author to specify the dependencies but the advantage is that every package can benefit from automatic package management.

The “./configure” approach tends to encourage packages that adapt themselves to the environment in which they are built, for example by disabling optional features so that they can continue to work when a particular dependency is not available. This approach makes sense in a world where installing additional dependencies is a tiresome manual process and so minimising dependencies is important. The automatic package management view is that packages should just declare what they need and the package manager will take responsibility for ensuring that all the dependencies are installed.

Sometimes of course optional features and optional dependencies do make sense. Cabal packages can have optional features and varying dependencies. These conditional dependencies are still specified in a declarative way however and remain compatible with automatic package management. The need to remain compatible with automatic package management means that Cabal’s conditional dependencies system is a bit less flexible than with the “./configure” approach.

> Note
> GNU autoconf places restrictions on paths, including the path that the user builds a package from. Package authors using build-type: configure should be aware of these restrictions; because users may be unexpectedly constrained and face mysterious errors, it is recommended that build-type: configure is only used where strictly necessary.

#### 4.2.6. Portability
One of the purposes of Cabal is to make it easier to build packages on different platforms (operating systems and CPU architectures), with different compiler versions and indeed even with different Haskell implementations. (Yes, there are Haskell implementations other than GHC!)

Cabal provides abstractions of features present in different Haskell implementations and wherever possible it is best to take advantage of these to increase portability. Where necessary however it is possible to use specific features of specific implementations.

For example a package author can list in the package’s .cabal what language extensions the code uses. This allows Cabal to figure out if the language extension is supported by the Haskell implementation that the user picks. Additionally, certain language extensions such as Template Haskell require special handling from the build system and by listing the extension it provides the build system with enough information to do the right thing.

Another similar example is linking with foreign libraries. Rather than specifying GHC flags directly, the package author can list the libraries that are needed and the build system will take care of using the right flags for the compiler. Additionally this makes it easier for tools to discover what system C libraries a package needs, which is useful for tracking dependencies on system libraries (e.g. when translating into Linux distribution packages).

In fact both of these examples fall into the category of explicitly specifying dependencies. Not all dependencies are other Cabal packages. Foreign libraries are clearly another kind of dependency. It’s also possible to think of language extensions as dependencies: the package depends on a Haskell implementation that supports all those extensions.

Where compiler-specific options are needed however, there is an “escape hatch” available. The developer can specify implementation-specific options and more generally there is a configuration mechanism to customise many aspects of how a package is built depending on the Haskell implementation, the operating system, computer architecture and user-specified configuration flags.

## 5. Nix-style Local Builds
Nix-style local builds are a new build system implementation inspired by Nix. The Nix-style local build system is commonly called “v2-build” for short after the cabal v2-* family of commands that control it. However, those names are only temporary now that Nix-style local builds have become the default. For those who do not wish to use the new functionality, the classic project style will not be removed immediately, but these legacy commands will require the usage of the v1- prefix as of Cabal 3.0 and will be removed in a future release. For a future-proof way to use these commands in a script or tutorial that anticipates the possibility of another UI paradigm being devised in the future, there are also v2- prefixed versions that will reference the same functionality until such a point as it is completely removed from Cabal.

Nix-style local builds combine the best of non-sandboxed and sandboxed Cabal:

Like sandboxed Cabal previously, we build sets of independent local packages deterministically and independent of any global state. v2-build will never tell you that it can’t build your package because it would result in a “dangerous reinstall.” Given a particular state of the Hackage index, your build is completely reproducible. For example, you no longer need to compile packages with profiling ahead of time; just request profiling and v2-build will rebuild all its dependencies with profiling automatically.

Like non-sandboxed Cabal today, builds of external packages are cached in ~/.cabal/store, so that a package can be built once, and then reused anywhere else it is also used. No need to continually rebuild dependencies whenever you make a new sandbox: dependencies which can be shared, are shared.

Nix-style local builds were first released as beta in cabal-install 1.24. They currently work with all versions of GHC supported by that release: GHC 7.0 and later.

Some features described in this manual are not implemented. If you need them, please give us a shout and we’ll prioritize accordingly.

5.1. Quickstart
5.1.1. Developing multiple packages
5.2. Cookbook
5.2.1. How can I profile my library/application?
5.3. How it works
5.3.1. Local versus external packages
5.3.2. Where are my build products?
5.3.3. Caching

### 5.1. Quickstart
Suppose that you are in a directory containing a single Cabal package which you wish to build (if you haven’t set up a package yet check out developing packages for instructions). You can configure and build it using Nix-style local builds with this command (configuring is not necessary):

```shell
$ cabal v2-build
```
To open a GHCi shell with this package, use this command:

```shell
$ cabal v2-repl
```
To run an executable defined in this package, use this command:
```shell
$ cabal v2-run <executable name> [executable args]
```
#### 5.1.1. Developing multiple packages
Many Cabal projects involve multiple packages which need to be built together. To build multiple Cabal packages, you need to first create a cabal.project file which declares where all the local package directories live. For example, in the Cabal repository, there is a root directory with a folder per package, e.g., the folders Cabal and cabal-install. The cabal.project file specifies each folder as part of the project:
```yaml
packages: Cabal/
          cabal-install/
```
The expectation is that a cabal.project is checked into your source control, to be used by all developers of a project. If you need to make local changes, they can be placed in cabal.project.local (which should not be checked in.)

Then, to build every component of every package, from the top-level directory, run the command: (using cabal-install-2.0 or greater.)

```shell
$ cabal v2-build all
```
To build a specific package, you can either run v2-build from the directory of the package in question:

```shell
$ cd cabal-install
$ cabal v2-build
```
or you can pass the name of the package as an argument to cabal v2-build (this works in any subdirectory of the project):

```shell
$ cabal v2-build cabal-install
```
You can also specify a specific component of the package to build. For example, to build a test suite named package-tests, use the command:

```shell
$ cabal v2-build package-tests
```
Targets can be qualified with package names. So to request package-tests from the Cabal package, use Cabal:package-tests.

Unlike sandboxes, there is no need to setup a sandbox or add-source projects; just check in cabal.project to your repository and v2-build will just work.

### 5.2. Cookbook
#### 5.2.1. How can I profile my library/application?
Create or edit your cabal.project.local, adding the following line:

```yaml
profiling: True
```
Now, cabal v2-build will automatically build all libraries and executables with profiling. You can fine-tune the profiling settings for each package using profiling-detail:

```yaml
package p
    profiling-detail: toplevel-functions
```
Alternately, you can call cabal v2-build --enable-profiling to temporarily build with profiling.

### 5.3. How it works
#### 5.3.1. Local versus external packages
One of the primary innovations of Nix-style local builds is the distinction between local packages, which users edit and recompile and must be built per-project, versus external packages, which can be cached across projects. To be more precise:

A local package is one that is listed explicitly in the packages, optional-packages or extra-packages field of a project. Usually, these refer to packages whose source code lives directly in a folder in your project. But you can list an arbitrary Hackage package in packages to force it to be treated as local.

Local packages, as well as the external packages (below) which depend on them, are built inplace, meaning that they are always built specifically for the project and are not installed globally. Inplace packages are not cached and not given unique hashes, which makes them suitable for packages which you want to edit and recompile.

An external package is any package which is not listed in the packages field. The source code for external packages is usually retrieved from Hackage.

When an external package does not depend on an inplace package, it can be built and installed to a global store, which can be shared across projects. These build products are identified by a hash based on all of the inputs which influence the compilation of a package (flags, dependency selection, etc.). Just as in Nix, these hashes uniquely identify the result of a build; if we compute this identifier and we find that we already have this ID built, we can just use the already built version.

The global package store is ~/.cabal/store (configurable via global store-dir option); if you need to clear your store for whatever reason (e.g., to reclaim disk space or because the global store is corrupted), deleting this directory is safe (v2-build will just rebuild everything it needs on its next invocation).

This split motivates some of the UI choices for Nix-style local build commands. For example, flags passed to cabal v2-build are only applied to local packages, so that adding a flag to cabal v2-build doesn’t necessitate a rebuild of every transitive dependency in the global package store.

In cabal-install 2.0 and above, Nix-style local builds also take advantage of a new Cabal library feature, per-component builds, where each component of a package is configured and built separately. This can massively speed up rebuilds of packages with lots of components (e.g., a package that defines multiple executables), as only one executable needs to be rebuilt. Packages that use Custom setup scripts are not currently built on a per-component basis.

#### 5.3.2. Where are my build products?
A major deficiency in the current implementation of v2-build is that there is no programmatic way to access the location of build products. The location of the build products is intended to be an internal implementation detail of v2-build, but we also understand that many unimplemented features can only be reasonably worked around by accessing build products directly.

The location where build products can be found varies depending on the version of cabal-install:

In cabal-install-1.24, the dist directory for a package p-0.1 is stored in dist-newstyle/build/p-0.1. For example, if you built an executable or test suite named pexe, it would be located at dist-newstyle/build/p-0.1/build/pexe/pexe.

In cabal-install-2.0, the dist directory for a package p-0.1 defining a library built with GHC 8.0.1 on 64-bit Linux is dist-newstyle/build/x86_64-linux/ghc-8.0.1/p-0.1. When per-component builds are enabled (any non-Custom package), a subcomponent like an executable or test suite named pexe will be stored at dist-newstyle/build/x86_64-linux/ghc-8.0.1/p-0.1/c/pexe; thus, the full path of the executable is dist-newstyle/build/x86_64-linux/ghc-8.0.1/p-0.1/c/pexe/build/pexe/pexe (you can see why we want this to be an implementation detail!)

In cabal-install-2.2 and above, the /c/ part of the above path is replaced with one of /l/, /x/, /f/, /t/, or /b/, depending on the type of component (sublibrary, executable, foreign library, test suite, or benchmark respectively). So the full path to an executable named pexe compiled with GHC 8.0.1 on a 64-bit Linux is now dist-newstyle/build/x86_64-linux/ghc-8.0.1/p-0.1/x/pexe/build/pexe/pexe; for a benchmark named pbench it now is dist-newstyle/build/x86_64-linux/ghc-8.0.1/p-0.1/b/pbench/build/pbench/pbench;

The paths are a bit longer in 2.0 and above but the benefit is that you can transparently have multiple builds with different versions of GHC. We plan to add the ability to create aliases for certain build configurations, and more convenient paths to access particularly useful build products like executables.

#### 5.3.3. Caching
Nix-style local builds sport a robust caching system which helps to reduce the time it takes to execute a rebuild cycle. While the details of how cabal-install does caching are an implementation detail and may change in the future, knowing what gets cached is helpful for understanding the performance characteristics of invocations to v2-build. The cached intermediate results are stored in dist-newstyle/cache; this folder can be safely deleted to clear the cache.

The following intermediate results are cached in the following files in this folder (the most important two are first):

solver-plan (binary)
The result of calling the dependency solver, assuming that the Hackage index, local cabal.project file, and local cabal files are unmodified. (Notably, we do NOT have to dependency solve again if new build products are stored in the global store; the invocation of the dependency solver is independent of what is already available in the store.)

source-hashes (binary)
The hashes of all local source files. When all local source files of a local package are unchanged, cabal v2-build will skip invoking setup build entirely (saving us from a possibly expensive call to ghc --make). The full list of source files participating in compilation is determined using cabal sdist --list-only. Thus if you do not list all your source files in a Cabal file, Cabal may fail to recompile when you edit them.

config (same format as cabal.project)
The full project configuration, merged from cabal.project (and friends) as well as the command line arguments.

compiler (binary)
The configuration of the compiler being used to build the project.

improved-plan (binary)
Like solver-plan, but with all non-inplace packages improved into pre-existing copies from the store.

plan.json (JSON)
A JSON serialization of the computed install plan intended for integrating cabal with external tooling. The cabal-plan package provides a library for parsing plan.json files into a Haskell data structure as well as an example tool showing possible applications.

Todo

Document JSON schema (including version history of schema)

Note that every package also has a local cache managed by the Cabal build system, e.g., in $distdir/cache.

## 6. cabal-install Commands
We now give an in-depth description of all the commands, describing the arguments and flags they accept.

### 6.1. cabal v2-configure
cabal v2-configure takes a set of arguments and writes a cabal.project.local file based on the flags passed to this command. cabal v2-configure FLAGS; cabal v2-build is roughly equivalent to cabal v2-build FLAGS, except that with v2-configure the flags are persisted to all subsequent calls to v2-build.

cabal v2-configure is intended to be a convenient way to write out a cabal.project.local for simple configurations; e.g., cabal v2-configure -w ghc-7.8 would ensure that all subsequent builds with cabal v2-build are performed with the compiler ghc-7.8. For more complex configuration, we recommend writing the cabal.project.local file directly (or placing it in cabal.project!)

cabal v2-configure inherits options from Cabal. semantics:

Any flag accepted by ./Setup configure.

Any flag accepted by cabal configure beyond ./Setup configure, namely --cabal-lib-version, --constraint, --preference and --solver.

Any flag accepted by cabal install beyond ./Setup configure.

Any flag accepted by ./Setup haddock.

The options of all of these flags apply only to local packages in a project; this behavior is different than that of cabal install, which applies flags to every package that would be built. The motivation for this is to avoid an innocuous addition to the flags of a package resulting in a rebuild of every package in the store (which might need to happen if a flag actually applied to every transitive dependency). To apply options to an external package, use a package stanza in a cabal.project file.

### 6.2. cabal v2-update
cabal v2-update updates the state of the package index. If the project contains multiple remote package repositories it will update the index of all of them (e.g. when using overlays).

Some examples:
```shell
$ cabal v2-update                  # update all remote repos
$ cabal v2-update head.hackage     # update only head.hackage
```
### 6.3. cabal v2-build
cabal v2-build takes a set of targets and builds them. It automatically handles building and installing any dependencies of these targets.

A target can take any of the following forms:

A package target: package, which specifies that all enabled components of a package to be built. By default, test suites and benchmarks are not enabled, unless they are explicitly requested (e.g., via --enable-tests.)

A component target: [package:][ctype:]component, which specifies a specific component (e.g., a library, executable, test suite or benchmark) to be built.

All packages: all, which specifies all packages within the project.

Components of a particular type: package:ctypes, all:ctypes: which specifies all components of the given type. Where valid ctypes are:

libs, libraries,

flibs, foreign-libraries,

exes, executables,

tests,

benches, benchmarks.

A module target: [package:][ctype:]module, which specifies that the component of which the given module is a part of will be built.

A filepath target: [package:][ctype:]filepath, which specifies that the component of which the given filepath is a part of will be built.

In component targets, package: and ctype: (valid component types are lib, flib, exe, test and bench) can be used to disambiguate when multiple packages define the same component, or the same component name is used in a package (e.g., a package foo defines both an executable and library named foo). We always prefer interpreting a target as a package name rather than as a component name.

Some example targets:

$ cabal v2-build lib:foo-pkg       # build the library named foo-pkg
$ cabal v2-build foo-pkg:foo-tests # build foo-tests in foo-pkg
$ cabal v2-build src/Lib.s         # build the library component to
                                   # which "src/Lib.hs" belongs
$ cabal v2-build app/Main.hs       # build the executable component of
                                   # "app/Main.hs"
$ cabal v2-build Lib               # build the library component to
                                   # which the module "Lib" belongs
Beyond a list of targets, cabal v2-build accepts all the flags that cabal v2-configure takes. Most of these flags are only taken into consideration when building local packages; however, some flags may cause extra store packages to be built (for example, --enable-profiling will automatically make sure profiling libraries for all transitive dependencies are built and installed.)

In addition cabal v2-build accepts these flags:

--only-configure: When given we will forego performing a full build and abort after running the configure phase of each target package.

### 6.4. cabal v2-repl
cabal v2-repl TARGET loads all of the modules of the target into GHCi as interpreted bytecode. In addition to cabal v2-build’s flags, it takes an additional --repl-options flag.

To avoid ghci specific flags from triggering unneeded global rebuilds these flags are now stripped from the internal configuration. As a result --ghc-options will no longer (reliably) work to pass flags to ghci (or other repls). Instead, you should use the new --repl-options flag to specify these options to the invoked repl. (This flag also works on cabal repl and Setup repl on sufficiently new versions of Cabal.)

Currently, it is not supported to pass multiple targets to v2-repl (v2-repl will just successively open a separate GHCi session for each target.)

It also provides a way to experiment with libraries without needing to download them manually or to install them globally.

This command opens a REPL with the current default target loaded, and a version of the vector package matching that specification exposed.

$ cabal v2-repl --build-depends "vector >= 0.12 && < 0.13"
Both of these commands do the same thing as the above, but only exposes base, vector, and the vector package’s transitive dependencies even if the user is in a project context.
```shell
$ cabal v2-repl --ignore-project --build-depends "vector >= 0.12 && < 0.13"
$ cabal v2-repl --project='' --build-depends "vector >= 0.12 && < 0.13"
```
This command would add vector, but not (for example) primitive, because it only includes the packages specified on the command line (and base, which cannot be excluded for technical reasons).
```shell
$ cabal v2-repl --build-depends vector --no-transitive-deps
```
### 6.5. cabal v2-run
cabal v2-run [TARGET [ARGS]] runs the executable specified by the target, which can be a component, a package or can be left blank, as long as it can uniquely identify an executable within the project. Tests and benchmarks are also treated as executables.

See the v2-build section for the target syntax.

Except in the case of the empty target, the strings after it will be passed to the executable as arguments.

If one of the arguments starts with - it will be interpreted as a cabal flag, so if you need to pass flags to the executable you have to separate them with --.

```shell
$ cabal v2-run target -- -a -bcd --argument
```
v2-run also supports running script files that use a certain format. With a script that looks like:
```Haskell
#!/usr/bin/env cabal
{- cabal:
build-depends: base ^>= 4.11
            , shelly ^>= 1.8.1
-}
main :: IO ()
main = do
    ...
```
It can either be executed like any other script, using cabal as an interpreter, or through this command:

```shell
$ cabal v2-run script.hs
$ cabal v2-run script.hs -- --arg1 # args are passed like this
```
### 6.6. cabal v2-freeze
cabal v2-freeze writes out a freeze file which records all of the versions and flags that are picked by the solver under the current index and flags. Default name of this file is cabal.project.freeze but in combination with a --project-file=my.project flag (see project-file) the name will be my.project.freeze. A freeze file has the same syntax as cabal.project and looks something like this:
```yaml
constraints: HTTP ==4000.3.3,
             HTTP +warp-tests -warn-as-error -network23 +network-uri -mtl1 -conduit10,
             QuickCheck ==2.9.1,
             QuickCheck +templatehaskell,
             -- etc...
```
For end-user executables, it is recommended that you distribute the cabal.project.freeze file in your source repository so that all users see a consistent set of dependencies. For libraries, this is not recommended: users often need to build against different versions of libraries than what you developed against.

6.7. cabal v2-bench
cabal v2-bench [TARGETS] [OPTIONS] runs the specified benchmarks (all the benchmarks in the current package by default), first ensuring they are up to date.

### 6.8. cabal v2-test
cabal v2-test [TARGETS] [OPTIONS] runs the specified test suites (all the test suites in the current package by default), first ensuring they are up to date.

### 6.9. cabal v2-haddock
cabal v2-haddock [FLAGS] [TARGET] builds Haddock documentation for the specified packages within the project.

If a target is not a library haddock-benchmarks, haddock-executables, haddock-internal, haddock-tests will be implied as necessary.

### 6.10. cabal v2-exec
cabal v2-exec [FLAGS] [--] COMMAND [--] [ARGS] runs the specified command using the project’s environment. That is, passing the right flags to compiler invocations and bringing the project’s executables into scope.

### 6.11. cabal v2-install
cabal v2-install [FLAGS] PACKAGES builds the specified packages and symlinks/copies their executables in installdir (usually ~/.cabal/bin).

For example this command will build the latest cabal-install and symlink its cabal executable:

$ cabal v2-install cabal-install
In addition, it’s possible to use cabal v2-install to install components of a local project. For example, with an up-to-date Git clone of the Cabal repository, this command will build cabal-install HEAD and symlink the cabal executable:

```shell
$ cabal v2-install exe:cabal
```
Where symlinking is not possible (eg. on some Windows versions) the copy method is used by default. You can specify the install method by using --install-method flag:

```shell
$ cabal v2-install exe:cabal --install-method=copy --installdir=$HOME/bin
```
Note that copied executables are not self-contained, since they might use data-files from the store.

It is also possible to “install” libraries using the --lib flag. For example, this command will build the latest Cabal library and install it:

```shell
$ cabal v2-install --lib Cabal
```
This works by managing GHC environments. By default, it is writing to the global environment in `~/.ghc/
$ARCH-$OS-$GHCVER/environments/default`. `v2-install` provides the `--package-env` flag to control which of these environments is modified.

This command will modify the environment file in the current directory:
```shell
$ cabal v2-install --lib Cabal --package-env .
```
This command will modify the environment file in the ~/foo directory:

```shell
$ cabal v2-install --lib Cabal --package-env foo/
```
Do note that the results of the previous two commands will be overwritten by the use of other v2-style commands, so it is not recommended to use them inside a project directory.

This command will modify the environment in the local.env file in the current directory:

```shell
$ cabal v2-install --lib Cabal --package-env local.env
```
This command will modify the myenv named global environment:

```shell
$ cabal v2-install --lib Cabal --package-env myenv
```
If you wish to create a named environment file in the current directory where the name does not contain an extension, you must reference it as `./myenv`.

You can learn more about how to use these environments in this section of the GHC manual.

### 6.12. cabal v2-clean
cabal v2-clean [FLAGS] cleans up the temporary files and build artifacts stored in the dist-newstyle folder.

By default, it removes the entire folder, but it can also spare the configuration and caches if the --save-config option is given, in which case it only removes the build artefacts (.hi, .o along with any other temporary files generated by the compiler, along with the build output).

### 6.13. cabal v2-sdist
cabal v2-sdist [FLAGS] [TARGETS] takes the crucial files needed to build TARGETS and puts them into an archive format ready for upload to Hackage. These archives are stable and two archives of the same format built from the same source will hash to the same value.

cabal v2-sdist takes the following flags:

-l, --list-only: Rather than creating an archive, lists files that would be included. Output is to stdout by default. The file paths are relative to the project’s root directory.

-o, --output-directory: Sets the output dir, if a non-default one is desired. The default is dist-newstyle/sdist/. --output-directory - will send output to stdout unless multiple archives are being created.

--null-sep: Only used with --list-only. Separates filenames with a NUL byte instead of newlines.

v2-sdist is inherently incompatible with sdist hooks (which were removed in Cabal-3.0), not due to implementation but due to fundamental core invariants (same source code should result in the same tarball, byte for byte) that must be satisfied for it to function correctly in the larger v2-build ecosystem. autogen-modules is able to replace uses of the hooks to add generated modules, along with the custom publishing of Haddock documentation to Hackage.

> Warning
> Packages that use Backpack will stop working if uploaded to Hackage, due to issue #6005. While this is happening, we recommend not uploading these packages to Hackage (and instead referencing the package directly as a source-repository-package).