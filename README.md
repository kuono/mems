# mems

![Haskell CI](https://github.com/kuono/mems/workflows/Haskell%20CI/badge.svg)

Tools for Rover Mini ECU development. 

# 参考情報

## 開発課題アイディア

- TonaTona ライブラリを使用し始める
-- 設定ファイルに初期値を書き込んでおく（接続に成功したポート名くらい？）
- UI として新たにHTML生成する機能を加える
- モデルに，array構造で保持するmutableな直近データを加える
- 型定義や関数の置き場所を再度整理し，モジュールの独立性を高める

- ひょっとして TestBits でうまくエラーコードを検出できていない？
- [こちら](https://minkara.carview.co.jp/userid/2834887/car/2442400/4981106/6/note.aspx#title)参照
- Status byte 1 (0x0d)
-- Bit 0 : Coolant Temp Sensor Error
-- Bit 1 : Inlet Air Temp Sensor Error
-- Bit 4 : Ambient Air Temp Sensor Error (But not installed on Mini)
-- Bit 5 : Fuel Temp Sensor Error (But not installed on Mini)
- Status byte 2 (0x0e)
-- Bit 1 : Fuel pump cirkit Error
-- Bit 5 : ECU Vaccum Sensor Error
-- Bit 7 : T-Pot cirkit Error

### わかっている問題点

- キーオンののち初回接続してしばらくしてエラーが出ると，まったくつながらない
- プロファイリングすると盛大なメモリリークあり（ギガ単位のメモリ利用）。Vtyモジュールのせい？
　※ただし少なくともCatalinaでモニタリングしている限りでは，数十Mバイト程度しかメモリは消費していない。

### 近日対応したい機能

- 既存ログのビューア機能
- ヘルプキー（ESCキー）を押した時にダイアログ表示
-- ポート値などをGUIを使って入力できるようにする
-- 各種設定値（上下限値，平均値等）を表示
- グラフ表示項目の選択ダイアログ・初期設定
- 各センサー値に異常値が出た時に，異常記録を画面・ログ双方に残す
- アイドリングセンサ値の常時表示　← 異常検知アルゴリズムをどうするか検討

### 保留とする開発予定項目

### 追加したい機能/解決したい問題点/課題

### いつ対応したかは忘れたが，解決済みのもの・無意味となったもの

-- #   ECUが停止した後QUITをするとhCloseが二回呼ばれ、例外が発生している。
-- #   キャラクタグラフのベースラインが移動する問題の原因究明。
-- #   parse関数で-- ２バイトデータを無視しているので注意
-- #   ReaderT, StateT モナド導入
