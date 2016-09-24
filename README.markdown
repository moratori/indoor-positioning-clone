# Indoor-Positioning

屋内位置推定を使ったアプリケーションがつくりたい...!

## Installation

### 1. Common Lisp処理系 sbcl のインストール
* Windowsの人  
[このページ](http://sbcl.org/platform-table.html)からバイナリを落としてインストール  

* Macの人
```
$ brew install sbcl
```

* Linuxのひと  
ソースからビルドするかパッケージマネージャでsbclを探してインストール

### 2. quicklisp(パッケージ管理ツール)のインストール
* Windows以外の人
```
$ curl -O https://beta.quicklisp.org/quicklisp.lisp && sbcl --load 'quicklisp.lisp' --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --quit
```
を実行. その後~/quicklisp/local-projectsにgit clone

## Usage
Indoor-positioningでは主に以下の機能を提供します  

1. Wi-Fi 位置指紋の測定とその可視化  
2. 位置指紋を用いた屋内位置推定  
3. labnow (だれがどこにいるのかわかる簡単アプリケーション)  

### 0. REPLの起動

```
$ sbcl --eval '(ql:quickload :qlot)' --eval '(qlot:quickload :indoor-positioning)' --eval '(indoor-positioning:main)'
```

### 1. 位置指紋の測定とその可視化  

位置指紋の可視化を行うことができます
![位置指紋](dist.png)

### 2. 位置指紋を用いた屋内位置推定  

TBA

### 3. labnow (だれがどこにいるのかわかる簡単アプリケーション)
サーバを起動する( 予めデフォルトの位置指紋を作成しておく必要あり )
```
$ sbcl --eval '(ql:quickload :qlot)' --eval '(qlot:quickload :indoor-positioning)' --eval '(server.server.indoor-positioning:start)'
```
いろんなパッケージが降ってくると思いますがそれがとまったら,
[http://localhost:5000/](http://localhost:5000/)にアクセス  

![表](ptable.png)

サーバに誰かの位置情報が登録されていれば、こんな感じにどこにいるのかわかります  
またこの一覧はjson形式でも得ることができます.  
その場合は/list/locationにアクセスしてみてください。  
以下のような結果のJSONを得ることができます
```
[{"registered-at": <ユニバーサルタイム>,
  "devicetype": <デバイスの種別>,
  "devicename": <デバイスの名前>,
  "name": <ユーザの名前>,
  "timeout": <データベースから消えるまでの時間>,
  "placelist": [<場所候補1>,<場所候補2>,...]},...]
```

現在位置を推定するには以下の様に現在位置で測定したアクセスポイントの情報を  
/estimate/location にjsonでPOSTする必要があります。  
この時投げるjsonのフォーマットは以下のような感じです.  
```
{"devicetype": <デバイスの種別>,
 "devicename": <デバイス名>,
 "require": <位置推定の結果の上位何件を最大取得したいか>,
 "accesspoints": 
 [[<SSID>,<BSSID>,<周波数>,<チャンネル>,<RSSI>,<QUALITY>]...]}
```
結果は次のようになります  
推定結果は単に場所名とその信頼度のkey-valueです。
```
{"place-A":6.880897913923603,
 "place-B":0.2762759337785308,
 "place-C":0}
```
最後に、上で得た推定結果をもとに自分の現在位置をサーバに登録する場合です  
以下の様なjsonを/register/locationにPOSTで投げます
```
{"devicetype": <デバイスの種別>,
 "devicename": <デバイスの名前>,
 "timeout": <DBから消えるまでの時間>,
 "name": <ユーザの名前>,
 "placelist": [<場所候補1>,<場所候補2>,...]}
```


## Author

* morator (teldev@live.jp)

## Copyright

Copyright (c) 2015 morator (teldev@live.jp)
