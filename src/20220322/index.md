---
marp: true
theme: gaia
paginate: true
---

# iOSのディープリンク

ウェブサイトからモバイルアプリを起動したり、モバイルアプリから別のモバイルアプリに遷移したりするためには、ディープリンクと呼ばれるアプリ内の特定のコンテンツに直接遷移可能なリンクを使用する。

---

## ディープリンクの種類

-   Custom URL Scheme (iOS, Android)
-   Chrome Intents (Android)
-   Universal Links (iOS 9以上)
-   Android App Links (Android 6以上)
-   Facebook App Links (Facebook App)

---

## [Custom URL Scheme](https://developer.apple.com/documentation/xcode/defining-a-custom-url-scheme-for-your-app)

アプリが独自に宣言したURL Schemeでアプリに遷移できる。

```uri
app:host/path?key=value
```

---

## [Chrome Intents](https://developer.chrome.com/docs/multidevice/android/intents/)

直接Custom URL Schemeを起動せずにChromeを経由することでアプリがインストールされていない場合はPlay Storeに遷移できる。

```uri
intent:
  HOST/URI-path // Optional host
  #Intent;
    package=\[string\];
    action=\[string\];
    category=\[string\];
    component=\[string\];
    scheme=\[string\];
  end;
```

---

## [Universal Links](https://developer.apple.com/ios/universal-links/)

アプリの初回起動時に事前に設定されたドメイン上に置かれたファイルがダウンロードされ、ファイルに記載のあるパスであればリンクをタップ時にアプリに遷移できる。

-   [Android App Links](https://developer.android.com/studio/write/app-link-indexing)も同様の機能
-   [Facebook App Links](https://developers.facebook.com/products/app-links)はFacebookアプリ内で使用する同等の機能

---

## Universal Linksの特徴1

サーバー上にファイルを置く必要がある。

-   初回起動時、更新時とその後数日間に1回程度の頻度で再取得される
-   ドメインをアプリの情報（Associated Domains）に登録しておく

---

## Universal Linksで使用するファイル例

```url
https://www.app.test/apple-app-site-association
```

```json
{
  "applinks": {
    "App": [],
    "details": {
      { "appID": "{TEAM_ID}.{BUNDLE_IDENTIFIER1}", "paths": [ "*" ] },
      { "appID": "{TEAM_ID}.{BUNDLE_IDENTIFIER2}", "paths": [ "/shops/", "NOT /users/" ] }
    }
  }
}
```

---

## Universal Linksの特徴2

-   指定したURLをユーザーが開いたときにアプリを起動する
    -   <https://www.app.test/shop/1>
-   アプリがインストールされていない場合はブラウザで開く
-   同一ドメインではリンクできない
    -   x <https://www.app.test/> -> <https://www.app.test/shop/1>
    -   o <https://www.app.test/> -> <https://shop.app.test/1>

---

## Custom URL Schemeの例

-   `line://nv/chat`
    -   LINEのトーク画面
-   `slack://user?team={TEAM_ID}&id={USER_ID}`
    -   SlackのDM画面
-   `twitter://user?screen_name=[Screen name]`
    -   Twitterのユーザー画面

---

## Custom URL Schemeでの起動

`UIApplicationDelegate`を実装したメインクラスのメソッドが呼び出される。

```swift
func application(_ app: UIApplication, open url: URL, options: [UIApplication.OpenURLOptionsKey: Any] = [:]) -> Bool {
    DDLogInfo("open: \(url)")
}
```

---

## Custom URL Schemeのメリット

-   iOS, Android両方に対応していて仕様が共通
-   サーバーサイドの実装が不要で簡単
-   連携が疎結合になるため問題の切り分けはしやすい

---

## Custom URL Schemeのデメリット1

遷移先のアプリがインストールされていないとエラーになる

-   事前に宣言しているものについては遷移可能かのチェックが可能

```swift
if UIApplication.shared.canOpenURL(url) {
    UIApplication.shared.open(url, options: [:]) { success in
        DDLogInfo("success=\(success)")
    }
}
```

---

## Custom URL Schemeのデメリット2

アプリの初回起動時に確認メッセージが表示される

-   Universal Linksであればメッセージは表示されない

---

## Custom URL Schemeのデメリット3

同じURL Schemeを宣言しているアプリが複数あった場合どのアプリが起動するかは定義されていない

-   リリース時のレビューでは弾かれていない模様
-   基本的に先にインストールしていたアプリが起動する
-   セキュリティリスクがあるためUniversal Links (Android App Links)への移行が推奨されている

---

## Custom URL Schemeのデメリット4

アプリのUIによっては動線が分かりづらくなる。

-   ディープリンクで遷移したときは左上に戻るボタンが付いてそのボタンをタップで戻ることができる
-   アプリスイッチャーやホーム画面への遷移もできる
-   呼び出し先のアプリへはクエリパラメータで情報を渡すため直接起動されると情報を渡せない

---

## Custom URL Schemeのデメリット5

アプリから別アプリを一時的に呼び出すために使用すると呼び出し元のアプリはバッググラウンドになるため、メモリ不足やアップデートによって終了してしまうことがある。

-   メモリ不足やアップデートによってバックグラウンドになっているアプリが終了するのはOSの仕様
-   呼び出し先のアプリから戻ってきたときに情報が復元されないため維持する仕組みの実装が必要
