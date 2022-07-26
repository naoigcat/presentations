---
marp: true
theme: gaia
paginate: true
style: |
    section.bottom pre {
        position: absolute;
        left: 5.5%;
        right: 5.5%;
        bottom: 10%;
    }
---

# iOSでのUI実装 - Interface Builder

iOSでUIを実装する方法は以下の3つがある。

1.  Interface Builder
2.  コードベース (UIKit)
3.  コードベース (SwiftUI)

---

## [Interface Builder](https://developer.apple.com/jp/xcode/interface-builder/)

Xcode付属のUI実装ツール。

> XcodeのInterface Builderエディタを使用すると、コードを1行も記述することなく完全なユーザーインターフェイスを構築できます。ウインドウ、ボタン、テキストフィールド、その他のオブジェクトをデザインキャンバスにドラッグしてドロップするだけで、機能的なユーザーインターフェイスを作成できます。

---

## 対応しているフレームワーク

-   Carbon (Classic Mac OS)
-   Cocoa (macOS)
-   Cocoa Touch (iOS, iPadOS, tvOS, watchOS)

---

## ファイルの種類 .nib

-   NeXTSTEP Interface Builderの略
-   バイナリ形式
-   Xcode 3.0未満
-   画面・コンポーネント単位

## ファイルの種類 .xib

-   XML Interface Builderの略
-   XMLファイル形式
-   画面・コンポーネント単位
-   Xcode 3.0以降

## ファイルの種類 .storyboard

-   XMLファイル形式
-   Xcode 4.2以降
-   .xibの機能に加えて画面遷移も定義できる
