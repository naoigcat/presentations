---
marp: true
theme: gaia
paginate: true
style: |
    section.center p {
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
    }
---

# 使ったことのあるプログラミング言語

## 3: 2015年〜2022年頃

---

## [VBA](https://ja.wikipedia.org/wiki/Visual_Basic_for_Applications)

-   1993年に登場したマイクロソフト製のツールを制御するスクリプト言語
-   VBAはMicrosoft Officeシリーズに搭載されている
-   文法はかなりレガシー

---

## [VBA](https://ja.wikipedia.org/wiki/Visual_Basic_for_Applications)

```vba
Sub Sample
On Error GoTo Catch
    'ここでエラーが発生したらCatchラベルに移動
On Error Resume Next
    'ここでエラーが発生したらErrオブジェクトにエラー内容が入るが処理は継続される
On Error GoTo 0
    'ここでエラーが発生したら処理が停止する
    Exit Sub
Catch:
    'エラー時の処理
End Sub
```

---

## [VBA](https://ja.wikipedia.org/wiki/Visual_Basic_for_Applications)

```vba
Sub Sample
On Error GoTo Catch
    ' エラーが発生する処理
    Exit Sub
Catch:
    Select Case Err
    Case 1
        Resume Next 'エラーが発生した次の行から再開
    Case 2
        Resume 'エラーが発生した行から再開
    End Select
End Sub
```

---

## [VBScript](https://ja.wikipedia.org/wiki/VBScript)

-   1996年に登場したVBAと似たスクリプト言語
-   Microsoft Officeから独立して実行できる
-   .NETやPowerShellに置き換えられている

---

## [PowerShell](https://ja.wikipedia.org/wiki/PowerShell)

-   2006年に登場したスクリプト言語
-   Windowsのコマンドプロンプトを置き換えるシェルでもある
-   .NETのクラスを呼び出している

---

## [SQL](https://ja.wikipedia.org/wiki/SQL)

-   [関係データベース管理システム](https://ja.wikipedia.org/wiki/関係データベース管理システム) (RDBMS) データの操作や定義を行うためのデータベース言語
-   RDBMSによって文法が一部異なる
-   データベースエンジン内部でプログラムを実行するストアドプロシージャが定義できる

---

## [Swift](https://ja.wikipedia.org/wiki/Swift_(プログラミング言語))

---

## 難解プログラミング言語

-   [Brainf*ck](https://ja.wikipedia.org/wiki/Brainfuck)
-   [Whitespace](https://ja.wikipedia.org/wiki/Whitespace)
-   [Pxem](https://ja.wikipedia.org/wiki/Pxem) (日本製)

## 日本語ベースプログラミング言語

-   [ひまわり](https://ja.wikipedia.org/wiki/ひまわり_(プログラミング言語)) (日本製)
-   [なでしこ](https://ja.wikipedia.org/wiki/なでしこ_(プログラミング言語)) (日本製)
    -   ひまわりの後継言語
-   [プロデル](https://ja.wikipedia.org/wiki/プロデル) (日本製)
-   [ドリトル](https://ja.wikipedia.org/wiki/ドリトル_(プログラミング言語)) (日本製)

## ビジュアルプログラミング言語

-   [Scratch](https://ja.wikipedia.org/wiki/Scratch_(プログラミング言語))
    -   無料の教育プログラミング言語
-   [プログラミン](https://ja.wikipedia.org/wiki/プログラミン) (日本製)
    -   文部科学省が運営していた子供向けのビジュアルプログラミング言語

## その他

-   [Blawn](https://github.com/Naotonosato/Blawn) (日本製)
    -   U-22プログラミングコンテスト2019で経済産業大臣賞＜総合＞を受賞した言語
-   [Hot Soup Processor](https://ja.wikipedia.org/wiki/Hot_Soup_Processor) (日本製)

<!-- _class: center -->

以上
