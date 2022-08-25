---
marp: true
theme: gaia
paginate: true
style: |
    section.center p {
        position: absolute;
        width: 100%;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        text-align: center;
    }
    section.split {
        overflow: visible;
        display: grid;
        grid-template-columns: 40% 20% 40%;
        grid-template-rows: 10% auto;
        grid-template-areas:
            "header header header"
            "left center right";
    }
    section.split h2 {
        grid-area: header;
    }
    section.split p {
        display: flex;
        justify-content: center;
        align-items: center;
    }
---

# 使ったことのあるプログラミング言語

## 3: 2015年〜2022年頃

---

## [AppleScript](https://ja.wikipedia.org/wiki/AppleScript)

-   1993年に登場したmacOS標準搭載されたスクリプト言語
-   Classic Mac OSから継承されている
-   OS X v10.10からはJavaScriptでも記述できるようになった
-   GUIベースのアプリケーションを操作できる
-   GUIも作れる
-   ドキュメントやサンプルが少ない
-   大文字小文字は区別されない

---

## [AppleScript](https://ja.wikipedia.org/wiki/AppleScript)

```applescript
# FinderでデスクトップのWorksディレクトリを開く
tell application "Finder"
    activate
    open folder "Works" of desktop
end tell

# TextEditを開いてHelloと入力する
tell application "TextEdit"
    activate
    tell application "System Events"
        keystroke "Hello"
    end tell
end tell
```

---

## [VBA](https://ja.wikipedia.org/wiki/Visual_Basic_for_Applications)

-   1993年に登場したマイクロソフト製のツールを制御するスクリプト言語
-   VBAはMicrosoft Officeシリーズに搭載されている
-   文法はかなりレガシー
    -   GoToステートメントが存在する
    -   ContinueやTry-Catchが存在しない
-   大文字小文字は区別されない

---

## [VBA](https://ja.wikipedia.org/wiki/Visual_Basic_for_Applications)

```vb
Sub Sample
On Error GoTo Label
    'ここでエラーが発生したらLabelラベルに移動
On Error Resume Next
    'ここでエラーが発生したらErrオブジェクトにエラー内容が入るが処理は継続される
On Error GoTo 0
    'ここでエラーが発生したら処理が停止する
    Exit Sub
Label:
    'エラー時の処理
End Sub
```

---

## [VBA](https://ja.wikipedia.org/wiki/Visual_Basic_for_Applications)

```vb
Sub Sample
On Error GoTo Label
    ' エラーが発生する処理
    Exit Sub
Label:
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
-   コマンドプロンプトよりセキュリティが厳しい
-   .NETのクラスを呼び出している
-   大文字小文字は区別されない

---

## [PowerShell](https://ja.wikipedia.org/wiki/PowerShell)

```powershell
# カレントディレクトリ内で"word"を含むファイルのフルパスの一覧をlist.txtに書き出す
Get-ChildItem -Recurse -Filter "word" | ForEach-Object { $_.FullName } | Out-File "list.txt"

# CSVファイルをXLSXに変換する
$Excel = New-Object -ComObject Excel.Application
$Book = $Excel.Workbooks.Open("C:\Users\username\Desktop\sample.csv")
$Excel.DisplayAlerts = $False
$Book.SaveAs("C:\Users\username\Desktop\sample.xlsx",
             [Microsoft.Office.Interop.Excel.XlFileFormat]::xlExcel5)
$Excel.Quit()
[System.Runtime.Interopservices.Marshal]::ReleaseComObject($Book)
[System.Runtime.Interopservices.Marshal]::ReleaseComObject($Excel)
```

---

## [SQL](https://ja.wikipedia.org/wiki/SQL)

-   [関係データベース管理システム](https://ja.wikipedia.org/wiki/関係データベース管理システム) (RDBMS) データの操作や定義を行うためのデータベース言語
    -   「**S**tructured **Q**uery **L**anguage」が由来だが略語ではない
-   RDBMSによって文法が一部異なる
-   データベースエンジン内部でプログラムを実行するストアドプロシージャが定義できる

---

## [SQL](https://ja.wikipedia.org/wiki/SQL)

```sql
CREATE PROCEDURE [dbo].[ConvertXML]
    @INPUT XML, -- 引数
    @OUTPUT XML OUTPUT -- 返り値
AS
BEGIN
    DECLARE @TEMP TABLE (ID INT, NAME NVARCHAR(20));
    INSERT @TEMP (ID, NAME)
    SELECT USER.VALUE("@ID", "INT"), USER.VALUE(".", "NVARCHAR(20)")
    FROM @INPUT.NODES("Users") AS T(USER);
    ...
    SELECT @OUTPUT = (SELECT * FROM @TEMP FOR XML AUTO);
    RETURN 0;
END
```

---

## [Python](https://ja.wikipedia.org/wiki/Python)

-   1991年にリリースされたスクリプト言語
    -   Pythonはニシキヘビ
-   インデントでブロックを表す
-   機械学習などデータサイエンスでよく使われる
-   開発環境として[Jupyter](https://ja.wikipedia.org/wiki/Project_Jupyter)が有用
    -   Jupyter自身はPython以外の言語もサポートしている

---

## [Python](https://ja.wikipedia.org/wiki/Python)

```py
def factorial(x):
    if x == 0:
        return 1
    else:
        return x * factorial(x - 1)
```

---

## [Objective-C](https://ja.wikipedia.org/wiki/Objective-C)

-   1984年に登場したオブジェクト指向言語
-   C言語にマクロ的な拡張を行なってオブジェクト指向を取り入れた
-   C言語の拡張であるC++と共存可能
    -   Objective-C++
-   ネームスペースが存在しない
    -   ライブラリと衝突するためプリフィックスをつける慣習がある
        -   NS (Apple標準のFoundationフレームワーク, NeXTSTEP)
        -   UI (Apple標準のUIKitフレームワーク)

---

## [Objective-C](https://ja.wikipedia.org/wiki/Objective-C)

```objc
@interface MyObject : NSObject
@property (nonatomic, strong) NSString* value;
@end
@implementation MyObject
- (instancetype)initWithValue:(NSString*)value {
    if (self = [super init]) {
        NSString* temp = @"";
        if (value.length < 3) temp = @"-";
        self.value = [NSString stringWithFormat:@"%@%@", temp, value];
    }
    return self;
}
@end
```

---

## [Objective-C](https://ja.wikipedia.org/wiki/Objective-C)

```objc
switch (condition) {
    case 0: {
        NSString* temp = @"-";
        ...
        break
    }
    case 1:
        NSString* temp = @"-"; // => Error: Expected expression
        ...
        break
    default:
        break;
}
```

---

## [Swift](https://ja.wikipedia.org/wiki/Swift_(プログラミング言語))

-   2014年にAppleが開発したコンパイラ型言語
-   Objective-Cの特長を受け継ぎつつモダンな言語機能を兼ね備えている

---

## [Swift](https://ja.wikipedia.org/wiki/Swift_(プログラミング言語))

```swift
let name: String? = ""
println(name?.length ?: 0) // Nullableなオブジェクトのメソッドを呼び出すときは?.を使用する
println(name!.length) // 強制アンラップする場合は!を使用する
if let unwrapped = name { // ifもしくはguardで代入すると以降はNullableでなくなる
    print(unwrapped)
}
guard let name = name else { // 同名の変数で上書きすることもできる
    return
}
print(name)
```

---

## [Kotlin](https://ja.wikipedia.org/wiki/Kotlin)

-   2011年にJetBrainsが開発したコンパイラ型言語
-   Java仮想マシンで動作する
-   2019年にGoogleがAndroid開発の推奨言語に指定している

---

## [Kotlin](https://ja.wikipedia.org/wiki/Kotlin)

```kotlin
var name: String? = "" // Nullableな文字列
println(name?.length ?: 0) // Nullableなオブジェクトのメソッドを呼び出すときは?.を使用する
println(name!!.length) // 強制アンラップする場合は!!を使用する
name?.let { // Nullでないときのみ実行される
    println(it)
}
if (name != nil) { // スコープ内ではNullableではなくなる
    println(name)
}
var unwrapped = name ?: return // Nullだったら後続の処理を実行しない
```

---

## [Dart](https://ja.wikipedia.org/wiki/Dart)

-   2011年にGoogleが開発したプログラミング言語
-   JavaScriptの代替になることを目指して開発された
-   [Flutter](https://ja.wikipedia.org/wiki/Flutter)の開発言語

---

<!-- _class: center -->

おまけ

---

<!-- _class: split -->
## [Whitespace](https://ja.wikipedia.org/wiki/Whitespace)

```ws















```

<div></div>

```ws















```

---

<!-- _class: split -->
## [Whitespace](https://ja.wikipedia.org/wiki/Whitespace)

```ws
␣␣␣→␣␣␣␣␣␣→␣⏎
␣␣␣→→→␣→→␣␣␣→→␣␣→␣⏎
␣␣␣␣⏎
→→␣␣␣␣→␣␣→␣␣␣⏎
→⏎
␣␣␣␣␣→→␣␣→␣→⏎
→⏎
␣␣␣␣␣→→␣→→␣␣⏎
␣⏎
␣→⏎
␣␣→⏎
␣␣␣␣␣→→␣→→→→⏎
→⏎
␣␣␣␣␣→␣␣␣␣␣⏎
→⏎
```

<div></div>

```ws
␣␣␣␣␣→␣→␣→→→⏎
→⏎
␣␣␣␣␣→→␣→→→→⏎
→⏎
␣␣␣␣␣→→→␣␣→␣⏎
→⏎
␣␣␣␣␣→→␣→→␣␣⏎
→⏎
␣␣␣␣␣→→␣␣→␣␣⏎
→⏎
␣␣␣␣␣→␣␣␣␣→⏎
→⏎
␣␣␣␣␣→␣→␣⏎
→⏎
␣␣␣⏎
```

---

## [なでしこ](https://ja.wikipedia.org/wiki/なでしこ_(プログラミング言語))

```nako3
Nを2から20まで繰り返す
　　# 3の倍数か確認
　　もし、N%3=0ならば「{N}は3の倍数」と表示。
　　もし、N%5=0ならば「{N}は5の倍数」と表示。
ここまで。
```

---

<!-- _class: center -->

以上
