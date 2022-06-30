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
    section.size30 pre {
        font-size: max(3.0vh, 9px);
    }
---

# 使ったことのあるプログラミング言語

---

## [JavaScript](https://ja.wikipedia.org/wiki/JavaScript)

-   1995年に登場したプロトタイプベースのオブジェクト指向スクリプト言語
-   開発環境がなくてもブラウザとメモ帳だけで動作させられる

---

## [JavaScript](https://ja.wikipedia.org/wiki/JavaScript)

-   開発元のネットスケープとJavaを開発したサン・マイクロシステムズが業務提携していたことからLiveScriptからJavaScriptに名前が変更された
-   マイクロソフトによる実装がJScriptと呼ばれていた時期もあった
-   JavaScriptの中核的な仕様はECMAScriptとして標準化されている

---

## [JavaScript](https://ja.wikipedia.org/wiki/JavaScript)

-   ブラウザで動くためシングルスレッドが前提になる
-   描画処理を止めないため非同期処理が多く、コールバック地獄になりがち
-   Promiseを使うとネストは浅くできるが記述量は長くなりがち (ES2015)
-   async/awaitを使うと簡潔に書けるがPromiseの理解は必要 (ES2017)

---

## [Google Apps Script](https://ja.wikipedia.org/wiki/Google_Apps_Script)

-   2009年に登場したJavaScriptをベースにしたGoogleサービス用のスクリプト言語
-   JavaScriptが元になっているが一部使えない機能があった
    -   2020年にV8エンジンに対応したためオプションを有効にしていれば新しい機能も使える

---

## [Google Apps Script](https://ja.wikipedia.org/wiki/Google_Apps_Script)

-   無料で定期実行ができる
-   実行時間の上限が6分という制限がある
-   引っかかることは少ないがほかにも色々制限がある

---

## [Google Apps Script](https://ja.wikipedia.org/wiki/Google_Apps_Script)

-   エラーメッセージをログに出力するときに後半を切り捨てるのでデータ量が多いとメッセージが読めない

    ```txt
    TypeError: オブジェクト ... のプロパティ length を呼び出せません。関数ではなく「number」です。
        at xxx(yyy:zzz)
    ```

    ↓

    ```txt
    ログ出力のサイズが大きすぎます。出力を切捨てます。 TypeError: オブジェクト ...
    ```

---

## [Google Apps Script](https://ja.wikipedia.org/wiki/Google_Apps_Script)

-   [Clasp](https://github.com/google/clasp)を使うとローカルで開発できる
    -   **C**ommand **L**ine **A**pps **S**cript **P**rojects
-   バージョン管理を行ったり、TypeScriptなどAltJSで開発ができる

---

## [Perl](https://ja.wikipedia.org/wiki/Perl)

-   1987年に登場したスクリプト言語
    -   **P**ractical **E**xtraction and **R**eport **L**anguage
    -   真珠を意味するPearlと名付けようとしたが既に存在したので綴りを変更し、後付けで意味を付けた
-   PHPが登場する前はWebアプリケーションのほとんどがPerlで書かれていた
-   gitやopensslはPerlで書かれている

---

## [Perl](https://ja.wikipedia.org/wiki/Perl)

-   可読性が低くなりがちでWeb系ではPHP, Python, Rubyなどに取って代わられた
-   テキスト処理は優れているので今でも使われている
    -   [AWK](https://ja.wikipedia.org/wiki/AWK)や[sed](https://ja.wikipedia.org/wiki/Sed_(コンピュータ)), [grep](https://ja.wikipedia.org/wiki/Grep)などと一緒にサーバー上でワンライナーで処理したいときに使う

---

## [PHP](https://ja.wikipedia.org/wiki/PHP_(プログラミング言語))

-   1995年に登場したスクリプト言語
-   **P**HP: **H**ypertext **P**reprocessor ([再帰的頭字語](https://ja.wikipedia.org/wiki/再帰的頭字語))
    -   元々は **P**ersonal **H**ome **P**age Tools

---

## [PHP](https://ja.wikipedia.org/wiki/PHP_(プログラミング言語))

-   HTML埋め込み型の構文

    ```php
    <?php echo 'Hello world!'; ?>
    ```

    -   ファイルの末尾の`?>`は省略できる

---

## [C言語](https://ja.wikipedia.org/wiki/C言語)

-   1972年に作られたコンパイラ型言語
-   低水準言語（ハードウェア寄りに位置する低レイヤーの言語）の特徴も持つ
-   派生言語が多い
    -   [C++](https://ja.wikipedia.org/wiki/C%2B%2B)
    -   [C#](https://ja.wikipedia.org/wiki/C_Sharp)
    -   [Objective-C](https://ja.wikipedia.org/wiki/Objective-C)など派生言語が多い
-   C++と合わせて使用率がトップクラス

---

## [C言語](https://ja.wikipedia.org/wiki/C言語)

-   ポインタはオブジェクトのメモリ上のアドレスを格納する変数
    -   参照はオブジェクトを指している情報を格納する変数
-   アドレスは書き換えられるためポインタが指しているオブジェクトを変更できる
    -   参照が指しているオブジェクトは変更できない
-   C言語学習者がつまずきやすい

---

## [C言語](https://ja.wikipedia.org/wiki/C言語)

-   C言語に文字列というものがなく`char`型の配列として扱う

```c
#include <string.h>

int main()
{
    char name[20] = "Smaregi, Inc."; // 文字列の終わりとしてNUL文字が入る
    char * p;
    p = strchr(name, 'i'); // 第二引数が最初に見つかった位置をポインタで返す
    printf("%s\n", p); // => i, Inc.
}
```

---

## [C言語](https://ja.wikipedia.org/wiki/C言語)

-   1963年 [CPL](https://ja.wikipedia.org/wiki/CPL) (Combined Programming Language)
-   1967年 [BCPL](https://ja.wikipedia.org/wiki/BCPL) (Basic Combined Programming Language)
-   1969年 [B](https://ja.wikipedia.org/wiki/B言語) (B Language)
-   1972年 [C](https://ja.wikipedia.org/wiki/C言語) (C Language)
-   2001年 [D](https://ja.wikipedia.org/wiki/D言語) (D Language)

---

## [Java](https://ja.wikipedia.org/wiki/Java)

-   1996年に登場した仮想マシン上で動作する中間コードをコンパイルするコンパイラ言語とインタプリタ言語の中間的言語
-   [write once, run anywhere](https://ja.wikipedia.org/wiki/Write_once,_run_anywhere)
-   派生言語が多い
    -   [Groovy](https://ja.wikipedia.org/wiki/Groovy)
    -   [Scala](https://ja.wikipedia.org/wiki/Scala)
    -   [Kotlin](https://ja.wikipedia.org/wiki/Kotlin)

---

## [Java](https://ja.wikipedia.org/wiki/Java)

-   検査例外により例外処理を強制できる
    -   throwsの型推論がなく、指定方法に柔軟性がない

---

## [Java](https://ja.wikipedia.org/wiki/Java)

-   デコンパイルが容易
    -   クライアントアプリケーションは難読化しないとセキュリティ情報が漏れる

---

## [Ruby](https://ja.wikipedia.org/wiki/Ruby)

-   1995年に登場したスクリプト言語
-   Perl (←Pearl=真珠) が6月の誕生石のためPerlに続く、という意味で7月の誕生石であるルビーから名付けられた

---

## [Ruby](https://ja.wikipedia.org/wiki/Ruby)

-   全てがオブジェクトになっている純粋なオブジェクト言語

    ```ruby
    1.class
    # => Integer
    "".class
    # => String
    ```

---

## [Ruby](https://ja.wikipedia.org/wiki/Ruby)

-   演算子もオーバーライドできる

    ```ruby
    class CustomString < String
      def +(other)
        other + self + other
      end
    end
    "foo" + "bar"
    # => "foobar"
    CustomString.new("foo") + "bar"
    # => "barfoobar"
    ```

---

## [Ruby](https://ja.wikipedia.org/wiki/Ruby)

-   メソッド呼び出しの括弧が省略できる

    ```ruby
    "1,2,3".split /,/
    # => ["1", "2", "3"]
    ```

---

## [Ruby](https://ja.wikipedia.org/wiki/Ruby)

-   [ドメイン固有言語](https://ja.wikipedia.org/wiki/ドメイン固有言語)に使いやすい
    -   [Homebrew](https://brew.sh/)
    -   [Fastlane](https://fastlane.tools)
    -   [CocoaPods](https://cocoapods.org)

        ```ruby
        platform :ios
        pod 'AFNetworking',    '~> 2.0.0'
        pod 'CocoaLumberjack', '< 1.7'
        target 'MyApp'
        ```

---

## [FORTRAN](https://ja.wikipedia.org/wiki/FORTRAN)

-   1954年に作られた、広く使われた世界最初の高水準言語
-   **FOR**mula **TRAN**slation
-   現在でも数値計算によく使われる
-   大文字と小文字は区別されない

---

## [FORTRAN](https://ja.wikipedia.org/wiki/FORTRAN)

-   言語仕様が更新され続けている
    -   FORTRAN 66
    -   FORTRAN 77
    -   Fortran 90
    -   Fortran 95
    -   Fortran 2003
    -   Fortran 2008
    -   Fortran 2018

---

## [FORTRAN](https://ja.wikipedia.org/wiki/FORTRAN)

```fortran
PROGRAM FizzBuzz
    INTEGER I
    I = 1
    DO WHILE (I < 101)
        IF (MOD(I, 3) == 0 .AND. MOD(I, 5) == 0) THEN PRINT *, "FizzBuzz"
        ELSE IF (MOD(I, 3) == 0) THEN PRINT *, "Fizz"
        ELSE IF (MOD(I, 5) == 0) THEN PRINT *, "Buzz"
        ELSE PRINT *, I
        END IF
    I = I + 1
    END DO
END PROGRAM FizzBuzz
```

---

## [LISP](https://ja.wikipedia.org/wiki/LISP)

-   1958年に作られた現役の言語の中で2番目に古い高水準言語
-   関数型プログラミング
-   「LISt Processor」に由来する
-   細かい振る舞いに違いある方言が多数ある
    -   広く知られているのは[Common Lisp](https://ja.wikipedia.org/wiki/Common_Lisp)と[Scheme](https://ja.wikipedia.org/wiki/Scheme)
-   人工知能の研究・開発に使われている
-   Webアプリも開発できる

---

## [LISP](https://ja.wikipedia.org/wiki/LISP)

```lisp
(defun fizzbuzz (n)
    (if (or (zerop (mod n 3))
            (zerop (mod n 5)))
        (progn
            (when (and (zerop (mod n 3)))
                (format t "fizz"))
            (when (and (zerop (mod n 5)))
                (format t "buzz"))
            (format t " "))
        (format t "~a " n)))
(dotimes (n 20)
    (fizzbuzz (1+ n)))
```

---

## [R言語](https://ja.wikipedia.org/wiki/R言語)

-   1993年に作られた統計解析向けのインタプリタ型言語
-   S言語を参考にしたオープンソースの言語
-   学術系でよく使われている
-   ライブラリが充実している
    -   [CRAN](https://cran.r-project.org)（シーラン）
-   日本語の[Wiki](http://www.okadajp.org/RWiki/)が充実している

---

## [R言語](https://ja.wikipedia.org/wiki/R言語)

```r
# 代入は`=`も使えるが`<-`が推奨されている
s <- 100000
# 逆向きにも代入できる
4 -> t
# `<-`は関数のため式の中にも書ける
sum(runif(s <- 100000)^2 + runif(s)^2 <= 1) * 4 / s
```

---

## [R言語](https://ja.wikipedia.org/wiki/R言語)

```r
s <- c(1, 2, 3)
t <- c(4, 5, 6)
s + t
# [1] 5 7 9
```

-   ループ処理を行う組み込み関数がC言語で記述されているため高速

---

## [R言語](https://ja.wikipedia.org/wiki/R言語)

-   関数の命名がバラバラ

    ```r
    # https://www.rdocumentation.org/packages/utils/versions/3.6.2/topics/read.table
    read.csv(file)
    # https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/readLines
    readLines(con)
    # https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/readline
    readline(prompt)
    # https://www.rdocumentation.org/packages/readxl/versions/0.1.1/topics/read_excel
    read_excel(path)
    # https://www.rdocumentation.org/packages/MCMCpack/versions/1.6-1/topics/read.Scythe
    read.Scythe(infile)
    # https://www.rdocumentation.org/packages/multiclassPairs/versions/0.4.3/topics/ReadData
    ReadData(Data)
    ```

---

## [R言語](https://ja.wikipedia.org/wiki/R言語)

-   クラスの記法としてS3, S4, R6などの異なる仕組みが共存している
-   標準パッケージとサードパーティのパッケージで使っているものが異なる

---

## [R言語](https://ja.wikipedia.org/wiki/R言語)

### S3 Class

```r
x <- list(value = 1)
class(x) <- "container"
class(x) # [1] "container"
print.container <- function (container) {
    print(container$value)
}
print(x) # [1] 1
```

-   `print`は総称関数なので`print.{class}`が呼ばれる

---

## [R言語](https://ja.wikipedia.org/wiki/R言語)

### S4 Class

```r
setClass("container", representation(id = "character"), contains = "numeric")
x <- new("container", id = "3 random numbers", runif(3))
# An object of class "container"
# [1] 0.8540727 0.1615458 0.8514492
# Slot "id":
# [1] "3 random numbers"
x@id
# [1] "3 random numbers"
```

---

## [R言語](https://ja.wikipedia.org/wiki/R言語)

### R6 Class

<!-- _class: size30 -->

```r
library(R6)
Person <- R6Class("Person",
    public = list(
        name = NULL,
        initialize = function (name = NA) {
            self$name <- name
        },
        introduce = function () {
            print(paste0("My name is ", self$name, ".\n"))
        }
    )
)
person <- Person$new(name = "John")
person$introduce() # My name is John.
```

---

## [VBA](https://ja.wikipedia.org/wiki/Visual_Basic_for_Applications)

-   1993年に登場したマイクロソフト製のツールを制御するスクリプト言語
-   VBAはMicrosoft Officeシリーズに搭載されている

---

## [VBA](https://ja.wikipedia.org/wiki/Visual_Basic_for_Applications)

-   文法はかなりレガシー
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

## [VBScript](https://ja.wikipedia.org/wiki/VBScript)

-   1996年に登場したVBAと似たスクリプト言語
-   .NETやPowerShellに置き換えられている

---

## [PowerShell](https://ja.wikipedia.org/wiki/PowerShell)

-   2006年に登場したスクリプト言語
-   Windowsのコマンドプロンプトを置き換えるシェルでもある

---

<!-- _class: center -->

以上
