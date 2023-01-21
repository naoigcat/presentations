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

## 2: 2008年〜2015年頃

---

## [XSLT](https://ja.wikipedia.org/wiki/XSL_Transformations)

-   XML文書の変換用言語
-   E**x**tensible **S**tylesheet **L**anguage **T**ransformations
-   [XPath](https://ja.wikipedia.org/wiki/XML_Path_Language)、[XSL-FO](https://ja.wikipedia.org/wiki/XSL_Formatting_Objects)とともにXML文書の組版処理システム[XSL](https://ja.wikipedia.org/wiki/Extensible_Stylesheet_Language)を構成する
-   変換規則を宣言的に記述したもの

---

## [XSLT](https://ja.wikipedia.org/wiki/XSL_Transformations)

-   XML文書を読み込んで別のテキストを出力する
-   XSLT自身もXMLで記述される
-   ブラウザでも変換処理は実行できる
-   静的サイトジェネレーターとして利用できる
-   XHTMLと親和性が高い

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

-   演算子もメソッド

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
-   e.g. CocoaPods

    ```ruby
    # Podfile
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
-   文字列処理は不得意

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

## [LISP](https://ja.wikipedia.org/wiki/LISP)

-   1958年に作られた現役の言語の中で2番目に古い高水準言語
-   関数型プログラミング
-   「**LIS**t **P**rocessor」に由来する
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
    (fizzbuzz (1 + n)))
```

---

## [LaTeX](https://ja.wikipedia.org/wiki/LaTeX)

-   1984年に作られたテキストベースの組版処理システム
-   組版処理システム[TeX](https://ja.wikipedia.org/wiki/TeX)にマクロパッケージを組み込むことで手軽に組版を行うことができるようになっている
    -   TeXはギリシア語「τέχνη（テクネ）」が由来
-   ソースコードをコンパイルすることでPDF等の閲覧用ファイルが生成される
-   環境構築が大変
-   エラーメッセージは分かりにくい
-   マクロパッケージがコンフリクトしやすい

---

## [R言語](https://ja.wikipedia.org/wiki/R言語)

-   1993年に作られた統計解析向けのインタプリタ型言語
-   S言語を参考にしたオープンソースの言語
-   学術系でよく使われている
-   ライブラリが充実している
    -   [CRAN](https://cran.r-project.org)（シーラン）
-   日本語の[Wiki](http://www.okadajp.org/RWiki/)も充実している

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

```r
library(R6)
Person <- R6Class("Person",
    public = list(
        name = NULL,
        initialize = function (name = NA) { self$name <- name },
        introduce = function () { print(paste0("My name is ", self$name, ".\n")) }
    )
)
person <- Person$new(name = "John")
person$introduce() # My name is John.
```

---

<!-- _class: center -->

以上
