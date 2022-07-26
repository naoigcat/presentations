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

## 1: 2003年〜2008年頃

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

<!-- _class: center -->

以上
