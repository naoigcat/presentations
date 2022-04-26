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

# Objective-Cの黒魔術

-   プログラミングにおける「黒魔術」とは可読性を犠牲にコードの記述量を大幅に減らせる技術のこと

    -   よく分からないが動いてしまう魔法のようなコード

-   とても便利だが仕様を理解せずに使用したり多用しすぎるとバグの原因にもなりうる

---

## 黒魔術の種類

-   Duck Typing

-   Preprocessor

-   Metaprogramming

-   Associated Object

-   Method Swizzling

---

## [Duck Typing](https://ja.wikipedia.org/wiki/ダック・タイピング) 1

-   動的型付けに対応した言語に特徴的なスタイル

-   "If it walks like a duck and quacks like a duck, it must be a duck"

    （もしもそれがアヒルのように歩き、アヒルのように鳴くのなら、それはアヒルに違いない）

-   弱い静的型付けであるC言語やObjective-Cでも使用できる（Swiftは強い静的型付けのため使用できない）

-   コンパイル時にエラーが出なくなるため静的型付けの言語のメリットが失われる

---

## Duck Typing 2

```objc
@interface Duck: NSObject
- (void) walk;
- (void) quack;
@end
@interface Dog: NSObject
- (void) walk;
- (void) bark;
@end
```

---

## Duck Typing 3

```objc
void walkAndQuack(Duck* duck) {
    [duck walk];
    [duck quack];
}
int main() {
    walkAndQuack([[Duck alloc] init]);
    walkAndQuack([[Dog alloc] init]); // コンパイルエラー
}
```

---

## Duck Typing 4

```objc
void walkAndQuack(id duck) {
    [duck walk];
    [duck quack]; // Dogクラスのインスタンスを渡したときに実行時エラー
}
int main() {
    walkAndQuack([[Duck alloc] init]);
    walkAndQuack([[Dog alloc] init]);
}
```

---

## Duck Typing 5

```objc
int main() {
    NSArray* animals = @[[[Duck alloc] init], [[Dog alloc] init]];
    for (id animal in animals) {
        [animal walk];
    }
}
```

---

## [Preprocessor](https://ja.wikipedia.org/wiki/プリプロセッサ) 1

-   コンパイル前に行われる処理

-   モダンな言語にはない機能

---

## Preprocessor 2

-   ソースコードの部分的選択

```objc
int main() {
#if DEBUG
    // デバッグビルド時のみコンパイルされる
#else
    // リリースビルド時のみコンパイルされる
#endif

#if TARGET_OS_SIMULATOR
    // シミュレーターでのビルド時のみコンパイルされる
#endif
}
```

---

## Preprocessor 3

-   条件に当てはまらないコードはコンパイルもされないためビルドエラーにもならない

-   リリースビルドに必要ないコードを含めないようにできるためバイナリを小さくできる

---

<!-- _class: bottom -->
## Preprocessor 4

-   マクロの展開

```objc
#define MAX_QUANTITY 99
#define MAX(A,B) (A > B ? A : B)

int main() {
    int a = 10, b = 100;
    if (MAX(a, b) > MAX_QUANTITY) {
        puts("a or b is greater than the maximum.");
    }
}
```

---

<!-- _class: bottom -->
## Preprocessor 5

```objc
int main() {
    int a = 10, b = 100;
    if ((a > b ? a : b) > 99) {
        puts("a or b is greater than the maximum.");
    }
}
```

---

## Preprocessor 6

-   文字列の置き換えのため予期せぬ置き換えが発生することがある

-   そのためマクロ定数は基本的に大文字を使用する

```objc
#define max 3

int main() {
    int max = 2; // `int 3 = 2;`と変換されてコンパイルエラー（`Expected identifier or '('`）
}
```

---

## Preprocessor 7

-   マクロ関数も文字列に置き換えで実施されるため計算順序が変わってしまう場合がある

-   そのため引数と返り値は全て括弧括る

```objc
#define DOUBLE(A) A * 2
#define PLUS_ONE(B) B + 1

int main () {
    printf("%d", DOUBLE(1 + 1));
    // `(1 + 1) * 2 = 4`が期待値だが、`printf("%d", 1 + 1 * 2);`と変換されて出力結果は3
    printf("%d", PLUS_ONE(1) * 2);
    // `(1 + 1) * 2 = 4`が期待値だが、`printf("%d", 1 + 1 * 2);`と変換されて出力結果は3
}
```

---

## Preprocessor 8

-   副作用のある式を引数に渡すと予期せず複数回実行されてしまう場合がある

```objc
#define TWICE(A) ((A) + (A))

int main() {
    int a = 1;
    printf("%d", TWICE(++a));
    // `2 + 2 = 4`が期待値だが、`printf("%d", ((++a) + (++a)));`変換されて出力結果は5
}
```

---

## Preprocessor 9

-   マクロ関数は関数呼び出しのオーバーヘッドを避けるために使用されてきた

-   今はインライン関数が使用できるのでそちらの方が良い

```objc
NS_INLINE int DOUBLE(int A) {
    return A * 2;
}
```

---

## [Metaprogramming](https://ja.wikipedia.org/wiki/メタプログラミング) 1

-   プログラム自体を生成するプログラムのこと

-   マクロやリフレクションもメタプログラミングの一種

---

## Metaprogramming 2

```objc
#import <objc/runtime.h>
@implementation Object
- (void) main {
    __block BOOL called = NO;
    SEL sel = NSSelectorFromString(@"aMethod");
    IMP imp = imp_implementationWithBlock(^(Object* self) {
        called = YES;
    });
    class_addMethod([Object class], sel, imp, "v@");
    [self performSelector:sel];
    NSAssert(called, @"Block is not called.");
}
@end
```

---

## Associated Object 1

-   クラスを継承せずに動的にプロパティを追加できる

-   メタプログラミングの一種

---

## Associated Object 2

```objc
#import <UIKit/UIKit.h>
#import <objc/runtime.h>
@interface UILabel (Tooltip)
// @property (nonatomic, copy) NSString* tooltip;
- (NSString*) tooltip;                  // `label.tooltip`で値を取得できる
- (void) setTooltip:(NSString*)tooltip; // `label.tooltip = @"";`で値を設定できる
@end
static void * tooltipKey = "tooltipKey";
@implementation UILabel (Tooltip)
- (NSString*) tooltip {
    return objc_getAssociatedObject(self, tooltipKey);
}
- (void) setTooltip:(NSString*)tooltip {
    objc_setAssociatedObject(self, tooltipKey, tooltip, OBJC_ASSOCIATION_COPY_NONATOMIC);
}
@end
```

---

## Method Swizzling 1

-   Swizzling (スウィズリング) = （カクテルなどの飲み物をマドラーで）かき混ぜる

-   フレームワークやライブラリの処理に割り込ませることができる

-   メタプログラミングの一種

---

## Method Swizzling 2

```objc
#import <UIKit/UIKit.h>
@interface ViewController: UIViewController; @end
@implementation ViewController
- (void) updateView {
    NSLog(@"updateView");
}
- (void) updateViewSwizzled {
    [self updateViewSwizzled];
    NSLog(@"updateViewSwizzled");
}
@end
```

---

## Method Swizzling 3

```objc
#import <objc/runtime.h>
@implementation ViewController
- (void) viewDidLoad {
    [self updateView]; // updateView
    [super viewDidLoad];
    Method from = class_getInstanceMethod([self class], @selector(updateView));
    Method to   = class_getInstanceMethod([self class], @selector(updateViewSwizzled));
    method_exchangeImplementations(from, to);
    [self updateView]; // updateView -> updateViewSwizzled
}
@end
```

---

<!-- _class: bottom -->
## Method Swizzling 4

-   メソッドの定義を入れ替える

```objc
@implementation ViewController
- (void) updateView {
    NSLog(@"updateView");
}
- (void) updateViewSwizzled {
    [self updateViewSwizzled];
    NSLog(@"updateViewSwizzled");
}
@end
```

---

<!-- _class: bottom -->
## Method Swizzling 5

```objc
@implementation ViewController
- (void) updateView {
    [self updateViewSwizzled];
    NSLog(@"updateViewSwizzled");
}
- (void) updateViewSwizzled {
    NSLog(@"updateView");
}
@end
```

---

## Swiftでの黒魔術 1

-   Duck Typing

    -   id型に相当するものがなく、メソッドを呼び出すときは必ずキャストが必要

---

## Swiftでの黒魔術 2

-   Preprocessor

    -   ソースコードの部分的選択`#if`, `#else`, `#endif`は使用可能

    -   マクロの定義`#define`は使用不可

    -   Objective-Cで定義されたマクロは使用可能

---

## Swiftでの黒魔術 3

-   Metaprogramming

    -   `#import <objc/runtime.h>`の代わりに`import ObjectiveC`で利用可能

```swift
import ObjectiveC
private var tooltipKey = 0
extension UILabel {
    var tooltip: String {
        get {
           return objc_getAssociatedObject(self, &tooltipKey) as String
        }
        set {
            objc_setAssociatedObject(self, &tooltipKey, newValue, objc_AssociationPolicy(OBJC_ASSOCIATION_COPY_NONATOMIC))
        }
    }
}
```
