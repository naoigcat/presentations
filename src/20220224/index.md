---
marp: true
theme: gaia
paginate: true
---

# iOSのメモリ管理

メモリは正しく管理しないとメモリ消費量が徐々に増えてアプリが強制終了したり、解放済みの領域にアクセスしてクラッシュしたりする

---

## C言語

```c
// main.c
#include <stdlib.h>
int main() {
    // 整数100個分のメモリを確保する
    int *p = malloc(100 * sizeof(int));
    // 確保した領域を0でクリアする
    memset(p, 0, 100 * sizeof(int));
    // 先頭の値を表示する
    printf("%d", *p);
    // メモリを解放する
    free(p);
}
```

---

## Objective-C - 手動管理

```objc
// main.m
int main(int argc, char *argv[]) {
    // 1, 2を要素に持つ配列を初期化する
    NSArray *p = [[NSArray alloc] initWithObjects:@1, @2, nil];
    // %@は文字列はそのままそれ以外のオブジェクトはdescriptionメソッドの返り値を出力する
    NSLog(@"%@", p);
    // メモリを解放する
    [p release];
}
```

---

## [参照カウント](https://ja.wikipedia.org/wiki/参照カウント)

-   オブジェクトに対して参照カウントを付与しておく
-   オブジェクトの生成時・参照時にカウントを増やす
-   オブジェクトを使用しなくなったときにカウントを減らす
-   参照カウントが0になったときメモリを解放する

---

## Objective-C - プロパティ宣言

```objc
// SomeClass.h
@interface SomeClass: NSObject
@property (nonatomic, strong) NSObject *object;
- (void) someMethod;
@end
```

-   プロパティはインスタンス変数へのゲッター・セッターを自動生成する仕組み
-   参照カウントを増やすのも自動的にやってくれる

---

## Objective-C - プロパティ実装

```objc
// SomeClass.m
@implementation SomeClass {
    NSObject *_object; // インスタンス変数の宣言
}
@synthesize object = _object;
- (void) someMethod {
    NSLog(@"%@", self.object);
}
- (void) dealloc {
    [super dealloc];
    [_object release]; // retainCount -1
}
@end
```

---

## Objective-C - ゲッター

```objc
// SomeClass.m
@implementation SomeClass
- (NSObject*) object {
    return _object;
}
@end
```

-   インスタンス変数の宣言と`@synthesize`は`@property`によって自動生成されるようになっているため今は書く必要がない

---

## Objective-C - セッター

```objc
// SomeClass.m
// @property (nonatomic, strong) NSObject *object;
- (void) setObject:(NSObject*)object {
    if (_object != object) {
        [_object release]; // retainCount -1
        _object = [object retain]; // retainCount +1
    }
}
```

---

## Objective-C - プロパティへのセット

```objc
// main.m
int main(int argc, char *argv[]) {
    SomeClass *someClass = [[SomeClass alloc] init];
    NSObject *object = [[NSObject alloc] init]; // [object retainCount] = 1
    someClass.object = object; // [object retainCount] = 2
    [object release]; // [object retainCount] = 1
    [someClass someMethod];
    [someClass release]; // [object retainCount] = 0
}
```

---

## Objective-C - オートリリースプール

```objc
// main.m
int main(int argc, char *argv[]) {
    @autoreleasepool {
        SomeClass *someClass = [[[SomeClass alloc] init] autorelease];
        NSObject *object = [[[NSObject alloc] init] autorelease];
        someClass.object = object;
        [someClass someMethod];
    }
}
```

-   `autorelease`を呼び出すとオートリリースプールに登録され、スコープを抜けるときに`release`が呼び出される

---

## Objective-C - ARC

-   Automatic Reference Counting <-> Manual Reference Counting
-   `retain`, `release`, `autorelease`, `dealloc`をコンパイラが補完する

```objc
// SomeClass.m
// @property (nonatomic, strong) NSObject *object;
- (void) setObject:(NSObject*)object {
    if (_object != object) {
        _object = object;
    }
}
- (void) dealloc {
    // [super dealloc];
}
```

---

## Swift

```swift
// SomeClass.swift
class SomeClass: NSObject {
    var object: NSObject
    func someMethod() {
        print("\(self.object)");
    }
}
```

```swift
// main.swift
let someClass = SomeClass()
someClass.object = NSObject()
someClass.someMethod()
```

---

## [循環参照](https://ja.wikipedia.org/wiki/参照カウント#循環参照の問題点)

-   参照カウントによるメモリ管理では循環参照によるメモリリークが発生しうる

---

## 循環参照 - 例

```swift
class A {
    var b: B? = nil
}
class B {
    var a: A? = nil
}
class C {
    func someMethod() {
        let a = A() // a=1, b=0
        let b = B() // a=1, b=1
        a.b = b     // a=1, b=2
        b.a = a     // a=2, b=2
    } // a=1, b=1
}
```

---

## 循環参照 - デリゲートパターン (弱い参照)

```swift
class A {
    var b: B? = nil
}
class B {
    weak var a: A? = nil
}
class C {
    func someMethod() {
        let a = A()    // a=1, b=0
        let b = B()    // a=1, b=1
        a.b = b        // a=1, b=2
        b.a = a // a=1, b=2
    } // a=0, b=1 -> a=0, b=0
}
```

---

## 循環参照 - クロージャ (呼び出し先)

```swift
class B {
    public typealias Completion = () -> ()
    var completion: Completion
    init(completion: @escaping Completion) {
        self.completion = completion
    }
    func async() {
        DispatchQueue.main.asyncAfter(deadline: .now() + 3.0, execute: { () in
            self.completion()
            self.completion = nil
        })
    }
}
```

---

## 循環参照 - クロージャ (呼び出し元)

```swift
class A {
    var b: B? = nil
}
class C {
    func someMethod() {
        let a = A()               // a=1, b=0
        let b = B(completion: {
            print("\(a.message)") // a=2, b=1
        })
        a.b = b                   // a=2, b=2
        b.async()
    } // a=1, b=1 -> 3s -> a=0, b=1 -> a=0, b=0
}
```

---

## 循環参照 - クロージャ

-   クロージャは内部で使用するオブジェクトの参照カウントを増やす
-   参照カウントを増やさずにオブジェクトを渡すこともできるがその場合スコープから抜けたときに解放されてしまうため非同期実行時に参照できない
-   スコープを抜けた後でも実行できるようにするためには循環参照にしてメモリが解放されないようにする必要がある
