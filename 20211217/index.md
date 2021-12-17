---
marp: true
theme: gaia
paginate: true
style: |
    section.split {
        overflow: visible;
        display: grid;
        grid-template-columns: 40% 20% 40%;
        grid-template-rows: 20% auto;
        grid-template-areas:
            "header header header"
            "left center right";
    }
    section.split h1 {
        grid-area: header;
    }
    section.split p {
        display: flex;
        justify-content: center;
        align-items: center;
    }
---

<!-- _class: split -->
# iOSのイベント通知

UI操作
プッシュ通知
データ変更
etc.

→

画面更新
etc.

---

## イベント通知の方法

-   Target Action
-   Delegate
-   Override
-   NotificationCenter
-   Key-Value Observing
-   Closure Callback
-   Data Binding

---

## Target Action 1

```swift
class ViewController: UIViewController {
    override func viewDidLoad() {
        let button = UIButton()
        button.frame = .init(x: 0, y: 0, width: 320, height: 44)
        button.addTarget(self, action: #selector(buttonTapped(_:)),
                                  for: .touchUpInside)
        button.setTitle("Button", for: .normal)
        self.view.addSubview(button)
    }
    @objc func buttonTapped(_ sender: UIButton) {
        // ボタンをタップしたときに呼ばれる
    }
}
```

---

## Target Action 2

-   Storyboardから接続できる
-   メソッドが存在しない場合実行時エラー (Objective-C)
-   引数の型が一致していない場合実行時エラー
-   Xcode上で呼び出し元を検索できない

---

## Delegate 1

```swift
class ViewController: UIViewController, UITextFieldDelegate {
    override func viewDidLoad() {
        let textField = UITextField()
        textField.frame = .init(x: 0, y: 0, width: 320, height: 44)
        textField.delegate = self
        self.view.addSubview(textField)
    }
    func textFieldDidEndEditing(_ textField: UITextField) {
        // キーボードのエンタキーをタップして確定したときに呼ばれる
    }
}
```

---

## Delegate 2

-   呼び出し元に値を返せる
-   プロトコルを適用することで厳密に型チェックが可能
-   複数同時に使った場合インスタンスの区別が面倒
-   名前が固定なため衝突しないよう長くなりがち
-   必要ないイベントはメソッドを実装しないことでスキップできる

---

## Override 1

```swift
class ViewController: UIViewController {
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        // 画面が表示される直前に呼ばれる
    }
    override func viewDidAppear(_ animated: Bool) {
        super.viewDidAppear(animated)
        // 画面が表示された直後に呼ばれる
    }
}
```

---

## Override 2

-   Target ActionとDelegateをこの形式に変換できる
-   Delegateと大体同じ

---

## NotificationCenter 1

```swift
class ViewController: UIViewController {
    override func viewWillAppear(_ animated: Bool) {
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillShow(_:)),
                                                         name: UIResponder.keyboardWillShowNotification,
                                                       object: nil)
    }
    override func viewDidDisappear(_ animated: Bool) {
        NotificationCenter.default.removeObserver(self, name: UIResponder.keyboardWillShowNotification,
                                                      object: nil)
    }
    @objc func keyboardWillShow(_ notification: Notification) {
        // キーボードが表示されたときに呼ばれる
    }
}
```

---

## NotificationCenter 2

-   イベント発生元と通知先に直接参照関係になくても通知できる
-   情報も付加できるが辞書形式のため型チェックはできない
-   呼び出し元に情報は返せない
-   Xcode上で呼び出し元を検索できない
-   処理の流れは追いづらい

---

## Key-Value Observing 1

```swift
class ViewController: UIViewController {
    var model = Model()
    var observation: NSKeyValueObservation?
    override func viewDidLoad() {
        observation = model.observe(\.value, options: [.new]) { model, change in
            // model.valueが変更したときに呼ばれる
        }
    }
}
class Model: NSObject {
    @objc dynamic var value: Int = 0
}
```

---

## Key-Value Observing 2

-   値を変更する箇所が多い場合に特に便利
-   Objective-CのNSObjectを継承しているクラスでないと使えない
-   処理の流れは追いづらい
-   どこで変更されたかは分からない
-   構造体では使用できない

---

## Closure Callback 1

```swift
class ViewController: UIViewController {
    override func viewDidDisappear(_ animated: Bool) {
        let alert = UIAlertController(title: "Alert", message: "", preferredStyle: .alert)
        alert.addAction(UIAlertAction(title: "OK", style: .default, handler: { _ in
            // アラートのOKがタップされたときに呼ばれる
        }))
        self.present(alert, animated: true, completion: { in
            // アラートが表示された直後に呼ばれる
        })
    }
}
```

---

## Closure Callback 2

-   処理を一箇所にまとめて書けるので使いやすい
-   インスタンスの区別のためプロパティに保持する必要がない
-   メソッドを分けないとインデントが深くなりがち
-   メモリ管理は難しい

---

## Data Binding 1

```swift
import Combine
class ViewController: UIViewController {
    @Published var text: String = "Default"
    var label: UILabel = UILabel()
    var cancellables = Set<AnyCancellable>()
    override func viewDidLoad() {
        self.$text
            .map({ Optional($0) })
            .receive(on: DispatchQueue.main)
            .assign(to: \UILabel.text, on: label)
            .store(in: &cancellables)
    }
}
```

---

## Data Binding 2

-   比較的最近使えるようになった
-   言語仕様のせいか実際の挙動が分かりづらい

---

## SwiftUI 1

```swift
struct ContentView: View {
    @State var buttonText = "Button"
    var body: some View {
        Button(action: {
            self.buttonText = "Button Tapped"
        }, label: {
            Text(self.buttonText)
        })
    }
}
```

---

## SwiftUI 2

-   今までのUIKitに代わる宣言的UIフレームワーク
-   iOS13 / iPadOS13以降のみ使用できる
-   Storyboardを使わなくて良くなる
-   UIKitならできることができない場合がある
-   基本的にClosure CallbackとData Bindingを使用する
