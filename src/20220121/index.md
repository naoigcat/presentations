---
marp: true
theme: gaia
paginate: true
---

# iOSのバーコード読取

レジアプリにはバーコード読取は必須機能

---

## バーコード読取の方法

-   カメラ読取
-   外部キーボード入力
-   外部機器 (SDK)
    -   ジャケット式
    -   プリンター経由

---

## カメラ読取

iOS標準のカメラにはバーコード読取機能が搭載されている

```swift
import AVFoundation
class BarcodeReaderViewController: UIViewController, AVCaptureMetadataOutputObjectsDelegate {
    let captureSession = AVCaptureSession()
    var metadataOutput = AVCaptureMetadataOutput()
    override func viewDidLoad() {
        self.captureSession.addInput(try! AVCaptureDeviceInput(device: .default(for: AVMediaType.video)!))
        self.captureSession.addOutput(self.metadataOutput)
        self.metadataOutput.metadataObjectTypes = [.code128, .qr]
        self.metadataOutput.setMetadataObjectsDelegate(self, queue: DispatchQueue.main)
        self.captureSession.startRunning()
    }
    func metadataOutput(_ output: AVCaptureMetadataOutput,
                        didOutput metadataObjects: [AVMetadataObject],
                        from connection: AVCaptureConnection) {
        print(metadataObjects.compactMap({ $0 as? AVMetadataMachineReadableCodeObject }).first?.stringValue ?? "")
    }
}
```

---

## カメラ読取が対応しているバーコード

```swift
extension AVMetadataObject.ObjectType {
    public static let upce: AVMetadataObject.ObjectType // UPC-E
    public static let code39: AVMetadataObject.ObjectType // Code 39
    public static let code39Mod43: AVMetadataObject.ObjectType // Code 39 Mod 43
    public static let ean13: AVMetadataObject.ObjectType // EAN-13
    public static let ean8: AVMetadataObject.ObjectType // EAN-8
    public static let code93: AVMetadataObject.ObjectType // Code 93
    public static let code128: AVMetadataObject.ObjectType // Code 128
    public static let pdf417: AVMetadataObject.ObjectType // PDF417
    public static let qr: AVMetadataObject.ObjectType // QRコード
    public static let aztec: AVMetadataObject.ObjectType // Aztec
    public static let interleaved2of5: AVMetadataObject.ObjectType // Interleaved 2 of 5 codes
    public static let itf14: AVMetadataObject.ObjectType // ITF14
    public static let dataMatrix: AVMetadataObject.ObjectType // Data Matrix
}
```

---

## カメラ読取が対応していないバーコード

-   NW-7
-   書籍JANコード (2段)

通常のバーコードリーダーで設定可能な

-   先頭の0を読み落とす
-   NW-7のスタートコード・ストップコードを変更する

ということができないため読み取れるバーコードに制限がある

---

## カメラ読取のメリット

-   追加の外部機器が必要ない

---

## カメラ読取のデメリット

-   どの部分を読み取っているか画面上に表示しないと分からないためUI上制限がある
-   読取精度は端末のカメラに依存する

---

## 外部キーボード入力

iOSにはBluetooth接続で外部キーボードを繋げることができる

この機能を利用して外部キーボードとしてバーコードリーダーを接続し、キーボード入力としてバーコードを送ることでバーコード読取を実現できる

-   Cyclops ARK-5000X
-   SocketScan S700
-   SF1-QBi
-   OPN-3200i

---

## 外部キーボード入力で読み取る仕組み

-   サイズが0の入力欄`UITextField`を画面上に置く
-   入力欄にフォーカスされたときに表示されるキーボードのサイズを0にしておく
-   バーコード読取を有効にしたいタイミングで入力欄にフォーカスを移す

---

## 外部キーボード入力のメリット

-   アプリから直接Bluetooth接続しない
    -   Bluetooth接続時に必要な申請が不要
-   仕組みが単純なため機器毎に実装する必要がない

---

## 外部キーボード入力のデメリット

-   iOSのアップデートによりフォーカスが移らなくなる場合がある
-   キーボード入力のため予期しない動作をする場合がある
-   バーコード読取を有効な画面が同時に複数表示されていても実際に読み取れるのは一画面のみ
    -   どれかで受け取って適切な画面に受け渡しをする必要がある
-   キーボード設定の影響を受ける
    -   ソフトウェアキーボードのかな入力設定
    -   ハードウェアキーボードのかな入力設定
    -   ハードウェアキーボードの自動大文字入力

---

## 外部機器 (SDK) ジャケット式

iPhoneやiPod touchに装着して使用するバーコードリーダー

電源供給をiOS端末から行ってBluetoothで通信し、SDK経由でバーコードを受け取る

-   ScanJacket
-   AsReader ASR-010D

---

## 外部機器 (SDK) プリンター経由

プリンターのUSB端子に接続して使用するバーコードリーダー

プリンターへコマンドを送ってそのレスポンスとしてバーコードを受け取るが、コマンドは非公開のためSDK経由で受け取る

-   BCR-POP1 (mPOP, mC-Printシリーズ)
-   OT-HS11 (TM-m30)
-   L-46X (TM-m30)

---

## 外部機器 (SDK) から画面への渡し方

1.  画面をSDKのデリゲートに指定してイベントとして受け取る
2.  フォーカスの当たっている入力欄にキーボード入力として入力する

---

## フォーカスの当たっている入力欄の検出 1

```swift
func read(barcode: String) {
    guard var view: UIView = self.window else { return }
    var subviews = view.subviews
    while !view.isFirstResponder && subviews.count > 0 {
        if let firstResponder = subviews.filter({ $0.isFirstResponder }).first {
            view = firstResponder
            break
        }
        subviews = subviews.flatMap({ $0.subviews })
    }
    guard view.isFirstResponder else { return }
    self.read(barcode: barcode, view: view)
}
```

---

## フォーカスの当たっている入力欄の検出 2

```swift
func read(barcode: String, view: UIView) {
    switch view {
    case let textField as UITextField:
        self.read(barcode: barcode, textField: textField)
    case let textView as UITextView:
        self.read(barcode: barcode, textView: textView)
    case let searchBar as UISearchBar:
        self.read(barcode: barcode, searchBar: searchBar)
    default:
        break
    }
}
```

---

## フォーカスの当たっている入力欄の検出 3

```swift
func read(barcode: String, textField: UITextField) {
    guard textField.delegate?.textField?(textField,
                                         shouldChangeCharactersIn: NSMakeRange(textField.text?.count ?? 0, 0),
                                         replacementString: barcode) ?? true else { return }
    textField.text = barcode
    let _ = textField.delegate?.textFieldShouldReturn?(textField)
}
```

---

## フォーカスの当たっている入力欄の検出 4

```swift
func read(barcode: String, textView: UITextView) {
    guard textView.delegate?.textView?(textView,
                                       shouldChangeTextIn: NSMakeRange(textView.text?.count ?? 0, 0),
                                       replacementText: barcode) ?? true else { return }
    textView.text = barcode
    textView.delegate?.textViewDidChange?(textView)
}
```

---

## フォーカスの当たっている入力欄の検出 5

```swift
func read(barcode: String, searchBar: UISearchBar) {
    guard searchBar.delegate?.searchBar?(searchBar,
                                         shouldChangeTextIn: NSMakeRange(searchBar.text?.count ?? 0, 0),
                                         replacementText: barcode) ?? true else { return }
    searchBar.text = barcode
    searchBar.delegate?.searchBarSearchButtonClicked?(searchBar)
}
```

---

## イベントとして受け取る場合のメリット

-   入力欄にフォーカスが当たっているかどうかを気にする必要がない
-   複数の画面に同時にバーコードを渡すことが可能

---

## イベントとして受け取る場合のデメリット

-   画面毎に実装が必要
-   バーコード入力を無効化したい場合は明示的に無効化する必要がある

---

## キーボード入力として入力させる場合のメリット

-   外部キーボード入力のバーコードリーダーの実装を流用できる

---

## キーボード入力として入力させる場合のデメリット

-   制御が複雑
