(in-package #:cl-simplebase64)
;;;This unit test framework is from PCL

(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defun run-test ()
  (time
   (combine-results
     (test-vector=)
     (test-bit-op)
     (test-encode)
     (test-decode))))

(deftest test-bit-op ()
  (test-make-bits)
  (test-bits2integer)
  (test-divide-bits))

(deftest test-vector=()
  (check
    (equal (vector= #(1 0 1) #(1 0 1)) t)
    (equal (vector= #(1) #(1 0 1)) nil)
    (equal (vector= #(1 1 1 1 1 1 1 1 1 1) #(1 1 1 1 1 1 1 1 1 1)) t)))

(deftest test-bits2integer ()
  (check
   (= (base64::bits2integer #(0)) 0)
   (= (base64::bits2integer #(1)) 1)
   (= (base64::bits2integer #(0 0 1)) 1)
   (= (base64::bits2integer #(1 0)) 2)
   (= (base64::bits2integer #(1 1)) 3)
   (= (base64::bits2integer #(1 1 0 0 1 0)) 50)
   (= (base64::bits2integer #(1 0 1 0)) 10)
   (= (base64::bits2integer #(1 1 1 1 1 1 1 1 1 1)) 1023)
   (= (base64::bits2integer #(1 0 0 0 0 0 0 0 0 0 0)) 1024)))


(deftest test-make-bits ()
  (check
    (vector= (base64::make-bits 0) #(0))
    (vector= (base64::make-bits 1) #(1))
    (vector= (base64::make-bits 2) #(1 0))
    (vector= (base64::make-bits 10) #(1 0 1 0))
    (vector= (base64::make-bits 15) #(1 1 1 1))
    (vector= (base64::make-bits 16) #(1 0 0 0 0))
    (vector= (base64::make-bits 200) #(1 1 0 0 1 0 0 0))
    (vector= (base64::make-bits 1024) #(1 0 0 0 0 0 0 0 0 0 0))
    (vector= (base64::make-bits 1023) #(1 1 1 1 1 1 1 1 1 1))))

(deftest test-divide-bits ()
  (check
    (vector-list= (base64::divide-bits #(0) 2) '(#(0)))
    (vector-list= (base64::divide-bits #(1 0 1 0 1 0 1 0) 2) '(#(1 0) #(1 0) #(1 0) #(1 0)))
    (vector-list= (base64::divide-bits #(1 1 1) 3) '(#(1 1 1)))
    (vector-list= (base64::divide-bits #() 3) '(#()))
    (vector-list= (base64::divide-bits #(1 0 1) 0) '())))

(deftest test-encode ()
  (check
   (string= (base64::encode "abcdefghijklmn") "YWJjZGVmZ2hpamtsbW4=")
   (string= (base64::encode "ABCDEFGHIJKLMNOPQRSTUVWXYZ") "QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVo=")
   (string= (base64::encode "1234567890") "MTIzNDU2Nzg5MA==")
   (string= (base64::encode "1 2 3 4 5") "MSAyIDMgNCA1")
   (string= (base64::encode "日本語変換テスト") "5pel5pys6Kqe5aSJ5o+b44OG44K544OI")
   (string= (base64::encode "this is a alphabet case.") "dGhpcyBpcyBhIGFscGhhYmV0IGNhc2Uu")
   (string= (base64::encode "w   i     t      h     m    a  n    y    s   p    a     c     e     s") "dyAgIGkgICAgIHQgICAgICBoICAgICBtICAgIGEgIG4gICAgeSAgICBzICAgcCAgICBhICAgICBjICAgICBlICAgICBz")
   (string= (base64::encode "aに1:ほl:ん2@p;ご3h.ま4,aじ(5)りb6%e#7t") "YeOBqzE644G7bDrjgpMyQHA744GUM2gu44G+NCxh44GYKDUp44KKYjYlZSM3dA==")
   (string= (base64::encode "私が先生と知り合いになったのは鎌倉（かまくら）である。その時私はまだ若々しい書生であった。暑中休暇を利用して海水浴に行った友達からぜひ来いという端書（はがき）を受け取ったので、私は多少の金を工面（くめん）して、出掛ける事にした。私は金の工面に二（に）、三日（さんち）を費やした。ところが私が鎌倉に着いて三日と経（た）たないうちに、私を呼び寄せた友達は、急に国元から帰れという電報を受け取った。電報には母が病気だからと断ってあったけれども友達はそれを信じなかった。友達はかねてから国元にいる親たちに勧（すす）まない結婚を強（し）いられていた。彼は現代の習慣からいうと結婚するにはあまり年が若過ぎた。それに肝心（かんじん）の当人が気に入らなかった。それで夏休みに当然帰るべきところを、わざと避けて東京の近くで遊んでいたのである。彼は電報を私に見せてどうしようと相談をした。私にはどうしていいか分らなかった。けれども実際彼の母が病気であるとすれば彼は固（もと）より帰るべきはずであった。それで彼はとうとう帰る事になった。せっかく来た私は一人取り残された。") "56eB44GM5YWI55Sf44Go55+l44KK5ZCI44GE44Gr44Gq44Gj44Gf44Gu44Gv6Y6M5YCJ77yI44GL44G+44GP44KJ77yJ44Gn44GC44KL44CC44Gd44Gu5pmC56eB44Gv44G+44Gg6Iul44CF44GX44GE5pu455Sf44Gn44GC44Gj44Gf44CC5pqR5Lit5LyR5pqH44KS5Yip55So44GX44Gm5rW35rC05rW044Gr6KGM44Gj44Gf5Y+L6YGU44GL44KJ44Gc44Gy5p2l44GE44Go44GE44GG56uv5pu477yI44Gv44GM44GN77yJ44KS5Y+X44GR5Y+W44Gj44Gf44Gu44Gn44CB56eB44Gv5aSa5bCR44Gu6YeR44KS5bel6Z2i77yI44GP44KB44KT77yJ44GX44Gm44CB5Ye65o6b44GR44KL5LqL44Gr44GX44Gf44CC56eB44Gv6YeR44Gu5bel6Z2i44Gr5LqM77yI44Gr77yJ44CB5LiJ5pel77yI44GV44KT44Gh77yJ44KS6LK744KE44GX44Gf44CC44Go44GT44KN44GM56eB44GM6Y6M5YCJ44Gr552A44GE44Gm5LiJ5pel44Go57WM77yI44Gf77yJ44Gf44Gq44GE44GG44Gh44Gr44CB56eB44KS5ZG844Gz5a+E44Gb44Gf5Y+L6YGU44Gv44CB5oCl44Gr5Zu95YWD44GL44KJ5biw44KM44Go44GE44GG6Zu75aCx44KS5Y+X44GR5Y+W44Gj44Gf44CC6Zu75aCx44Gr44Gv5q+N44GM55eF5rCX44Gg44GL44KJ44Go5pat44Gj44Gm44GC44Gj44Gf44GR44KM44Gp44KC5Y+L6YGU44Gv44Gd44KM44KS5L+h44GY44Gq44GL44Gj44Gf44CC5Y+L6YGU44Gv44GL44Gt44Gm44GL44KJ5Zu95YWD44Gr44GE44KL6Kaq44Gf44Gh44Gr5Yun77yI44GZ44GZ77yJ44G+44Gq44GE57WQ5ama44KS5by377yI44GX77yJ44GE44KJ44KM44Gm44GE44Gf44CC5b2844Gv54++5Luj44Gu57+S5oWj44GL44KJ44GE44GG44Go57WQ5ama44GZ44KL44Gr44Gv44GC44G+44KK5bm044GM6Iul6YGO44GO44Gf44CC44Gd44KM44Gr6IKd5b+D77yI44GL44KT44GY44KT77yJ44Gu5b2T5Lq644GM5rCX44Gr5YWl44KJ44Gq44GL44Gj44Gf44CC44Gd44KM44Gn5aSP5LyR44G/44Gr5b2T54S25biw44KL44G544GN44Go44GT44KN44KS44CB44KP44GW44Go6YG/44GR44Gm5p2x5Lqs44Gu6L+R44GP44Gn6YGK44KT44Gn44GE44Gf44Gu44Gn44GC44KL44CC5b2844Gv6Zu75aCx44KS56eB44Gr6KaL44Gb44Gm44Gp44GG44GX44KI44GG44Go55u46KuH44KS44GX44Gf44CC56eB44Gr44Gv44Gp44GG44GX44Gm44GE44GE44GL5YiG44KJ44Gq44GL44Gj44Gf44CC44GR44KM44Gp44KC5a6f6Zqb5b2844Gu5q+N44GM55eF5rCX44Gn44GC44KL44Go44GZ44KM44Gw5b2844Gv5Zu677yI44KC44Go77yJ44KI44KK5biw44KL44G544GN44Gv44Ga44Gn44GC44Gj44Gf44CC44Gd44KM44Gn5b2844Gv44Go44GG44Go44GG5biw44KL5LqL44Gr44Gq44Gj44Gf44CC44Gb44Gj44GL44GP5p2l44Gf56eB44Gv5LiA5Lq65Y+W44KK5q6L44GV44KM44Gf44CC")))

(deftest test-decode ()
  (check
   (string= (base64::decode "YWJjZGVmZ2hpamtsbW4=") "abcdefghijklmn")
   (string= (base64::decode "QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVo=") "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
   (string= (base64::decode "MTIzNDU2Nzg5MA==") "1234567890")
   (string= (base64::decode "MSAyIDMgNCA1") "1 2 3 4 5")
   (string= (base64::decode "5pel5pys6Kqe5aSJ5o+b44OG44K544OI") "日本語変換テスト")
   (string= (base64::decode "dGhpcyBpcyBhIGFscGhhYmV0IGNhc2Uu") "this is a alphabet case.")
   (string= (base64::decode "dyAgIGkgICAgIHQgICAgICBoICAgICBtICAgIGEgIG4gICAgeSAgICBzICAgcCAgICBhICAgICBjICAgICBlICAgICBz") "w   i     t      h     m    a  n    y    s   p    a     c     e     s")
   (string= (base64::decode "YeOBqzE644G7bDrjgpMyQHA744GUM2gu44G+NCxh44GYKDUp44KKYjYlZSM3dA==") "aに1:ほl:ん2@p;ご3h.ま4,aじ(5)りb6%e#7t")
   (string= (base64::decode "56eB44GM5YWI55Sf44Go55+l44KK5ZCI44GE44Gr44Gq44Gj44Gf44Gu44Gv6Y6M5YCJ77yI44GL44G+44GP44KJ77yJ44Gn44GC44KL44CC44Gd44Gu5pmC56eB44Gv44G+44Gg6Iul44CF44GX44GE5pu455Sf44Gn44GC44Gj44Gf44CC5pqR5Lit5LyR5pqH44KS5Yip55So44GX44Gm5rW35rC05rW044Gr6KGM44Gj44Gf5Y+L6YGU44GL44KJ44Gc44Gy5p2l44GE44Go44GE44GG56uv5pu477yI44Gv44GM44GN77yJ44KS5Y+X44GR5Y+W44Gj44Gf44Gu44Gn44CB56eB44Gv5aSa5bCR44Gu6YeR44KS5bel6Z2i77yI44GP44KB44KT77yJ44GX44Gm44CB5Ye65o6b44GR44KL5LqL44Gr44GX44Gf44CC56eB44Gv6YeR44Gu5bel6Z2i44Gr5LqM77yI44Gr77yJ44CB5LiJ5pel77yI44GV44KT44Gh77yJ44KS6LK744KE44GX44Gf44CC44Go44GT44KN44GM56eB44GM6Y6M5YCJ44Gr552A44GE44Gm5LiJ5pel44Go57WM77yI44Gf77yJ44Gf44Gq44GE44GG44Gh44Gr44CB56eB44KS5ZG844Gz5a+E44Gb44Gf5Y+L6YGU44Gv44CB5oCl44Gr5Zu95YWD44GL44KJ5biw44KM44Go44GE44GG6Zu75aCx44KS5Y+X44GR5Y+W44Gj44Gf44CC6Zu75aCx44Gr44Gv5q+N44GM55eF5rCX44Gg44GL44KJ44Go5pat44Gj44Gm44GC44Gj44Gf44GR44KM44Gp44KC5Y+L6YGU44Gv44Gd44KM44KS5L+h44GY44Gq44GL44Gj44Gf44CC5Y+L6YGU44Gv44GL44Gt44Gm44GL44KJ5Zu95YWD44Gr44GE44KL6Kaq44Gf44Gh44Gr5Yun77yI44GZ44GZ77yJ44G+44Gq44GE57WQ5ama44KS5by377yI44GX77yJ44GE44KJ44KM44Gm44GE44Gf44CC5b2844Gv54++5Luj44Gu57+S5oWj44GL44KJ44GE44GG44Go57WQ5ama44GZ44KL44Gr44Gv44GC44G+44KK5bm044GM6Iul6YGO44GO44Gf44CC44Gd44KM44Gr6IKd5b+D77yI44GL44KT44GY44KT77yJ44Gu5b2T5Lq644GM5rCX44Gr5YWl44KJ44Gq44GL44Gj44Gf44CC44Gd44KM44Gn5aSP5LyR44G/44Gr5b2T54S25biw44KL44G544GN44Go44GT44KN44KS44CB44KP44GW44Go6YG/44GR44Gm5p2x5Lqs44Gu6L+R44GP44Gn6YGK44KT44Gn44GE44Gf44Gu44Gn44GC44KL44CC5b2844Gv6Zu75aCx44KS56eB44Gr6KaL44Gb44Gm44Gp44GG44GX44KI44GG44Go55u46KuH44KS44GX44Gf44CC56eB44Gr44Gv44Gp44GG44GX44Gm44GE44GE44GL5YiG44KJ44Gq44GL44Gj44Gf44CC44GR44KM44Gp44KC5a6f6Zqb5b2844Gu5q+N44GM55eF5rCX44Gn44GC44KL44Go44GZ44KM44Gw5b2844Gv5Zu677yI44KC44Go77yJ44KI44KK5biw44KL44G544GN44Gv44Ga44Gn44GC44Gj44Gf44CC44Gd44KM44Gn5b2844Gv44Go44GG44Go44GG5biw44KL5LqL44Gr44Gq44Gj44Gf44CC44Gb44Gj44GL44GP5p2l44Gf56eB44Gv5LiA5Lq65Y+W44KK5q6L44GV44KM44Gf44CC") "私が先生と知り合いになったのは鎌倉（かまくら）である。その時私はまだ若々しい書生であった。暑中休暇を利用して海水浴に行った友達からぜひ来いという端書（はがき）を受け取ったので、私は多少の金を工面（くめん）して、出掛ける事にした。私は金の工面に二（に）、三日（さんち）を費やした。ところが私が鎌倉に着いて三日と経（た）たないうちに、私を呼び寄せた友達は、急に国元から帰れという電報を受け取った。電報には母が病気だからと断ってあったけれども友達はそれを信じなかった。友達はかねてから国元にいる親たちに勧（すす）まない結婚を強（し）いられていた。彼は現代の習慣からいうと結婚するにはあまり年が若過ぎた。それに肝心（かんじん）の当人が気に入らなかった。それで夏休みに当然帰るべきところを、わざと避けて東京の近くで遊んでいたのである。彼は電報を私に見せてどうしようと相談をした。私にはどうしていいか分らなかった。けれども実際彼の母が病気であるとすれば彼は固（もと）より帰るべきはずであった。それで彼はとうとう帰る事になった。せっかく来た私は一人取り残された。")))

