(ns eqn.calc)

(def opinfo {
              "*" { :prec 3 :inv "/" :op *}
              "/" { :prec 3 :inv "*" :op /}
              "+" { :prec 2 :inv "-" :op +}
              "-" { :prec 2 :inv "+" :op -}
              "=" { :prec 1}})

(defn left
  "gets the left child of tree"
  [tree]
  (if (list? tree)
    (nth tree 1)
    nil))

(defn right
  "gets the right child of tree"
  [tree]
  (if (list? tree)
    (nth tree 2)
    nil))

(defn evaltree
  "evaluates an expression given its ast"
  [tree]
  (if (list? tree)
    ((get-in opinfo [(first tree) :op]) (evaltree (left tree)) (evaltree (right tree)))
    tree))

(defn hasvar?
  "checks if the (sub)tree has the variable"
  [tree]
  (if (list? tree)
    (or
      (hasvar? (left tree))
      (hasvar? (right tree))
      false)
    (= tree :var)))

(defn solvetree
  "solve an equation in one variable given its ast"
  ([eqn] ; initial run- make sure the var is on the left side of the equation
   (if (hasvar? (right eqn))
    (solvetree (list (first eqn) (right eqn) (left eqn)) 0)
    (solvetree eqn 0)))
  ([eqn _]
   (if (= (left eqn) :var)
    (evaltree (right eqn))
    (if (hasvar? (left (left eqn)))
      (recur (list "=" (left (left eqn)) (list (get-in opinfo [(first (left eqn)) :inv]) (right eqn) (right (left eqn)) ) ) 0)
      (case (first (left eqn))
        "+" (recur (list "=" (right (left eqn)) (list "-" (right eqn) (left (left eqn)))) 0)
        "-" (recur (list "=" (right (left eqn)) (list "-" (left (left eqn)) (right eqn))) 0)
        "*" (recur (list "=" (right (left eqn)) (list "/" (right eqn) (left (left eqn)))) 0)
        "/" (recur (list "=" (right (left eqn)) (list "/" (left (left eqn)) (right eqn))) 0))))))

(defn shunting-yard
  "use shunting-yard algorithm to convert infix to ast"
  ([toks] (shunting-yard toks '() '()))
  ([toks ops res]
   (let [[type val] (first toks)]
    (case type
      :num (recur (next toks) ops (conj res val))
      :var (recur (next toks) ops (conj res :var))
      :lparen (recur (next toks) (conj ops val) res)
      :rparen (if (= (first ops) "(")
                (recur (next toks) (pop ops) res)
                (recur toks (pop ops) (conj (nnext res) (list (peek ops) (second res) (first res)))))
      :binop (if (>= (get-in opinfo [(first ops) :prec] 0) (get-in opinfo [val :prec]))
              (recur toks (pop ops) (conj (nnext res) (list (peek ops) (second res) (first res))))
              (recur (next toks) (conj ops val) res))
      :end (if (empty? ops)
            (first res)
            (recur toks (pop ops) (conj (nnext res) (list (peek ops) (second res) (first res)))))))))



(defn characterize
  "characterize a specific token (number,variable,paren,binop)"
  [tok]
  (let [num (nth tok 1)
        var (nth tok 2)
        bop (nth tok 3)]
    (cond
      (some? num) [:num (js/parseInt num)]
      (some? var) [:var var]
      (= bop "(") [:lparen bop]
      (= bop ")") [:rparen bop]
      :else [:binop bop])))

(defn tokenize
  "split string into tokens"
  [x]
  (concat (map characterize (re-seq #"(?:(\d+)|(\w+)|(.))" x)) (list [:end ""])))

(defn solve
  "solve equation string"
  [eqn]
  (solvetree (shunting-yard (tokenize eqn))))
