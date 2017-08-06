(ns demo-rpg.core)

(def max-lvl 10)

;;lvl(integer) -> att(integer)
;;given lvl calculates attack by formula
;;att = level*6
(defn calc-attack
	"given lvl calculates attack"
	[lvl]
	(* lvl 6))

;;lvl(integer) -> def(integer)
;;given lvl calculates defence by formula
;;def = level*1.4
(defn calc-defence
	[lvl]
	(int (* lvl 1.4)))

;;lvl(integer) -> hp(integer)
;;def = level*20
(defn calc-hitpoints
	[lvl]
	(* lvl 20))

(defn calc-sides
	[lvl]
	(if (> lvl 5) 4 6))

(defn kill-negative
	[n]
	(if (neg? n) 0 n))

(defn calc-base-damage
	[att def]
	(kill-negative (- att def)))

;;given sides count roll the dice
;;sides(int) -> roll dice(int)
(defn roll-dice
	[sides]
	(inc (rand-int sides)))

(defn create-character
	[name lvl]
	{:name name
	 :lvl lvl
	 :att (calc-attack lvl)
	 :def (calc-defence lvl)
	 :hp (calc-hitpoints lvl)})

(defn real-damage
	[base sides]
	(let [rd (roll-dice sides)
		  s (/ sides 2)]
		(cond
		 (= rd sides) (* base 2)
		 (<= rd s) (int (/ base 2))
		 (> rd s) base)))

;;from(character) + to(character)
;;[damage(integer), character]
(defn take-damage
	[from to]
	(let [bd (calc-base-damage (:att from) (:def to))
		  s (calc-sides (:lvl from))
		  rd (real-damage bd s)]
	    [rd (update-in to [:hp] #(- % rd))]))

(def log-template 
	"Character %s received %d damage.
His new life is %d")

(defn print-battle-log
	[damage character]
	(let [name (:name character)
		  newhp (:hp character)
		  s (format log-template name damage newhp)]
	    (println s)))

(defn print-winner
	[p-hp e-hp]
	(if (<= p-hp 0)
		(println "Enemy won.")
		(println "Player won.")))

(def config
	{:player player
	 :enemy troll})

(defn game-logic
	[config]
	(loop [player (:player config)
		   enemy (:enemy config)
		   round 1]
	    (if (or (<= (:hp player) 0)
	    		(<= (:hp enemy) 0))
	    (print-winner (:hp player) (:hp enemy))
	    (let [pl2en (take-damage player enemy)
	    	  en2pl (take-damage enemy player)]
	    	(do 
	    		(println (str "Round " round ":"))
	    		(print-battle-log (pl2en 0) (pl2en 1))
	    		(print-battle-log (en2pl 0) (en2pl 1))
	    		(recur (en2pl 1) (pl2en 1) (inc round)))))))

(def player (create-character "Robert" 6))
(def troll (create-character "Troll" 3))
(def big-troll (create-character "Big Troll" 5))
	