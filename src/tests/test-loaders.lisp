(in-package :abstock)

(defparameter *shelves*
  '((:|name| "0-1 ans livres tissus" :|id| 5)
    (:|name| "0-3 ans / Livres animés" :|id| 14)
    (:|name| "0-3 ans / Livres jeux" :|id| 25)
    (:|name| "Classiques illustrés" :|id| 27)
    (:|name| "0-3 ans / Livres sonores" :|id| 26)
    (:|name| "0-3 ans / Livres tissus" :|id| 2)
    (:|name| "Documentaires" :|id| 21)
    (:|name| "0-3 ans / Tout carton" :|id| 15) (:|name| "13-17 Poche" :|id| 10)
    (:|name| "3-6 ans / Albums" :|id| 3)
    (:|name| "3-6 ans / Émotions" :|id| 17)
    (:|name| "3-6 ans / Poches" :|id| 1)
    (:|name| "6-9 ans / Albums" :|id| 18) (:|name| "6-9 ans / BDs" :|id| 30)
    (:|name| "9-12 Poche" :|id| 9) (:|name| "Activités" :|id| 23)
    (:|name| "Albums" :|id| 19) (:|name| "Art" :|id| 6)
    (:|name| "0-3 ans / Albums" :|id| 13) (:|name| "BDs" :|id| 24)
    (:|name| "Contes" :|id| 20) (:|name| "Famille" :|id| 32)
    (:|name| "Jeux" :|id| 36) (:|name| "La mort" :|id| 37)
    (:|name| "Langues" :|id| 7) (:|name| "3-6 ans / Documentaires" :|id| 16)
    (:|name| "Le nouveau bébé" :|id| 35) (:|name| "Livres disques" :|id| 38)
    (:|name| "Livres originaux" :|id| 34)
    (:|name| "Papeterie" :|id| 39) (:|name| "Parentalité " :|id| 8)
    (:|name| "Personnages célèbres" :|id| 29)
    (:|name| "Romans illustrés" :|id| 28)
    (:|name| "Philosophie" :|id| 33) (:|name| "Poésie" :|id| 12)
    (:|name| "Pop Up" :|id| 11) (:|name| "Revues" :|id| 31)
    (:|name| "Romans grand format" :|id| 22)
    (:|name| "6 - 9 Poches" :|id| 4)))

#+nil
(abstock::sort-shelves *shelves*)
#+nil
(abstock::sort-shelves-by-number-prefix *shelves*)

(parachute:define-test load-shelves
  (parachute:true (abstock::sort-shelves-by-number-prefix *shelves*)))
