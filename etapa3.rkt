#lang racket
(require "etapa2.rkt")
(require racket/match)
(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor heap-urilor de 
;; împerechere, pe care le vom folosi pentru:
;;  - a extrage cele mai bune filme dintr-o listă, conform
;;    cu un anumit criteriu
;;  - a extrage cele mai bune recenzii dintr-o colecție
;;    de recenzii pentru diverse filme (o recenzie bună
;;    corespunde unei note bune acordate filmului respectiv)
;;
;; Pentru completarea cu succes a etapei este necesar să
;; rezolvați sarcinile cu algoritmii dedicați PH-urilor 
;; (descriși în enunț). Punctajul acordat de checker va fi 
;; retras dacă sarcinile sunt rezolvate cu alți algoritmi.


; TODO 1 (40p)
; Definiți funcția best-k într-o formă care
; facilitează derivarea ulterioară a funcțiilor
; best-k-rating și best-k-duration.
; in: criteriu de comparație op (care compară 2 filme),
;     listă de filme movies, număr k
; out: lista sortată a celor mai "bune" filme
;      conform criteriului (cel mai "bun" primul)
; Algoritm:
;  1. construiește un PH de filme pe baza listei movies 
;     și a criteriului op
;  2. extrage în mod repetat root-ul acestui PH până
;     când rezultatul conține k filme (sau PH-ul devine vid)
; RESTRICȚII (20p):
;  - Folosiți named let pentru a efectua pasul 2 al
;    algoritmului.
(define (best-k op movies k)
  (define merge-func (merge-f op))
  (define heap
    (foldr (lambda (movie heap_curent)
             (ph-insert merge-func movie heap_curent))
           empty-ph
           movies))
  (let loop ((rezultat '())
             (heap_current heap)
             (remaining k))
    (if (= remaining 0)
        (reverse rezultat)
        (if (ph-empty? heap_current)
            (reverse rezultat)
            (loop (cons (ph-root heap_current) rezultat)
                  (ph-del-root merge-func heap_current)
                  (sub1 remaining))))))
; best-k-rating : [Movie] x Int -> [Movie]
; in: listă de filme movies, număr k
; out: cele mai bune k filme din movies (ca rating)
; RESTRICȚII (5p):
;  - Obțineți best-k-rating ca aplicație a lui best-k.
(define (best-k-rating movies k)
  (let ((cmp (lambda (m2 m1)
               (> (movie-rating m1) (movie-rating m2)))))
    (best-k cmp movies k)))

; best-k-duration : [Movie] x Int -> [Movie]
; in: listă de filme movies, număr k
; out: cele mai scurte k filme din movies 
; RESTRICȚII (5p):
;  - Obțineți best-k-duration ca aplicație a lui best-k.
(define best-k-duration
  (lambda (movies k)
    (best-k
     (lambda (m2 m1)
       (let ((d1 (movie-duration m1))
             (d2 (movie-duration m2)))
         (< (+ (* (car d1) 60) (cadr d1))
            (+ (* (car d2) 60) (cadr d2)))))
     movies
     k)))

; TODO 2 (30p)
; update-pairs : ((Symbol, PH) -> Bool) x [(Symbol, PH)]
;                -> [(Symbol, PH)]
; in: predicat p, listă de perechi (nume-film . PH)
;     (PH este un max-PH care conține notele acordate
;      filmului în diverse recenzii - așadar un PH
;      de numere)
; out: lista pairs actualizată astfel:
;      - pentru prima pereche care satisface predicatul 
;        p, PH-ului perechii i se șterge rădăcina
;      - dacă PH-ul perechii este vid sau dacă nicio pereche
;        nu satisface p, se întoarce lista pairs nemodificată
; RESTRICȚII (20p):
;  - Folosiți named let pentru a itera prin perechi.
(define (update-pairs p pairs)
  (let loop ((acc '()) (lst pairs))
    (if (null? lst)
        (reverse acc)
        (if (p (car lst))
            (let* ((film-ph (car lst))
                   (name (car film-ph))
                   (ph (cdr film-ph)))
              (if (ph-empty? ph)
                  (append (reverse acc) lst)
                  (let ((new_ph (or (ph-del-root merge-max ph) empty-ph)))
                    (if (ph-empty? new_ph)
                        (append (reverse acc) lst)
                        (append (reverse acc)
                                (cons (cons name new_ph) (cdr lst)))))))
            (loop (cons (car lst) acc) (cdr lst))))))


; TODO 3 (50p)
; best-k-ratings-overall : [(Symbol, PH)] x Int
;                          -> [(Symbol, Number)]
; in: listă de perechi (nume-film . PH)
;     (ca mai sus, PH este un max-PH de rating-uri)
;     număr k 
; out: lista sortată a celor mai bune k perechi
;      (nume-film . rating), corespunzând celor mai
;      bune rating-uri din toate PH-urile
; Algoritm:
;  1. Se inițializează un PH de perechi (nume . rating), 
;     corespunzând celui mai bun rating al fiecărui film
;     (adică se extrage rădăcina fiecărui PH de ratinguri,
;      în pereche cu numele filmului aferent)
;  2. Repetă de k ori:
;     - extrage rădăcina PH-ului de rădăcini
;       (name-root . rating-root)
;       (adică extrage cea mai bună pereche per total)
;     - adu în PH-ul de rădăcini următorul cel mai bun 
;       rating al filmului name-root (dacă există)
; RESTRICȚII (20p):
;  - Folosiți named let pentru a efectua pasul 2 al
;    algoritmului.
(define (best-k-ratings-overall pairs k)
  (define initial_global_PH
    (let loop ((ps pairs) (acc empty-ph))
      (if (null? ps)
          acc
          (let ((film (car (car ps)))
                (local (cdr (car ps))))
            (if (ph-empty? local)
                (loop (cdr ps) acc)
                (loop (cdr ps)
                      (ph-insert (merge-f (lambda (p1 p2)
                                             (< (cdr p1) (cdr p2))))
                                 (cons film (ph-root local))
                                 acc)))))))
  
  (let step ((rez '())
             (gph initial_global_PH)
             (n k)
             (allP pairs))
    (if (zero? n)
        (reverse rez)
        (if (ph-empty? gph)
            (reverse rez)
            (let* ((bestPair (ph-root gph))
                   (film_per (car bestPair))
                   (new-gph (ph-del-root (merge-f (lambda (p1 p2)
                                                     (< (cdr p1) (cdr p2))))
                                           gph))
                   (localPH-ofFilm (cdr (assoc film_per allP)))
                   (local_updatat (ph-del-root merge-max localPH-ofFilm))
                   (new_all_pairs (cons (cons film_per local_updatat)
                                      (filter (lambda (x)
                                                (not (eq? (car x) film_per)))
                                              allP)))
                   (new-gph2 (if (ph-empty? local_updatat)
                                 new-gph
                                 (ph-insert (merge-f (lambda (p1 p2)
                                                        (< (cdr p1) (cdr p2))))
                                            (cons film_per (ph-root local_updatat))
                                            new-gph))))
              (step (cons bestPair rez) new-gph2 (sub1 n) new_all_pairs))))))
  


