#lang racket
(require racket/match)
(require "etapa2.rkt")
(provide (all-defined-out))

;; Această etapă continuă seria aplicațiilor heap-urilor  
;; de împerechere, pe care le vom folosi pentru a calcula
;; în mod dinamic mediana recenziilor unui film, simulând
;; condițiile din realitate - în care apar în permanență
;; noi recenzii pentru diverse filme.
;;    
;; Pentru a modela această dinamică folosim un flux de
;; perechi (nume-film . rating), pe baza căruia calculăm
;; un flux de stadii evolutive astfel:
;;  - fiecare stadiu este reprezentat ca listă de perechi
;;    * o pereche pentru fiecare film cu minim o recenzie
;;    * fiecare pereche este de forma
;;      (nume-film . mediană-rating-uri-primite-până-acum)
;;  - fiecare nouă recenzie determină actualizarea unei
;;    mediane, adică trecerea într-un alt stadiu,
;;    generând un nou element în fluxul rezultat
;;
;; Algoritmul folosit este următorul:
;;  Fluxul de perechi este transformat într-un flux de
;;  liste de cvartete (nume-film delta max-ph min-ph)
;;   - fiecare element din flux conține câte un cvartet 
;;     pentru fiecare film care are minim o recenzie
;;   - dacă filmul are un număr par de recenzii:
;;     - max-ph și min-ph au aceeași dimensiune
;;     - delta = size(max-ph) - size(min-ph) = 0
;;     - max-ph = max-PH cu cele mai mici rating-uri
;;     - min-ph = min-PH cu cele mai mari rating-uri
;;     - mediana este media rădăcinilor celor 2 PH-uri
;;   - dacă filmul are un număr impar de recenzii:
;;     - max-ph are un element în plus față de min-ph
;;     - delta = size(max-ph) - size(min-ph) = 1
;;     - max-ph = max-PH cu cele mai mici rating-uri
;;     - min-ph = min-PH cu cele mai mari rating-uri
;;     - mediana este rădăcina lui max-ph
;;
;; Pentru completarea cu succes a etapei este necesar să
;; calculați medianele cu algoritmul descris în enunț.
;; În caz contrar, punctajul acordat de checker va fi retras.


; TODO 1 (45p)
; add-rating : (Symbol, Int, PH, PH) x Number
;              -> (Symbol, Int, PH, PH)
; in: cvartet (nume delta max-ph min-ph),
;     rating de adăugat
; out: cvartet actualizat prin adăugarea 
;      rating-ului, astfel:
;  - dacă rating <= root(max-ph)
;    inserează rating în max-ph, actualizând delta
;  - altfel
;    inserează rating în min-ph, actualizând delta
;  - dacă delta > 1
;    mută root(max-ph) în min-ph
;  - dacă delta < 0
;    mută root(min-ph) în max-ph
(define (add-rating q r)
  (let* ((n (first q))
         (d (second q))
         (max (third q))
         (min (fourth q))
         (to-max? (or (ph-empty? max) (<= r (ph-root max))))
         (new-max (if to-max? (ph-insert merge-max r max) max))
         (new-min (if to-max? min (ph-insert merge-min r min)))
         (new-d (if to-max? (+ d 1) (- d 1))))
    (if (> new-d 1)
        (let* ((rt (ph-root new-max))
               (up-max (ph-del-root merge-max new-max))
               (up-min (ph-insert merge-min rt new-min)))
          (list n 0 up-max up-min))
        (if (< new-d 0)
            (let* ((rt (ph-root new-min))
                   (up-min (ph-del-root merge-min new-min))
                   (up-max (ph-insert merge-max rt new-max)))
              (list n 1 up-max up-min))
            (list n new-d new-max new-min)))))



; TODO 2 (45p)
; reviews->quads : Stream<(Symbol, Number)> ->
;                  Stream<[(Symbol, Int, PH, PH)]>
; in: stream de perechi (nume . rating)
; out: stream de liste de cvartete
;      (nume delta max-ph min-ph)
;  - elementul k din rezultat corespunde primelor
;    k recenzii din input (ex: dacă primele 10
;    recenzii sunt pentru 3 filme distincte, al
;    10-lea element din fluxul rezultat conține o
;    listă de 3 cvartete - unul pentru fiecare film)
; RESTRICȚII (20p):
;  - Lucrați cu operatorii pe fluxuri, fără a
;    converti liste în fluxuri sau fluxuri în liste.
(define (reviews->quads reviews)
  (define (asociere-get film asociere)
    (if (null? asociere)
        #f
        (if (eq? (caar asociere) film)
            (cdar asociere)
            (asociere-get film (cdr asociere)))))
  (define (asociere-set film quad asociere)
    (if (null? asociere)
        (list (cons film quad))
        (if (eq? (caar asociere) film)
            (cons (cons film quad) (cdr asociere))
            (cons (car asociere) (asociere-set film quad (cdr asociere))))))
  (define (loop asociere revs)
    (if (not (stream-empty? revs))
        (let* ((review (stream-first revs))
               (film (car review))
               (old-q (asociere-get film asociere))
               (rating (cdr review))
               (new-q (if old-q
                          (add-rating old-q rating)
                          (cons film (cons 1 (cons (val->ph rating)
                                                   (cons empty-ph null))))))
               (new-asociere (asociere-set film new-q asociere)))
          (stream-cons (map cdr new-asociere)
                       (loop new-asociere (stream-rest revs))))
        empty-stream))
  (loop null reviews))

; TODO 3 (30p)
; quads->medians : Stream<[(Symbol, Int, PH, PH)]> ->
;                  Stream<[(Symbol, Number)]>  
; in: stream de liste de cvartete (ca mai sus)
; out: stream de liste de perechi (nume-film . mediană)
;  - mediana se calculează pe baza PH-urilor din
;    fiecare cvartet, conform algoritmului de mai sus
; RESTRICȚII (20p):
;  - Nu folosiți recursivitate explicită. Folosiți cel
;    puțin o funcțională pe fluxuri.
(define (quads->medians quads)
  (if (not (stream-empty? quads))
      (stream-map (lambda (quadsList)
                    (map (lambda (quad)
                           (if (= (second quad) 0)
                               (cons (first quad)
                                     (/ (+ (ph-root (third quad))
                                           (ph-root (fourth quad)))
                                        2))
                               (cons (first quad) (ph-root (third quad)))))
                         quadsList))
                  quads)
      empty-stream))
