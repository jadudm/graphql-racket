#lang racket
(require db sql)

(provide conn)

(when (file-exists? "/tmp/dbase.sqlite")
  (delete-file "/tmp/dbase.sqlite"))

(define conn (sqlite3-connect #:database
                              "/tmp/dbase.sqlite"
                              #:mode 'create
                              ))

(query-exec
 conn
 (create-table starships
               #:columns
               [id integer]
               [name text]
               #:constraints
               [primary-key id]))
(query-exec
 conn
 (create-table humans
               #:columns
               [id integer]
               [name text]
               #:constraints
               [primary-key id]
               ))

(query-exec
 conn
 (create-table episodes
               #:columns
               [id integer]
               [enum text]
               [episode text]))

(define pe_ctr 0)
(query-exec
 conn
 (create-table people_episode
               #:columns
               ;; [id integer]
               [human_id integer]
               [episode integer]))

(define ps_ctr 0)
(query-exec
 conn
 (create-table people_starship
               #:columns
               ;; [id integer]
               [human_id integer]
               [starship integer]))

(define ships
  `((0 "Death Star")
    (1 "X-Wing")
    (2 "Milennium Falcon")
    (3 "Speeder Bike")))

(define esodes
  '((0 "NEWHOPE" "A New Hope" )
    (1 "EMPIRE" "Empire Strikes Back")
    (2 "JEDI" "Return of the Jedi" )))

(for ([e esodes])
  (query-exec
   conn
   (insert #:into episodes
           #:set
           [id ,(first e)]
           [enum ,(second e)]
           [episode ,(third e)])))

(for ([ship ships])
  (query-exec
   conn
   (insert #:into starships #:set
           [id ,(first ship)]
           [name ,(second ship)])))

(define peeps
  `((1 "Luke Skywalker" (0 1 2) (0 1))
    (2 "Ewok" (2) (3))))

(for ([peep peeps])
  (define eps (third peep))
  (define ships (fourth peep))
  
  (for ([s ships])
    (query-exec
     conn
     (insert #:into people_starship
             #:set
             [human_id ,(first peep)]
             [starship ,s]))
    (set! ps_ctr (add1 ps_ctr)))
  
  (for ([e eps])
    (query-exec
     conn
     (insert #:into people_episode
             #:set
             [human_id ,(first peep)]
             [episode ,e]))
    (set! pe_ctr (add1 pe_ctr)))
  
  (query-exec
   conn
   (insert #:into humans
           #:set
           [id ,(first peep)]
           [name ,(second peep)]))
  )
           