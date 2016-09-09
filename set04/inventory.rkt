;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname inventory) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide make-reorder
         make-book
         make-line-item
         inventory-potential-profit
         inventory-total-volume
         price-for-line-item
         fillable-now?
         days-til-fillable
         inventory-after-deliveries
         price-for-order
         inventory-after-order
         increase-prices
         make-empty-reorder
         reorder-present?)
;--------------------------------------------------------------------------

;; DATA DEFINITIONS

;; A MaybeInteger is one of
;; -- false, describes that it doesn't have any value defined
;; -- Integer, describes that it has some value
;; TEMPLATE:
;; mi-fn : MaybeInteger -> ??
;;   (define (mi-fn mi)
;;     (cond
;;       [(fals? mi) ...]
;;       [(Integer? mi) ...]))


(define-struct reorder-status (days copies present?))
;; A ReorderStatus is a (make-reorder-status PosInt PosInt Boolean)
;; --days, describes the number of days untill the next shipment
;; --copies, describes number of copies that will arrive at the next shipment
;; --present?, is true iff the given reorder shows a pending request
;; TEMPLATE:
;; reorder-fn : ReorderStatus -> ??
;;  (define (reorder-fn r)
;;    (... (reorder-status-days r)
;;         (reorder-status-copies r)
;;         (reorder-status-present? r)))

(define-struct book (isbn title author publisher unit-price unit-cost  
                          copies reorder-status cuft))
;; A Book is a (make-book Integer String String String NonNegInt NonNegInt 
;;                                NonNegInt ReorderStatus Real)
;; --isbn, an integer (the "international standard book number") 
;;         This serves as a unique identifier for this book
;; --title, a string, describes the title of the book
;; --author, a string, describes the name of the author of the book
;; --publisher, a string, describes the name of the publisher of the book
;; --unit price, a non-negative integer, the price at which we will sell the
;;               book (in USD*100, ie $14.99 is represented as 1499)
;; --unit cost, a non-negative number, the cost of the book to the bookstore
;;              in USD*100
;; --copies, describes the number of copies on hand
;; --reorder-status, describes the outstanding reorder status of this book.
;;                   There can be at most one outstanding reorder. A book with 
;;                   no reorder has its present? set to false
;; --cuft, the volume taken up by one unit of this item, in cubic feet
;; TEMPLATE:
;; book-fn : Book -> ??
;;  (define (book-fn b)
;;    (... (book-isbn b) (book-title b) (book-author b) (book-publisher b)
;;         (book-unit-price b) (book-unit-cost b) (book-copies b)
;;         (book-reorder-status b) (book-cuft)))

;;An Inventory (list of books - lob) is a list of books in the inventory
;; Inventory is one of
;; --empty
;; --(cons Book Inventory)
;; WHERE: no two Books have the same isbn
;; TEMPLATE:
;; lob-fn : Inventory -> ??
;;  (define (lob-fn lst)
;;    (cond
;;      [(empty? lst) ...]
;;      [else (... (first lst)
;;                 (lob-fn (rest lst)))]))

(define-struct line-item (isbn copies))
;; A LineItem is a (make-line-item Integer NonNegInt)
;; --isbn, is the unique identifier the book being ordered for
;; --copies, describes the number of copies of the book being ordered
;; TEMPLATE:
;; line-item-fn : LineItem -> ??
;;  (define (line-item-fn li)
;;    (... (line-item-isbn li)
;;         (line-item-copies li))

;;An Order is a list of line items that are being ordered from any inventory
;; Order is one of:
;; --empty
;; --(cons LineItem Order)
;; WHERE: no two LineItems have the same isbn
;; TEMPLATE:
;; order-fn : Order -> ??
;;  (define (order-fn o)
;;    (cond
;;      [(empty? order) ...]
;;      [else (... (first order)
;;                 (order-fn (rest order)))]))

;--------------------------------------------------------------------------

;; CONSTANTS
(define ZERO 0)
(define RESET-VALUE 1)
(define HUNDRED 100)

(define book1 
  (make-book 44876 "Network Security" "Huand Scott" "SpringerLink" 4200 5800 23
             (make-reorder-status 9 12 true) 1))

(define book2 
  (make-book 47758 "Semistructured Database Design" "Ling" "MIT Press" 9800 8700
             34 (make-reorder-status 4 5 false) 2))

(define book3 
  (make-book 12348 "The Nature of Code" "Daniel Shiffman" "Illustrator" 8700 
             6400 40 (make-reorder-status 4 50 false) 3/13))

(define book4 
  (make-book 12834 "A.I:Artificial Intelligence" "Stanley Kubrick" "DreamWorks"
             3400 4300 7 (make-reorder-status 7 10 true) 2/5))

(define book5 
  (make-book 68576 "Coming of Age in Second Life" "Tom Boellstorff" 
             "Princeton Univesity Press" 4300 4800 1 (make-reorder-status 
                                                      4 10 true) 1/2))

(define book6 
  (make-book 94539 "Statistics" "Robert S. Witte" "John Wiley & Sons" 9800 10050
             5 (make-reorder-status 9 9 false) 0.23))

(define book7 
  (make-book 27564 "Calculus and its Applications"  "Goldstein" 
             "Pearson Education" 2300 2300 10 (make-reorder-status 12 3 false)
             3))

(define book8 
  (make-book 23783 "Solid State Physics" "Neil W. Ashcroft" "Pacific Grove" 9000
             8900 10 (make-reorder-status 3 4 false) 1))

(define book9 
  (make-book 89437 "Medical Chemistry" "William O. Foye" 
             "MIT Press" 3450 3000 6 (make-reorder-status 5 5 true) 0.23))

(define book10 
  (make-book 47437 "Visualizing data" "Ben Fry" "O'Reilly Media" 3300 3550 25
             (make-reorder-status 5 10 true) 1.5))

(define book11
  (make-book 23899 "Programming Design Paradigms" "Mitchel Wand" "NU Press" 
             4500 3800 5 (make-reorder-status 1 50 true) 2))

(define book1-1 
  (make-book 44876 "Network Security" "Huand Scott" "SpringerLink" 4200 5800 3
             (make-reorder-status 9 12 true) 1))

(define book2-1 
  (make-book 47758 "Semistructured Database Design" "Ling" "MIT Press" 
             10780 8700 34 (make-reorder-status 4 5 false) 2))

(define book4-1 
  (make-book 12834 "A.I:Artificial Intelligence" "Stanley Kubrick" "DreamWorks"
             3400 4300 0 (make-reorder-status 7 10 true) 2/5))

(define book5-1 
  (make-book 68576 "Coming of Age in Second Life" "Tom Boellstorff" 
             "Princeton Univesity Press" 4300 4800 1 (make-reorder-status 
                                                      3 10 true) 1/2))

(define book9-1 
  (make-book 89437 "Medical Chemistry" "William O. Foye" 
             "MIT Press" 3795 3000 6 (make-reorder-status 5 5 true) 0.23))

(define book11-1
  (make-book 23899 "Programming Design Paradigms" "Mitchel Wand" "NU Press" 
             4500 3800 55 (make-reorder-status 1 1 false) 2))

(define lob1 (list book1 book2 book3 book4 book5 
                   book6 book7 book8 book9 book10))

(define lob2 (list book1 book2 book3 book4 book5))

(define lob3 (list book2 book9 book8))

(define lob4 (list book2-1 book9-1 book8))

(define lob5 (list book3 book5 book11))

(define lob6 (list book3 book5-1 book11-1))

(define li1 (make-line-item 44876 20))

(define li2 (make-line-item 12348 50))

(define li3 (make-line-item 12834 7))

(define li4 (make-line-item 68576 4))

(define li5 (make-line-item 47437 50))

(define li6 (make-line-item 00000 10))

(define order1 (list li1 li3))

(define order2 (list li1 li2 li3))

(define order3 (list li1 li3 li4))

(define order4 (list li1 li3 li5))

;--------------------------------------------------------------------------

;;inventory-potential-profit : Inventory ->  Integer
;;GIVEN: an inventory
;;RETURNS: the total profit, in USD*100, for all the items in stock 
;;         (i.e., how much the bookstore would profit if it sold every book
;;         in the inventory)
;;EXAMPLES:
;;  (inventory-potential-profit lob1) => 82000
;;  (inventory-potential-profit lob2) => 85800
;;STRATEGY: HOFC

(define (inventory-potential-profit lob)
  (foldr
   (; Book Integer -> Integer
    ;GIVEN: a book and the price of the remaining books of the inventory
    ;RETURNS: the total price of the inventory so far
    lambda (b price)
     (+ (profit-of-book b) price))
   ZERO
   lob))

;;profit-of-book : Book -> Integer
;;GIVEN: a Book
;;RETURNS: the profit, in USD*100, of this book
;;EXAMPLES:
;;  (book-profit book1) => -36800
;;  (book-profit book2) => 37400
;;STRATEGY: Structural Decomposition b : Book

(define (profit-of-book b)
  (* (book-copies b) (- (book-unit-price b) (book-unit-cost b))))

;;TESTS:
(begin-for-test
  (check-equal? (inventory-potential-profit lob2) 85800
                "the potential profit function is not behaving properly")
  (check-equal? (profit-of-book book1) -36800
                "a book on loss should show negative profit"))

;--------------------------------------------------------------------------

;;inventory-total-volume : Inventory -> Real
;;GIVEN: an inventory
;;RETURNS: the total volume needed to store all the books in stock
;;EXAMPLES:                                ______
;;  (inventory-total-volume lob1) => 183.56076923
;;                                        ______
;;  (inventory-total-volume lob2) => 103.5307692
;;STRATEGY: HOFC

(define (inventory-total-volume lob)
  (foldr
   (; Book Integer -> Integer
    ;GIVEN: the volume of the current book
    ;RETURNS: the total volume of the rest of the books
    lambda (b volume)
     (+ (book-volume b) volume))
   ZERO
   lob))

;;book-volume : Book -> Real
;;GIVEN: a book
;;RETURNS: the volume needed to store all the copies in stock
;;EXAMPLES:
;;  refer tests
;;STRATEGY: Structural Decomposition on b : Book
(define (book-volume b)
  (* (book-copies b) (book-cuft b)))

;;TESTS:
(begin-for-test
  (check-equal? (round (inventory-total-volume lob1)) 184
                "inventory total volume is not working properly"))

;--------------------------------------------------------------------------

;;price-for-line-item : Inventory LineItem -> MaybeInteger
;;GIVEN: an inventory and a line item
;;RETURNS: the price for that line item (the quantity times the unit price
;;         for that item).  Returns false if that isbn does not exist in
;;         the inventory.
;;EXAMPLES:
;; (price-for-line-item lob1 li3) => 23800
;; (price-for-line-item lob1 li6) => false
;;STRATEGY: Structural Decomposition on li : LineItem

(define (price-for-line-item lob li)
  (price-for-line-item-isbn lob (line-item-isbn li) (line-item-copies li)))

;;price-for-line-item-isbn : Inventory Integer NonNegInt -> MaybeInteger
;;GIVEN: an inventory and the isbn and no of copies of a line item
;;RETURNS: the price for that line item (the quantity times the unit price
;;         for that item).  Returns false if that isbn does not exist in
;;         the inventory.
;;EXAMPLES:
;; refer tests
;;STRATEGY: Structural Decomposition on b : Book
(define (price-for-line-item-isbn lob isbn copies)
  (foldr
   (; Book Boolean -> Integer
    ;GIVEN: a book and a random value
    ;RETURNS: an integer value for the price of a line item
    lambda (b any)
     (line-item-price (book-unit-price b) copies))
   false
   (line-item-in-inventory lob isbn)))

;;line-item-in-inventory : Inventory Integer -> Inventory
;;GIVEN: an inventory and an integer for the isbn of the line item
;;RETURNS: a subset of the inventory with the matching isbn's
;;EXAMPLES:
;;   (line-item-in-inventory lob1 23899) => (list book11)
;;   (line-item-in-inventory lob1 00000) => empty
;;STRATEGY: Structural Decomposition on b : Book
(define (line-item-in-inventory lob isbn)
  (filter
   (; Book -> Boolean
    ;GIVEN: a book from the inventory
    ;RETURNS: true iff the given isbn in line-item-in-inventory matches the isbn
    ;         of the current book
    lambda (b)
     (= (book-isbn b) isbn))
   lob))

;;line-item-price : NonNegInt LineItem -> Integer
;;GIVEN: a line item and the unit price of that item
;;RETURNS: the total price for that line item
;;EXAMPLE:
;; (line-item-price 8700 li2) => 435000
;;STRATEGY: Function Composition

(define (line-item-price unit-price copies)
  (* unit-price copies))

;;TEST:
(begin-for-test
  (check-equal? (price-for-line-item lob1 li3) 23800
                "the line item price should be (no of copies * unit price)")
  (check-equal? (price-for-line-item lob1 li6) false
                "if a line item is not found, it should return false")
  (check-equal? (line-item-price 8700 50) 435000
                "the line item price should be (no of copies * unit price)"))

;--------------------------------------------------------------------------

;;fillable-now? : Order Inventory -> Boolean.
;;GIVEN: an order and an inventory
;;RETURNS: true iff there are enough copies of each book on hand to fill the
;;         order.  If the order contains a book that is not in the inventory,
;;         then the order is not fillable.
;;EXAMPLES:
;;  (fillable-now? order1 lob1) => true
;;  (fillable-now? order4 lob)1 => false
;;STRATEGY: Structural Decomposition on li : LineItem

(define (fillable-now? order lob)
  (andmap
   (; LineItem -> Boolean
    ;GIVEN: a line item from the given order to fillable-now? function
    ;RETURNS: true iff the line-item is fillable now
    lambda(li)
     (line-item-available? (line-item-isbn li) (line-item-copies li) lob))
   order))

;;TESTS:
(begin-for-test
  (check-equal? (fillable-now? order1 lob1) true
                "if all the line items in an order are available now, it should
                  return true")
  (check-equal? (fillable-now? order4 lob1) false
                "if even one line item in an order is not available now, it
                  should return false"))

;;line-item-available? : Integer NonNegInt Inventory -> Boolean
;;GIVEN: the isbn and no of copies of a line item and an inventory
;;RETURNS: true iff the isbn of the line item is present in the inventory and
;;         enough copies of it are available now
;;EXAMPLES:
;;  (line-item-available? li1 lob1) => true
;;  (line-item-available? li2 lob1) => false
;;STRATEGY: Structural Decomposition on b : Book

(define (line-item-available? isbn copies lob)
  (ormap
   (; Book -> Boolean
    ;GIVEN: a book in the inventory
    ;RETURNS: true iff the book has enough copies to fill the corrosponding
    ;         line item in the order
    lambda (b)
     (book-fillable? copies (book-copies b)))
   (line-item-in-inventory lob isbn)))


;;book-fillable? : NonNegInt NonNegInt -> Boolean
;;GIVEN: non-negative integer values for no of copies in line item and book
;;RETURNS: true iff the ordered no of copies are completely available on hand
;;EXAMPLES:
;;  (book-fillable? 5 7) => true
;;  (book-fillable? 10 1) => false
;;STRATEGY: Function Composition
(define (book-fillable? li-copies b-copies)
  (<= li-copies b-copies))

;;TESTS:
(begin-for-test
  (check-equal? (line-item-available? (line-item-isbn li1)
                                      (line-item-copies li1)
                                      lob1)
                true
                "true should be returned when a line item is available now")
  (check-equal? (line-item-available? (line-item-isbn li2)
                                      (line-item-copies li2)
                                      lob1)
                false
                "false should be returned when line item is not available now")
  (check-equal? (line-item-available? (line-item-isbn li6)
                                      (line-item-copies li6)
                                      lob1)
                false
                "false should be returned when a line item is not present in the
                inventory"))

;--------------------------------------------------------------------------

;;days-til-fillable : Order Inventory -> MaybeInteger
;;GIVEN: an order and an inventory
;;RETURNS: the number of days until the order is fillable, assuming all the
;;         shipments come in on time.  Returns false if there won't be enough
;;         copies of some book, even after the next shipment of that book comes
;;EXAMPLES: 
;;  (days-til-fillable order1 lob1) => 0
;;  (days-til-fillable order2 lob1) => false
;;  (days-til-fillable order3 lob1) => 4
;;STRATEGY: HOFC

(define (days-til-fillable order lob)
  (foldr
   (; LineItem MaybeInteger -> MaybeInteger
    ;GIVEN: a line item whose days till fillable needs to be computed and the
    ;       days till fillable of the rest of the order seen so far
    ;RETURNS: the days till fillable of the combined order
    lambda (li days-of-rest)
     (compare-days li lob days-of-rest))
   ZERO
   order))

;;compare-days : LineItem Inventory MaybeInteger -> MaybeInteger
;;GIVEN: a line item, an inventory and the days till the rest of the order till
;;       fillable
;;RETURNS: the maximum of the days till fillable of the given line item in the
;;         given inventory and the days till fillable of the rest of the order
;;EXAMPLES:
;;  (compare-days li3 lob1 4) => 0
;;  (compare-days li6 lob1 0) => false
;;STRATEGY: Function Composition
(define (compare-days li lob days-of-rest)
  (maybe-integer-max (days-til-line-item-fillable li lob)
                     days-of-rest))

;;TESTS:
(begin-for-test
  (check-equal? (days-til-fillable order1 lob1) 0
                "If order is fillable now, then days to fill the order 
                 should be zero")
  (check-equal? (days-til-fillable order2 lob1) false
                "If order cant be filled, then days to fill should be false")
  (check-equal? (days-til-fillable order3 lob1) 4
                "If order can be filled by a reorder, then days to fill should
               return the number of days to arrive")
  (check-equal? (days-til-fillable order4 lob1) false
                "If the order cant be filled even after the reorder arrives,
             then days to fill should be false")
  (check-equal? (days-til-fillable (list li6 li5) lob1) false
                "If a line item in order is not present in the inventory, it
                 should return false"))

;;maybe-integer-max : MaybeInteger MaybeInteger -> MaybeInteger
;;maybe-integer-max-2 : MaybeInteger MaybeInteger -> MaybeInteger
;;GIVEN: two MaybeIntegers
;;RETURN: false if either of the two are false, otherwise the max of them
;;EXAMPLES:
;;  (maybe-integer-max 23 false) => false
;;  (maybe-integer-max 23 43) => 43
;;STRATEGY: Structural Decomposition on mi1 : MaybeInteger
(define (maybe-integer-max mi1 mi2)
  (cond
    [(false? mi1) false]
    [(integer? mi1) (maybe-integer-max-2 mi1 mi2)]))

;;STRATEGY: Structural Decomposition on mi2 : MaybeInteger
(define (maybe-integer-max-2 mi1 mi2)
  (cond
    [(false? mi2) false]
    [(integer? mi2) (max mi1 mi2)]))

;;found-line-item? : LineItem Integer -> Boolean
;;GIVEN: the line item being searched for and an isbn to check with
;;RETURNS: true iff the line-item and the book being compared are the same
;;         i.e., if their isbn's match
;;EXAMPLES:
;; (found-line-item? li1 44876) => true
;; (found-line-item? li6 12348) => false
;;STRATEGY: Structural Decomposition on li : LineItem
(define (found-line-item? li isbn)
  (= (line-item-isbn li) isbn))

;;TESTS:
(begin-for-test
  (check-equal? (found-line-item? li1 44876) true
                "found-line-item? should return true if book is found")
  (check-equal? (found-line-item? li6 44876) false
                "found-line-item? should return false if book is not found"))

;;days-til-line-item-fillable : LineItem MaybeBook -> MaybeInteger
;;GIVEN: a line item and a book (or false, if no matching book is found)
;;RETURNS: an integer value for the no of days till line item to be filled,
;;         else returns false
;;EXAMPLES:
;;  (days-til-line-item-fillable li5 book10) => false
;;  (days-til-line-item-fillable li3 book4) => 0
;;  (days-til-line-item-fillable li4 book5) => 4
;;STRATEGY: Structural Decomposition on li : LineItem

(define (days-til-line-item-fillable li lob)
  (foldr
   (; Book Any -> MaybeInteger
    ;GIVEN: a book in the inventory with the matching isbn of a line item
    ;       being considered
    ;RETURNS: the number of days for the line item to be filled, or false if
    ;         it cant be filled even after a reorder (if exists)
    lambda (book any)
     (days-til-line-item-fillable-in-inventory li book))
   false
   (line-item-in-inventory lob (line-item-isbn li))))

;;days-til-line-item-fillable-in-inventory : LineItem Book -> MaybeInteger
;;GIVEN: a line item and a book
;;RETURNS: an integer value for the no of days till line item to be filled,
;;         else returns false
;;EXAMPLES:
;;     see test cases
;;STRATEGY: Structural decomposition on b : Book
(define (days-til-line-item-fillable-in-inventory li b)
  (if (book-fillable? (get-copies li) (book-copies b))
      ZERO
      (days-til-shipment-arrives li
                                 (book-copies b) 
                                 (book-reorder-status b))))

;;get-copies : LineItem -> NonNegInt
;;GIVEN: a line item
;;RETURNS: the number of copies in the line item
;;STRATEGY: Structural Decomposition on li : LineItem
(define (get-copies li)
  (line-item-copies li))

;;days-til-shipment-arrives : LineItem NonNegInt ReorderStatus -> MaybeInteger
;;GIVEN: a line item, its corrosponding no of copies on hand in the inventory
;;       and the corrosponding reorder status
;;RETURNS: the no of days for the line item to be filled
;;STRATEGY: Structural Decomposition on li : LineItem
(define (days-til-shipment-arrives li b-copies b-status)
  (days-til-shipment-arrives-to-inventory (line-item-copies li)
                                          b-copies
                                          b-status))

;;days-til-shipment-arrives-to-inventory : 
;;                       NonNegInt NonNegInt ReorderStatus -> MaybeInteger
;;GIVEN: two integer values for the no of copies on hand and in the order,
;;       and the book's reorder status
;;RETURNS: no of days till the line item can be filled, or false if it cant be
;;         filled even after the next shipment
;;EXAMPLES:
;;  (days-til-shipment-arrives 50 25 (make-reorder-status 5 10 true)) => false
;;  (days-til-shipment-arrives 4 1 (make-reorder-status 4 5 true)) => 4
;;STRATEGY: Structural Decomposition b-status : ReorderStatus
(define (days-til-shipment-arrives-to-inventory li-copies b-copies b-status)
  (if (reorder-status-present? b-status)
      (if (>= (+ (reorder-status-copies b-status) b-copies) li-copies)
          (reorder-status-days b-status)
          false)
      false))

;--------------------------------------------------------------------------

;;inventory-after-deliveries : Inventory -> Inventory
;;GIVEN: today's inventory
;;RETURNS: an Inventory representing tomorrow's inventory, in which all
;;         reorders that were due in 1 day are now available, and all other
;;         reorders have their expected times decreased by 1.
;;EXAMPLES:
;;  (inventory-after-deliveries lob5) => lob6
;;STRATEGY: HOFC

(define (inventory-after-deliveries lob)
  (map
   (; Book -> Book
    ;GIVEN: a book
    ;RETURNS: a book with updated fields as described by update-reorder-status
    lambda (book)
     (update-reorder-status book))
   lob))

;;update-reorder-status : Book -> Book
;;GIVEN: a book
;;RETURNS: a book just like the given, with its reorder status updated as it
;;         should be on th next day
;;EXAMPLES:
;;  (update-reorder-status book5) => book5-1
;;  (update-reorder-status book3) => book3
;;STRATEGY: Structural Decomposition on b : Book

(define (update-reorder-status b)
  (make-book (book-isbn b)
             (book-title b)
             (book-author b)
             (book-publisher b)
             (book-unit-price b)
             (book-unit-cost b)
             (change-copies (book-copies b) (book-reorder-status b))
             (check-reorder-status (book-reorder-status b))
             (book-cuft b)))

;;change-copies : NonNegInt ReorderStatus -> NonNegInt
;;GIVEN: the no of copies and the reorder-status
;;RETURNS: the no of copies on hand as it should be on the next day
;;EXAMPLES:
;;  (change-copies 5 (make-reorder-status 1 50 true)) => 55
;;  (change-copies 40 (make-reorder-status 4 50 false)) => 40
;;STRATEGY: Structural Decomposition on status : ReorderStatus
(define (change-copies copies status)
  (if (and (<= (reorder-status-days status) 1) (reorder-status-present? status))
      (+ copies (reorder-status-copies status))
      copies))

;;check-reorder-status : ReorderStatus -> Boolean
;;GIVEN: a reorder status
;;RETURNS: true iff the reorder status is present
;;EXAMPLES: see tests below
;;STRATEGY: Structural Decomposition on status : ReorderStatus
(define (check-reorder-status status)
  (if (reorder-status-present? status)
      (change-reorder-status status)
      status))

;;change-reorder-status : ReorderStatus -> ReorderStatus
;;GIVEN: a reorder-status
;;RETURNS: a reorder-status just like the given, with its no of days to delivery
;;         updated as it should be on th next day
;;EXAMPLES:
;;  (change-reorder-status (make-reorder-status 1 50 true))
;;               => (make-reorder-status 1 1 false)
;;STRATEGY: Structural Decomposition on status : ReorderStatus

(define (change-reorder-status status)
  (if (<= (reorder-status-days status) 1)
      (make-empty-reorder 0)
      (make-reorder-status (- (reorder-status-days status) 1)
                           (reorder-status-copies status)
                           true)))

;;TESTS:
(begin-for-test
  (check-equal? (inventory-after-deliveries lob5)
                lob6
                "after one day, the reorder statuses and no of copies on hand
                 should change accordingly"))
;--------------------------------------------------------------------------

;;price-for-order : Inventory Order -> NonNegInt
;;GIVEN: an inventory and an order
;;RETURNS: the total price for the given order, in USD*100.  The price does not
;;         depend on whether any particular line item is in stock.  Line items
;;         for an ISBN that is not in the inventory count as 0
;;EXAMPLES:
;;  (price-for-order lob1 order1) => 107800
;;  (price-for-order lob1 order2) => 542800
;;STRATEGY: HOFC

(define (price-for-order lob order)
  (foldr
   (; LineItem NonNegInt -> NonNegInt
    ;GIVEN: a the current line item and the price for rest of the order
    ;RETURNS: the price of the combined order
    lambda (li price-for-rest)
     (add-prices (price-for-line-item lob li) price-for-rest))
   ZERO
   order))

;;add-prices : MaybeInteger NonNegInt -> NonNegInt
;;GIVEN: the price of the current line item (or false,if it is not found in 
;;       the inventory) and the price of the rest of the order
;;RETURNS: the sum of the prices. If the price of the current line item is false
;;         then the price of th rest of the order is returned
;;EXAMPLES:
;;  (add-prices false 2033) => 2033
;;  (add-prices 2000 3000) => 5000
;;STRATEGY: Structural Decomposition on p1 : MaybeInteger
(define (add-prices p1 p2)
  (cond
    [(false? p1) p2]
    [(integer? p1) (+ p1 p2)]))

;;TESTS:
(begin-for-test
  (check-equal? (price-for-order lob1 order1) 107800
                "total price is (req. no of copies * unit-price")
  (check-equal? (price-for-order lob1 (cons li6 order1)) 107800
                "if an isbn is not found, the price for tha line item is 0"))

;--------------------------------------------------------------------------

;;inventory-after-order : Inventory Order -> Inventory.
;;GIVEN: an order
;;WHERE: the order is fillable now
;;RETURNS: the inventory after the order has been filled.
;;EXAMPLES:
;;  (inventory-after-order (list book1 book2 book4) order1)
;;   => (list book1-1 book2 book4-1)
;;STRATEGY: HOFC

(define (inventory-after-order lob order)
  (map
   (; Book -> Book
    ;GIVEN: a book from the inventory
    ;RETURNS: a book, just like the given, with the no of copies updated to
    ;         the change that occurs after an order is taken
    lambda (book)
     (book-after-order book order))
   lob))

;;book-after-order : Book Order -> Book
;;GIVEN: a book and an order
;;RETURNS: a book just like the given, but if the book has been requested in an
;;         order, then the no of copies of it will be reduced by the no ordered
;;EXAMPLES:
;;  (book-after-order book1 order1) => book1-1
;;  (book-after-order book2 order1) => book2
;;STRATEGY: Structural Decomposition li : LineItem
(define (book-after-order b order)
  (foldr
   (; LineItem Book -> Book
    ;GIVEN: a line item
    ;RETURNS: a book of matching isbn with its no of copies updated
    lambda (li any-book)
     (update-book b (line-item-isbn li) (line-item-copies li)))
   b
   (filter
    (; LineItem -> Boolean
     ;GIVEN: a line item
     ;RETURNS: true iff the line item's isbn matches the book's isbn
     lambda (li)
      (found-book? b (line-item-isbn li)))
    order)))

;;found-book? : Book Integer -> Boolean
;;GIVEN: a book and the isbn of the current line item
;;RETURNS: true iff the book's isbn and the given isbn match
;;EXAMPLES:
;;  (found-book? book1 ) => true
;;  (found-book? book2 ) => false
;;STRATEGY: Structural Decomposition on b : Book
(define (found-book? b li-isbn)
  (= (book-isbn b) li-isbn))

;;update-book : Book NonNegInt NonNegInt -> Book
;;GIVEN: a book and a value equal to the no of copies of this book ordered
;;RETURNS: a book, just like the given, with its no of copies updated after the
;;         processing the order
;;EXAMPLES:
;;  (update-book book1 ) => book1-1
;;STRATEGY: Structural Decomposition on b : Book
(define (update-book b li-isbn li-copies)
  (make-book (book-isbn b)
             (book-title b)
             (book-author b)
             (book-publisher b)
             (book-unit-price b)
             (book-unit-cost b)
             (- (book-copies b) li-copies)
             (book-reorder-status b)
             (book-cuft b)))

;;TESTS:
(begin-for-test
  (check-equal? (inventory-after-order (list book1 book2 book4) order1)
                (list book1-1 book2 book4-1)
                "after an order, the ordered books' no of copies on hand should
                 be reduced by the no of copies of the line item"))

;--------------------------------------------------------------------------

;;increase-prices : Inventory String Real -> Inventory
;;GIVEN: an inventory, a publisher, and a percentage
;;RETURNS: an inventory like the original, except that all items by that
;;         publisher have their unit prices increased by the specified
;;         percentage
;;EXAMPLE: 
;; (increase-prices lob3 "MIT Press" 10) => lob4
;;STRATEGY: HOFC

(define (increase-prices lob pub perc)
  (map
   (;Book -> Book
    ;GIVEN: a book
    ;RETURNS: a book, just like the given, with its price increased if the name
    ;         of the publisher match
    lambda (b)
     (publisher-match? b pub perc))
   lob))

;;TESTS:
(begin-for-test
  (check-equal? (increase-prices lob3 "MIT Press" 10) lob4
                "increase-prices should increase the prices of all the books
                that have the same publisher as the one mentioned, by the amount
                of percentage specified"))

;;publisher-match? : Book String Integer -> Book
;;GIVEN: takes a book, a publisher's name and a value for the percentage
;;RETURNS: a book, just like the given, but the price is incresed if the
;;         publisher name matches
;;EXAMPLES:
;;   (publisher-match? book2 "MIT Press" 10) => book2-1
;;   (publisher-match? book1 "Illustrator" 20) => book1
;;STRATEGY: Structural Decomposition on b : Book

(define (publisher-match? b pub perc)
  (if (string=? pub (book-publisher b))
      (increase-price b perc)
      b))

;;increase-price : Book Integer -> Book
;;GIVEN: a book and an integer value for the percent of increase
;;RETURNS: a new book with the price increased by the percent value
;;EXAMPLES: refer tests
;;STRATEGY: Structural Decomposition on b : Book
(define (increase-price b perc)
  (make-book (book-isbn b)
             (book-title b)
             (book-author b)
             (book-publisher b)
             (+ (book-unit-price b)
                (round (/ (* perc (book-unit-price b)) HUNDRED)))
             (book-unit-cost b)
             (book-copies b)
             (book-reorder-status b)
             (book-cuft b)))
;--------------------------------------------------------------------------

;;make-empty-reorder : Any -> ReorderStatus
;;GIVEN: any value, which is ignored
;;RETURNS: a Reorder showing no pending re-order
;;EXAMPLES:
;; (make-empty-reorder 32) => (make-reorder-status 1 1 false)
;;STRATEGY: Function Composition

(define (make-empty-reorder any)
  (make-reorder-status RESET-VALUE RESET-VALUE false))

(begin-for-test
  (check-equal? (make-empty-reorder 32) (make-reorder-status 1 1 false)
                "empty reorder should return an empty reorder"))
;--------------------------------------------------------------------------

;;make-reorder : PosInt PosInt -> ReorderStatus
;;GIVEN: a number of days and a number of copies
;;RETURNS: a ReorderStatus with the given data
;;EXAMPLE:
;;  (make-reorder 3 5) => (make-reorder-status 3 5 true)
;;STRATEGY: Function Composition
(define (make-reorder days copies)
  (make-reorder-status days copies true))

;;TESTS:
(begin-for-test
  (check-equal? (make-reorder 3 5) (make-reorder-status 3 5 true)
                "make-reorder should make a reorder with the given values"))

;--------------------------------------------------------------------------

;;reorder-present? : ReorderStatus -> Boolean
;;GIVEN: a reorder status
;;RETURNS: true iff a valid reorder status is present
;;EXAMPLE:
;;  (reorder-present? (make-reorder-status 3 5 true)) => true
;;  (reorder-present? (make-reorder-status 3 5 false)) => false
;;STRATEGY: Structural Decomposition on r : ReorderStatus

(define (reorder-present? r)
  (reorder-status-present? r))

;;TESTS:
(begin-for-test
  (check-equal? (reorder-present? (make-reorder-status 3 5 true)) true
                "should return true iff a valid reorder status is present"))

;--------------------------------------------------------------------------