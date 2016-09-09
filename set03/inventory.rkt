;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname inventory) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
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
         price-for-order
         inventory-after-order
         increase-prices
         make-empty-reorder)
;--------------------------------------------------------------------------

;; DATA DEFINITIONS

;; A MaybeInteger is one of
;; -- Integer, describes that it has some value
;; -- false, describes that it doesn't have any value defined

(define-struct reorder-status (days copies present?))
;; A ReorderStatus is a (make-reorder-status PosInt PosInt Boolean)
;; --days, describes the number of days untill the next shipment
;; --copies, describes number of copies that will arrive at the next shipment
;; --present?, is true iff the given reorder shows a pending request
;; Note: If there is no reorder, both the fields should be zero
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

;;A Inventory (list of books - lob) is a list of books in the inventory
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

;; A MaybeBook is one of:
;; -- Book, describes a book in the inventory
;; -- false, describes that it doesn't have any value defined
;; TEMPLATE:
;; mb-fn : MaybeBook -> ??
;;   (define (mb-fn mb)
;;     (cond
;;       [(book? mb) ...]
;;       [(fals? mb) ...]))

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
          "Pearson Education" 2300 2300 10 (make-reorder-status 12 3 false) 3))

(define book8 
  (make-book 23783 "Solid State Physics" "Neil W. Ashcroft" "Pacific Grove" 9000
             8900 10 (make-reorder-status 3 4 false) 1))

(define book9 
  (make-book 89437 "Medical Chemistry" "William O. Foye" 
             "MIT Press" 3450 3000 6 (make-reorder-status 5 5 true) 0.23))

(define book10 
  (make-book 47437 "Visualizing data" "Ben Fry" "O'Reilly Media" 3300 3550 25
             (make-reorder-status 5 10 true) 1.5))

(define book1-1 
  (make-book 44876 "Network Security" "Huand Scott" "SpringerLink" 4200 5800 3
             (make-reorder-status 9 12 true) 1))

(define book2-1 
  (make-book 47758 "Semistructured Database Design" "Ling" "MIT Press" 
             10780 8700 34 (make-reorder-status 4 5 false) 2))

(define book4-1 
  (make-book 12834 "A.I:Artificial Intelligence" "Stanley Kubrick" "DreamWorks"
             3400 4300 0 (make-reorder-status 7 10 true) 2/5))

(define book9-1 
  (make-book 89437 "Medical Chemistry" "William O. Foye" 
             "MIT Press" 3795 3000 6 (make-reorder-status 5 5 true) 0.23))

(define lob1 (list book1 book2 book3 book4 book5 
                   book6 book7 book8 book9 book10))

(define lob2 (list book1 book2 book3 book4 book5))

(define lob3 (list book2 book9 book8))

(define lob4 (list book2-1 book9-1 book8))

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
;;STRATEGY: Structural Decomposition on Inventory : lob

(define (inventory-potential-profit lob)
  (cond
    [(empty? lob) ZERO]
    [else (+ (book-profit (first lob))
             (inventory-potential-profit (rest lob)))]))

;;book-profit : Book -> Integer
;;GIVEN: a Book
;;RETURNS: the profit, in USD*100, of this book
;;EXAMPLES:
;;  (book-profit book1) => -36800
;;  (book-profit book2) => 37400
;;STRATEGY: Structural Decomposition Book : b

(define (book-profit b)
  (* (book-copies b) (- (book-unit-price b) (book-unit-cost b))))

;;TESTS:
(begin-for-test
  (check-equal? (inventory-potential-profit lob2) 85800
                "the potential profit function is not behaving properly")
  (check-equal? (book-profit book1) -36800
                "a book on loss should show negative profit"))

;--------------------------------------------------------------------------

;;inventory-total-volume : Inventory -> Real
;;GIVEN: an inventory
;;RETURNS: the total volume needed to store all the books in stock
;;EXAMPLES:                                ______
;;  (inventory-total-volume lob1) => 183.56076923
;;                                        ______
;;  (inventory-total-volume lob2) => 103.5307692
;;STRATEGY:Structural Decomposition on Inventory : lob
;;                                 and Book : (first lob)

(define (inventory-total-volume lob)
  (cond 
    [(empty? lob) ZERO]
    [else (+ (* (book-copies (first lob)) (book-cuft (first lob)))
             (inventory-total-volume (rest lob)))]))

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
;;STRATEGY: Structural Decomposition on Inventory : lob
;;                                  and Book : (first lob)

(define (price-for-line-item lob li)
  (cond
    [(empty? lob) false]
    [else (if (found-line-item? li (book-isbn (first lob)))
              (line-item-price (book-unit-price (first lob)) li)
              (price-for-line-item (rest lob) li))]))

;;TEST:
(begin-for-test
  (check-equal? (price-for-line-item lob1 li3) 23800
                "the line item price should be (no of copies * unit price)")
  (check-equal? (price-for-line-item lob1 li6) false
                "if a line item is not found, it should return false"))

;;line-item-price : NonNegInt LineItem -> Integer
;;GIVEN: a line item and the unit price of that item
;;RETURNS: the total price for that line item
;;EXAMPLE:
;; (line-item-price 8700 li2) => 435000
;;STRATEGY: Structural Decomposition on LineItem : li

(define (line-item-price unit-price li)
  (* (line-item-copies li) unit-price))

;;TESTS:
(begin-for-test
  (check-equal? (line-item-price 8700 li2) 435000
                "the line item price should be (no of copies * unit price)"))

;;found-line-item? : LineItem Integer -> Boolean
;;GIVEN: the line item being searched for and an isbn to check with
;;RETURNS: true iff the line-item and the book being compared are the same
;;         i.e., if their isbn's match
;;EXAMPLES:
;; (found-line-item? li1 44876) => true
;; (found-line-item? li6 12348) => false
;;STRATEGY: Structural Decomposition on LineItem : li
(define (found-line-item? li isbn)
  (= (line-item-isbn li) isbn))

;;TESTS:
(begin-for-test
  (check-equal? (found-line-item? li1 44876) true
                "found-line-item? should return true if book is found")
  (check-equal? (found-line-item? li6 44876) false
                "found-line-item? should return false if book is not found"))
;--------------------------------------------------------------------------

;;fillable-now? : Order Inventory -> Boolean.
;;GIVEN: an order and an inventory
;;RETURNS: true iff there are enough copies of each book on hand to fill the
;;         order.  If the order contains a book that is not in the inventory,
;;         then the order is not fillable.
;;EXAMPLES:
;;  (fillable-now? order1 lob1) => true
;;  (fillable-now? order4 lob)1 => false
;;STRATEGY:Structural Decomposition on Order : order

(define (fillable-now? order lob)
  (cond
    [(empty? order) true]
    [else (if (line-item-available? (first order) lob)
              (fillable-now? (rest order) lob)
              false)]))

;;TESTS:
(begin-for-test
  (check-equal? (fillable-now? order1 lob1) true
                "if all the line items in an order are available now, it should
                  return true")
  (check-equal? (fillable-now? order4 lob1) false
                "if even one line item in an order is not available now, it
                  should return false"))

;;line-item-available? : LineItem Inventory -> Boolean
;;GIVEN: a line item and the inventory
;;RETURNS: true iff the isbn of the line item is present in the inventory and
;;         enough copies of it are available now
;;EXAMPLES:
;;  (line-item-available? li1 lob1) => true
;;  (line-item-available? li2 lob1) => false
;;STRATEGY: Structural Decomposition on Inventory : lob
;;                                  and Book : (first lob)
(define (line-item-available? li lob)
  (cond
    [(empty? lob) false]
    [else (if (found-line-item? li (book-isbn (first lob)))
              (book-fillable? li (book-copies (first lob)))
              (line-item-available? li (rest lob)))]))

;;book-fillable? : LineItem NonNegInt -> Boolean
;;GIVEN: a line item and an integer value for no of copies in stock
;;RETURNS: true iff the ordered no of copies are completely available in hand
;;EXAMPLES:
;;  (book-fillable? li3 7) => true
;;  (book-fillable? li4 1) => false
;;STRATEGY: Structural Decomposition on LineItem : li
(define (book-fillable? li copies)
  (<= (line-item-copies li) copies))

;;TESTS:
(begin-for-test
  (check-equal? (line-item-available? li1 lob1) true
                "true should be returned when a line item is available now")
  (check-equal? (line-item-available? li2 lob1) false
              "false should be returned when a line item is not available now")
  (check-equal? (line-item-available? li6 lob1) false
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
;;STRATEGY: Structural Decomposition on Order : order
(define (days-til-fillable order lob)
  (cond
    [(empty? order) ZERO]
    [else (maybe-integer-max
           (days-til-line-item-fillable 
            (first order) 
            (line-item-in-lob (first order) lob))
           (days-til-fillable (rest order) lob))]))

;;TESTS:
(begin-for-test
  (check-equal? (days-til-fillable order1 lob1) 0
       "If order is fillable now, then days to fill the order should be zero")
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
;;GIVEN: two MaybeIntegers
;;RETURN: false if either of the two are false, otherwise the max of them
;;EXAMPLES:
;;  (maybe-integer-max 23 false) => false
;;  (maybe-integer-max 23 43) => 43
;;STRATEGY: Function Composition
(define (maybe-integer-max mi1 mi2)
  (if (or (false? mi1) (false? mi2))
      false
      (max mi1 mi2)))

;;line-item-in-lob : LineItem Inventory -> MaybeBook
;;GIVEN: a line item and the inventory
;;RETURNS: a book if a matching isbn is found, else returns false
;;EXAMPLES:
;;  (line-item-in-lob li3 lob1) => book4
;;  (line-item-in-lob li6 lob1) => false
;;STRATEGY: Structural Decomposition Inventory : lob
;;                               and Book : (first lob)
(define (line-item-in-lob li lob)
  (cond
    [(empty? lob) false]
    [else (if (found-line-item? li (book-isbn (first lob)))
              (first lob)
              (line-item-in-lob li (rest lob)))]))

;;days-til-line-item-fillable : LineItem MaybeBook -> MaybeInteger
;;GIVEN: a line item and a book (or false, if no matching book is found)
;;RETURNS: an integer value for the no of days till line item to be filled,
;;         else returns false
;;EXAMPLES:
;;  (days-til-line-item-fillable li5 book10) => false
;;  (days-til-line-item-fillable li3 book4) => 0
;;  (days-til-line-item-fillable li4 book5) => 4
;;STRATEGY: Structural Decomposition on MaybeBook : b
(define (days-til-line-item-fillable li b)
  (cond
    [(book? b) (days-til-line-item-fillable-in-inventory li b)]
    [(false? b) false]))

;;days-til-line-item-fillable-in-inventory : LineItem Book -> MaybeInteger
;;GIVEN: a line item and a book
;;RETURNS: an integer value for the no of days till line item to be filled,
;;         else returns false
;;EXAMPLES:
;;     see test cases
;;STRATEGY: Structural decomposition on Book : b
(define (days-til-line-item-fillable-in-inventory li b)
  (if (book-fillable? li (book-copies b))
      ZERO
      (days-til-shipment-arrives li
                                 (book-copies b) 
                                 (book-reorder-status b))))

;;days-til-shipment-arrives : LineItem NonNegInt ReorderStatus -> MaybeInteger
;;GIVEN: a line item, its corrosponding no of copies on hand in the inventory
;;       and the corrosponding reorder status
;;RETURNS: the no of days for the line item to be filled
;;STRATEGY: Structural Decomposition on LineItem : li
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
;;STRATEGY: Structural Decomposition ReorderStatus : b-status
(define (days-til-shipment-arrives-to-inventory li-copies b-copies b-status)
  (if (reorder-status-present? b-status)
      (if (>= (+ (reorder-status-copies b-status) b-copies) li-copies)
          (reorder-status-days b-status)
          false)
      false))

;--------------------------------------------------------------------------

;;price-for-order : Inventory Order -> NonNegInteger
;;GIVEN: an inventory and an order
;;RETURNS: the total price for the given order, in USD*100.  The price does not
;;         depend on whether any particular line item is in stock.  Line items
;;         for an ISBN that is not in the inventory count as 0
;;EXAMPLES:
;;  (price-for-order lob1 order1) => 107800
;;  (price-for-order lob1 order2) => 542800
;;STRATEGY: Structural Decompostion on Order : order

(define (price-for-order lob order)
  (cond
    [(empty? order) ZERO]
    [else (add-prices (price-for-line-item lob (first order))
                      (price-for-order lob (rest order)))]))

;;add-prices : MaybeInteger NonNegInt -> NonNegInt
;;GIVEN: the price of the current line item (or false,if it is not found in 
;;       the inventory) and the price of the rest of the order
;;RETURNS: the sum of the prices. If the price of the current line item is false
;;         then the price of th rest of the order is returned
;;EXAMPLES:
;;  (add-prices false 2033) => 2033
;;  (add-prices 2000 3000) => 5000
;;STRATEGY: Function Composition
(define (add-prices p1 p2)
  (if (false? p1) p2 (+ p1 p2)))

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
;;STRATEGY: Structural Decomposition on Inventory : lob
(define (inventory-after-order lob order)
  (cond
    [(empty? lob) empty]
    [else (cons (book-after-order (first lob) order)
                (inventory-after-order (rest lob) order))]))

;;book-after-order : Book Order -> Book
;;GIVEN: a book and an order
;;RETURNS: a book just like the given, but if the book has been requested in an
;;         order, then the no of copies of it will be reduced by the no ordered
;;EXAMPLES:
;;  (book-after-order book1 order1) => book1-1
;;  (book-after-order book2 order1) => book2
;;STRATEGY: Structural Decomposition on Order : order
;;                                  and LineItem : (first order)
(define (book-after-order b order)
  (cond
    [(empty? order) b]
    [else (if (found-book? b (line-item-isbn (first order)))
              (update-book b (line-item-copies (first order)))
              (book-after-order b (rest order)))]))

;;found-book? : Book Integer -> Boolean
;;GIVEN: a book and the isbn of the current line item
;;RETURNS: true iff the book's isbn and the given isbn match
;;EXAMPLES:
;;  (found-book? book1 ) => true
;;  (found-book? book2 ) => false
;;STRATEGY: Structural Decomposition on Book : b
(define (found-book? b li-isbn)
  (= (book-isbn b) li-isbn))

;;update-book : Book NonNegInteger -> Book
;;GIVEN: a book and a value equal to the no of copies of this book ordered
;;RETURNS: a book, just like the given, with its no of copies updated after the
;;         processing the order
;;EXAMPLES:
;;  (update-book book1 ) => book1-1
;;STRATEGY: Structural Decomposition on Book : b
(define (update-book b li-copies)
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
;;STRATEGY: Structural Decomposition on Inventory : lob

(define (increase-prices lob pub perc)
  (cond
    [(empty? lob) empty]
    [else (cons (if (publisher-match? pub (first lob))
                    (increase-price (first lob) perc)
                    (first lob))
                (increase-prices (rest lob) pub perc))]))

;;TESTS:
(begin-for-test
  (check-equal? (increase-prices lob3 "MIT Press" 10) lob4
                "increase-prices should increase the prices of all the books
                that have the same publisher as the one mentioned, by the amount
                of percentage specified"))

;;publisher-match? : String Book -> Boolean
;;GIVEN: takes a publisher name and a book
;;RETURNS: true iff the publisher names match
;;EXAMPLES:
;;
;;
;;STRATEGY: Structural Decomposition on Book : b

(define (publisher-match? pub b)
  (string=? pub (book-publisher b)))

;;increase-price : Book Integer -> Book
;;GIVEN: 
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
(define (make-reorder d c)
  (make-reorder-status d c true))

;;TESTS:
(begin-for-test
  (check-equal? (make-reorder 3 5) (make-reorder-status 3 5 true)
                "make-reorder should make a reorder with the given values"))