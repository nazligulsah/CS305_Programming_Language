(define symbol-length
	(lambda(inSym)
	    (if(symbol? inSym)
		(string-length(symbol->string inSym))
		0
		)
	)
 )

(define sequence? 
	(lambda (inSeq)
		(if (list? inSeq)
		      
	    	     (if (null? inSeq)
			#t
			(if(and (symbol? (car inSeq)) (eq? 1  (symbol-length(car inSeq ))))
				(sequence? (cdr inSeq))
				#f
			)


	            )
	         #f
		)

	)
)

(define same-sequence?
	(lambda(inSeq1 inSeq2)
		(if (sequence? inSeq1)
		    (if (sequence? inSeq2)
			(if (or (null? inSeq1) (null? inSeq2))
			    (if(and (null? inSeq1)(null? inSeq2)) #t #f)
                            (if(eq? (car inSeq1)(car inSeq2))
				(same-sequence? (cdr inSeq1)(cdr inSeq2))
				#f
			   	) 
			)
		     (error "ERROR305: inSeq2 is not a sequence")
	
	)
		 (error "ERROR305: inSeq1 is not a sequence") 
		

)
)

)

;(define x?
;        (lambda(inSym inSeq)
;                (if (symbol? inSym)
;                    (if (sequence? inSeq)
;                        (if (or (symbol? inSym) (sequence? inSeq))
;                            (if(and (symbol? inSym)(sequence? inSeq))
;				(if(null? inSeq) #f
;                           		 (if(equal? inSym (car inSeq)) #t
;                              			  (member? inSym (cdr inSeq))
;                                
;                                )))
;                        (error "ERROR305:inSym is not a symbol and  inSeq is not a sequence"))
;                     (error "ERROR305: inSeq is not a sequence"))
;
;                  
;                 (error "ERROR305: inSym is not a symbol"))
;)
;
;)






(define reverse-sequence
	(lambda (inSeq)
		(if (sequence? inSeq)
		   (if (null? inSeq)
			 ()
		  	 (append (reverse-sequence(cdr inSeq)) (list (car inSeq)))

                   )
		(error "ERROR305: inSeq is not a sequence" )
	)
	)
 )


(define palindrome? 
	(lambda (inSeq)
		(if (sequence? inSeq)
			(if(same-sequence? inSeq(reverse-sequence inSeq))
			#t #f)
 		(error "ERROR305: inSeq is not a sequence")
	
)
)
)

(define member?
	(lambda (inSym inSeq)
	   (if (and (symbol? inSym )(sequence? inSeq))
	   
		(if (null? inSeq) #f
		    (if (equal? inSym (car inSeq))#t 
			(member? inSym (cdr inSeq))
))
	
 

	 (error "ERROR305: inSym is not a symbol or/and inSeq is not a sequence")
)
)
)

(define remove-member 
	(lambda (inSym inSeq)
	   (if (and (symbol? inSym)(sequence? inSeq))
		(if(member? inSym inSeq)
		  (cond 
			((equal? inSym (car inSeq)) (cdr inSeq))
			(else (cons (car inSeq) (remove-member inSym (cdr inSeq))))
				
 )
	(error "ERROR305: inSym  is not a member of inSeq")
)
(error "ERROR305: inSym is not a symbol or/and inSeq is not a sequence")
)


))



;(define member2?
;        (lambda (inSym inSeq)
;		(cond 
;         	        (not (symbol? inSym)
;                        (error "EROR305: inSym is not a symbol"))
;               	         (not (sequence? inSeq)
;                         (error "EROR305: inSeq is not a sequence"))
;          )
;	  (if(and (symbol? inSym)(sequence? inSeq))
;		
;			(if(null? inSeq)#f
;				(if(equal? inSym (car inSeq)) #t
;					(member? inSym (cdr inSeq))))
;		)
;	 )
;)





(define anagram?
	(lambda (inSeq1 inSeq2)
		(if (and (sequence? inSeq1) (sequence? inSeq2))
			(if (or (null? inSeq1) (null? inSeq2)) 
				(if (and (null? inSeq1) (null? inSeq2)) #t #f)
	   				(if (member? (car inSeq1) inSeq2)
	         				(anagram? (cdr inSeq1) (remove-member (car inSeq1) inSeq2))
         					#f
)
)	
      (error "ERROR305: inSeq1 is not a sequence and/or inSeq2 is not a sequence"))
)
)

(define anapoli?
	(lambda (inSeq1 inSeq2)
		(if (and (sequence? inSeq1)(sequence? inSeq2 ))
		   
			(if (and (palindrome? inSeq2)(anagram? inSeq1 inSeq2))
			#t #f)

			 (error "ERROR305: inSeq1 is not a sequence and/or inSeq2 is  not a sequence")
)
)
)

