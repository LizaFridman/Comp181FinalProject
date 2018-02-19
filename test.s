;(seq ((const 1) (if3 (applic (const 1) ((fvar >) (const 0))) (const 1) (const 0))))
;(const 1)
	MOV RAX, 0

;(if3 (applic (const 1) ((fvar >) (const 0))) (const 1) (const 0))
;(applic (const 1) ((fvar >) (const 0)))
;Applic (applic (const 1) ((fvar >) (const 0)))
	CMP RAX, SOB_FALSE
	JE L_if3Dif2
;(const 1)
	MOV RAX, 0

	JMP L_if3End1
L_if3Dif2:
;(const 0)
	MOV RAX, 0

L_if3End1:

