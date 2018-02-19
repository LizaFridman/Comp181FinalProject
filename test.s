;(or ((const 1) (const 2) (const 3)))
;(const 1)
	MOV RAX, 0

	CMP RAX, SOB_FALSE
	JNE L_orEnd1
;(const 2)
	MOV RAX, 0

	CMP RAX, SOB_FALSE
	JNE L_orEnd1
;(const 3)
	MOV RAX, 0

L_orEnd1:
