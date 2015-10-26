TITLE Boolean Calc II(main.asm)
INCLUDE Irvine32.inc

.data
selectStr BYTE "Select a function: ", 0
str1 BYTE "Enter element x: ",0
str2 BYTE "Enter element y: ", 0
str3 BYTE "Enter element x (hex): ",0
str4 BYTE "Enter element y (hex): ", 0

opt1 BYTE "1. x AND y", 0
opt2 BYTE "2. x OR y", 0
opt3 BYTE "3. NOT x", 0
opt4 BYTE "4. x XOR y", 0
opt5 BYTE "5. x AND y (hex)", 0
opt6 BYTE "6. x OR y (hex)", 0
opt7 BYTE "7. NOT x (hex)", 0
opt8 BYTE "8. x XOR y (hex)", 0
opt9 BYTE "9. Exit Program", 0

xVal SDWORD ?
yVal SDWORD ?

.code
main PROC
call promptUser
	exit
main ENDP


promptUser PROC uses edx eax
mov edx, offset selectStr
call writestring
call crlf
call functions 

call readint
jmp op1
call crlf

;x and y

op1: 
	cmp eax,1
	jg op2
	mov edx, offset str1
	call writestring
	call readint
	mov xVal, eax
	call dumpregs
	call crlf 
	

	mov edx, offset str2
	call writeString
	call readint
	mov yVal, eax
	call crlf
	call dumpregs

	mov eax, xVal
	and eax, yVal

	call writebin
	jmp endprog

op2: 
	cmp eax,2
	jg op3
	mov edx, offset str1
	call writestring
	call readint
	mov xVal, eax
	call dumpregs
	call crlf 
	

	mov edx, offset str2
	call writeString
	call readint
	mov yVal, eax
	call crlf
	call dumpregs

	mov eax, xVal
	or eax, yVal

	call writebin
	jmp endprog
	;not x
op3: 
	cmp eax, 3
	jg op4
	mov edx, offset str1
	call writestring
	call readint
	mov xVal, eax
	call dumpregs
	call crlf 

	not xVal
	mov eax,xVal
	call writebin

	jmp endprog


	;xory
op4:

	cmp eax, 4
	jg op5
	mov edx, offset str1
	call writestring
	call readint
	mov xVal, eax
	call dumpregs
	call crlf 
	

	mov edx, offset str2
	call writeString
	call readint
	mov yVal, eax
	call crlf
	call dumpregs

	mov eax, xVal
	xor eax, yVal

	call writebin
	jmp endprog

	;AND_op
op5:

	cmp eax, 5
	jg op6

	mov edx, offset str3
	call writestring
	call readhex
	mov xVal, eax
	call dumpregs
	call crlf 
	

	mov edx, offset str4
	call writeString
	call readhex
	mov yVal, eax
	call crlf
	call dumpregs
	mov eax, xVal
	and eax, yVal

	call writehex

	call crlf
	jmp endprog

	;Or_op
op6:

	cmp eax, 6
	jg op7



	mov edx, offset str3
	call writestring
	call readhex
	mov xVal, eax
	call dumpregs
	call crlf 
	

	mov edx, offset str4
	call writeString
	call readhex
	mov yVal, eax
	call crlf
	call dumpregs
	mov eax, xVal
	or eax, yVal

	call writehex

	call crlf

	jmp endprog
	;Not_op
op7:

	cmp eax, 7
	jg op8
	mov edx, offset str3
	call writestring
	call readhex
	not eax
	call writehex
	call crlf
	jmp endprog
	;XOR_op
op8:
	cmp eax, 8
	jg endprog



	mov edx, offset str3
	call writestring
	call readhex
	mov xVal, eax
	call dumpregs
	call crlf 
	

	mov edx, offset str4
	call writeString
	call readhex
	mov yVal, eax
	call crlf
	call dumpregs
	mov eax, xVal
	xor eax, yVal

	call writehex



	call crlf
	jmp endprog


endprog:
	ret

promptUser ENDP

functions proc
mov edx, offset opt1
call writestring
call crlf

mov edx, offset opt2
call writestring
call crlf

mov edx, offset opt3
call writestring
call crlf

mov edx, offset opt4
call writestring
call crlf

mov edx, offset opt5
call writestring
call crlf 

mov edx, offset opt6
call writestring
call crlf 

mov edx, offset opt7
call writestring
call crlf 

mov edx, offset opt8
call writestring
call crlf 

mov edx, offset opt9
call writestring
call crlf 


ret
functions ENDP



END main