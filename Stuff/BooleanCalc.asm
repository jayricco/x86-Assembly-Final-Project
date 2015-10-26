TITLE Filling an array (main.asm)
INCLUDE Irvine32.inc

.data
selectStr BYTE "Select a function: ", 0
str1 BYTE "Enter element x: ",0
str2 BYTE "Enter element y: ", 0
opt1 BYTE "1. x AND y", 0
opt2 BYTE "2. x OR y", 0
opt3 BYTE "3. NOT x", 0
opt4 BYTE "4. x XOR y", 0
opt5 BYTE "5. Exit Program", 0

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
	jg endprog
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


ret
functions ENDP



END main