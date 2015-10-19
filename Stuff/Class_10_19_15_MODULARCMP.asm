TITLE LAB#, ProgName, Author, Date
INCLUDE Irvine32.inc
.data
numa DWORD ?
numb DWORD ? 
numc DWORD ?
p1 BYTE "Please enter a number 'a': ", 0
p2 BYTE "Please enter a number 'b': ", 0
p3 BYTE "Please enter a number 'c': ", 0
o1 BYTE "a is the greatest!", 0Ah, 0
o2 BYTE "b is the greatest!", 0Ah, 0
o3 BYTE "c is the greatest!", 0Ah, 0
.code
main PROC
	;INPUT NUM A
	mov edx, OFFSET p1
	call writeString
	call readInt
	mov numa, eax

	;INPUT NUM B
	mov edx, OFFSET p2
	call writeString
	call readInt
	mov numb, eax

	;INPUT NUM C
	mov edx, OFFSET p3
	call writeString
	call readInt
	mov numc, eax

	;COMPARE AND PRINT
	mov eax, numa
	mov ebx, numb
	mov edx, OFFSET o1
	mov ecx, OFFSET o2
	call greater
	
	mov ebx, numc
	mov ecx, OFFSET o3
	call greater

	call writeString
	exit
main ENDP
;------------------------------------------
;PROCEDURE greater
;PARAMS: eax = val1, ebx = val2, edx = o1str, ecx = o2str
;RETURN: eax = greatest val, edx = output str offset
;------------------------------------------
greater PROC
	cmp eax, ebx
	jge done
	jmp v2greater

	v2greater:
		mov eax, ebx
		mov edx, ecx
		jmp done
	done:
	ret
greater ENDP
END main