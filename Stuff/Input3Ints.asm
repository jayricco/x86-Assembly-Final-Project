TITLE Input3Ints,AlexF, 10-19-15
INCLUDE Irvine32.inc
.data
varA DWORD ?
varB DWORD ?
varC DWORD ?
greeting DWORD ?
str1 BYTE "Enter an integer: ",0
str2 BYTE "Enter another integer: ",0

.code
main PROC
	call Clrscr
	call readInp
	exit
main ENDP


readInp PROC

	mov edx, offset str1 ;prompt user
	call writestring  ;print value into edx
	call readint ; Ask user for input
	mov varA, eax ; Store that into variable A
	call crlf ; new line


	mov edx, offset str2 ;prompt user
	call writestring  ;print value from edx
	call readint ; Ask user for input
	mov varB, eax ; Store that into variable B
	call crlf ; new line

	call writestring  ;print value into edx
	call readint ; Ask user for input and store into eax
	mov varC, eax ; Store that into variable B
	call crlf ; new line

ret
readInp ENDP


END main