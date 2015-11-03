TITLE Lab#, Program Name, Jake Hall, Due Date

INCLUDE Irvine32.inc 

.data
  ;==================== Misc Variables
  total BYTE 0; keeps track of the total number of bombs marked 
  N DWORD 10; size of current grid
  flg BYTE 10; number of flags avaliable (equal to bombs)

  ;keepLooping BYTE 1
  ;nextLevel BYTE 0

  Counter DWORD 0
  FLAG BYTE 0

  X BYTE 0
  Y BYTE 0
  ;===================== Keeping track of Flags
  flagX BYTE 20 Dup(0)
  flagY BYTE 20 Dup(0)

  BombX BYTE 20 Dup(0)
  BombY BYTE 20 Dup(0)

  GridValues BYTE 400 Dup(254) ;ESI values correspond to user input Via (x-1) + ((y-1) * N)

  XYinVals BYTE 6 Dup(0)
  ;====================== Text Color Changes
  GreyTextOnGray = Gray + (white * 16)
  DefaultColor = white + (black * 16)
  ;====================== Strings
  flgsLeft BYTE "You have ",0
  flgsLeft2 BYTE " flags remaining to be placed",0

  inpMsg BYTE "Would you like to Click (C), Set a Flag(F), or Remove a Flag (R): ",0
  errorMsg BYTE "ERROR, Invalid Choice",0
  errorMsgSize BYTE "Error, values are not within valid range",0

  setFlagMsg BYTE "Enter a location to flag (x,y): ",0
  rmFlagMsg BYTE "Enter a flag's location to remove (x,y): ",0
  clickMsg BYTE "Enter a location to click (x,y): ",0
.code
; - = - = - = - = - = - = - = - = - = - = - =  
main PROC
  mov eax, DefaultColor
  call SetTextColor
  call Randomize
  Call Start
  exit
main ENDP
; - = - = - = - = - = - = - = - = - = - = - =
Start PROC
  nextLevel:
    ;call LevelUp ;need to make
	call genBombs 
   contLevel:
     Call PrintGrid
     Call Input
     call clrscr
   jmp contLevel
  jmp nextLevel
  ret
Start ENDP
; - = - = - = - = - = - = - = - = - = - = - =
genBombs PROC  
  movzx ecx, flg
  mov esi, 0
  Julia:    
    mov eax, N
	dec eax
	call randomRange
	mov X, al
	dec eax
	mov eax, N
	call randomRange
	mov Y, al

	inc X
	inc Y

	call checkBombsXY
	cmp dl, 1
  je Julia  
	mov ah, X
	mov al, Y

	mov BombX[esi], ah
	mov BombY[esi], al
	inc esi
  loop Julia

  movzx ecx, flg
  mov esi, 0
  mov eax, 0
  Jul:
     mov eax, 0
	 mov al, BombY[esi]
     dec eax
     mov ebx, N
   
     call multiplication
     mov ebx, 0
     mov bl, BombX[esi]
     dec ebx
   
     add eax, ebx
	 mov GridValues[eax], "B"
	 inc esi
  loop Jul
    call crlf
  ret
genBombs ENDP
; - = - = - = - = - = - = - = - = - = - = - =
checkBombsXY PROC uses eax ebx ecx esi; check X and Y, 1 in dl if bomb location
  mov ah, X
  mov al, Y
  
  movzx ecx, flg
  mov esi, 0
  mov edx, 0

  Julia:
    cmp ah, BombX[esi]
	  jne lp
	cmp al, BombY[esi]
	  jne lp
    
	mov dl, 1
	jmp dne
	
	lp:
	  inc esi
  loop Julia
  dne:
  ret
checkBombsXY ENDP
; - = - = - = - = - = - = - = - = - = - = - =
Input PROC
  mov edx, OFFSET inpMsg
  call WriteString
  call readchar
  call writechar
  call crlf

  mov bl, 67 ; check for C or c
  cmp bl, al
  je Click
  mov bl, 99
  cmp bl, al
  je Click

  mov bl, 70 ; check for F or f
  cmp bl, al
  je SFlag
  mov bl, 102
  cmp bl, al
  je SFlag

  mov bl, 82 ; check for R or r
  cmp bl, al
  je RFlag
  mov bl, 114
  cmp bl, al
  je RFlag

  jmp Err ; other choice

  Click:
    call userClicked
	call crlf
    jmp cnt
  SFlag:
    mov eax, 0
	cmp flg, al
	jle Err
    Call setFlag
	call crlf
    jmp cnt
  RFlag:
    mov EAX, N
	cmp flg, al
	jge Err
    Call removeFlag
    jmp cnt
  Err:
    mov edx, OFFSET errorMsg
	call WriteString
	mov eax, 2500
	call Delay
  cnt:
  ret
Input ENDP
; - = - = - = - = - = - = - = - = - = - = - =
userClicked PROC
  
  ret
userClicked ENDP
; - = - = - = - = - = - = - = - = - = - = - =
setFlag PROC
  mov edx, OFFSET setFlagMsg
  call WriteString

  call ValidateInput
  mov al, 0
  mov ah, 0
  mov ebx, N
  mov ecx, N

  cmp ah, X
  jge notValid
  cmp al, y
  jge notValid

  mov eax, 15
  call writeint

  cmp bl, X
    jl notValid
  cmp cl, Y
    jl notValid

  ;ESI values correspond to user input Via (x-1) + ((y-1) * n)
   mov eax, 0
   mov al, Y
   dec eax
   mov ebx, N
   
   call multiplication
   mov ebx, 0
   mov bl, X
   dec ebx
   
   add eax, ebx

   cmp GridValues[eax], 88
   je notValid

   mov GridValues[eax], 88
   dec flg
  jmp finish

  jmp finish
  notValid:
	call crlf
    mov edx, OFFSET errorMsgSize
	Call WriteString
    
	mov eax,2500
    call Delay
  
  finish:
  ret
setFlag ENDP
; - = - = - = - = - = - = - = - = - = - = - =
removeFlag PROC
  mov edx, OFFSET rmFlagMsg
  call WriteString

  call ValidateInput
  mov al, 0
  mov ah, 0
  mov ebx, N
  mov ecx, N

  cmp ah, X
  jge notValidrm
  cmp al, y
  jge notValidrm

  mov eax, 15
  call writeint

  cmp bl, X
    jl notValidrm
  cmp cl, Y
    jl notValidrm

  ;ESI values correspond to user input Via (x-1) + ((y-1) * n)
   mov eax, 0
   mov al, Y
   dec eax
   mov ebx, N
   
   call multiplication
   mov ebx, 0
   mov bl, X
   dec ebx
   
   add eax, ebx

   cmp GridValues[eax], 254
   je notValidrm

   mov GridValues[eax], 254
   inc flg
  jmp finishRm
  
  notValidrm:
    mov edx, OFFSET errorMsgSize
	Call WriteString
    
	mov eax,2500
    call Delay
  
  finishRm:
  ret
removeFlag ENDP
; - = - = - = - = - = - = - = - = - = - = - =
PrintGrid PROC
  mov edx, OFFSET flgsLeft
  call WriteString
  mov eax, 0
  mov al, flg
  call writeint
  mov edx, OFFSET flgsLeft2
  call WriteString
  call crlf
  call crlf

  call Space
  call Space
  call Space
  call Space

  mov ecx, N
  mov al, 48
  mov bl, 0
  toplp:
    inc ebx
	mov dl, 10
	
    cmp dl, bl
	jne valid

	ten:
	  inc al
	  mov ebx, 0
	valid:
	  call writechar
	call Space
  loop toplp

  call crlf
  
  mov ecx, N

  call Space
  call Space
  call Space
  call Space

  mov al, 48
  mov bl, 0
  toplp2:
    inc al
    inc ebx
	mov dl, 10
	
    cmp dl, bl
	jne valid2
	mov al, 48
	mov ebx, 0
	valid2:
	  call writechar
    call Space
  loop toplp2

  call crlf

  ;= = = = = = = = Seperation Line
  call Space
  call Space
  call Space

  mov eax, GreyTextOnGray
  call SetTextColor

  mov al, 201
  call writechar

  mov ecx, N
  add ecx, ecx ;double to comp for spaces
  dec ecx
  mov al, 205
  
  Seperation:
    call writechar
  loop Seperation
  
  mov al, 187
  call writechar

  mov eax, DefaultColor
  call SetTextColor

  call crlf

  ;=================== Fill with Boxes
  mov ebx, 48; tens place
  mov edx, 49; ones place
  mov counter, 1; counter

  mov ecx, N
  mov esi, 0
  
  Print:
    mov edi, 10
	cmp edi, counter
	jne cont

	tens:
	  inc ebx
	  mov edx, 48
	  mov counter, 0
	cont:
	  mov al, bl ; tens place
	  call WriteChar

	  mov al, dl ; ones place
	  call WriteChar

	  Call Space
       
	  mov eax, GreyTextOnGray
      call SetTextColor
	  
	  mov al, 186
	  call WriteChar

	  inc counter
	  inc edx

    call GridFill

	mov al, 186
	call writechar
   
    mov eax, DefaultColor
    call SetTextColor
	call crlf
  loop Print
  ;=====================Bottom of Grid
  call space
  call space
  call space
  
  mov eax, GreyTextOnGray
  call SetTextColor

  mov al, 200
  call writechar

  mov ecx, N
  add ecx, ecx
  dec ecx

  mov al, 205
  bottom:
    call writechar
  loop bottom

  mov al, 188
  call writechar

  mov eax, DefaultColor
  call SetTextColor
  
  call crlf
  call crlf
  ret
PrintGrid ENDP
; - = - = - = - = - = - = - = - = - = - = - =
GridFill PROC uses eax ebx ecx
  mov ecx, N
  
  fill:
    mov al, GridValues[esi]
    call writechar

    mov ebx, 1
    cmp ecx, ebx
   
    je skp
	  call Space
    skp:	
	  inc esi
  loop fill
  ret
GridFill ENDP
; - = - = - = - = - = - = - = - = - = - = - =
ValidateInput PROC
  mov X, 0
  mov Y, 0
  mov esi, 0
  
  mov edx, OFFSET XYinVals
  mov ecx, 6
  Call readString
 
  cmp eax, 3
  je oneOne
  cmp eax, 4
  je oneTwo
  cmp eax, 5
  je twoTwo
    mov X, 0 ;throw error if not a valid length
	mov Y, 0
	jmp XYFound
  oneOne:
    movzx eax, XYinVals[0]
	sub eax, 48
	mov X, al

	movzx eax, XYinVals[2]
	sub eax, 48
	mov Y, al

	jmp XYFound
  oneTwo:
    cmp XYinVals[2], ","
	je twoOne

	movzx eax, XYinVals[0]
	sub eax, 48
	mov X, al

	movzx eax, XYinVals[2]
	sub eax, 48
	mov ebx, 10

	call multiplication

	movzx ecx, XYinVals[3]
	sub ecx, 48

	add eax, ecx
	mov Y, al

	jmp XYFound
  twoOne:
	movzx eax, XYinVals[0]
	sub eax, 48
	mov ebx, 10

	call multiplication

	movzx ecx, XYinVals[1]
	sub ecx, 48

	add eax, ecx
	mov X, al
	
	movzx eax, XYinVals[3]
	sub eax, 48
	mov Y, al

	jmp XYFound
  twoTwo:
    movzx eax, XYinVals[0]
	sub eax, 48
	mov ebx, 10

	call multiplication

	movzx ecx, XYinVals[1]
	sub ecx, 48

	add eax, ecx
	mov X, al

	movzx eax, XYinVals[3]
	sub eax, 48
	mov ebx, 10

	call multiplication

	movzx ecx, XYinVals[4]
	sub ecx, 48

	add eax, ecx
	mov Y, al
  XYFound:
  ret
ValidateInput ENDP
; - = - = - = - = - = - = - = - = - = - = - =
Space PROC uses eax
  mov al, " "
  call writechar
  ret
Space ENDP
; - = - = - = - = - = - = - = - = - = - = - =
multiplication PROC uses ecx ;numbers into ebx and eax, stores in eax
  mov ecx, ebx
  mov ebx, eax
  mov eax, 0
  Julia:
    add eax, ebx
  loop Julia
  ret
multiplication ENDP
; - = - = - = - = - = - = - = - = - = - = - =
END main