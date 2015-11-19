TITLE Lab#, Program Name, Jake Hall, Due Date

INCLUDE Irvine32.inc 

.data
  ;==================== Misc Variables
  N DWORD 5; size of current grid
  
  levelNum DWORD 1 ;what level player is on (1 is first level, 4 is the last level)
  flg BYTE 9; number of flags avaliable (equal to number of bombs)
  bombTotal BYTE 9; number of bombs total in current level: lv1 = 9, lv2 = 35, lv3 = 79, lv4 = 140 (value ~35% of volume (N times N))
  
  bombExploded BYTE 0

  Counter DWORD 0
  FLAG BYTE 0

  X BYTE 0
  Y BYTE 0

  showBombs BYTE 1; If set to 1, the first grid will print "B" in the location of the bombs when they are generated
  aroundArray BYTE 8 Dup(5)
  aroundArrayX BYTE 8 Dup(5)
  aroundArrayY BYTE 8 Dup(5)
  ;===================== Keeping track of Flags
  flagX BYTE 140 Dup(0)
  flagY BYTE 140 Dup(0)
  FLGLOC DWORD 0

  BombX BYTE 140 Dup(0)
  BombY BYTE 140 Dup(0)

  GridValues BYTE 400 Dup(254) ;ESI values correspond to user input Via (x-1) + ((y-1) * N)

  XYinVals BYTE 6 Dup(0)

  ;====================== Text Color Changes
  GreyTextOnGray = Gray + (white * 16)
  
  lightBlueTextOnGray = lightBlue + (white * 16); 1 bomb Surrounding
  GreenTextOnGray = Green + (white * 16); 2 bombs surrounding
  RedTextOnGray = Red + (white * 16); 3 bombs surrounding
  lightGreenTextOnGray = lightGreen + (white * 16); 4 bombs surrounding
  CyanTextOnGray = Cyan + (white * 16); 5 bombs surrounding
  MagentaTextOnGray = Magenta + (white * 16); 6 bombs surrounding
  lightCyanTextOnGray = lightCyan + (white * 16); 7 bombs surrounding
  lightRedTextOnGray = lightRed + (white * 16); 8 bombs surrounding

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

  winMsg BYTE "Congradulations, you've beaten all 4 levels,",0
  winMsg2 BYTE "Diffusing 263 bombs in total!",0
  winMsg3 BYTE "YOU WIN!!",0

  lvMsg BYTE "You have diffused all ",0
  lvMsg2 BYTE " bombs, moving on to the next level!",0

  lossMsg BYTE "You've Triggered a bomb.....",0
  lossMsg2 BYTE "***GAME OVER***",0
  lossMsg3 BYTE "What you you like to do? Press '1' to restart the level or '2' to quit: ",0

  doneMsg BYTE "Thank you for playing!",0
.code
; - = - = - = - = - = - = - = - = - = - = - =  Starts the Program
main PROC
  mov eax, DefaultColor
  call SetTextColor
  call Randomize
  Call Start
  exit
main ENDP
; - = - = - = - = - = - = - = - = - = - = - = Handles most of the calls are the looping
Start PROC
  jmp restart; to start the game with a clean slate
  nextLevel:
    mov edx, 0
    call LevelUp
	cmp dl, 1
	je win
	restart:
	call resetAll
	call genBombs 
   contLevel:
     Call PrintGrid
     call crlf
     Call Input
     call clrscr
	 call Update

	 cmp dl, 0; nothing
	 je contLevel
	 cmp dl, 1; bombexploded
	 je endGame
	 cmp dl, 2; level up/win
	 je nextLevel

   jmp contLevel
  jmp nextLevel
  win:
    call clrscr

    mov edx, OFFSET winMsg
	call writestring
	call crlf
	mov eax, 250
	call delay

	mov edx, OFFSET winMsg2
	call writestring
	call crlf
    call delay


	mov edx, OFFSET winMsg3
	call writestring
	call crlf
	call crlf
    call delay

	jmp done
  endGame:
    call clrscr
    call Loss
    cmp dl, 1
	je restart
  done:
  mov edx, OFFSET doneMsg
  call writestring
  call crlf
  mov eax, 10000
  call delay
  ret
Start ENDP
; - = - = - = - = - = - = - = - = - = - = - = Checks to see if the bomb flag is high or all the flags match all the bomb XYs. returns 1 for bomb, 2 for flags matching, or 0 if no change
Update PROC uses eax ebx ecx esi edi
  cmp bombExploded, 1
  jne nxt1
    mov dl, 1
	jmp done
  nxt1:
  
  movzx eax, bombTotal
  mov esi, 0
 
  bombLoop:
    movzx ebx, bombTotal
    mov edi, 0
	mov edx, 0
	flagLoop:
	  mov ecx, 0
	  mov ch, flagX[edi]
	  mov cl, flagY[edi]
	  cmp ch, BombX[esi]
	   jne lp
     cmp cl, BombY[esi]
	   jne lp
       mov dl, 1
	  lp:
	inc edi
    dec ebx
	jnz flagLoop
   cmp dl, 0
   je done
  inc esi
  dec eax
  jnz bombLoop
  mov dl, 2; flags and bombs match

  jmp done
  done:
  ret
Update ENDP
; - = - = - = - = - = - = - = - = - = - = - = Sees if the user won(return 1), else handle level up
levelUp PROC uses eax
  mov eax, 4
  cmp levelNum, eax
  je win

  call clrscr
  mov edx, OFFSET lvMsg
  call writestring
  movzx eax, bombTotal
  call writedec
  mov edx, OFFSET lvMsg2
  call writestring
  call crlf
  mov eax, 2000
  call delay
  call crlf
  call crlf
  add N, 5; increase grid size by 5
  inc levelNum; add 1 to the current level


  ; number of bombs total in current level: lv1 = 9, lv2 = 35, lv3 = 79, lv4 = 140 (value ~35% of volume (N times N))
  mov eax, levelNum
  cmp eax, 2
    je two
  cmp eax, 3
    je three
  cmp eax, 4
    je four

  two:
    mov flg, 35
	mov bombTotal, 35
	jmp done
  three:
    mov flg, 79
	mov bombTotal, 79
	jmp done
  four:
    mov flg, 140
	mov bombTotal, 140
	jmp done
  win:
    mov dl, 1
  done:
  ret
levelUp ENDP
; - = - = - = - = - = - = - = - = - = - = - = Sees if the user wants to quit or restart level (1 in dl if restart)
Loss PROC uses eax
  mov edx, OFFSET lossMsg
  call writestring
  call crlf
  mov eax, 250
  call delay

  mov edx, OFFSET lossMsg2
  call writestring
  call crlf
  call delay


  badInput:

  mov eax, 0
  mov edx, OFFSET lossMsg3
  call writestring
  call readint

  mov edx, 0
  cmp eax, 1
    jne chk2
	mov dl, 1
	jmp done
  chk2:
  cmp eax, 2
	je done
	call clrscr
	jmp badInput
  done:
  call crlf
  call crlf
  ret
Loss ENDP
; - = - = - = - = - = - = - = - = - = - = - = Generate the Bomb Locations
genBombs PROC uses eax ebx ecx esi
  mov ecx, 0
  mov cl, bombTotal
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

;============================= For De-bugging

  cmp showBombs, 1 
  jne finBombs
    movzx ecx, bombTotal
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
  finBombs:
  ret
genBombs ENDP
; - = - = - = - = - = - = - = - = - = - = - = Grabs the user's input for what to do (Click, Flag, Remove Flag)
Input PROC
  mov edx, OFFSET inpMsg
  call WriteString
  call readchar
  call writechar
  call crlf

  cmp al, 67; check for C or c
  je Click
  cmp al, 99
  je Click

  cmp al, 70; check for F or f
  je SFlag
  cmp al, 102
  je SFlag
 
  cmp al, 82; check for R or r
  je RFlag
  cmp al, 114
  je RFlag

  jmp Err ; other choices

  Click:
    call userClicked
	call crlf
    jmp cnt
  SFlag:
	cmp flg, 0
	je Err
    Call setFlag
	call crlf
    jmp cnt
  RFlag:
    mov al, flg
	cmp al, bombTotal
	je Err
    Call removeFlag
    jmp cnt
  Err:
    mov edx, OFFSET errorMsg
	call WriteString
	mov eax, 1750
	call Delay
  cnt:
  ret
Input ENDP
; - = - = - = - = - = - = - = - = - = - = - = If the user wants to click
userClicked PROC
  mov edx, OFFSET clickMsg
  call WriteString

  call ValidateInput
  call XYRange
  cmp dl, 1
   je notValidC

  ;DO THINGS HERE
  
  ;Check if bomb
  call checkbombsXY
  cmp dl, 1 
   je Boom
  
  ;check if bomb(s) surround
  call checkAround
  cmp dl, 0
   jg Surrnd

  ;otherwise whitespace
  jmp whiteSpace

  Boom:
    mov bombExploded, 1
    jmp finishC
  Surrnd:
    call convertXYVal
    add dl, 48
    mov GridValues[eax], dl
    jmp finishC
  whiteSpace:
    call whitespaceClick
    jmp finishC
  notValidC:
	call crlf
    mov edx, OFFSET errorMsgSize
	Call WriteString
    
	mov eax, 1750
    call Delay
  
  finishC:
  ret
userClicked ENDP
; - = - = - = - = - = - = - = - = - = - = - = Sets a Flag at user specified X,Y coordinates
setFlag PROC
  mov edx, OFFSET setFlagMsg
  call WriteString

  call ValidateInput
  call XYRange
  cmp dl, 1
  je notValid

  call convertXYVal

  cmp GridValues[eax], 254
  je cnt
  cmp GridValues[eax], 66
  jne notValid

  cnt:
  mov GridValues[eax], 88
  dec flg
  mov ah, X
  mov al, Y
  mov ebx, FLGLOC

  mov flagX[ebx], ah
  mov flagY[ebx], al

  inc FLGLOC

  jmp finish
  notValid:
	call crlf
    mov edx, OFFSET errorMsgSize
	Call WriteString
    
	mov eax, 1750
    call Delay
  
  finish:
  ret
setFlag ENDP
; - = - = - = - = - = - = - = - = - = - = - = Removes a Flag at user specified X,Y coordinates
removeFlag PROC
  mov edx, OFFSET rmFlagMsg
  call WriteString

  call ValidateInput
  call XYRange
  cmp dl, 1
  je notValidrm

  call convertXYVal

  cmp GridValues[eax], 254
  je notValidrm
  cmp GridValues[eax], 66 ; compensate if debug is on and "B"s are on grid
  je notValidrm

  mov GridValues[eax], 254
  inc flg
  
  mov esi, 0
  mov ah, X
  mov al, Y

  movzx ecx, bombTotal
  FindFlag:
    cmp flagX[esi], ah
	jne notMatch
	cmp flagY[esi], al
	jne notMatch
	jmp found
	notMatch:
	 inc esi
  loop FindFlag
  found:

  cmp esi, 139
  jne cnt
    mov flagX[139], 0
	mov flagY[139], 0
  cnt:

  mov flagX[esi], 0;null out the removed flag
  mov flagY[esi], 0
  
  shift:
    mov bh, flagX[esi]
	mov bl, flagY[esi]
	xchg flagX[esi + 1], bh
	xchg flagY[esi + 1], bl
	mov flagX[esi], bh
	mov flagY[esi], bl

	inc esi
  loop shift
    
  dec FLGLOC
  jmp finishRm
  notValidrm:
    mov edx, OFFSET errorMsgSize
	Call WriteString
    
	mov eax , 1750
    call Delay
  
  finishRm:
  ret
removeFlag ENDP
; - = - = - = - = - = - = - = - = - = - = - = Writes out the the shell of the grid, numbering and seperation lines (N X N in size)
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
; - = - = - = - = - = - = - = - = - = - = - = Populates the Minefield with proper Values
GridFill PROC uses eax ebx ecx
  mov ecx, N
  
  fill:
    mov al, GridValues[esi]
    
    cmp al, 49
      je Bombs1
    cmp al, 50
      je Bombs2
    cmp al, 51
      je Bombs3 
    cmp al, 52
      je Bombs4 
    cmp al, 53
      je Bombs5 
    cmp al, 54
      je Bombs6   
    cmp al, 55
      je Bombs7
    cmp al, 56
      je Bombs8
    jmp other
    
    Bombs1:
      mov eax, OFFSET lightBlueTextOnGray
      call SetTextColor
      mov al, GridValues[esi]
     jmp other
    Bombs2:
      mov eax, OFFSET GreenTextOnGray
      call SetTextColor
      mov al, GridValues[esi]
     jmp other
    Bombs3:
      mov eax, OFFSET RedTextOnGray
      call SetTextColor
      mov al, GridValues[esi]
     jmp other
    Bombs4:
      mov eax, OFFSET lightGreenTextOnGray
      call SetTextColor
      mov al, GridValues[esi]
     jmp other
    Bombs5:
      mov eax, OFFSET CyanTextOnGray
      call SetTextColor
      mov al, GridValues[esi]
     jmp other
    Bombs6:
      mov eax, OFFSET MagentaTextOnGray
      call SetTextColor
      mov al, GridValues[esi]
     jmp other
    Bombs7:
      mov eax, OFFSET lightCyanTextOnGray
      call SetTextColor
      mov al, GridValues[esi]
     jmp other
    Bombs8:
      mov eax, OFFSET lightRedTextOnGray
      call SetTextColor
      mov al, GridValues[esi]
    other:
    call writechar
    
    mov eax, OFFSET GreyTextOnGray
    call SetTextColor

    mov ebx, 1
    cmp ecx, ebx
   
    je skp
	  call Space
    skp:	
	  inc esi
   dec ecx
  jnz fill
  ret
GridFill ENDP
; - = - = - = - = - = - = - = - = - = - = - = Takes in user values; double checks they are numbers and stores them in X and Y
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
; - = - = - = - = - = - = - = - = - = - = - = Update GridValues[] with whitespace and bomb values if a user has clicked on whitespace
whitespaceClick PROC uses eax ebx ecx esi edi
  call convertXYval
  mov GridValues[eax], 250
  call updateSurroundings
  
  mov edi, 0
  call clearAroundArray
  call fillAroundArray

  cmp edi, 0 ;if no spaces around it
  je done
    mov ecx, 8
	mov esi, 0

	store:
	  cmp aroundArray[esi], 3
	  jne skp
	    mov bh, aroundArrayX[esi]
		mov bl, aroundArrayY[esi]
		push bx
	  skp:
	  inc esi
	loop store

	mov eax, 0
	mov ecx, edi
	up:
	  pop ax
	  mov X, ah
	  mov Y, al
	  call whitespaceClick
    loop up
  Done:
  ret
whitespaceClick ENDP
; - = - = - = - = - = - = - = - = - = - = - = Does the updating outlined in whitespaceClick
whitespaceUpdate PROC uses eax edx
  call XYRange
  cmp dl, 1
  je LeaveAlone

  call convertXYval

  cmp GridValues[eax], 254 ;check if regular square, not flagged
  jne LeaveAlone

  call checkBombsXY; check to see if bomb at location
  cmp dl, 1
  je LeaveAlone

  call checkAround
  cmp dl, 0
  jg putVal
  
   ;call convertXYval
   ;mov GridValues[eax], 250
   jmp LeaveAlone
  putVal:
    add dl, 48
    call convertXYval
    mov GridValues[eax], dl
  LeaveAlone:
  ret
whitespaceUpdate ENDP
; - = - = - = - = - = - = - = - = - = - = - = check X and Y, puts 1 in dl if there is a bomb at x,y
checkBombsXY PROC uses eax ebx ecx esi
  mov ah, X
  mov al, Y
  
  movzx ecx, bombTotal
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
; - = - = - = - = - = - = - = - = - = - = - = Checks the 8 values around X and Y for bombs, number of bombs is stored in dl
checkAround PROC uses eax ebx ecx esi edi
  mov ch, X
  mov cl, Y
  mov eax, 0
  checkVals:
  ;x-2, y-2
   sub X, 1
   sub Y, 1
   call checkBombsXY
   add al, dl
  ;x-1, y-2
   inc X
   call checkBombsXY
   add al, dl
  ;x-0, y-2
   inc X
   call checkBombsXY
   add al, dl
  ;x-2, y-1
   sub X, 2
   inc Y
   call checkBombsXY
   add al, dl
  ;x-1, y-1
   inc X
   call checkBombsXY
   add al, dl
  ;x-0, y-1
   inc X
   call checkBombsXY
   add al, dl
  ;x-2, y-0
   sub X, 2
   inc Y
   call checkBombsXY
   add al, dl
  ;x-1, y-0
   inc X
   call checkBombsXY
   add al, dl
  ;x-0, y-0
   inc X
   call checkBombsXY
   add al, dl

  mov dl, al
  mov X, ch
  mov Y, cl
  ret
checkAround ENDP
; - = - = - = - = - = - = - = - = - = - = - = Checks to make sure X and Y are within proper range, returns 0 if valid or 1 if invalid
XYRange PROC uses eax ebx ecx
  mov edx, 0
  mov ebx, N
  mov bh, 0
  
  cmp X, bl ;if X > N
    jg bad
  cmp Y, bl ;else if Y > N
    jg bad
  cmp X, 0  ;else if X <= 0
    jle bad
  cmp Y, 0 
    jle bad ;else if Y <= 0
  
  jmp good  ;else
  
  bad:
    mov dl, 1
  good:
  ret
XYRange ENDP
; - = - = - = - = - = - = - = - = - = - = - = Converts X,Y coordinates to a single index value returned as eax
convertXYVal PROC
  ;eax values correspond to user input Via (x-1) + ((y-1) * n)
   mov eax, 0
   mov al, Y
   dec eax
   mov ebx, N
   
   call multiplication
   mov ebx, 0
   mov bl, X
   dec ebx
   
   add eax, ebx
  ret
convertXYVal ENDP
; - = - = - = - = - = - = - = - = - = - = - = Outputs a Space
Space PROC uses eax
  mov al, " "
  call writechar
  ret
Space ENDP
; - = - = - = - = - = - = - = - = - = - = - = Takes in values x,y and updates all area around them to proper values/whitespace
updateSurroundings PROC uses edx edi esi
  mov edi, 0
  mov edx, 0
  mov esi, 0

  ;row above click
  dec X
  dec Y
  call whitespaceUpdate
  inc X
  inc esi
  call whitespaceUpdate
  inc X
  inc esi
  call whitespaceUpdate
  
  ;row of click
  sub X, 2
  inc Y
  inc esi
  call whitespaceUpdate
  inc x
  call whitespaceUpdate
  inc x
  inc esi
  call whitespaceUpdate
  
  ;row below click
  sub X, 2
  inc Y
  inc esi
  call whitespaceUpdate
  inc X
  inc esi
  call whitespaceUpdate
  inc X
  inc esi
  call whitespaceUpdate

  dec X ;return to original values
  dec Y
  ret
updateSurroundings ENDP
; - = - = - = - = - = - = - = - = - = - = - = Checks X,Y values and stores in AroundArray[esi], increases edi if white space
fillAroundArray PROC uses eax edx ecx
  mov edx, 0
  call XYRange
  cmp dl, 1
   je zer
 
  call checkBombsXY
  mov edx, 0
  cmp dl, 1
   je boom
  
  call checkAround
  mov edx, 0
  cmp dl, 0
   jg num

  call convertXYVal
  cmp GridValues[esi], 254
   je whiteSp
  mov aroundArray[esi], 0;assume null if not other option
  jmp next
 
  zer:
   mov aroundArray[esi], 0
   jmp next
  whiteSp:
   mov aroundArray[esi], 3
   mov cl, X
   mov ch, Y
   mov aroundArrayX[esi], cl
   mov aroundArrayY[esi], ch
   inc edi
   jmp next
  boom:
   mov aroundArray[esi], 1
   jmp next
  num:
   mov aroundArray[esi], 2
  next:  
  ret
fillAroundArray ENDP
; - = - = - = - = - = - = - = - = - = - = - = Fill AroundArray[] with zeros
clearAroundArray PROC uses ecx esi
  mov ecx, 8
  mov esi, 0
  Julia:
    mov aroundArray[esi], 5
	mov aroundArrayX[esi], 30
	mov aroundArrayY[esi], 30
    inc esi
  loop Julia
 
  ret
clearAroundArray ENDP
; - = - = - = - = - = - = - = - = - = - = - = takes numbers into ebx and eax, multiplies them, and store the result in eax
multiplication PROC uses ecx
  mov ecx, ebx
  mov ebx, eax
  mov eax, 0
  Julia:
    add eax, ebx
  loop Julia
  ret
multiplication ENDP
; - = - = - = - = - = - = - = - = - = - = - = Resets the Grid contents to blank state (default starting setting)
resetAll PROC uses ecx esi
  mov ecx, 400
  mov esi, 0
  Julia:
    mov GridValues[esi], 254
    inc esi
  loop Julia

  mov ecx, 140
  mov esi, 0
  Julia2:
    mov flagX[esi], 0
    mov flagY[esi], 0

    mov BombX[esi], 0
    mov BombY[esi], 0

	inc esi
  loop Julia2

  mov bombExploded, 0
  mov al, bombTotal
  mov flg, al
  
  mov FLGLOC, 0
  ret
resetAll ENDP
END main