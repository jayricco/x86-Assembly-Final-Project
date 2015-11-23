TITLE Lab#, Program Name, Jake Hall, Due Date
.MODEL flat, STDCALL
.386
.STACK 4096
INCLUDE Windows.inc
INCLUDE Irvine32_NOWIN.inc 
INCLUDE /masm32/include/kernel32.inc
INCLUDE /masm32/include/user32.inc
INCLUDELIB /masm32/lib/kernel32.lib
INCLUDELIB /masm32/lib/user32.lib
checkTileInd PROTO, ind:DWORD
padGrid PROTO, numSpace:DWORD
writeGridRow PROTO, rowNum:DWORD
enqueueValidNeighbors PROTO, ind:DWORD
enterCoordinates PROTO, outMessage:DWORD
clickEvent PROTO, X:DWORD, Y:DWORD


setColor MACRO color
	pushad
	mov eax, color
	call setTextColor
	popad
ENDM

putSpace MACRO
	pushad
	mov al, ' '
	call writechar
	popad
ENDM
coordToInd MACRO X, Y, M
LOCAL Xt
LOCAL Yt
.data
Xt DWORD ?
Yt DWORD ?
.code
	mov Xt, X
	mov Yt, Y
	push ebx
	push edx
	mov eax, Yt
	dec eax
	mov ebx, dimension
	mul bx
	shl edx, 16
	mov dx, ax
	add edx, Xt
	dec edx
	mov eax, edx
	mov ebx, M
	mul bx
	shl edx, 16
	mov dx, ax
	mov eax, edx
	pop edx
	pop ebx
ENDM
indToCoord MACRO I, M
LOCAL It
.data
It DWORD ?
.code
mov It, I
push edx
mov edx, 0
mov eax, It
mov ebx, M
div ebx
mov ebx, dimension
div ebx
xchg eax, edx
inc eax
inc edx
mov ebx, edx
pop edx
ENDM
TILE STRUCT
	t_type BYTE ?
	cMem BYTE ?
TILE ENDS

T_BOMB = 1
T_FLAGGEDBOMB = 2
T_FLAG = 3
T_ACTIVE = 4
T_INACTIVE = 5

.data
valueTable BYTE 1, 178, 10h, 10h, 254, 254

bombDet BYTE 0
gameWon BYTE 0
reset BYTE 0
quit BYTE 0

showBombs BYTE 0
showComponents BYTE 0

dimension DWORD 20
maxBombs DWORD 25
totalTiles DWORD ?
numActivated DWORD ?
maxFlags DWORD 25
numFlags DWORD 0

firstClickInd SDWORD -1

Grid TILE 400 DUP(<>)
queue DWORD 400 DUP (0)
qsize DWORD 0
qhead DWORD 0
qtail DWORD 0

currLab BYTE 1

errorFlg DWORD ?
errorOffset DWORD ?

hConWnd HWND ?

;====Text Color Changes====
GrayTextOnWhite = Gray + (white * 16)
WhiteTextOnGray = white + (Gray * 16)
blacktextOnGray = black + (Gray * 16)

RedTextOnWhite = Red + (White * 16)
CyanTextOnGray = Cyan + (Gray * 16)
MagentaTextOnGray = Magenta + (Gray * 16)

lightBlueTextOnWhite = lightBlue + (White * 16)

lightBlueTextOnGray = lightBlue + (Gray * 16)
lightGreenTextOnGray = lightGreen + (Gray * 16)
lightCyanTextOnGray = lightCyan + (Gray * 16)
lightRedTextOnGray = lightRed + (Gray * 16)

DefaultColor = white + (black * 16)
errorColor = lightRed + (black * 16)

;====Strings====
f_Status_1 BYTE "You have ", 0
f_Status_2 BYTE " flags remaining to be placed.", 0

inputPrompt BYTE "Would you like to Click (C), Set a Flag(F), or Remove a Flag (R): ", 0

chooseError BYTE "ERROR: You entered an invalid choice!", 0
inputError BYTE "ERROR: That was an invalid input!", 0

maxFlagError BYTE "ERROR: You have already set all allowed flags, remove one first!", 0
noFlagError BYTE "ERROR: You cannot remove a flag you haven't set, young one...", 0

coordMaxError BYTE "Error: Coordinates are outside of allowed range!", 0
coordError BYTE "Error: Invalid Euclidian coordinates entered!", 0

flag_rCoordError BYTE "Error: There's no flag here to remove...", 0
flag_sCoordError BYTE "Error: That's not a flaggable space.", 0

clickErrFlag BYTE "Can't click there, it's flagged!", 0
clickErrActive BYTE "Can't click there, that location's already been uncovered!", 0

setFlagPrompt BYTE "Enter a location to flag (x,y): ", 0
rmFlagPrompt BYTE "Enter a flag's location to remove (x,y): ", 0
clickPrompt BYTE "Enter a location to click (x,y): ", 0

.code
main PROC
	setColor DefaultColor
	call Randomize
init:
	call generateBoard
gameLoop:
	call printGrid
	call displayError
	call userSelect
	
	cmp reset, 1
	je init
	cmp quit, 1
	je exitGame
	cmp bombDet, 1
	je gameLoss
	cmp gameWon, 1
	je gameWin

gameLoss:
	call youLost
	jmp playAgain
gameWin:
	call youWon
playAgain:
	call againPrompt
	cmp eax, 1
	je init
exitGame:
	;exit
main ENDP

;------------------------------------------
;PROCEDURE: generateBoard
;Initializes the board to a default state 
;INPUT: NONE
;OUTPUT: NONE
;------------------------------------------
generateBoard PROC
	mov edx, 0
	mov eax, LENGTHOF Grid
	sub eax, maxBombs
	mov totalTiles, eax
	mov numActivated, 0

	mov ecx, LENGTHOF Grid
InitBoard:
	mov BYTE PTR Grid[edx].t_type, T_INACTIVE
	mov BYTE PTR Grid[edx].cMem, 0

	add edx, TYPE Grid
	loop InitBoard
	ret
generateBoard ENDP

;------------------------------------------
;PROCEDURE: genBombs
;generates the bombs and initializes stuffs
;INPUT: NONE
;OUTPUT: NONE
;------------------------------------------
genBombs PROC USES eax ebx ecx edx

	mov ebx, 0
	mov ecx, maxBombs
loopGridArray: 

	;Generate Random Y
	mov eax, dimension
	dec eax
	call randomRange
	inc eax
	mov ebx, eax

	;Generate Random X
	mov eax, dimension
	dec eax
	call randomRange
	inc eax 
	
	coordToInd eax, ebx, TYPE Grid
	mov edx, eax
	cmp edx, firstClickInd
	je loopGridArray
	cmp Grid[edx].t_type, T_BOMB
	je loopGridArray
	cmp Grid[edx].t_type, T_FLAGGEDBOMB
	je loopGridArray

	cmp Grid[edx].t_type, T_FLAG
	je turnflagged
	mov BYTE PTR Grid[edx].t_type, T_BOMB
	jmp contLoop

turnflagged:
	mov BYTE PTR Grid[edx].t_type, T_FLAGGEDBOMB
contLoop:
	;mov BYTE PTR Grid[edx].cMem, -1
	dec ecx
	jnz loopGridArray
;====Connected Component Building====
	mov ebx, 0
	mov ecx, LENGTHOF Grid
loopOverTiles:

	cmp Grid[ebx].cMem, 0
	jne skip
	cmp Grid[ebx].t_type, T_INACTIVE
	je allGood
	cmp Grid[ebx].t_type, T_FLAG
	jne skip

	allGood:
	mov al, currLab
	mov Grid[ebx].cMem, al
	;enqueue item
	mov esi, qtail
	mov queue[esi], ebx
	add qtail, TYPE DWORD
	inc qsize
	cmp qtail, SIZEOF queue
	jl no_t_reset
	mov qtail, 0
no_t_reset:

	jmp checkComponents

return:
	inc currLab
skip:
	add ebx, TYPE TILE
	dec ecx
	jnz loopOverTiles
	jmp done

checkComponents:
	;dequeue item
	mov esi, qhead
	mov edi, queue[esi]
	add qhead, TYPE DWORD
	dec qsize
	cmp qhead, SIZEOF queue
	jl no_h_reset
	mov qhead, 0
no_h_reset:
	mov al, currLab
	mov Grid[edi].cMem, al
	invoke enqueueValidNeighbors, edi

continue:
	cmp qsize, 0
	je return
	jmp checkComponents
done:
	ret
genBombs ENDP

;------------------------------------------
;PROCEDURE: userSelect
;Retrieves the user's input
;INPUT: NONE
;OUTPUT: NONE
;------------------------------------------
userSelect PROC USES eax ebx edx

	mov edx, OFFSET inputPrompt
	call WriteString
	call EnterChar
	cmp errorFlg, 1
	je continue
	call CRLF

	and al, 0DFh
	cmp al, 43h ;check for click
	je Click
	cmp al, 46h; check for set flag
	je SetFlag
	cmp al, 52h; check for remove flag
	je RemoveFlag

	jmp Err ;User hasn't entered a valid choice
	
Click:
	invoke enterCoordinates, OFFSET clickPrompt
	cmp eax, -1
	je clickError_coord
	
	cmp firstClickInd, -1
	jne firstDone
	CoordToInd eax, ebx, TYPE Grid
	mov firstClickInd, eax
	call genBombs
	mov eax, firstClickInd
	indToCoord eax, TYPE Grid

	firstDone:
	invoke clickEvent, eax, ebx

	call CRLF
	jmp continue
clickError_coord:
	call printGrid
	call displayError
	jmp Click
clickError_active:
	mov errorFlg, 1
	mov edx, OFFSET clickErrActive
	mov errorOffset, edx
	call printGrid
	call displayError
	jmp Click
clickError_flagged:
	mov errorFlg, 1
	mov edx, OFFSET clickErrFlag
	mov errorOffset, edx
	call printGrid
	call displayError
	jmp Click
	
SetFlag:
	mov eax, maxFlags
	cmp numFlags, eax
	je SFlagError_max

	invoke enterCoordinates, OFFSET setFlagPrompt
	cmp eax, -1
	je SFlagError_coord

	coordToInd eax, ebx, TYPE Grid
	cmp Grid[eax].t_type, T_ACTIVE
	je SFlagError_noFlag

	cmp Grid[eax].t_type, T_INACTIVE
	jne tryBomb
	mov Grid[eax].t_type, T_FLAG
	jmp setFlagDone

tryBomb:
	cmp Grid[eax].t_type, T_BOMB
	jne doNothing
	mov Grid[eax].t_type, T_FLAGGEDBOMB

setFlagDone:
	inc numFlags
doNothing:
	call CRLF
	jmp continue
SFlagError_coord:
	call printGrid
	call DisplayError
	jmp SetFlag
SFlagError_max:
	mov errorFlg, 1
	mov edx, OFFSET maxFlagError
	mov errorOffset, edx
	jmp continue
SFlagError_noFlag:
	mov errorFlg, 1
	mov edx, OFFSET flag_sCoordError
	mov errorOffset, edx
	jmp continue

RemoveFlag:
	cmp numFlags, 0
	je RFlagError_none

	invoke enterCoordinates, OFFSET rmFlagPrompt
	cmp eax, -1
	je RFlagError_coord

	CoordToInd eax, ebx, TYPE Grid

	cmp Grid[eax].t_type, T_FLAG
	je isFlag
	cmp Grid[eax].t_type, T_FLAGGEDBOMB
	je isFBomb
	jmp RFlagError_noFlag
isFlag:
	mov Grid[eax].t_type, T_INACTIVE
	jmp rflagdone
isFBomb:
	mov Grid[eax].t_type, T_BOMB
rflagdone:
	dec numFlags
	call CRLF
	jmp continue

RFlagError_coord:
	call PrintGrid
	call DisplayError
	jmp RemoveFlag
RFlagError_none:
	mov errorFlg, 1
	mov edx, OFFSET noFlagError
	mov errorOffset, edx
	jmp continue
RFlagError_noFlag:
	mov errorFlg, 1
	mov edx, OFFSET flag_rCoordError
	mov errorOffset, edx
	jmp continue

Err:
	mov errorFlg, 1
	mov edx, OFFSET chooseError
	mov errorOffset, edx
continue:
	ret
userSelect ENDP

;-------------------------------------------
;PROCEDURE: clickEvent
;Registers and deals with a click 
;INPUT: X, Y
;OUTPUT: NONE
;-------------------------------------------
clickEvent PROC USES eax esi, X:DWORD, Y:DWORD
	mov eax, X
	mov ebx, Y

	CoordToInd eax, ebx, TYPE Grid
	mov esi, eax
	cmp Grid[esi].t_type, T_BOMB
	je detonation
	cmp Grid[esi].t_type, T_FLAG
	je f_error
	cmp Grid[esi].t_type, T_FLAGGEDBOMB
	je f_error
	cmp Grid[esi].t_type, T_ACTIVE
	je a_error


	mov al, Grid[esi].cMem
	mov edi, 0
	mov ecx, LENGTHOF Grid
board:
	cmp Grid[edi].cMem, al
	jne skip
	mov Grid[edi].t_type, T_ACTIVE
	inc numActivated
	skip:
	add edi, TYPE Grid
	loop board
	jmp done
detonation:
	mov bombDet, 1
	jmp done
f_error:
	mov edx, OFFSET clickErrFlag
	mov errorOffset, edx
	mov errorFlg, 1
	jmp done
a_error:	
	mov edx, OFFSET clickErrActive
	mov errorOffset, edx
	mov errorFlg, 1
done:
	ret
clickEvent ENDP

;------------------------------------------
;PROCEDURE: checkTileInd
;Returns Number of bombs around tile
;INPUT: ind
;OUTPUT: EAX = number of bombs | -1 if invalid coordinate
;------------------------------------------
checkTileInd PROC USES ebx ecx edx, ind:DWORD
LOCAL self:DWORD
LOCAL totalB:DWORD
LOCAL chY:DWORD
LOCAL chX:DWORD

	mov totalB, 0
	mov eax, ind
	mov self, eax
	cmp Grid[eax].t_type, T_BOMB
	je invalid
	cmp Grid[eax].t_type, T_FLAGGEDBOMB
	je invalid

	cmp ind, 0
	jl invalid
	cmp ind, SIZEOF Grid
	jge invalid

	mov eax, dimension
	mov ebx, TYPE Grid
	mul bx
	shl edx, 16
	mov dx, ax
	mov eax, edx
	mov chY, eax

	mov eax, 3
	mov ebx, TYPE Grid
	mul bx
	shl edx, 16
	mov dx, ax
	mov chX, edx

	mov edx, ind
	sub edx, chY
	sub edx, TYPE Grid
	mov ecx, 3
traverseY:
	push ecx
	mov ecx, 3
	traverseX:
		cmp edx, 0
		jl continue
		cmp edx, SIZEOF Grid
		jge continue
		cmp edx, self
		je continue

		mov eax, self
		indToCoord eax, TYPE Grid
		cmp eax, 1
		jne checkOpp
		indToCoord edx, TYPE Grid
		cmp eax, 20
		je continue

		checkOpp:
		mov eax, self
		indToCoord eax, TYPE Grid
		cmp eax, 20
		jne fine
		indToCoord edx, TYPE Grid
		cmp eax, 1
		je continue

		fine:
		cmp Grid[edx].t_type, T_BOMB
		je addOne
		cmp Grid[edx].t_type, T_FLAGGEDBOMB
		je addOne

	continue:
		add edx, TYPE Grid
		dec ecx
		jnz traverseX
	pop ecx
	sub edx, chX
	add edx, chY
	dec ecx
	jnz traverseY
	jmp valid
addOne:
	inc totalB
	jmp continue
invalid:
	mov eax, -1
	jmp done
valid:
	mov eax, totalB
done:
	ret
checkTileInd ENDP

;------------------------------------------
;PROCEDURE: getViableNeighbors
;Returns viable neighbors in the array specified
;INPUT: Index, array offset
;OUTPUT: EAX = number of neigbors
;------------------------------------------
enqueueValidNeighbors PROC USES ebx ecx edx edi, ind:DWORD
LOCAL self:DWORD
LOCAL chY:DWORD
LOCAL chX:DWORD

	mov eax, ind
	mov self, eax

	mov eax, dimension
	mov ebx, TYPE Grid
	mul bx
	shl edx, 16
	mov dx, ax
	mov chY, edx

	mov eax, 3
	mov ebx, TYPE Grid
	mul bx
	shl edx, 16
	mov dx, ax
	mov chX, edx

	mov edx, ind
	sub edx, chY
	sub edx, TYPE Grid

	mov ebx, 0
	mov ecx, 3
traverseY:
	push ecx
	mov ecx, 3
	traverseX:
		;check if out of bounds
		;----------------------
		cmp edx, 0
			jl continue

		cmp edx, SIZEOF Grid
			jge continue
		;----------------------
		mov eax, self
		indToCoord eax, TYPE Grid
		cmp eax, 1
			jne checkOpp
		indToCoord edx, TYPE Grid
		cmp eax, 20
			je continue

	checkOpp:
		mov eax, self
		indToCoord eax, TYPE Grid
		cmp eax, 20
			jne fine
		indToCoord edx, TYPE Grid
		cmp eax, 1
			je continue

	fine:
		cmp edx, self ;Check if not itself
			je continue

		cmp Grid[edx].t_type, T_INACTIVE
			jne continue
		cmp Grid[edx].cMem, 0
			jne continue

		invoke checkTileInd, edx
		cmp eax, 0
			jne wap

		mov al, currLab
		mov Grid[edx].cMem, al
		
		;enqueue
		mov edi, qtail
		mov queue[edi], edx
		add qtail, TYPE DWORD 
		inc qsize
		cmp qtail, SIZEOF queue
			jl continue
		mov qtail, 0
		jmp continue

	wap:
		mov al, currLab
		mov Grid[edx].cMem, al
	continue:
		add edx, TYPE Grid
		dec ecx
		jnz traverseX
skip:
	pop ecx
	sub edx, chX
	add edx, chY
	dec ecx
	jnz traverseY
done:
	movzx eax, bx
    shr ebx, 16
	mov dx, bx
	mov ebx, 4
	div bx
	ret
enqueueValidNeighbors ENDP

;------------------------------------------
;PROCEDURE: printGrid
;Draws the Dim x Dim grid into the console
;INPUT: NONE
;OUTPUT: NONE
;------------------------------------------
printGrid PROC
	
	call ClrScr

	mov edx, OFFSET f_Status_1
	call WriteString

	mov eax, maxFlags
	sub eax, numFlags
	call writeInt

	mov edx, OFFSET f_Status_2
	call WriteString
	call CRLF
	call CRLF

;====X Coordinate Reference====
	invoke padGrid, 4

	mov ecx, dimension

	mov al, 30h
	mov ebx, 1
XRefTens:
	cmp ebx, 0Ah
	jne printNextTen
	inc al
	mov ebx, 0
printNextTen:
	call writechar
	putSpace
	inc ebx
	loop XRefTens

	call CRLF
	mov ecx, dimension

	invoke padGrid, 4

	mov al, 30h
	mov ebx, 0
XRefOnes:
	inc al
	inc ebx
	cmp ebx, 0Ah
	jne prntNextOne
	mov al, 30h
	mov ebx, 0
prntNextOne:
	call WriteChar
	invoke padGrid, 1
	loop XRefOnes

	call CRLF

;====Top of Grid====
	invoke padGrid, 3

	setColor GrayTextOnWhite

	mov al, 201
	call WriteChar
	
	mov al, 205


	mov ecx, dimension
	add ecx, ecx
	dec ecx
seperation:

	call WriteChar
	loop seperation
  
	mov al, 187
	call WriteChar

	setColor DefaultColor
	call CRLF

;===========Grid Body===========
	mov ebx, 30h
	mov eax, 30h

	mov edx, 0
	mov ecx, dimension
row_wisePrint:
	inc al
	cmp al, 3Ah
	jne printNums
	mov al, 30h
	inc bl

printNums:
	xchg al, bl
	call writeChar
	xchg al, bl

	call writeChar
	pushad
;=======Draw Left Wall=======
	putSpace
	setColor GrayTextOnWhite
	mov al, 186
	call WriteChar
;=========Fill Row=========
	pushad
	invoke writeGridRow, dl
	popad
	mov al, 186
	call WriteChar
;====Reset For Next Row====
	setColor DefaultColor
	call CRLF
	popad
	inc dl
	loop row_wisePrint

;====Bottom of Grid====
	putSpace
	putSpace
	putSpace
	setColor GrayTextOnWhite
	mov al, 200
	call WriteChar
	mov ecx, dimension
	add ecx, ecx
	dec ecx
	mov al, 205
bottom:
	call WriteChar
	loop bottom
	mov al, 188
	call WriteChar
	setColor DefaultColor
	call CRLF
	call CRLF

	ret
printGrid ENDP

;------------------------------------------
;PROCEDURE: writeGridRow
;Fills Grid with relevant values
;INPUT: NONE
;OUTPUT: NONE
;------------------------------------------
writeGridRow PROC, rowNum:DWORD

mov eax, dimension
mov ebx, rowNum
mul bx
shl edx, 16
mov dx, ax
mov eax, edx
mov ebx, TYPE Grid
mul bx
shl edx, 16
mov dx, ax
mov edi, edx
mov ecx, dimension
fillRow:
	movzx edx, BYTE PTR Grid[edi].t_type
	mov al, valueTable[edx]

	cmp dl, T_INACTIVE
		je Inactive
	cmp dl, T_ACTIVE
		je Active
	cmp dl, T_FLAGGEDBOMB
		je Flag
	cmp dl, T_FLAG
		je Flag
	cmp dl, T_BOMB
		je Bomb
	jmp Err

Inactive:
	cmp showComponents, 0
	je writeAndCont
	mov al, Grid[edi].cMem
	add al, 30h
	jmp writeAndCont

Active:
	setColor BlackTextOnGray
	push ax
	invoke checkTileInd, edi
	pop dx
	cmp eax, 0
	jne keepASCII

	mov al, dl
	jmp writeAndCont

keepASCII:
	cmp al, 1
		je oneBColor
	cmp al, 2
		je twoBColor
	cmp al, 3
		je threeBColor
	cmp al, 4
		je fourBColor
	cmp al, 5
		je fiveBColor
	cmp al, 6
		je sixBColor
	cmp al, 7
		je sevenBColor
	cmp al, 8
		je eightBColor
	jmp writeAndCont

asciiadjust:
	add al, 30h
	jmp writeAndCont
oneBColor:
	setColor lightBlueTextOnGray
	jmp asciiadjust
twoBColor:
	setColor lightGreenTextOnGray
	jmp asciiadjust
threeBColor:
	setColor lightRedTextOnGray
	jmp asciiadjust
fourBColor:
	setColor lightGreenTextOnGray
	jmp asciiadjust
fiveBColor:
	setColor CyanTextOnGray
	jmp asciiadjust
sixBColor:
	setColor MagentaTextOnGray
	jmp asciiadjust
sevenBColor:
	setColor lightCyanTextOnGray
	jmp asciiadjust
eightBColor:
	setColor lightRedTextOnGray
	jmp asciiadjust

Flag:
	setColor lightBlueTextOnWhite
	jmp writeAndCont

Bomb:
	cmp showBombs, 1
	je showem
	mov al, valueTable[T_INACTIVE]
	jmp writeAndCont
	showem:
	setColor RedTextOnWhite
	jmp writeAndCont

Err:
	hlt

writeAndCont:	
	call WriteChar
    setColor GrayTextOnWhite
    cmp ecx, 1
    je noDivider

	mov al, 179
	call writechar
	
noDivider:
	add edi, TYPE Grid
	dec ecx
	jnz fillRow
	ret
writeGridRow ENDP

;------------------------------------------
;PROCEDURE: padGrid
;adds spaces as padding
;INPUT: number of spaces
;OUTPUT: NONE
;------------------------------------------
padGrid PROC, numSpace:DWORD
	push ecx
	mov ecx, numSpace
loopPad:
	putSpace
	loop loopPad
	pop ecx
	ret
padGrid ENDP

;------------------------------------------
;PROCEDURE: enterChar
;Allows user to enter character; blocks for ENTER press
;INPUT: NONE
;OUTPUT: al = input character
;------------------------------------------
enterChar PROC USES ecx edx
LOCAL charBuff:BYTE
LOCAL conHandle:HANDLE
LOCAL conInfo:CONSOLE_SCREEN_BUFFER_INFO
LOCAL initX:BYTE
LOCAL initY:BYTE

INVOKE GetStdHandle, STD_OUTPUT_HANDLE
mov conHandle, eax
INVOKE GetConsoleScreenBufferInfo, conHandle, ADDR conInfo
mov ax, (COORD PTR conInfo.dwCursorPosition).y
mov initY, al
mov ax, (COORD PTR conInfo.dwCursorPosition).x
mov initX, al

mov eax, 0
mov charBuff, 0
askUser:
	call readchar

	mov dh, initY
	mov dl, initX
	call goToXY

	cmp ax, 011Bh
		je escape
	cmp ax, 1C0Dh
		je charRecieved
	cmp ax, 0E08h
		je backsp
	mov charBuff, al
	call writeChar
	jmp askUser

charRecieved:
	cmp charBuff, 0
	je err
done:
	mov al, charBuff
	ret
backsp:
	mov charBuff, 0
	mov dh, initY
	mov dl, initX
	call goToXY
	mov al, ' '
	call writechar
	jmp askUser
err:
	mov edx, OFFSET inputError
	mov errorOffset, edx
	mov errorFlg, 1
	jmp done
escape:
	mov bombDet, 1
	ret
enterChar ENDP

;------------------------------------------
;PROCEDURE: displayError
;Shows errpr
;INPUT: NONE
;OUTPUT: NONE
;------------------------------------------
displayError PROC USES edx
	cmp errorFlg, 1
	jne done

	setColor errorColor
	mov edx, errorOffset
	call writeString
	setColor DefaultColor
	call CRLF
	mov errorFlg, 0

done:
	ret
displayError ENDP

;------------------------------------------
;PROCEDURE: enterCoordinates
;Allows user to enter coordinates
;INPUT: NONE
;OUTPUT: eax = X, ebx = Y
;------------------------------------------
enterCoordinates PROC USES ecx edx esi edi, outMessage:DWORD
LOCAL X:DWORD
LOCAL Y:DWORD
LOCAL strBuff[20]:BYTE
LOCAL xStr[20]:BYTE
LOCAL yStr[20]:BYTE
LOCAL yStrLen:DWORD
LOCAL xStrLen:DWORD
LOCAL cPos:DWORD
LOCAL cFlag:DWORD
LOCAL inLen:DWORD

	mov edx, outMessage
	call writeString
	
	mov cPos, 0
	mov cFlag, 0
	lea edx, strBuff
	mov ecx, 20
	call readString
	cmp eax, 0
	je invalid
	mov inLen, eax
	mov ebx, 0
	mov esi, 0 ;X
	mov edi, 0 ;Y
	mov ecx, eax
passOne:
	movzx edx, BYTE PTR strBuff[ebx]
	cmp dl, ','
	jne continue
	mov cFlag, 1
	mov cPos, ebx
	jmp nextPass

continue:
	cmp cFlag, 1
	je addToY

	mov BYTE PTR xStr[esi], dl
	inc esi
	jmp nextPass
addToY:
	mov BYTE PTR yStr[edi], dl
	inc edi
nextPass:
	inc ebx
	loop passOne

mov xStrLen, esi
mov yStrLen, edi

mov eax, cPos
cmp cPos, 0
je invalid
mov eax, inLen
dec eax
cmp cPos, eax
je invalid

mov ecx, xStrLen
mov edi, ecx
dec edi
mov esi, 0
mov ebx, 1
convertX:
	movzx eax, BYTE PTR xStr[edi]
	cmp eax, ' '
	je skipX
	cmp eax, 30h
	jl invalid
	cmp eax, 39h
	jg invalid

	sub eax, 30h
	mul bx
	shl edx, 16
	mov dx, ax
	add esi, edx

	mov eax, ebx
	mov ebx, 10
	mul bx
	shl edx, 16
	mov dx, ax
	mov ebx, edx

	dec edi
skipX:
	loop convertX

	mov X, esi
	mov eax, esi

mov ecx, yStrLen
mov edi, ecx
dec edi
mov esi, 0
mov ebx, 1
convertY:
	movzx eax, BYTE PTR yStr[edi]
	cmp eax, ' '
	je skipY
	cmp eax, 30h
	jl invalid
	cmp eax, 39h
	jg invalid

	sub eax, 30h
	mul bx
	shl edx, 16
	mov dx, ax
	add esi, edx

	mov eax, ebx
	mov ebx, 10
	mul bx
	shl edx, 16
	mov dx, ax
	mov ebx, edx

	dec edi
skipY:

	loop convertY
	mov Y, esi
	mov eax, esi
	jmp checkRng

invalid:
	mov errorFlg, 1
	mov edx, OFFSET coordError
	mov errorOffset, edx
	mov eax, -1
	ret

checkRng:
	mov eax, X
	mov ebx, Y

	cmp eax, 0
	jle badDim
	cmp eax, dimension
	jg badDim

	cmp ebx, 0
	jle badDim
	cmp ebx, dimension
	jg badDim

	jmp done
badDim:
	mov errorFlg, 1
	mov edx, OFFSET coordMaxError
	mov errorOffset, edx
	mov eax, -1
	ret
done:
	mov eax, X
	mov ebx, Y
	ret
enterCoordinates ENDP

;------------------------------------------
;PROCEDURE: youLost
;displays raster bitmap telling user that they're dum.
;INPUT: NONE
;OUTPUT: NONE
;------------------------------------------
youLost PROC 
LOCAL maxX:WORD
LOCAL maxY:WORD
LOCAL lInd:DWORD
	ret
youLost ENDP

;------------------------------------------
;PROCEDURE: youWon
;displays raster bitmap telling user that they're great.
;INPUT: NONE
;OUTPUT: NONE
;------------------------------------------
youWon PROC  
	
	ret
youWon ENDP

;------------------------------------------
;PROCEDURE: againPrompt
;displays raster bitmap asking user to play again
;INPUT: NONE
;OUTPUT: NONE
;------------------------------------------
againPrompt PROC
	
	ret
againPrompt ENDP
;------------------------------------------
;PROCEDURE: initRasterDraw
;Initializes everything for writing bitmap to console window
;INPUT: NONE
;OUTPUT: NONE
;------------------------------------------
initDraw PROC
LOCAL szClassName[128]:BYTE
LOCAL tt[128]:BYTE

;lodsb tt, "ttyGrab"


INVOKE GetWindow, ADDR hConWnd, GW_CHILD
mov hConWnd, eax
INVOKE GetClassName, hConWnd, ADDR szClassName, 128
	ret
initDraw ENDP
END main