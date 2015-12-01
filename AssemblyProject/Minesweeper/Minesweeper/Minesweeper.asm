TITLE Assembly Project: Minesweeper, Jay Ricco, Jake Hall, Alex Fallah

.686
.MODEL flat, STDCALL
.STACK 4096

OPTION CASEMAP:NONE
INCLUDE Irvine32_NOWIN.inc
INCLUDELIB Irvine32.lib
INCLUDE Windows.inc
INCLUDE Kernel32.inc
INCLUDELIB Kernel32.lib
INCLUDE User32.inc
INCLUDELIB User32.lib
INCLUDE Gdi32.inc
INCLUDELIB gdi32.lib

checkTileInd PROTO, ind:DWORD
padGrid PROTO, numSpace:DWORD
writeGridRow PROTO, rowNum:DWORD
enqueueValidNeighbors PROTO, ind:DWORD
enterCoordinates PROTO, outMessage:DWORD
initRasterDraw PROTO, fileStr:DWORD

setColor MACRO color
	pushad
	mov eax, color
	invoke SetTextColor
	popad
ENDM

putSpace MACRO
	pushad
	mov al, ' '
	call WriteChar
	popad
ENDM

levelToInd MACRO
	push eax
	push edx
	mov eax, currLevel
	dec eax
	mov ebx, TYPE DWORD
	mul ebx
	mov ebx, eax
	pop edx
	pop eax
ENDM

CoordToInd MACRO X, Y, M
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

IndToCoord MACRO I, M
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
levelWon BYTE 0
reset BYTE 0
quit BYTE 0

showBombs BYTE 0
showComponents BYTE 0

currLevel DWORD 1
maxLevel DWORD 4
levelDimension BYTE 5, 10, 15, 20
dimension DWORD ?
levelMaxBombs BYTE 6, 30, 79, 160
maxBombs DWORD ?
levelMaxFlags BYTE 6, 30, 79, 160
maxFlags DWORD ?

totalTiles DWORD ?
totalTilesSO DWORD ?
numActivated DWORD ?
numFlags DWORD 0

firstClickInd SDWORD -1

Grid TILE 4000 DUP(<>)
queue DWORD 4000 DUP (0)
qsize DWORD 0
qhead DWORD 0
qtail DWORD 0

currLab BYTE 1

loseBMP BYTE "resources\loser.bmp", 0
winBMP BYTE "resources\winner.bmp", 0
againBMP BYTE "resources\again.bmp", 0
splashBMP BYTE "resources\splash.bmp", 0

bmH DWORD ?
bmW DWORD ?
fileBuffer BYTE 3000 DUP(0)
bitmapBuffer BYTE 3000 DUP(0)

rHnd HANDLE ?
oHnd HANDLE ?
numEventsRead DWORD ?
numEventsOccurred DWORD ?
eventBuffer INPUT_RECORD 128 DUP(<>)

Mouse_X DWORD ?
Mouse_Y DWORD ?
H_X DWORD ?
H_Y DWORD ?
PH_X DWORD -1
PH_Y DWORD -1
resetHighlight DWORD 0

clickFlag DWORD ?
bombFlagged DWORD ?

;====Text Color Changes====
GrayTextOnWhite = white + (gray * 16)
WhiteTextOnGray = white + (gray * 16)
BlackTextOnGray = black + (gray * 16)
GrayTextOnGray = gray + (gray * 16)

RedTextOnWhite = red + (white * 16)
CyanTextOnGray = cyan + (gray * 16)
MagentaTextOnGray = magenta + (gray * 16)

lightBlueTextOnWhite = lightBlue + (white * 16)

lightBlueTextOnGray = lightBlue + (gray * 16)
lightGreenTextOnGray = lightGreen + (gray * 16)
lightCyanTextOnGray = lightCyan + (gray * 16)
lightRedTextOnGray = lightRed + (gray * 16)

DefaultColor = white + (black * 16)
ErrorColor = lightRed + (black * 16)

highlight = red + (yellow*16)

;====Strings====
f_Status_1 BYTE "You have ", 0
f_Status_2 BYTE " flags remaining to be placed.", 0

winTitle BYTE "ASM Minesweeper", 0
face BYTE "Terminal", 0

.code
main PROC
LOCAL cI:CONSOLE_CURSOR_INFO
LOCAL sbi:CONSOLE_SCREEN_BUFFER_INFO
LOCAL sbSize:DWORD
LOCAL sbSize_reset:DWORD
LOCAL winsr:SMALL_RECT
LOCAL winsr_reset:SMALL_RECT

	invoke GetStdHandle, STD_INPUT_HANDLE
	mov rHnd, eax
	invoke GetStdHandle, STD_OUTPUT_HANDLE
	mov oHnd, eax

	mov cI.bVisible, 0
	mov cI.dwSize, 100

	mov eax, 30
	shl eax, 16
	mov ax, 80

	mov sbSize, eax

	mov winsr.Left, 0
	mov winsr.Top, 0
	mov winsr.Right, 79
	mov winsr.Bottom, 29

	invoke SetConsoleWindowInfo, oHnd, 1, ADDR winsr

	invoke GetConsoleScreenBufferInfo, oHnd, ADDR sbi
	mov eax, sbi.dwSize
	mov sbSize_reset, eax

	mov ax, sbi.srWindow.Left
	mov winsr_reset.Left, ax
	mov ax, sbi.srWindow.Top
	mov winsr_reset.Top, ax
	mov ax, sbi.srWindow.Right
	mov winsr_reset.Right, ax
	mov ax, sbi.srWindow.Bottom
	mov winsr_reset.Bottom, ax


	invoke SetConsoleScreenBufferSize, oHnd, sbSize
	invoke SetConsoleTitle, OFFSET winTitle

	invoke SetConsoleCursorInfo, oHnd, ADDR cI
	invoke SetConsoleMode, rHnd, ENABLE_LINE_INPUT OR ENABLE_MOUSE_INPUT OR ENABLE_EXTENDED_FLAGS


	call Randomize

	invoke initRasterDraw, OFFSET splashBMP
	call drawSplash
	mov eax, 4000
	call Delay

init:
	
levelLoop:
	mov ecx, SIZEOF bitmapBuffer
	mov ebx, 0

    setColor DefaultColor
	mov ebx, currLevel
	dec ebx
	movzx eax, levelDimension[ebx]
	mov dimension, eax
	movzx eax, levelMaxBombs[ebx]
	mov maxBombs, eax
	movzx eax, levelMaxFlags[ebx]
	mov maxFlags, eax

	mov eax, dimension
	mov ebx, dimension
	mul ebx
	
	mov totalTiles, eax

	mov ebx, TYPE Grid
	mul ebx
	mov totalTilesSO, eax
	
	call Clrscr
	mov bombFlagged, 0
	mov bombDet, 0
	mov reset, 0
	mov quit, 0
	mov levelWon, 0
	mov numFlags, 0
	mov numActivated, 0
	mov numEventsOccurred, 0
	mov numEventsRead, 0
	mov qsize, 0
	mov qhead, 0
	mov qtail, 0
	call generateBoard

gameLoop:
	call PrintGrid

	mov clickFlag, 0
	mouseLoop:
		invoke GetNumberOfConsoleInputEvents, rHnd, OFFSET numEventsOccurred
		cmp numEventsOccurred, 0
			je mouseLoop
		invoke ReadConsoleInput, rHnd, OFFSET eventBuffer, numEventsOccurred, OFFSET numEventsRead
		mov ecx, numEventsRead
		mov esi, OFFSET eventBuffer
		scanEvent:
			cmp (INPUT_RECORD PTR [esi]).EventType, MOUSE_EVENT
			jne checkKey

			cmp (INPUT_RECORD PTR [esi]).MouseEvent.dwEventFlags, MOUSE_MOVED
			jne checkClickL


			cmp resetHighlight, 0
			je doThing
			
			mov eax, PH_X
			mov ebx, PH_Y
			
			mov dh, bl
			mov dl, al
			call Gotoxy

			setColor GrayTextOnWhite

			mov ebx, T_INACTIVE
			mov al, valueTable[ebx]

			call WriteChar
			mov resetHighlight, 0

			doThing:
			movzx eax, (INPUT_RECORD PTR [esi]).MouseEvent.dwMousePosition.x
			test eax, 1h
			jnz continue

			mov H_X, eax

			sub eax, 3
			mov edx, 0
			mov ebx, 2
			div ebx
			add eax, 1

			cmp eax, 1
			jl continue
			cmp eax, dimension
			jg continue

			movzx ebx, (INPUT_RECORD PTR [esi]).MouseEvent.dwMousePosition.y
			mov H_Y, ebx
			sub ebx, 4
			cmp ebx, 1
			jl continue
			cmp ebx, dimension
			jg continue


			CoordToInd eax, ebx, TYPE Grid
			mov edi, eax

			cmp Grid[edi].t_type, T_ACTIVE
			je continue
			cmp Grid[edi].t_type, T_FLAG
			je continue
			cmp Grid[edi].t_type, T_FLAGGEDBOMB
			je continue

			movzx eax, Grid[edi].t_type

			mov eax, H_X
			mov ebx, H_Y
			mov PH_X, eax
			mov PH_Y, ebx
			
			mov resetHighlight, 1

			mov dh, bl
			mov dl, al
			call Gotoxy

			setColor highlight

			mov ebx, T_INACTIVE
			mov al, valueTable[ebx]
			call WriteChar

			jmp continue

		checkClickL:
			test (INPUT_RECORD PTR [esi]).MouseEvent.dwButtonState, FROM_LEFT_1ST_BUTTON_PRESSED
			jz checkClickR
			
			movzx eax, (INPUT_RECORD PTR [esi]).MouseEvent.dwMousePosition.x
			test eax, 1h
			jnz continue

			mov clickFlag, 1
			mov resetHighlight, 0
			sub eax, 3
			mov edx, 0
			mov ebx, 2
			div ebx
			add eax, 1
			cmp eax, 1
			jl continue
			cmp eax, dimension
			jg continue
			movzx ebx, (INPUT_RECORD PTR [esi]).MouseEvent.dwMousePosition.y
			sub ebx, 4
			cmp ebx, 1
			jl continue
			cmp ebx, dimension
			jg continue
			mov Mouse_X, eax
			mov Mouse_Y, ebx

			call MouseClickL

			jmp continue

		checkClickR:
			test (INPUT_RECORD PTR [esi]).MouseEvent.dwButtonState, RIGHTMOST_BUTTON_PRESSED 
			jz continue
			
			movzx eax, (INPUT_RECORD PTR [esi]).MouseEvent.dwMousePosition.x
			test eax, 1h
			jnz continue
			mov clickFlag, 1
			mov resetHighlight, 0
			sub eax, 3
			mov edx, 0
			mov ebx, 2
			div ebx
			add eax, 1
			cmp eax, 1
			jl continue
			cmp eax, dimension
			jg continue
			movzx ebx, (INPUT_RECORD PTR [esi]).MouseEvent.dwMousePosition.y
			sub ebx, 4
			cmp ebx, 1
			jl continue
			cmp ebx, dimension
			jg continue
			mov Mouse_X, eax
			mov Mouse_Y, ebx

			call MouseClickR

		checkKey:
			cmp (INPUT_RECORD PTR [esi]).EventType, KEY_EVENT
			jne continue

			mov eax, (INPUT_RECORD PTR [esi]).KeyEvent.bKeyDown
			cmp eax, 1
			jne continue
			movzx eax, (INPUT_RECORD PTR [esi]).KeyEvent.wVirtualKeyCode
			cmp eax, 1Bh
			je pressedEscape

			jmp continue
		pressedEscape:
			mov quit, 1
			jmp break

		continue:
			setColor DefaultColor
			add esi, TYPE INPUT_RECORD
			dec ecx
			jnz scanEvent
		cmp clickFlag, 0
		je mouseLoop
break:
	cmp reset, 1
	je init
	cmp quit, 1
	je exitGame
	cmp bombDet, 1
	je gameLoss
	cmp levelWon, 1
	je levelWin
	jmp gameLoop
gameLoss:
	invoke initRasterDraw, OFFSET loseBMP
	call youLost
	mov bombDet, 0
	jmp playAgain
gameWin:
	invoke initRasterDraw, OFFSET winBMP
	call youWon
playAgain:
	mov currLevel, 1
	mov eax, 1500
	call Delay
	invoke initRasterDraw, OFFSET againBMP
	call againPrompt
	cmp eax, 1
	je init
	jmp exitGame
levelWin:
	mov eax, currLevel
	cmp eax, maxLevel
	je gameWin
	invoke initRasterDraw, OFFSET winBMP
	call youWon
	mov eax, 1500
	call Delay
	inc currLevel
	mov levelWon, 0
	jmp levelLoop

exitGame:
	invoke SetConsoleScreenBufferSize, oHnd, sbSize_reset
	invoke SetConsoleWindowInfo, oHnd, 1, ADDR winsr_reset
	exit
	ret
main ENDP

drawSplash PROC
	mov eax, bmH
	mov ebx, bmW
	mul ebx
	mov ecx, eax
	mov edi, 0
	mov al, 219
	print:
		mov dl, bitmapBuffer[edi]
		cmp dl, 1
		je colorWhite
		setColor BlackTextOnGray
		jmp writeit1
	colorWhite:
		setColor DefaultColor
	writeit1:
		call WriteChar
		inc edi
	loop print
	ret
drawSplash ENDP

;------------------------------------------
;PROCEDURE: generateBoard
;Initializes the board to a default state 
;INPUT: NONE
;OUTPUT: NONE
;------------------------------------------
generateBoard PROC
	mov edx, 0
	mov eax, totalTiles
	sub eax, maxBombs
	mov totalTiles, eax
	mov numActivated, 0
	mov firstClickInd, -1

	mov qsize, 0
	mov qhead, 0
	mov qtail, 0

	mov ecx, totalTilesSO
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
	mov currLab, 1
	mov ebx, 0
	mov ecx, maxBombs
loopGridArray: 

	;Generate Random Y
	mov eax, dimension
	dec eax
	call RandomRange
	inc eax
	mov ebx, eax

	;Generate Random X
	mov eax, dimension
	dec eax
	call RandomRange
	inc eax 
	
	CoordToInd eax, ebx, TYPE Grid
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
	dec ecx
	jnz loopGridArray

;====Connected Component Building====
	mov ebx, 0
	mov ecx, totalTilesSO
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

;--------------------------------------------
;PROCEDURE MouseClickL
;Handles Mouse Left Click
;INPUT: 
;OUTPUT:
;--------------------------------------------
MouseClickL PROC 
	pushad
	mov eax, Mouse_X
	mov ebx, Mouse_Y

	CoordToInd eax, ebx, TYPE Grid
	cmp firstClickInd, -1
	jne firstDone
	mov firstClickInd, eax
	call genBombs
	mov eax, firstClickInd

	firstDone:
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
	mov ecx, totalTilesSO
board:
	cmp Grid[edi].cMem, al
	jne skip
	mov Grid[edi].t_type, T_ACTIVE
	inc numActivated
	skip:
	add edi, TYPE Grid
	loop board
	mov eax, numActivated
	cmp eax, totalTiles
	je levelwin
	jmp done
levelwin:
	mov levelWon, 1
	jmp done
detonation:
	mov bombDet, 1
	jmp done
f_error:
	mov clickFlag, 0
a_error:	
	mov clickFlag, 0
done:
	popad
	ret
MouseClickL ENDP

;--------------------------------------------
;PROCEDURE MouseClickR
;Handles Mouse Right Click
;INPUT: 
;OUTPUT:
;--------------------------------------------
MouseClickR PROC
	pushad
	CoordToInd eax, ebx, TYPE Grid

	cmp Grid[eax].t_type, T_ACTIVE
	je doNothing


	cmp Grid[eax].t_type, T_INACTIVE
	jne tryBomb
	mov ebx, maxFlags
	cmp numFlags, ebx
	jge doNothing
	mov Grid[eax].t_type, T_FLAG
	inc numFlags
	jmp continue

tryBomb:
	cmp Grid[eax].t_type, T_BOMB
	jne tryFlag
	mov ebx, maxFlags
	cmp numFlags, ebx
	jge doNothing
	mov Grid[eax].t_type, T_FLAGGEDBOMB
	inc numFlags
	inc bombFlagged
	mov eax, bombFlagged
	cmp eax, maxBombs
	jne continue
	jmp levelwin

tryFlag:
	cmp Grid[eax].t_type, T_FLAG
	jne tryBFlag

	mov Grid[eax].t_type, T_INACTIVE
	dec numFlags
	jmp continue

tryBFlag:
	cmp Grid[eax].t_type, T_FLAGGEDBOMB
	jne doNothing
	
	mov Grid[eax].t_type, T_BOMB
	dec numFlags
	dec bombFlagged
	jmp continue

doNothing:
	mov clickFlag, 0
	jmp continue
levelwin:
	mov levelWon, 1
continue:
	popad
	ret
MouseClickR ENDP

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

	mov eax, ind
	cmp eax, 0
	jl invalid
	cmp eax, totalTilesSO
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
		cmp edx, totalTilesSO
		jge continue
		cmp edx, self
		je continue

		mov eax, self
		IndToCoord eax, TYPE Grid
		cmp eax, 1
		jne checkOpp
		IndToCoord edx, TYPE Grid
		cmp eax, dimension
		je continue

		checkOpp:
		mov eax, self
		IndToCoord eax, TYPE Grid
		cmp eax, dimension
		jne fine
		IndToCoord edx, TYPE Grid
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

		cmp edx, totalTilesSO
			jge continue
		;----------------------
		mov eax, self
		IndToCoord eax, TYPE Grid
		cmp eax, 1
			jne checkOpp
		IndToCoord edx, TYPE Grid
		cmp eax, dimension
			je continue

	checkOpp:
		mov eax, self
		IndToCoord eax, TYPE Grid
		cmp eax, dimension
			jne fine
		IndToCoord edx, TYPE Grid
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
			jne resetLab

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

	resetLab:
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
;PROCEDURE: PrintGrid
;Draws the Dim x Dim grid into the console
;INPUT: NONE
;OUTPUT: NONE
;------------------------------------------
PrintGrid PROC
	
	call Clrscr

	mov edx, OFFSET f_Status_1
	call WriteString

	mov eax, maxFlags
	sub eax, numFlags
	call WriteInt

	mov edx, OFFSET f_Status_2
	call WriteString
	call Crlf
	call Crlf

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
	call WriteChar
	putSpace
	inc ebx
	loop XRefTens

	call Crlf
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

	call Crlf

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
	call Crlf

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
	call WriteChar
	xchg al, bl

	call WriteChar
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
	call Crlf
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
	call Crlf
	call Crlf

	ret
PrintGrid ENDP

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
	setColor GrayTextOnGray
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
	setColor lightBlueTextOnGray
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
	call WriteChar
	
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
;PROCEDURE: youLost
;displays raster bitmap telling user that they're dum.
;INPUT: NONE
;OUTPUT: NONE
;------------------------------------------
youLost PROC 
	call Clrscr
	mov eax, bmH
	mov ebx, bmW
	mul ebx
	mov ecx, eax
	mov edi, 0
	mov al, 219
	print:
		mov dl, bitmapBuffer[edi]
		cmp dl, 1
		je colorWhite
		setColor RedTextOnWhite
		jmp writeit
	colorWhite:
		setColor DefaultColor
	writeit:
		call WriteChar
		inc edi
	loop print
	setColor DefaultColor
	ret
youLost ENDP

;------------------------------------------
;PROCEDURE: youWon
;displays raster bitmap telling user that they're great.
;INPUT: NONE
;OUTPUT: NONE
;------------------------------------------
youWon PROC  
	call Clrscr
	mov eax, bmH
	mov ebx, bmW
	mul ebx
	mov ecx, eax
	mov edi, 0
	mov al, 219
	print:
		mov dl, bitmapBuffer[edi]
		cmp dl, 1
		je colorWhite
		setColor lightGreenTextOnGray
		jmp writeit
	colorWhite:
		setColor DefaultColor
	writeit:
		call WriteChar
		inc edi
	loop print
	setColor DefaultColor
	ret
youWon ENDP

;------------------------------------------
;PROCEDURE: againPrompt
;displays raster bitmap asking user to play again
;INPUT: NONE
;OUTPUT: NONE
;------------------------------------------
againPrompt PROC
	call Clrscr
	mov eax, bmH
	mov ebx, bmW
	mul ebx
	mov ecx, eax
	mov edi, 0
	mov al, 219
	print:
		mov dl, bitmapBuffer[edi]
		cmp dl, 1
		je colorWhite
		setColor BlackTextOnGray
		jmp writeit1
	colorWhite:
		setColor DefaultColor
	writeit1:
		call WriteChar
		inc edi
	loop print
	setColor DefaultColor
loopChar:
	call ReadChar
	cmp al, 'y'
	je again
	cmp al, 'n'
	je qt
	jmp loopChar
again:
	mov eax, 1
	jmp done
qt:
	mov eax, 0
done:
	ret
againPrompt ENDP

;------------------------------------------
;PROCEDURE: initRasterDraw
;Initializes everything for writing bitmap to console window
;INPUT: NONE
;OUTPUT: NONE
;------------------------------------------

initRasterDraw PROC USES eax ebx ecx edx edi esi, fileStr:DWORD
LOCAL fHand:DWORD
LOCAL fsize:DWORD
LOCAL w:DWORD
LOCAL h:DWORD
LOCAL ls:DWORD
LOCAL wd:DWORD
LOCAL revj:DWORD
LOCAL i:DWORD
LOCAL j:DWORD
LOCAL fpos:DWORD
LOCAL pos:DWORD

	mov edx, fileStr
	call OpenInputFile
	mov fHand, eax

	mov edx, OFFSET fileBuffer
	mov ecx, 3000
	call ReadFromFile

	mov fsize, eax

	mov eax, fHand
	call CloseFile

	mov edi, 18
	movzx eax, BYTE PTR fileBuffer[edi]
	inc edi
	movzx ebx, BYTE PTR fileBuffer[edi]
	shl ebx, 8
	add eax, ebx
	inc edi
	movzx ebx, BYTE PTR fileBuffer[edi]
	shl ebx, 16
	add eax, ebx
	inc edi
	movzx ebx, BYTE PTR fileBuffer[edi]
	shl ebx, 24
	add eax, ebx
	mov w, eax

	mov edi, 22
	movzx eax, BYTE PTR fileBuffer[edi]
	inc edi
	movzx ebx, BYTE PTR fileBuffer[edi]
	shl ebx, 8
	add eax, ebx
	inc edi
	movzx ebx, BYTE PTR fileBuffer[edi]
	shl ebx, 16
	add eax, ebx
	inc edi
	movzx ebx, BYTE PTR fileBuffer[edi]
	shl ebx, 24
	add eax, ebx
	mov h, eax
	
	mov edx, 0
	mov eax, w
	mov ebx, 8
	div ebx
	mov wd, eax
	mov edx, 0
	mov ecx, eax
	mov ebx, 4
	div ebx
	
	mov eax, ecx
	mov edx, edx
	add eax, edx
	mov ls, eax
	mov eax, h
	mov revj, eax
	dec revj

	mov j, 0
	
	mov ecx, h
loopJ:	
	push ecx

	mov ecx, wd
	mov i, 0
	loopI:
	;generate FPOS
		mov edx, 0
		mov eax, j
		mov ebx, ls
		mul ebx
		add eax, i
		add eax, 62
		mov fpos, eax
	;---------------
	;generate POS

		mov eax, revj 
		mov ebx, w
		mul ebx
		mov edx, eax
		push edx
		mov eax, i
		mov ebx, 8
		mul ebx
		pop edx
		add eax, edx
		
		mov pos, eax
		;----------------
		push ecx
			mov ecx, 0
			mov edx, 8
			loopK:
				mov esi, fpos
				mov eax, 0
				mov al, BYTE PTR fileBuffer[esi]
				shr al, cl
				and al, 1h
				
				mov edi, pos
				add edi, 7
				sub edi, ecx

				mov BYTE PTR bitmapBuffer[edi], al
				
				inc ecx
				dec edx
				cmp edx, 0
				jg loopK
		pop ecx
		;-------------

		inc i
		dec ecx
		cmp ecx, 0
		jg loopI

	pop ecx
	inc j
	dec revj
	dec ecx
	cmp ecx, 0
	jg loopJ

	mov eax, h
	mov bmH, eax
	mov eax, w
	mov bmW, eax

	mov dh, 0
	mov dl, 0
	call Gotoxy

	ret
initRasterDraw ENDP
END main