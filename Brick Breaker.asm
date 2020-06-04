[org 0100h]
jmp start
;LOSE
LOSE:
mov bp,sp
 push bp
 push ax
 push bx
 push cx
 push dx
 push si
 push di
 push es
 push 0xB800
 pop es
 mov si,0
 mov di,1180
 mov cx,8
 mov ah,0x05
 PRINT_LOSE1:
 mov al,[you_LOSE+si]
 mov [es:di],ax
 add si,1
 add di,2
 loop PRINT_LOSE1
 mov di,1340
 mov cx,7
 PRINT_LOSE2:
 mov al,[you_LOSE+si]
 mov [es:di],ax
 add si,1
 add di,2
 loop PRINT_LOSE2
 mov di,1840
 mov cx,13
 PRINT_LOSE3:
 mov al,[you_LOSE+si]
 mov [es:di],ax
 add si,1
 add di,2
 loop PRINT_LOSE3
 add di,6
 push di
 push word[Score]
 call printnum1 
 mov di,2000
 mov cx,15
 PRINT_LOSE4:
 mov al,[you_LOSE+si]
 mov [es:di],ax
 add si,1
 add di,2
 loop PRINT_LOSE4
 add di,2
 mov word[es:di],0x0330
 mov di,2160
 mov cx,14
 PRINT_LOSE5:
 mov al,[you_LOSE+si]
 mov [es:di],ax
 add si,1
 add di,2
 loop PRINT_LOSE5
 add di,4
 push di
 push word[Score]
 call printnum1 
 mov cx,20
 SCORE_BOARD1
 call Delay
 loop SCORE_BOARD1
 pop es
 pop di
 pop si
 pop dx
 pop cx
 pop bx
 pop ax
 pop bp
 ret
;WINNING
WIN:
mov bp,sp
 push bp
 push ax
 push bx
 push cx
 push dx
 push si
 push di
 push es
 push 0xB800
 pop es
 mov si,0
 mov di,1180
 mov cx,7
 mov ah,0x05
 PRINT_WIN1:
 mov al,[you_WIN+si]
 mov [es:di],ax
 add si,1
 add di,2
 loop PRINT_WIN1
 mov di,1340
 mov cx,11
 PRINT_WIN2:
 mov al,[you_WIN+si]
 mov [es:di],ax
 add si,1
 add di,2
 loop PRINT_WIN2
 mov di,1840
 mov cx,13
 PRINT_WIN3:
 mov al,[you_WIN+si]
 mov [es:di],ax
 add si,1
 add di,2
 loop PRINT_WIN3
 add di,6
 mov word[es:di],0x0532
 add di,2
 mov word[es:di],0x0534
 cmp word [Second],0
 je no_BONOUS
 mov di,2000
 mov cx,15
 PRINT_WIN4:
 mov al,[you_WIN+si]
 mov [es:di],ax
 add si,1
 add di,2
 loop PRINT_WIN4
 add di,2
 
 mov word[es:di],0x0531
 add di,2
 mov word[es:di],0x0530
 mov di,2160
 mov cx,14
 PRINT_WIN5:
 mov al,[you_WIN+si]
 mov [es:di],ax
 add si,1
 add di,2
 loop PRINT_WIN5
 add di,4
 mov word[es:di],0x0533
 add di,2
 mov word[es:di],0x0534
  jmp WIN_END
 no_BONOUS
 mov word[es:di],0x0530
 add di,2
 mov word[es:di],0x0530
 mov di,2160
 mov cx,14
 PRINT_WIN51:
 mov al,[you_WIN+si]
 mov [es:di],ax
 add si,1
 add di,2
 loop PRINT_WIN51
 add di,4
 mov word[es:di],0x0532
 add di,2
 mov word[es:di],0x0534
 WIN_END:
 mov cx,20
 SCORE_BOARD
 call Delay
 loop SCORE_BOARD
 pop es
 pop di
 pop si
 pop dx
 pop cx
 pop bx
 pop ax
 pop bp
 ret
;GAME OVER
GAME_OVER:
 mov bp,sp
 push bp
 push ax
 push bx
 push cx
 push dx
 push si
 push di
 push es
 push 0xB800
 pop es
 mov ah,0x63
 mov di,1160
 mov si,0
 mov cx,8
 call Delay
 OVER_DETAILS1
 mov byte al,[GAME_OVER_DETAILS+si]
 mov [es:di],ax
 add di,2
 add si,1
 loop OVER_DETAILS1
 mov di,1310
 mov cx,18
 call Delay
 OVER_DETAILS2
 mov byte al,[GAME_OVER_DETAILS+si]
 mov [es:di],ax
 add di,2
 add si,1
 loop OVER_DETAILS2
 mov di,1476
 mov cx,11
 call Delay
 OVER_DETAILS3
 mov byte al,[GAME_OVER_DETAILS+si]
 mov [es:di],ax
 add di,2
 add si,1
 loop OVER_DETAILS3
 
 pop es
 pop di
 pop si
 pop dx
 pop cx
 pop bx
 pop ax
 pop bp
 ret
;LOADING PAGE
 loading_Page:
 mov bp,sp
 push bp
 push ax
 push bx
 push cx
 push dx
 push si
 push di
 push es
 push 0xB800
 pop es
 mov ah,0x13
 mov cx,42
 mov si,0
 mov di,0
 printing_DETAILS1:
 mov byte al,[LOADING_WORDS+si]
 mov [es:di],ax
 add si,1
 add di,2
 loop printing_DETAILS1
 mov cx,6
 d1DETAIL1:
 call Delay
 loop d1DETAIL1
 mov ah,0x53
 add di,150
 mov cx,23
 printing_DETAILS2:
 mov byte al,[LOADING_WORDS+si]
 mov [es:di],ax
 call Delay
 add si,1
 add di,2
 loop printing_DETAILS2
 mov cx,6
 d1DETAIL2:
 call Delay
 loop d1DETAIL2
 mov ah,0x63
 mov di,480
 mov cx,22
 printing_DETAILS3:
 mov byte al,[LOADING_WORDS+si]
 mov [es:di],ax
 add si,1
 add di,2
 loop printing_DETAILS3
 mov cx,6
 d1DETAIL3:
 call Delay
 loop d1DETAIL3
 mov ah,0x71
 mov di,850
 mov cx,15
 printing_DETAILS4:
 mov byte al,[LOADING_WORDS+si]
 mov [es:di],ax
 call Delay
 add si,1
 add di,2
 loop printing_DETAILS4
 mov cx,6
 d1DETAIL4:
 call Delay
 loop d1DETAIL4
 mov ah,0x62
 mov di,1172
 mov cx,13
 printing_DETAILS5:
 mov byte al,[LOADING_WORDS+si]
 mov [es:di],ax
 call Delay
 add si,1
 add di,2
 loop printing_DETAILS5
 mov cx,6
 d1DETAIL5:
 call Delay
 loop d1DETAIL5
 mov ah,0x47
 mov di,3284
 mov cx,35
 printing_DETAILS6:
 mov byte al,[LOADING_WORDS+si]
 mov [es:di],ax
 add si,1
 add di,2
 loop printing_DETAILS6
 mov cx,6
 d1DETAIL6:
 call Delay
 loop d1DETAIL6
 mov ah,0x47
 mov di,3604
 mov cx,19
 printing_DETAILS7:
 mov byte al,[LOADING_WORDS+si]
 mov [es:di],ax
 add si,1
 add di,2
 loop printing_DETAILS7
  mov cx,6
 d1DETAIL7:
 call Delay
 loop d1DETAIL7
 mov ah,0x74
 mov di,2400
 mov cx,10
 printing_DETAILS8:
 mov byte al,[LOADING_WORDS+si]
 mov [es:di],ax
 call Delay
 add si,1
 add di,2
 loop printing_DETAILS8
 
 mov si,2260
 mov cx,60
 printing_DETAILS9:
 mov word[es:di],0x075F
 mov word[es:si],0x075F
 add si,2
 add di,2
 loop printing_DETAILS9
 mov cx,10
 d1DETAIL8:
 call Delay
 loop d1DETAIL8
 sub di,120
 mov cx,60
 printing_DETAILS10:
 call Delay
 call Delay
 mov word[es:di],0x475F
 add di,2
 loop printing_DETAILS10
 mov cx,20
 DETAILS_DELAY
 call Delay
 loop DETAILS_DELAY
 pop es
 pop di
 pop si
 pop dx
 pop cx
 pop bx
 pop ax
 pop bp
 ret
; HOME SCREEN
homescreen:
;SET BP TO SP AND PUSH ALL REGISTERS
mov bp,sp
push bp
push ax
push bx
push cx
push dx
push si
push di
;GAME NAME ON SCREEN
mov di,380
mov ah,0x74
mov cx,23
mov si,0
NAMEING:
mov al,[GAME_NAME+si]
mov word[es:di],ax
add si,1
add di,2
loop NAMEING
;FRIST TWO LINES FOR RECORD LIVES,TIME,SCORE ETC 
;TABLE BAR WITH CHARACTER UNDERSCORE '7F' ASCII OF UNDERSCORE 
mov ax,0x075F
mov di,480
mov cx,80
rep stosw
;TILES ON SCREEN
mov ax,0x485F
mov di,800
mov cx,80
rep stosw ;FIRSTLINE RED
mov ax,0x285F
mov di,1120
mov cx,80
rep stosw ;SECO0NDLINE GREEN
mov ax,0x185F
mov di,1440
mov cx,80
rep stosw ;THIRDLINE BLUE
;INSERT SPACES TO SHOW TILE DIFFERENCE
;STARTING FOR FIRST TILE ENDING ON LAST ONE
mov di,800
mov cx,400
mov ax,0x0720
mov dx,9
spcaes:
add di,2
sub cx,1
sub dx,1
cmp dx,0
jne spcaes
stosw 
mov dx,9
cmp cx,0
jg spcaes 
;BAR ON SCREEN
mov di,3740
mov cx,20
mov ax,0x385F
rep stosw
;PRINT BALL ON SCREEN
mov di,[BallStarting]
mov ax,0x0C02
stosw
;POP ALL REGISTERS
pop di
pop si
pop dx
pop cx
pop bx
pop ax
pop bp
ret 4

BarKbisr:
	push ax
	push es
	push di
	push cx
	push bx
	;mov di,3740

	push 0xb800
	pop es
	mov di, [savedi]  
	in al,0x60
	cmp al,0x4D			; right arrow 4d
	jne NextCheck
	cmp di, 3800
	je NoMatch
	mov ax,[es:di+2]  ; picks from starting point
	mov word[es:di+40], ax
	mov word[es:di], 0x0720
	add di,2
	
NextCheck:
	cmp al,0x4B  ; left arrow
	jne NoMatch
	cmp di, 3680
	je NoMatch
	mov ax,[es:di+38]   
	mov word[es:di],ax  ; copies attribute at start
	mov word[es:di+40], 0x0720   ; places space in last position
	sub di,2
	
NoMatch:
	mov word[savedi],di
	pop bx
	pop cx
	pop di
	pop es
	pop ax

	jmp far [cs:oldisr]  ; call oldisr
	
timer:
	push ax
	cmp word[StartTimer],1
	jne ignore
	cmp word[flag],1
	je ignore
	cmp word[cs:Second],0x0
	je noMorePrinting
	inc word[cs:Counter]
	cmp word[cs:Counter],18
	jne ignore
	dec word[cs:Second]
	push 150
	push word[cs:Second]
	mov word[cs:Counter],0
	call printnum
	jmp ignore
noMorePrinting:
	mov word[flag],1
ignore:
	mov al,0x20
	out 20h,al
	pop ax
	iret
	
printnum:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push es
	push si
	push di
	inc word[loopval]
	push 0xb800
	pop es
	mov ax,[bp+4] ; num
	mov bx,10
	mov cx,0
nextdigit:
	mov dx,0
	div bx
	add dl,0x30
	push dx
	inc cx
	
	cmp ax,0
	jnz nextdigit
	mov di,[bp+6] ; position
nextpos:
	pop dx
	cmp word[loopval], 90
	jge TurnRed
	mov dh,0x02 ; attribute
	jmp NowPrint
TurnRed:
mov dh, 0x8C
NowPrint:

	mov[es:di],dx
	add di,2
	loop nextpos
	cmp word[loopval],110
	jng jump1
	mov word[es:152],0x0720 ; removes the second digit
	jump1:
	cmp word[loopval],20  
	jng  jump
	mov word[es:154], 0x0720	; removes the third digit 
	jump:
	pop di
	pop si
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 4
BallMove:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push es
	push si
	push di
	push bp
	
	Infinite:
	push 0xb800
	pop es
	mov di,[BallStarting]
	mov bp,0
	push 0xb800
	pop es
	mov di,[BallStarting]
	mov bp,0
	mov word[RL],0  ; resetting the direction of ball
	mov word[angle],0 ; angle = 0
	
	mov ah,0
	int 0x16
	mov word[StartTimer],1
	

	LOOP1:  ; UP DIRECTION LOOP
	mov word[Hit],0
	call mxplusc  ; FUNCTION TO KEEP THE ANGLE
	mov word [es: di], 0x0c02  ; places the ball at one place
	mov word[BallPos], di
	cmp word[BallPos],800
	jbe loop2A    ; DOWN DIRECITION LOOP
	call SideWalls ; IF COLLISION WITH SIDE WALLS
	call Delay    ; SLOWS DOWN THE BALL  
   
	mov word [es: di], 0x0720  ; removes the ball from the same place
	sub di,160  ; next position
	
	push di
	call BallBrickCollision
	
	
	cmp word [Hit],1
	 jne LOOP1
	inc word[Score]  ; inc the score
	push 310
	push word[Score]
	call printnum1  ; prints the score
	
	mov dx, word[Score] ; copying current score
		cmp dx,word[WinScore] ; cmp it with win score 
		je winquit1
	
	;------------------------------------------
loop2A:	
mov word[Hit],0 ; reset	
	mov word[es:di],0x0720
	add di,160
	
LOOP2: ; DOWN DIRECTION LOOP

		call mxplusc  ; FUNCTION TO KEEP THE ANGLE
		mov word [es: di], 0x0c02  ; next ball position
		mov word[BallPos], di
		call SideWalls
		call Delay

		mov word [es: di], 0x0720
		add di,160
		
		push di
		call BallBrickCollision
		
		cmp word[Hit],1
		jne nextcmp
		add word[Score],1  ; inc the score
		push 310
		push word[Score]
		call printnum1   ; Prints the score
		
		mov dx, word[Score] ; copying current score
		cmp dx,word[WinScore] ; cmp it with win score 
		je winquit
		
		jmp LOOP1	
nextcmp:
		cmp di, 3840
		jng forward1
		push word[Livesdi]
		call RemoveLives
		add word[Livesdi],4
		cmp word[Livesdi], 480
		jne Infinite
		je quit1
		forward1:
		mov bx,[es:di]
		cmp bh,38h  		; checking contact with bar
jne LOOP2
	;------------------------------
	jmp b
	
	winquit1:
	jmp winquit
	b:
	push di   
	call BallDirection 
	
	sub di,160
	
	mov ah,0
	int 0x16
	cmp al,27
	jne LOOP1
	jmp quit
	
	pop di
	pop si
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 2
		winquit:
	jmp quit
	RemoveLives:
	push bp
	mov bp,sp
	push di
	push ax
	
	push 0xb800
	pop es
	
	push 1000
	call noteon
	call Delay
	call noteoff
	
	mov di,[bp+4]
	mov word[es:di] , 0x0720
	
	pop ax
	pop di
	pop bp
	ret

Delay:
	push bx
	mov bx,0xffff
l2:
	sub bx,1
	jnz l2
	mov bx,0xffff
l3:
	sub bx,1
	jnz l3
	mov bx,0xffff
	
	mov bx,0xffff
	pop bx
	ret
	quit1:
	jmp quit
mxplusc:
	cmp word[RL],1
		jne NotPositive
		add di,[angle]
		jmp OutOFHere
NotPositive:
		cmp word[RL],2
		jne OutOFHere
		sub di,[angle]
		
OutOFHere:
ret
	
SideWalls:

	push ax
	push bx
	push cx
	push dx
	push es
	push si
	push di

	mov ax,[BallPos]	;Left wall
		mov bx,160
		mov dx,0
		div bx
		cmp dx,0
jne NextCmp 
		push 9121
	call noteon
	call Delay
	call noteoff
		mov word[RL],1	 ;MAKE ONE IF NOT ONE
NextCmp:
		cmp dx,158		 ;Right wall
jne Exit
		push 3619
	call noteon
	call Delay
	call noteoff
		mov word[RL],2  ; MAKE TWO IF NOT TWO
		
Exit:
	
	
	
	pop di
	pop si
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	ret 
BallDirection:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push es
	push si
	push di
	
	mov ax,[bp+4]  ;  di - point of Ball-Bar collion 
	mov bx,[savedi] ; start val of bar
	add bx,20   ; mid of bar
	

	cmp ax,bx  ; pos vs mid
	jb Negative
	je zero
	mov word[RL],1 ; +ve
	sub ax,bx		
	jmp next
	Negative:
	mov word[RL],2 ; -ve
	sub bx,ax
	mov ax,bx
	jmp next
	zero:
	mov word[RL],0 ; 0
	mov ax,0
	next:
	
	cmp ax,5  
	jng forward
	mov ax,2
	forward:
	mov word[angle],ax  ; value stored
	
	
	pop di
	pop si
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 2

BallBrickCollision:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push es
	push si
	push di
	
mov ax, [es:di]
	cmp ah,48h				
	jne nextTile  ; cmp with tiles on top
	jmp finish
nextTile:
	cmp ah,28h
	jne LastTile
	jmp finish
LastTile:
	cmp ah, 18h
	jne end1
finish:
	push 3416
	call noteon
	push di		
	call RemoveTile
	call Delay
	call noteoff
	mov word[Hit],1
	end1:


	pop di
	pop si
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 2
	
RemoveTile:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push es
	push si
	push di
	
	push 0xb800
	pop es
	mov ax,[bp+4]  ; val of di
	mov di,ax
	
	mov ax, 0x0720   ;space
	cld
rem:
	stosw
	cmp word[es:di],0x0720
	jne rem

	mov cx,10
	std
rem2:
	stosw
	loop rem2
	
	
	
	
	pop di
	pop si
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 2
	
displayTime:
		push bp
		mov bp,sp
		push ax
		push es
		push di
		push cx
		push si
		
		mov cx,11
		push 0xb800
		pop es
		mov di, 130
		mov si, [bp+6]  ; points
		mov ah, 0x09
	d1:
		lodsb
		stosw
	loop d1
		mov si, [bp+4]  ; Time
		mov ah, 0x08
		mov di, 290
		mov cx,8
	d2:
		lodsb
		stosw
	loop d2
	
		mov cx,8
		mov si, [bp+8] ;lives
		mov di, 450
		mov ah,0x06
		d3:
		lodsb
		stosw
		loop d3
		
		add di,2 ;  di -> 468
		mov ax, 0x0C02
		mov cx,3
		d4:
		stosw ; 468 ->472 -> 476
		add di,2
		loop d4
		
		pop si
		pop cx
		pop di
		pop es
		pop ax
		pop bp
		ret 4

printnum1:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push es
	push si
	push di

	push 0xb800
	pop es
	mov ax,[bp+4] ; num
	mov bx,10
	mov cx,0
nextdigit1:
	mov dx,0
	div bx
	add dl,0x30
	push dx
	inc cx
	
	cmp ax,0
	jnz nextdigit1
	mov di,[bp+6] ; position
nextpos1:
	pop dx
	
	mov dh,0x03 ; attribute

	mov[es:di],dx
	add di,2
	loop nextpos1
	
	pop di
	pop si
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 4

	noteon:

push bp
mov bp,sp
push ax

mov ax, [bp+4] ;frequency

out 42h, al ;output low byte
mov al, ah ;output high byte
out 42h, al

mov al, 61h ;turn on note
mov al, 11b ;set bits 1 and 0
out 61h, al ;send new value

pop ax
pop bp
ret 2
;-------------------END

;-------------------NOTE ENDER
noteoff:
mov al, 61h ;turn off note
out 61h, al ;send new value
ret
	
start:
	mov ax,0x0720
	push 0xb800
	pop es
	mov di,0
	cli
	mov cx,2000
	rep stosw
	;call loading_Page
	mov ax,0x0720
	push 0xb800
	pop es
	mov di,0
	cli
	mov cx,2000
	rep stosw
	push 10
	call homescreen
	push Lives  ; lives
	push TimeLeft ; message pushed
	push Poi;  message of 'point'
	call displayTime
	xor ax,ax
	mov es,ax
	mov ax,[es:9*4]  ; saving orignal values
	mov [oldisr],ax
	mov ax,[es:9*4+2] ; old segment
	mov [oldisr+2],ax	
	cli
	mov word[es:9*4],BarKbisr ; new KB ISR 
	mov [es:9*4+2],cs
	
	mov word[es:8*4], timer  ;Timer ISR
	mov [es:8*4+2],cs
	sti
	
	
	push word[savedi]
	call BallMove
	
	
quit:
push 0xB800
pop es
mov ax,0x0720
mov di,640
mov cx,1760
Deleting
mov word[es:di],0x0720
add di,2
loop Deleting
cmp word [Score],24
jne nextO
call WIN
jmp OVER
nextO:
call LOSE
OVER:
mov cx,20
ddddd:
call Delay
loop ddddd
push 0xB800
pop es
mov ax,0x0720
mov di,640
mov cx,1760
Deleting1
mov word[es:di],0x0720
add di,2
loop Deleting1
call GAME_OVER
mov ax,4c00h
int 21h

oldisr: dd 0
savedi: dw 3740
angle: dw 0
RL: dw 0
Counter: dw 0
Second: dw 120
BallStarting: dw 3600
BallPos: dw 0
Hit: dw 0
TimeLeft: db 'Time Left : '
Poi: db 'Score : '
Lives: db 'Lives : '
loopval: dw 0
flag: dw 0
Score: dw 0
Livesdi: dw 468
WinScore: dw 24
StartTimer: dw 0
LOADING_WORDS:db 'The Game is about a "Ball" and "Bricks"...','Named as "BRICK_BRAKER"','The MAKER OF GAME are:',' MUHAMMAD ZAIN ',' HAMZA NAZIR ','FOR ASSEMBLY_LANGUAGE FINAL PROJECT','MISS SIDRA BASHARAT','Loading-->'
GAME_NAME:db'0-->"BRICK BRAKERS"<--0'
GAME_OVER_DETAILS:db 'GAME END','THANkS FOR PLAYING','ALLAH HAFIZ'
you_WIN:db 'YOU WIN','CONGRATS!!!','YOUR SCORE-->','BOUNUS SCORE-->','TOTAL SCORE-->'
you_LOSE:db 'YOU LOSE','OOPS!!!','YOUR SCORE-->','BOUNUS SCORE-->','TOTAL SCORE-->'