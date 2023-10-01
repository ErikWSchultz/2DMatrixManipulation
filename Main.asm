TITLE Program Template			(main.asm)

; Program Description:		
; Author:					Erik Schultz
; 							based on Kip Irvine's Template
; Date Created:				
; Last Modification Date:	


INCLUDE Irvine32.inc
INCLUDE Macros.inc

.data
MAX_ROWS = 10                                         ; constant for num rows
MAX_COLS = 10                                           ; constant for num cols

my2DArray dword MAX_ROWS * MAX_COLS dup(0)               ; creates 2-d array
spaceStr byte " ",0                                       ; for displaying a space
inputRow byte "Enter Row(Type 99 To Exit): ",0           ; for displaying massage for entering a Row
inputColumn byte "Enter Column:",0                       ; for displaying massage for entering a Col
currentValue byte "The daisies there was:",0           ; for displaying massage for a Current value
inputValue byte "Daisies Gathered:",0                   ; for displaying massage for entering a New Value
Bye byte "Bye!",0										; Displaying end message
displayHeader byte "Filed of Daisies:",0               ; string to display before report
finalMsg byte "Total Daises Gathered:" , 0				;displays a message when the user logs out
numSearchs byte "Number of Gatherings:" ,0				;displays how many searches occured
total dword ?											;total ammount of dasies gathered
searchCount dword 0										;total number of searches
mainPoint dword ?
randRow dword ?                                           ; holds random row
randCol dword ?                                           ; holds random row
randValue dword ?                                       ; holds random value to assign into array
replaceRow dword ?                                       ; holds the replaced Row
replaceColumn dword ?                                   ; holds the replaced Column
newValue dword ?                                       ; holds the new value
																			


addnum sdword 0                                           ; add # for Row
addnumc sdword 0                                       ; add # for Column


.code
main PROC
   
   CALL randomize                   ; stir the pot

    
                             
   MOV ECX, MAX_ROWS * MAX_COLS   ; Move 2-D array to ECX
   MOV EDI, offSet my2DArray       ; display prompt 2-D array

genLoop:
  
   MOV EAX, 100                   ; load EAX with upper bound for random value
   CALL randomRange               ; generate random value 0..upper bound-1
   inc eax
   MOV randValue, EAX
   MOV [EDI], EAX
   add EDI, type my2DArray           ; edi is to stick value into array
  

   loop genLoop


SecondLoop:
   mWrite "Field of Daisies:"      
   Call crlf						;\n
   Call crlf						;\n
                                   ; at this point my2DArray is full of 0s
                                   ; call proc to display array

   CALL displayMy2DArray     
   jmp next
 error:
	mWrite "Error: out of range, please choose a number within the range of the array"
	call crlf
next:
  
   MOV EDX, offset inputRow       ; display prompt inputRow
   CALL writestring               ; display input Row message
   CALL readDec                   ; display random row
   
   MOV replaceRow, EAX            ; store random # into randRow
   push eax						  ; preserve eax
      
   CMP replaceRow, 99               ; quit the program when typing # 99
   JE TakeCare                       ; get number
   MOV randRow, EAX               ; store in memory

   MOV EDX, offset inputColumn       ; display prompt inputColumn
   CALL writestring               ; display input Column
   CALL readDec                   ; get number
   MOV replaceColumn, EAX           ; store in memory
   push eax							;restore eax
   CALL CRLF                       ;\n
  mov eax, MAX_ROWS				;move max rows to eax
  cmp replaceRow, eax			;compare max row to row selected
  Ja error						;jump if above to error
  mov eax, MAX_COLS				;move max columns to eax
  cmp replaceColumn, eax		;compare max columns to column selected
  Ja error						;jump if above to error


   MOV EDX, offset currentValue   ; display prompt currentValue
   CALL writestring               ; display Currently the value message
   MOV EAX, replaceRow               ; get number
   MOV EBX, replaceColumn           ; store in memory

   CALL getElementOfArray           ; get element at [replaceRow][replaceColumn] to newValue
   CALL WriteDec                   ; display get element of array
   CALL CRLF                       ;\n
  
  
  
  
  inc searchcount


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
    MOV EDX, offset inputValue       ; display prompt inputValue
    CALL writestring               ; display input Value message
	mov ecx, eax					;store original number
    mov ebx, 8						;set up for multiplication
	mul  ebx						;multiply by ebx
	mov ebx,10						;set up for div
	div ebx							;divide by ebx
	call writedec					;print
	call crlf

	push eax						;preserve eax
	add total, eax					;add eax to total
	mWrite "Total:"					;label
	mov eax,total					;move total value back into eax
	call writeDec					;print
	call crlf
	mWrite "Number of times collected: "
	mov eax, searchcount
	call writeDec
	call crlf


	pop eax							;restore eax
	call crlf
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	sub ecx, eax					;subtract eax from original
	
	
	pop ebx							;restore ebx to put into setElement of Array
	pop eax							;restore eax
	;call dumpregs
	CALL setElementOfArray        ; get element at [replaceRow][replaceColumn] to newValue

      
  
  
                          
  ; CALL setElementOfArray           ; set element at [replaceRow][replaceColumn] to newValue

  
   JMP SecondLoop                   ; Jump the second loop to TakeCare
  

TakeCare:
	mWrite "Final number of times Collected: "
	mov eax, searchcount
	call writeDec
	call crlf
	mWrite "Final Daisies Collected: "
	mov eax,total							;Bring final total into eax
	call writeDec							;print

	CALL CRLF                           ;\n
	MOV EDX, offset Bye                   ; display prompt Good Bye message
	CALL writestring                   ; display Good Bye message
	CALL CRLF                           ;\n

   exit
main ENDP
       ; =============================================
       ; setElementOfArray procedure expects 3 parameters
       ;   EAX row
       ;   EBX col
       ;   ECX value
       ;
       ; logiCALLy this will be like: my2DArray[EAX,EBX]=ECX
setElementOfArray PROC uses EDX EDI

   MOV EDX, 0               ; prepare for mul

   push EBX               ; put EBX the column on the stack

   MOV EBX, MAX_COLS*4       ; calculate the number of bytes in a row
                           ; since EAX has the rowNum
   mul EBX                   ; EAX is now rowNum * MAX_COLS*4 - total bytes before current row
   MOV EDI, EAX           ; put byte offset into EDI

   pop EAX                   ; we had put EBX on the stack which was the colNum
                           ; put that on EAX

   MOV EBX, 4               ; load EBX with 4 for size of element
   mul EBX                   ; EAX is now colNum*4 which is the byte offset in the current row
   add EDI, EAX           ; EDI is now rowNum * MAX_COLS*4 + colNum*4
  
  
                           ; which is the byte offset from the beginning
                           ; of the array to this element rowNum,colNum
  
   ;call dumpregs
   MOV my2DArray[EDI], ECX ; stick value in ECX into array

  		
  

                           
   ret
setElementOfArray ENDP

       ; =============================================
       ; displayMy2DArray procedure expects no parameters
       ;      
displayMy2DArray PROC uses EDX EDI ECX EAX

   mov ecx, MAX_COLS                   ; move ecx to coulmn
   push eax								;preserve eax
   mov eax, 0							;clear eax
   mWrite " "                       ; Space
   call padOnLeft					;space
   call padOnLeft					;

dispCol:    
   call padOnLeft                           ; display Column Numbers
   call writeDec
   mWrite " "
   mWrite " "							; Space
                         ; pad with spaces
   inc eax
   loop dispCol

   call crlf                           ;\n
   pop eax								;restore eax

   mov ecx, MAX_COLS				;move max columns into ecx
   mov eax, 0
   mWrite " "                       ; Space
   call padOnLeft                       ; pad with spaces
   mWrite" "
   mWrite" "
UnderLine:

   mWrite " ---- "                       ; line of the col
   loop UnderLine                      
   call crlf                           ;\n

   mov edi, 0                           ; load edi with 0 offset

   mov ecx, MAX_ROWS                   ; load ecx with number of rows so we can loop through rows

displayRow:                               ; top of outerloop on rows
  
   Mov eax, addnum						;move row number into eax
   call padOnLeft						;space
   call writedec						;print
   inc addnum							;i++
  
   mWrite" :"
   push ecx                           ; preserve ecx from outloop to use innerloop

   mov ecx, MAX_COLS                   ; load ecx with number of cols so we can loop through cols
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
displayCol:                               ; top of innerloop on cols
   mov eax, my2DArray[edi]               ; move element from array to eax for display
   mWrite " "
    call padOnLeft                       ; pad with spaces
  
                                       ; for debugging purposes show . instead of 0
                       ; display .
      

   .if EAX == 0                       ; if element to display is 0
       push EAX                       ; preserve EAX
       MOV al, '.'                       ; display .
       CALL writeChar
       pop EAX                           ; restore EAX
   .else
       CALL writedec                   ; display element
   .endIf

  

   MOV EDX, offset spaceStr       ; display a space
       CALL writestring                   ; display element
  

   add EDI,4                           ; advance dsi to next element

  
   loop displayCol                       ; bottom of innerloop (loop on cols)

  
   CALL CRLF                           ; now that a row has been displayed, MOVe to beginning of next line for next row

   pop ECX                               ; restore ECX for outerloop on rows

   loop displayRow                       ; bottom of outerloop (loop on rows)
      
  

   mov addnum, 0                       ; Start with 0 # Row
   mov addnumc, 0                       ; Start with 0 # Column

   ret                                   ; done with this method
displayMy2DArray ENDP

       ; =============================================
       ; padOnLeft procedure expects 1 parameters
       ;       EAX with value to pad
       ;      
padOnLeft PROC uses EDX

.if EAX < 1000
       MOV EDX, offset spaceStr           ; display space
       CALL writestring
   .endif

.if EAX < 100
       MOV EDX, offset spaceStr           ; display space
       CALL writestring
   .endif

.if EAX < 10
       MOV EDX, offset spaceStr           ; display space
       CALL writestring
   .endif
      
   ret
padOnLeft ENDP
       ; =============================================
       ;   getElementOfArray procedure expects 2 parameters
       ;   EAX row
       ;   EBX col
       ;
       ; logiCALLy this will be like: my2DArray[EAX,EBX]=ECX
getElementOfArray PROC
  
   MOV EDX, 0               ; prepare for mul

   push EBX               ; put EBX the column on the stack

   MOV EBX, MAX_COLS*4       ; calculate the number of bytes in a row
                           ; since EAX has the rowNum
   mul EBX                   ; EAX is now rowNum * MAX_COLS*4 - total bytes before current row
   MOV EDI, EAX           ; put byte offset into EDI

   pop EAX                   ; we had put EBX on the stack which was the colNum
                           ; put that on EAX

   MOV EBX, 4               ; load EBX with 4 for size of element
   mul EBX                   ; EAX is now colNum*4 which is the byte offset in the current row
   add EDI, EAX           ; EDI is now rowNum * MAX_COLS*4 + colNum*4
  
                           ; which is the byte offset from the beginning
                           ; of the array to this element rowNum,colNum
  
   MOV EAX, my2DArray[EDI] ; stick value in EAX into array
  

	
                          
   ret                       ; Return
getElementOfArray ENDP



; (insert additional procedures here)

END main
