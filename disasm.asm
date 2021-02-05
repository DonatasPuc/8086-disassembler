; Donatas Pučinskas
; 4 grupė, 1 pogrupis

.model small
.stack 100h

BUFF_SIZE EQU 10h

.data
	; FORMATS
;+	; 0. UNRECOGNIZED
;+	; 1. ---- ----                                                            RET 2 variantai
;+	; 2. ---sr ---                                                            PUSH, POP with segment register
;+	; 3. ---- -reg                                                            INC, DEC, PUSH, POP with 2-byte register
;+	; 4. ---- ----  ip_inc8                                                   conditional jumps, LOOP, JMP (vidinis artimas)
;+	; 5. ---- ----  immed8                                                    INT
;+	; 6. ---- ----  ip_inc16                                                  CALL JMP (vidinis tiesioginis)
;+	; 7. ---- ---w  addr16                                                    MOV
;+	; 8. ---- ----  ip_val  cs_val                                            CALL JMP (isorinis tiesioginis)
;+	; 9. ---- ---w  immed8(or immed16 if w=1)                                 ADD, OR, ADC, SBB, AND, SUB, XOR, CMP (operations with accumulator - AX if 2 bytes, AL if 1 byte)
;+	;10. ---- wreg  immed8(or immed16 if w=1)                                 MOV
;+	;11. ---- ----  immed16                                                   RET 2 variantai
;+	;12. ---- --dw  mod reg r/m  [optional disp]                              ADD, OR, ADC, SBB, AND, SUB, XOR, CMP, MOV
;+	;13. ---- --d-  mod -sr r/m  [optional disp]                              MOV
;+	;14. ---- ---w  mod --- r/m  [optional disp]                              MUL, DIV, INC, DEC, TEST
;+	;15. ---- ---w  mod --- r/m  [optional disp]  immed8(or immed16 if w=1)   MOV
;+	;16. ---- --sw  mod --- r/m  [optional disp]  immed8(or immed16 if w=1)   ADD, OR, ADC, SBB, AND, SUB, XOR, CMP
;+	;17. ---- ----  mod --- r/m  [optional disp]                              CALL JMP (vidinis netiesioginis), CALL JMP (isorinis netiesioginis), PUSH, POP
	format_table db 12, 12, 12, 12, 9, 9, 2, 2, 12, 12, 12, 12, 9, 9, 2, 0, 12, 12, 12, 12, 9, 9, 2, 2, 12, 12, 12, 12, 9, 9, 2, 2, 12, 12, 12, 12, 9, 9, 0, 0, 12, 12, 12, 12, 9, 9, 0, 0, 12, 12, 12, 12, 9, 9, 0, 0, 12, 12, 12, 12, 9, 9, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 16, 16, 16, 16, 14, 14, 0, 0, 12, 12, 12, 12, 13, 0, 13, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 7, 7, 7, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 0, 0, 11, 1, 0, 0, 15, 15, 0, 0, 11, 1, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 6, 6, 8, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 14, 14, 0, 0, 0, 0, 0, 0, 14, 17
	
	; ============= VARIABLES ===============
	ip_counter    dw 100h            ; current instruction pointer value
	ip_start      dw ?               ; ip value at the start of reading opcode
	; opcode info
	first_byte    db ?               ; first byte of opcode
	curr_format   db ?               ; format number of current opcode
	sr            db ?               ; segment register number 00b - ES, 01b - CS, 10b - SS, 11b - DS
	d_bit         db ?               ; direction bit
	w_bit         db ?               ; operand size bit (width)
	s_bit         db ?               ; immediate value size bit
	mode          db ?               ; mod field in address byte
	reg           db ?               ; reg field in address byte
	r_m           db ?               ; r/m field in address byte
	disp8         db ?               ; 8-bit optional displacement; MOD = 01b
	disp16        dw ?               ; 16-bit optional displacement; MOD = 10b
	immed8        db ?               ; 8-bit immediate value
	immed16       dw ?               ; 16-bit immediate value
	ip_inc8       db ?               ; 8-bit ip increment
	ip_inc16      dw ?               ; 16-bit ip increment
	ip_val        dw ?               ; new ip value
	cs_val        dw ?               ; new cs value
	addr16        dw ?               ; 16-bit address value
	; segment prefix info
	is_prefix_read    db 0           ; 0 - no segment prefix was read, 1 - segment prefix was read
	prefix_flag       db 0           ; flag set when segment prefix is present in current opcode
	segment_prefix    db ?           ; segment prefix number    00b - ES, 01b - CS, 10b - SS, 11b - DS
	
	; file info
    input_file     db 20 dup (0)
    input_handle   dw ?
	output_file    db 20 dup (0)
	output_handle  dw ?
	; read buffer data
	read_buffer     db BUFF_SIZE dup(?)  ; raw data
	read_buff_pos   dw 0                 ; current position in read_buffer
	read_bytes      dw 0                 ; successfully read bytes
	eof             db 0                 ; end of file flag
	; write buffer data
	write_buffer    db BUFF_SIZE dup(?)  ; raw data
	write_buff_pos  dw 0                 ; current position in write_buffer
	
	; ============== STRINGS ================         ; s_ prefix indicates that it is an asciiz string
	s_enter db 13, 10, 0
	s_unrecognized db 'UNRECOGNIZED', 0
	s_byte_ptr db 'byte ptr ', 0
	s_word_ptr db 'word ptr ', 0
	s_far      db 'far ', 0
	; opcode names
	s_test db 'test', 0
	s_or   db 'or',   0
	s_adc  db 'adc',  0
	s_sbb  db 'sbb',  0
	s_and  db 'and',  0
	s_xor  db 'xor',  0
	s_mov  db 'mov',  0
	s_push db 'push', 0
	s_pop  db 'pop',  0
	s_add  db 'add',  0
	s_inc  db 'inc',  0
	s_sub  db 'sub',  0
	s_dec  db 'dec',  0
	s_cmp  db 'cmp',  0
	s_mul  db 'mul',  0
	s_div  db 'div',  0
	s_call db 'call', 0
	s_ret  db 'ret',  0
	s_jmp  db 'jmp',  0
	s_cond_jmps db 'jo', 0, 0, 'jno', 0, 'jb', 0, 0, 'jae', 0, 'je', 0, 0, 'jne', 0, 'jbe', 0, 'ja', 0, 0, 'js', 0, 0, 'jns', 0, 'jp', 0, 0, 'jnp', 0, 'jl', 0, 0, 'jge', 0, 'jle', 0, 'jg', 0, 0
	s_loop db 'loop', 0
	s_int  db 'int',  0
	; 2 byte registers
	s_ax db 'ax', 0
	s_bx db 'bx', 0
	s_cx db 'cx', 0
	s_dx db 'dx', 0
	s_si db 'si', 0
	s_di db 'di', 0
	s_sp db 'sp', 0
	s_bp db 'bp', 0
	; 1 byte registers
	s_ah db 'ah', 0
	s_al db 'al', 0
	s_bh db 'bh', 0
	s_bl db 'bl', 0
	s_ch db 'ch', 0
	s_cl db 'cl', 0
	s_dh db 'dh', 0
	s_dl db 'dl', 0
	; segment registers
	s_es db 'es', 0
	s_cs db 'cs', 0
	s_ss db 'ss', 0
	s_ds db 'ds', 0
	; register combinations for addressing
	s_bx_si db 'bx+si', 0
	s_bx_di db 'bx+di', 0
	s_bp_si db 'bp+si', 0
	s_bp_di db 'bp+di', 0
	; help strings
    help_str db 'Intel 8088 procesoriaus komandu disasembleris, veikiantis su COM failais.', 13, 10
			 db 'Programos autorius: Donatas Pucinskas 1 kursas, 4 grupe.', 13, 10, 13, 10
			 db 'Programa turi buti paleista su dviem parametrais:', 13, 10
             db '1 param - duomenu failo vardas', 13, 10
			 db '2 param - rezultatu failo vardas$'
	open_err_str    db 'Klaida atidarant duomenu faila.$'
	create_err_str  db 'Klaida sukuriant rezultatu faila.$'
	close_err_str   db 'Klaida uzdarant failus.$'
	read_err_str    db 'Klaida skaitant duomenu faila.$'
	write_err_str   db 'Klaida rasant i rezultatu faila.$'

.code
start:
    mov ax, @data
	mov ds, ax
	
	call parse_parameters
	call prepare_files
	
; ------ algorithm --------
	main_loop:
		cmp byte ptr[eof], 1                
		je finish                            ; end program if all data was read
		
		call print_ip_counter
		mov ax, [ip_counter]
		mov [ip_start], ax
		
	main_loop_prefix:
		call get_byte                        ; read first byte of opcode
		cmp byte ptr[eof], 1                 
		je finish                            ; end program if first byte was not read
		
		mov [first_byte], dl                 
		call print_hex_byte                  ; print first byte of opcode or segment prefix
		
		call check_segment_prefix            ; look for segment prefix
		cmp byte ptr[is_prefix_read], 1
		je main_loop_prefix                  ; continue reading if prefix was found
		
		call get_opcode_info                 ; gather information about opcode and print read bytes
		call print_asm_instruction           ; print instruction based on gathered information
		
		mov dx, offset s_enter   
		call put_string                      ; go to next line after printing instruction
		
		mov byte ptr[prefix_flag], 0         ; reset prefix flag
		jmp main_loop
; -------------------------

	finish:
		call print_buffer    ; print what is left in write_buffer
		call close_files
    exit:
        mov ax,4C00h     
        int 21h 
		
		
; prints ip_counter
print_ip_counter:
	mov ax, [ip_counter]
	mov dl, ah
	call print_hex_byte
	mov dl, al
	call print_hex_byte
	mov dl, ':'
	call put_byte
	mov dl, 9
	call put_byte
RET

; Checks for segment prefix
; Sets is_prefix_read flag 0 or 1 depending whether prefix was read
; Sets prefix_flag 1 if prefix was read
check_segment_prefix:
	cmp byte ptr[first_byte], 26h
	je get_segment_prefix
	cmp byte ptr[first_byte], 2Eh
	je get_segment_prefix
	cmp byte ptr[first_byte], 36h
	je get_segment_prefix
	cmp byte ptr[first_byte], 3Eh
	je get_segment_prefix
	
	mov byte ptr[is_prefix_read], 0
RET

get_segment_prefix:
	mov byte ptr[is_prefix_read], 1
	mov byte ptr[prefix_flag], 1
	mov al, [first_byte]
	and al, 00011000b
	shr al, 1
	shr al, 1
	shr al, 1
	mov [segment_prefix], al
RET


; ============= OPCODE INFO PROCEDURES =============

; Gets format of opcode by first_byte
get_format:
	mov al, [first_byte]
	mov ah, 0
	mov bx, offset format_table
	add bx, ax
	mov dl, [bx]
	mov [curr_format], dl
RET

; Gets all information about opcode and prints all parsed bytes
get_opcode_info:
	call get_format
	
	cmp byte ptr[curr_format], 1
	je check_format_1
	cmp byte ptr[curr_format], 2
	je check_format_2
	cmp byte ptr[curr_format], 3
	je check_format_3
	cmp byte ptr[curr_format], 4
	je check_format_4
	cmp byte ptr[curr_format], 5
	je check_format_5
	cmp byte ptr[curr_format], 6
	je check_format_6
	cmp byte ptr[curr_format], 7
	je check_format_7
	cmp byte ptr[curr_format], 8
	je check_format_8
	cmp byte ptr[curr_format], 9
	je check_format_9
	cmp byte ptr[curr_format], 10
	je check_format_10
	cmp byte ptr[curr_format], 11
	je check_format_11
	cmp byte ptr[curr_format], 12
	je check_format_12
	cmp byte ptr[curr_format], 13
	je check_format_13
	cmp byte ptr[curr_format], 14
	je check_format_14
	cmp byte ptr[curr_format], 15
	je check_format_15
	cmp byte ptr[curr_format], 16
	je check_format_16
	cmp byte ptr[curr_format], 17
	je check_format_17
	
	jmp unknown_format             ; if format is invalid / not implemented
	
	check_format_1:
		call get_info_1
		RET
	check_format_2:
		call get_info_2
		RET
	check_format_3:
		call get_info_3
		RET
	check_format_4:
		call get_info_4
		RET
	check_format_5:
		call get_info_5
		RET
	check_format_6:
		call get_info_6
		RET
	check_format_7:
		call get_info_7
		RET
	check_format_8:
		call get_info_8
		RET
	check_format_9:
		call get_info_9
		RET
	check_format_10:
		call get_info_10
		RET
	check_format_11:
		call get_info_11
		RET
	check_format_12:
		call get_info_12
		RET
	check_format_13:
		call get_info_13
		RET
	check_format_14:
		call get_info_14
		RET
	check_format_15:
		call get_info_15
		RET
	check_format_16:
		call get_info_16
		RET
	check_format_17:
		call get_info_17
		RET
	
	unknown_format:                ; don't get information about unknown opcode
RET

get_info_1:
	; format needs no additional information
RET
	
get_info_2:                        ; get sr field from first byte
	mov al, [first_byte]
	and al, 00011000b
	shr al, 1
	shr al, 1
	shr al, 1
	mov [sr], al
RET

get_info_3:                        ; get reg from first byte
	mov al, [first_byte]
	and al, 00000111b
	mov [reg], al
RET

get_info_4:                        ; get ip_inc8
	call get_byte
	call print_hex_byte
	mov [ip_inc8], dl
RET

get_info_5:                        ; get 1 byte immediate value
	mov byte ptr[w_bit], 0
	call get_immed_by_w
RET

get_info_6:                        ; get ip_inc16
	call get_byte
	call print_hex_byte
	mov al, dl
	call get_byte
	call print_hex_byte
	mov ah, dl
	mov [ip_inc16], ax
RET

get_info_7:                        ; get addr16 and w_bit
	mov al, [first_byte]
	and al, 00000001b
	mov [w_bit], al
	
	call get_byte
	call print_hex_byte
	mov al, dl
	call get_byte
	call print_hex_byte
	mov ah, dl
	mov [addr16], ax
RET

get_info_8:                        ; get ip_val, cs_val
	; ip
	call get_byte
	call print_hex_byte
	mov al, dl
	call get_byte
	call print_hex_byte
	mov ah, dl
	mov [ip_val], ax
	; cs
	call get_byte
	call print_hex_byte
	mov al, dl
	call get_byte
	call print_hex_byte
	mov ah, dl
	mov [cs_val], ax
RET

get_info_9:                        ; get w_bit and immed value
	mov al, [first_byte]
	and al, 00000001b
	mov [w_bit], al
	call get_immed_by_w
RET

get_info_10:                       ; get w_bit and reg from first byte, read immed value
	mov al, [first_byte]
	and al, 00000111b
	mov [reg], al
	mov al, [first_byte]
	shr al, 1
	shr al, 1
	shr al, 1
	and al, 00000001b
	mov [w_bit], al
	call get_immed_by_w
RET

get_info_11:
	mov byte ptr[w_bit], 1
	call get_immed_by_w
RET

get_info_12:                       ; get w_bit, d_bit, read address byte and optional displacement
	mov al, [first_byte]
	and al, 00000001b
	mov [w_bit], al
	mov al, [first_byte]
	and al, 00000010b
	shr al, 1
	mov [d_bit], al
	
	call get_addr_byte
	call get_disp_by_mod
RET

get_info_13:                       ; get d_bit, read address byte and optional displacement, sr from reg
	mov al, [first_byte]
	and al, 00000010b
	shr al, 1
	mov [d_bit], al
	
	call get_addr_byte
	call get_disp_by_mod
	
	mov al, [reg]
	and al, 011b
	mov [sr], al
RET

get_info_14:                       ; get w_bit, read address byte and optional displacement
	mov al, [first_byte]
	and al, 00000001b
	mov [w_bit], al
	
	call get_addr_byte
	call get_disp_by_mod
RET

get_info_15:                       ; get w_bit, read address byte, optional displacement and immed value
	mov al, [first_byte]
	and al, 00000001b
	mov [w_bit], al
	
	call get_addr_byte
	call get_disp_by_mod
	call get_immed_by_w
RET

get_info_16:                       ; get w_bit, s_bit, read address byte, optional displacement and immed value (1 byte if s=1 and w=1)
	mov al, [first_byte]
	and al, 00000001b
	mov [w_bit], al
	mov al, [first_byte]
	and al, 00000010b
	shr al, 1
	mov [s_bit], al
	
	call get_addr_byte
	call get_disp_by_mod
	
	cmp byte ptr[w_bit], 0
	je no_sign_extention
	cmp byte ptr[s_bit], 0
	je no_sign_extention
	
	; get only 1 byte immed
	mov byte ptr[w_bit], 0
	call get_immed_by_w
	mov byte ptr[w_bit], 1
	; sign extend it
	mov al, [immed8]
	call sign_extend
	mov [immed16], ax
RET
	no_sign_extention:
	call get_immed_by_w
RET

get_info_17:                       ; read address byte and optional displacement
	call get_addr_byte
	call get_disp_by_mod
	
	; This format is sometimes ambiguous. If reg is 0 or 1, then also get w_bit and set format to 14
	cmp byte ptr[reg], 0
	je get_info_17_ambiguous
	cmp byte ptr[reg], 1
	je get_info_17_ambiguous
	
	mov byte ptr[w_bit], 1         ; this format always operates on 2-byte registers
RET
	get_info_17_ambiguous:
	mov al, [first_byte]
	and al, 00000001b
	mov [w_bit], al
	
	mov byte ptr[curr_format], 14
RET


; Reads and prints address byte. Parses it's information into mod, reg, r/m.
; USING:  get_byte, print_hex_byte
; RETURN: mode, reg, r_m
PROC get_addr_byte
	push ax
	push dx
		call get_byte
		call print_hex_byte
		
		; mod
		mov al, dl
		and al, 11000000b
		mov cl, 6
		shr al, cl
		mov [mode], al
		
		; reg
		mov al, dl
		and al, 00111000b
		mov cl, 3
		shr al, cl
		mov [reg], al
		
		; r/m
		mov al, dl
		and al, 00000111b
		mov [r_m], al
	pop dx
	pop ax
	RET
ENDP get_addr_byte

; Reads disp8 if mod = 1. Reads disp16 if mod = 10. Reads addr16 if mod = 0 and r/m = 110. Reads no bytes otherwise.
; Prints parsed bytes
; USING:  get_byte, print_hex_byte
; INPUT:  mode, r_m
; RETURN: disp8 or disp16 or addr16
PROC get_disp_by_mod
	push ax
	push dx
		cmp byte ptr[mode], 0
		jne get_disp_not_0
		cmp byte ptr[r_m], 110b
		je get_direct_addr
		
		get_disp_not_0:
		cmp byte ptr[mode], 01b
		je get_disp_mod01
		cmp byte ptr[mode], 10b
		je get_disp_mod10
		
		jmp get_disp_by_mod_end
		
		get_disp_mod01:
			call get_byte
			call print_hex_byte
			mov [disp8], dl
		jmp get_disp_by_mod_end
		
		get_disp_mod10:
			call get_byte
			call print_hex_byte
			mov al, dl
			call get_byte
			call print_hex_byte
			mov ah, dl
			mov [disp16], ax
		jmp get_disp_by_mod_end
		
		get_direct_addr:
			call get_byte
			call print_hex_byte
			mov al, dl
			call get_byte
			call print_hex_byte
			mov ah, dl
			mov [addr16], ax
		jmp get_disp_by_mod_end
		
get_disp_by_mod_end:
	pop dx
	pop ax
	RET
ENDP get_disp_by_mod

; Reads next byte into immed8 if w=0. Reads 2 next bytes into immed16 if w=1
; Prints parsed bytes
; USING:  get_byte, print_hex_byte
; INPUT:  w_bit
; RETURN: immed8 or immed16
PROC get_immed_by_w
	push ax
	push dx
		cmp byte ptr[w_bit], 1
		je get_immed_w1
		
		get_immed_w0:
			call get_byte
			call print_hex_byte
			mov [immed8], dl
		jmp get_immed_end
		
		get_immed_w1:
			call get_byte
			call print_hex_byte
			mov al, dl
			call get_byte
			call print_hex_byte
			mov ah, dl
			mov [immed16], ax

get_immed_end:
	pop dx
	pop ax
	RET
ENDP get_immed_by_w



; ================ CALCULATION PROCEDURES ===================

; Returns ip_counter value incremented by ax
; INPUT:  ax - increment value, ip_counter - current ip value
; RETURN: ax - incremented value
PROC get_incremented_ip
		add ax, word ptr[ip_counter]
	RET
ENDP get_incremented_ip

; Sign extends given byte
; INPUT:  al
; RETURN: ax
PROC sign_extend
		cmp al, 7Fh
		ja sign_extend_negative
		
		mov ah, 0
		jmp sign_extend_end
		
		sign_extend_negative:
		mov ah, 0FFh
sign_extend_end:
	RET
ENDP sign_extend



; ============= INSTRUCTION PRINTING PROCEDURES =============

; prints assembly instruction by format and gathered information
print_asm_instruction:
	mov dl, 9
	call put_byte
	
	mov ax, [ip_counter]              ; check if second tab is needed
	sub ax, [ip_start]
	cmp ax, 3
	ja long_opcode
	
	mov dl, 9
	call put_byte
	long_opcode:
	
	cmp byte ptr[curr_format], 1
	je pr_format_1
	cmp byte ptr[curr_format], 2
	je pr_format_2
	cmp byte ptr[curr_format], 3
	je pr_format_3
	cmp byte ptr[curr_format], 4
	je pr_format_4
	cmp byte ptr[curr_format], 5
	je pr_format_5
	cmp byte ptr[curr_format], 6
	je pr_format_6
	cmp byte ptr[curr_format], 7
	je pr_format_7
	cmp byte ptr[curr_format], 8
	je pr_format_8
	cmp byte ptr[curr_format], 9
	je pr_format_9
	cmp byte ptr[curr_format], 10
	je pr_format_10
	cmp byte ptr[curr_format], 11
	je pr_format_11
	cmp byte ptr[curr_format], 12
	je pr_format_12
	cmp byte ptr[curr_format], 13
	je pr_format_13
	cmp byte ptr[curr_format], 14
	je pr_format_14
	cmp byte ptr[curr_format], 15
	je pr_format_15
	cmp byte ptr[curr_format], 16
	je pr_format_16
	cmp byte ptr[curr_format], 17
	je pr_format_17
	jmp pr_unknown               ; if format is invalid / not implemented
	
	pr_format_1:
		call print_format_1
		RET
	pr_format_2:
		call print_format_2
		RET
	pr_format_3:
		call print_format_3
		RET
	pr_format_4:
		call print_format_4
		RET
	pr_format_5:
		call print_format_5
		RET
	pr_format_6:
		call print_format_6
		RET
	pr_format_7:
		call print_format_7
		RET
	pr_format_8:
		call print_format_8
		RET
	pr_format_9:
		call print_format_9
		RET
	pr_format_10:
		call print_format_10
		RET
	pr_format_11:
		call print_format_11
		RET
	pr_format_12:
		call print_format_12
		RET
	pr_format_13:
		call print_format_13
		RET
	pr_format_14:
		call print_format_14
		RET
	pr_format_15:
		call print_format_15
		RET
	pr_format_16:
		call print_format_16
		RET
	pr_format_17:
		call print_format_17
		RET
	
	pr_unknown:
		call print_unknown
		RET
RET

print_format_1:                      ; prints name
	call print_opcode_name
RET

print_format_2:                      ; prints name and segment register
	call print_opcode_name
	mov dl, 9
	call put_byte
	
	mov al, [sr]
	call print_sr
RET

print_format_3:                      ; prints name and 2-byte register
	call print_opcode_name
	mov dl, 9
	call put_byte
	call print_reg_w1
RET

print_format_4:                      ; prints name and address
	call print_opcode_name
	mov dl, 9
	call put_byte
	mov al, [ip_inc8]
	call sign_extend
	call get_incremented_ip
	mov dl, ah
	call print_hex_byte
	mov dl, al
	call print_hex_byte
RET

print_format_5:                      ; prints name and 1 byte immediate value
	call print_opcode_name
	mov dl, 9
	call put_byte
	call print_immed_by_w
RET

print_format_6:                      ; prints name and address
	call print_opcode_name
	mov dl, 9
	call put_byte
	mov ax, [ip_inc16]
	call get_incremented_ip
	mov dl, ah
	call print_hex_byte
	mov dl, al
	call print_hex_byte
RET

print_format_7:                      ; prints mov with accumulator and memory
	call print_opcode_name
	mov dl, 9
	call put_byte
	
	mov byte ptr[mode], 0       ; get ready for direct address
	mov byte ptr[r_m], 110b
	mov byte ptr[reg], 0        ; get ready for accumulator
	
	
	cmp [first_byte], 0A0h
	je pr_format_7_from_mem
	cmp [first_byte], 0A1h
	je pr_format_7_from_mem
	cmp [first_byte], 0A2h
	je pr_format_7_to_mem 
	cmp [first_byte], 0A3h
	je pr_format_7_to_mem 
	RET
	
	pr_format_7_from_mem:
		call print_reg_by_w
		mov dl, ','
		call put_byte
		mov dl, ' '
		call put_byte
		mov ax, [addr16]
		call print_memory
	RET
	
	pr_format_7_to_mem:
		mov ax, [addr16]
		call print_memory
		mov dl, ','
		call put_byte
		mov dl, ' '
		call put_byte
		call print_reg_by_w
RET

print_format_8:                      ; prints name and cs:ip
	call print_opcode_name
	mov dl, 9
	call put_byte
	
	; cs
	mov ax, [cs_val]
	mov dl, ah
	call print_hex_byte
	mov dl, al
	call print_hex_byte
	
	mov dl, ':'
	call put_byte
	
	; ip
	mov ax, [ip_val]
	mov dl, ah
	call print_hex_byte
	mov dl, al
	call print_hex_byte
RET

print_format_9:                      ; prints name, accumulator and immediate value
	call print_opcode_name
	mov dl, 9
	call put_byte
	
	cmp byte ptr[w_bit], 1
	je pr_format_9_w1
	
	mov dx, offset s_al              ; al if w=0
	jmp pr_format_9_w0
	
	pr_format_9_w1:                 
	mov dx, offset s_ax              ; ax if w=1
	pr_format_9_w0:
		
	call put_string
	mov dl, ','
	call put_byte
	mov dl, ' '
	call put_byte
	call print_immed_by_w
RET

print_format_10:                     ; prints name register and immediate value
	call print_opcode_name
	mov dl, 9
	call put_byte
	
	call print_reg_by_w
	
	mov dl, ','
	call put_byte
	mov dl, ' '
	call put_byte
	call print_immed_by_w
RET

print_format_11:                     ; prints name and 2-byte immediate value
	call print_opcode_name
	mov dl, 9
	call put_byte
	call print_immed_by_w
RET

print_format_12:                     ; prints name, reg and r/m (d = 0) or r/m and reg (d = 1)
	call print_opcode_name
	mov dl, 9
	call put_byte
	
	cmp byte ptr[d_bit], 0
	je pr_format_12_d0
	
		call print_reg_by_w
		mov dl, ','
		call put_byte
		mov dl, ' '
		call put_byte
		call print_r_m
	RET
	
	pr_format_12_d0:
		call print_r_m
		mov dl, ','
		call put_byte
		mov dl, ' '
		call put_byte
		call print_reg_by_w
RET

print_format_13:                     ; prints name, r/m and sr (d = 0) or sr and r/m (d = 1)
	call print_opcode_name
	mov dl, 9
	call put_byte
	
	mov byte ptr[w_bit], 1     ; always 2-byte registers
	
	cmp byte ptr[d_bit], 0
	je pr_format_13_d0
		
		mov al, [sr]
		call print_sr
		mov dl, ','
		call put_byte
		mov dl, ' '
		call put_byte
		call print_r_m
	RET
	
	pr_format_13_d0:
		call print_r_m
		mov dl, ','
		call put_byte
		mov dl, ' '
		call put_byte
		mov al, [sr]
		call print_sr
RET

print_format_14:                     ; prints name, byte/word ptr and r/m (MUL DIV INC DEC)
	call print_opcode_name
	mov dl, 9
	call put_byte
	
	; if print reg is needed
	cmp byte ptr[first_byte], 0F6h
	jae pr_format_14_no_reg
	
	; print reg
	call print_reg_by_w
	mov dl, ','
	call put_byte
	mov dl, ' '
	call put_byte
	
	pr_format_14_no_reg:
	
	cmp byte ptr[mode], 11b
	je pr_format_14_no_ptr
	call print_ptr_by_w
	
	pr_format_14_no_ptr:
	call print_r_m
	
	
RET

print_format_15:                     ; prints name, byte/word ptr, r/m and immed
	call print_opcode_name
	mov dl, 9
	call put_byte
	
	cmp byte ptr[mode], 11b
	je pr_format_15_no_ptr
	call print_ptr_by_w
	
	pr_format_15_no_ptr:
	call print_r_m
	mov dl, ','
	call put_byte
	mov dl, ' '
	call put_byte
	call print_immed_by_w
RET

print_format_16:                     ; prints name, byte/word ptr, r/m and immed
	call print_opcode_name
	mov dl, 9
	call put_byte
	
	cmp byte ptr[mode], 11b
	je pr_format_16_no_ptr
	call print_ptr_by_w
	
	pr_format_16_no_ptr:
	call print_r_m
	mov dl, ','
	call put_byte
	mov dl, ' '
	call put_byte
	call print_immed_by_w
RET

print_format_17:                     ; prints name, r/m or far and memory
	call print_opcode_name
	mov dl, 9
	call put_byte
	
	; check if it's a far call/jmp
	cmp byte ptr[first_byte], 0FFh
	jne pr_format_17_not_far
	cmp byte ptr[reg], 011b
	je pr_format_17_is_far
	cmp byte ptr[reg], 101b
	je pr_format_17_is_far
	jmp pr_format_17_not_far
	
	pr_format_17_is_far:
	mov dx, offset s_far
	call put_string
	
	pr_format_17_not_far:
	call print_r_m
RET

print_unknown:
	mov dx, offset s_unrecognized
	call put_string
RET


; Prints byte ptr if w_bit = 0, word ptr if w_bit = 1
; INPUT: w_bit
; USING: put_string
PROC print_ptr_by_w
	push dx
		cmp byte ptr[w_bit], 1
		je print_word_ptr
		
			mov dx, offset s_byte_ptr
			call put_string
		jmp print_ptr_end
		
		print_word_ptr:
			mov dx, offset s_word_ptr
			call put_string
			
print_ptr_end:
	pop dx
	RET
ENDP print_ptr_by_w

; Prints memory location or register based on mod, reg, r/m and w_bit
; INPUT: mod, reg, r/m, w_bit, disp8, disp16, addr16
; USING: print_memory, print_reg_w0, print_reg_w1
PROC print_r_m
	push dx
	push ax
	push bx
		cmp byte ptr[mode], 11b
		jne print_r_m_mem                    
		
		mov bl, [reg]                     ; save reg value to BL and put r/m to reg
			mov al, [r_m]
			mov [reg], al
			call print_reg_by_w
		mov [reg], bl
		jmp print_r_m_end
		
		print_r_m_mem:
			cmp byte ptr[mode], 0
			je addr_2byte
			cmp byte ptr[mode], 1
			je disp_1byte
			cmp byte ptr[mode], 10b
			je disp_2byte
			
			disp_1byte:
				mov ah, 0
				mov al, [disp8]
				call print_memory
			jmp print_r_m_end
				
			disp_2byte:
				mov ax, [disp16]
				call print_memory
			jmp print_r_m_end
			
			addr_2byte:
				mov ax, [addr16]
				call print_memory
			jmp print_r_m_end

print_r_m_end:
	pop bx
	pop ax
	pop dx
	RET
ENDP print_r_m

; Prints segment prefix if present and memory location or direct address
; INPUT: ax - address or displacement, mod, r/m, segment_prefix, prefix_flag
; USING: print_sr, put_string, put_byte
PROC print_memory
	push ax
	push dx
		cmp byte ptr[prefix_flag], 0
		je print_memory_no_prefix
		
		push ax
			mov al, [segment_prefix]                    ; print segment register
			call print_sr
		pop ax
		mov dl, ':'
		call put_byte
		
	print_memory_no_prefix:
		mov dl, '['                                     ; open brace
		call put_byte
		
			cmp byte ptr[r_m], 110b
			jne print_mem_not_direct
			cmp byte ptr[mode], 0
			jne print_mem_not_direct
			
			print_mem_direct:                           ; print only address
				mov dl, ah
				call print_hex_byte
				mov dl, al
				call print_hex_byte
			jmp print_memory_close
			
			print_mem_not_direct:                       ; print register combination based on r/m
				cmp byte ptr[r_m], 0b
				je pr_mem_000
				cmp byte ptr[r_m], 1b
				je pr_mem_001
				cmp byte ptr[r_m], 10b
				je pr_mem_010
				cmp byte ptr[r_m], 11b
				je pr_mem_011
				cmp byte ptr[r_m], 100b
				je pr_mem_100
				cmp byte ptr[r_m], 101b
				je pr_mem_101
				cmp byte ptr[r_m], 110b
				je pr_mem_110
				cmp byte ptr[r_m], 111b
				je pr_mem_111
				
				pr_mem_000:
					mov dx, offset s_bx_si
					call put_string
				jmp check_mem_disp
				pr_mem_001:
					mov dx, offset s_bx_di
					call put_string
				jmp check_mem_disp
				pr_mem_010:
					mov dx, offset s_bp_si
					call put_string
				jmp check_mem_disp
				pr_mem_011:
					mov dx, offset s_bp_di
					call put_string
				jmp check_mem_disp
				pr_mem_100:
					mov dx, offset s_si
					call put_string
				jmp check_mem_disp
				pr_mem_101:
					mov dx, offset s_di
					call put_string
				jmp check_mem_disp
				pr_mem_110:
					mov dx, offset s_bp
					call put_string
				jmp check_mem_disp
				pr_mem_111:
					mov dx, offset s_bx
					call put_string
				jmp check_mem_disp
				
				
			check_mem_disp:
				cmp byte ptr[mode], 0
				je print_memory_close                   ; don't print displacement if mod = 0
				cmp byte ptr[mode], 1
				je print_mem_disp8
				cmp byte ptr[mode], 10b
				je print_mem_disp16
			
			print_mem_disp8:                            ; print 1-byte disp
				cmp al, 7Fh
				ja print_mem_disp_neg
				
					mov dl, '+'                         ; prints positive displacement
					call put_byte
					mov dl, al
					call print_hex_byte
				jmp print_memory_close
				
				print_mem_disp_neg:                     ; prints negative displacement
					mov dl, '-'
					call put_byte
					mov dl, al
					neg dl
					call print_hex_byte
				jmp print_memory_close
			jmp print_memory_close
			
			print_mem_disp16:                           ; print 2-byte disp
				mov dl, '+'
				call put_byte
				mov dl, ah
				call print_hex_byte
				mov dl, al
				call print_hex_byte
			jmp print_memory_close
			
	print_memory_close:
		mov dl, ']'
		call put_byte                                   ; close brace
	pop ax
	pop dx
	RET
ENDP print_memory

; prints segment register name by AL
; USING: put_string
; INPUT: al
PROC print_sr
	push ax
	push dx
		cmp al, 0
		je pr_sr_00
		cmp al, 01b
		je pr_sr_01
		cmp al, 10b
		je pr_sr_10
		cmp al, 11b
		je pr_sr_11
		
		pr_sr_00:
			mov dx, offset s_es
			call put_string
			jmp print_sr_end
		pr_sr_01:
			mov dx, offset s_cs
			call put_string
			jmp print_sr_end
		pr_sr_10:
			mov dx, offset s_ss
			call put_string
			jmp print_sr_end
		pr_sr_11:
			mov dx, offset s_ds
			call put_string
			
print_sr_end:
	pop dx
	pop ax
	RET
ENDP print_sr

; Prints immediate value by w_bit
; USING: print_hex_byte
; INPUT: w_bit, immed8 or immed16
PROC print_immed_by_w
	push ax
	push dx
		cmp byte ptr[w_bit], 1
		je print_immed16
		
		mov dl, [immed8]
		call print_hex_byte
		jmp print_immed_end
		
		print_immed16:
			mov ax, [immed16]
			mov dl, ah
			call print_hex_byte
			mov dl, al
			call print_hex_byte
		
print_immed_end:
	pop dx
	pop ax
	RET
ENDP print_immed_by_w

; Prints 8-bit or 16-bit register name by reg and w_bit
; USING: print_reg_w0, print_reg_w1
; INPUT: reg, w_bit
PROC print_reg_by_w
		cmp byte ptr[w_bit], 1
		je print_reg_by_w1
		
		call print_reg_w0
	RET
		print_reg_by_w1:
			call print_reg_w1
	RET
ENDP print_reg_by_w

; Prints 16-bit register name by reg
; USING: put_string
; INPUT: reg
PROC print_reg_w1
	push ax
	push dx
		mov al, [reg]
		
		cmp al, 0b
		je pr_reg_w1_000
		cmp al, 1b
		je pr_reg_w1_001
		cmp al, 10b
		je pr_reg_w1_010
		cmp al, 11b
		je pr_reg_w1_011
		cmp al, 100b
		je pr_reg_w1_100
		cmp al, 101b
		je pr_reg_w1_101
		cmp al, 110b
		je pr_reg_w1_110
		cmp al, 111b
		je pr_reg_w1_111
		
		pr_reg_w1_000:
			mov dx, offset s_ax
			jmp print_reg_w1_end
		pr_reg_w1_001:
			mov dx, offset s_cx
			jmp print_reg_w1_end
		pr_reg_w1_010:
			mov dx, offset s_dx
			jmp print_reg_w1_end
		pr_reg_w1_011:
			mov dx, offset s_bx
			jmp print_reg_w1_end
		pr_reg_w1_100:
			mov dx, offset s_sp
			jmp print_reg_w1_end
		pr_reg_w1_101:
			mov dx, offset s_bp
			jmp print_reg_w1_end
		pr_reg_w1_110:
			mov dx, offset s_si
			jmp print_reg_w1_end
		pr_reg_w1_111:
			mov dx, offset s_di
			jmp print_reg_w1_end
		
print_reg_w1_end:
		call put_string
	pop ax
	pop dx
	RET
ENDP print_reg_w1

; prints one byte register name by reg
; USING: put_string
; INPUT: reg
PROC print_reg_w0
	push ax
	push dx
		mov al, [reg]
		
		cmp al, 0b
		je pr_reg_w0_000
		cmp al, 1b
		je pr_reg_w0_001
		cmp al, 10b
		je pr_reg_w0_010
		cmp al, 11b
		je pr_reg_w0_011
		cmp al, 100b
		je pr_reg_w0_100
		cmp al, 101b
		je pr_reg_w0_101
		cmp al, 110b
		je pr_reg_w0_110
		cmp al, 111b
		je pr_reg_w0_111
		
		pr_reg_w0_000:
			mov dx, offset s_al
			jmp print_reg_w0_end
		pr_reg_w0_001:
			mov dx, offset s_cl
			jmp print_reg_w0_end
		pr_reg_w0_010:
			mov dx, offset s_dl
			jmp print_reg_w0_end
		pr_reg_w0_011:
			mov dx, offset s_bl
			jmp print_reg_w0_end
		pr_reg_w0_100:
			mov dx, offset s_ah
			jmp print_reg_w0_end
		pr_reg_w0_101:
			mov dx, offset s_ch
			jmp print_reg_w0_end
		pr_reg_w0_110:
			mov dx, offset s_dh
			jmp print_reg_w0_end
		pr_reg_w0_111:
			mov dx, offset s_bh
			jmp print_reg_w0_end
		
print_reg_w0_end:
		call put_string
	pop ax
	pop dx
	RET
ENDP print_reg_w0

; prints opcode name by first_byte and sometimes by opcode extention (reg field)
; USING:  put_string
; INPUT:  first_byte, reg
PROC print_opcode_name
	push ax
	push dx
	push bx
		mov al, [first_byte]
		
		check_extended_opc:                            
			cmp al, 80h
			jae is_extended_opc
			jmp no_extended_opc
			is_extended_opc:
			cmp al, 84h
			jb check_80_83
			cmp al, 0F6h
			je check_F6_F7
			cmp al, 0F7h
			je check_F6_F7
			cmp al, 0FEh
			je check_FE_FF
			cmp al, 0FFh
			je check_FE_FF
		jmp no_extended_opc
		
		check_80_83:
			cmp byte ptr[reg], 0b
			jne no_add
			jmp is_add
			no_add:
			cmp byte ptr[reg], 1b
			jne no_or
			jmp is_or
			no_or:
			cmp byte ptr[reg], 10b
			jne no_adc
			jmp is_adc
			no_adc:
			cmp byte ptr[reg], 11b
			jne no_sbb
			jmp is_sbb
			no_sbb:
			cmp byte ptr[reg], 100b
			jne no_and
			jmp is_and
			no_and:
			cmp byte ptr[reg], 101b
			jne no_sub
			jmp is_sub
			no_sub:
			cmp byte ptr[reg], 110b
			jne no_xor
			jmp is_xor
			no_xor:
			cmp byte ptr[reg], 111b
			jne no_cmp
			jmp is_cmp
			no_cmp:
		jmp print_opcode_name_end
		
		check_F6_F7:
			cmp byte ptr[reg], 100b
			jne no_mul
			jmp is_mul
			no_mul:
			cmp byte ptr[reg], 110b
			jne no_div
			jmp is_div
			no_div:
		jmp print_opcode_name_end
		
		check_FE_FF:
			cmp byte ptr[reg], 0b
			jne no_inc
			jmp is_inc
			no_inc:
			cmp byte ptr[reg], 1b
			jne no_dec
			jmp is_dec
			no_dec:
			cmp byte ptr[reg], 10b
			jb no_call
			cmp byte ptr[reg], 11b
			ja no_call
			jmp is_call
			no_call:
			cmp byte ptr[reg], 100b
			jb no_jmp
			cmp byte ptr[reg], 101b
			ja no_jmp
			jmp is_jmp
			no_jmp:
			cmp byte ptr[reg], 110b
			jne no_push
			jmp is_push
			no_push:
		jmp print_opcode_name_end
		
		no_extended_opc:
		
		check_test:
			cmp al, 84h
			je is_test
			cmp al, 85h
			je is_test
		jmp check_or
		
		is_test:
			mov dx, offset s_test
			call put_string
		jmp print_opcode_name_end
		
		check_or:
			cmp al, 8h
			jb check_adc
			cmp al, 0Eh
			jb is_or
		jmp check_adc
		
		is_or:
			mov dx, offset s_or
			call put_string
		jmp print_opcode_name_end
		
		check_adc:
			cmp al, 10h
			jb check_sbb
			cmp al, 16h
			jb is_adc
		jmp check_sbb
		
		is_adc:
			mov dx, offset s_adc
			call put_string
		jmp print_opcode_name_end
		
		check_sbb:
			cmp al, 18h
			jb check_xor
			cmp al, 1Eh
			jb is_sbb
		jmp check_xor
		
		is_sbb:
			mov dx, offset s_sbb
			call put_string
		jmp print_opcode_name_end
		
		check_xor:
			cmp al, 30h
			jb check_and
			cmp al, 36h
			jb is_xor
		jmp check_and
		
		is_xor:
			mov dx, offset s_xor
			call put_string
		jmp print_opcode_name_end
		
		check_and:
			cmp al, 20h
			jb check_mov
			cmp al, 26h
			jb is_and
		jmp check_mov
		
		is_and:
			mov dx, offset s_and
			call put_string
		jmp print_opcode_name_end
		
		check_mov:
			cmp al, 88h
			jb check_add
			cmp al, 8Dh
			jb is_mov
			cmp al, 8Eh
			je is_mov
			cmp al, 0A0h
			jb check_add
			cmp al, 0A4h
			jb is_mov
			cmp al, 0B0h
			jb check_add
			cmp al, 0C0h
			jb is_mov
			cmp al, 0C6h
			je is_mov
			cmp al, 0C7h
			je is_mov
		jmp check_add
		
		is_mov:
			mov dx, offset s_mov
			call put_string
		jmp print_opcode_name_end
		
		check_add:                   ; TODO tikrinti pagal reg
			cmp al, 06h
			jb is_add
		jmp check_push
		
		is_add:
			mov dx, offset s_add
			call put_string
		jmp print_opcode_name_end
		
		check_push:                  ; TODO tikrinti pagal reg
			mov bl, al
			and bl, 11100111b
			cmp bl, 00000110b
			je is_push
			cmp al, 50h
			jb check_pop
			cmp al, 58h
			jb is_push
		jmp check_pop
		
		is_push:
			mov dx, offset s_push
			call put_string
		jmp print_opcode_name_end
		
		check_pop:
			mov bl, al
			and bl, 11100111b
			cmp bl, 00000111b
			je is_pop
			cmp al, 58h
			jb check_sub
			cmp al, 60h
			jb is_pop
			cmp al, 8Fh
			je is_pop
		jmp check_sub
		
		is_pop:
			mov dx, offset s_pop
			call put_string
		jmp print_opcode_name_end
		
		check_sub:                     ; TODO tikrinti pagal reg
			cmp al, 28h
			jb check_cmp
			cmp al, 2Eh
			jb is_sub
		jmp check_cmp
		
		is_sub:
			mov dx, offset s_sub
			call put_string
		jmp print_opcode_name_end
		
		check_cmp:                     ; TODO tikrinti pagal reg
			cmp al, 38h
			jb check_inc
			cmp al, 3Eh
			jb is_cmp
		jmp check_inc
		
		is_cmp:
			mov dx, offset s_cmp
			call put_string
		jmp print_opcode_name_end
		
		check_inc:                     ; TODO tikrinti pagal reg
			cmp al, 40h
			jb check_dec
			cmp al, 48h
			jb is_inc
		jmp check_dec
		
		is_inc:
			mov dx, offset s_inc
			call put_string
		jmp print_opcode_name_end
		
		check_dec:                    ; TODO tikrinti pagal reg
			cmp al, 48h
			jb check_call
			cmp al, 50h
			jb is_dec
		jmp check_call
		
		is_dec:
			mov dx, offset s_dec
			call put_string
		jmp print_opcode_name_end
		
		check_call:                   ; TODO tikrinti pagal reg
			cmp al, 9Ah
			je is_call
			cmp al, 0E8h
			je is_call
		jmp check_ret
		
		is_call:
			mov dx, offset s_call
			call put_string
		jmp print_opcode_name_end
		
		check_ret:
			cmp al, 0C2h
			je is_ret
			cmp al, 0C3h
			je is_ret
			cmp al, 0CAh
			je is_ret
			cmp al, 0CBh
			je is_ret
		jmp check_jmp
		
		is_ret:
			mov dx, offset s_ret
			call put_string
		jmp print_opcode_name_end
		
		check_jmp:                     ; TODO tikrinti pagal reg
			cmp al, 0E9h
			jb check_cond_jmps
			cmp al, 0ECh
			jb is_jmp
		jmp check_cond_jmps
		
		is_jmp:
			mov dx, offset s_jmp
			call put_string
		jmp print_opcode_name_end
		
		check_cond_jmps:
			cmp al, 70h
			jb check_loop
			cmp al, 80h
			jb is_cond_jmps
		jmp check_loop
		
		is_cond_jmps:
			push ax
				mov dx, offset s_cond_jmps
				
				mov bl, al
				sub bl, 70h
				mov al, 4
				mul bl
				add dx, ax
				
				call put_string
			pop ax
		jmp print_opcode_name_end
		
		check_loop:
			cmp al, 0E2h
			je is_loop
		jmp check_int
		
		is_loop:
			mov dx, offset s_loop
			call put_string
		jmp print_opcode_name_end
		
		check_int:
			cmp al, 0CDh
			je is_int
		jmp name_not_found
		
		is_int:
			mov dx, offset s_int
			call put_string
		jmp print_opcode_name_end
		
		is_mul:
			mov dx, offset s_mul
			call put_string
		jmp print_opcode_name_end
		
		is_div:
			mov dx, offset s_div
			call put_string
		jmp print_opcode_name_end
		
		name_not_found:
			mov dx, offset s_ss
			call put_string
		jmp print_opcode_name_end
	
print_opcode_name_end:
	pop bx
	pop dx
	pop ax
	RET
ENDP


; ================= I/O PROCEDURES =================

; Gets next byte from read_buffer, reads new buffer if needed. Increments ip_counter.
; USING:  read_new_buffer
; INPUT:  ip_counter
; RETURN: DL
PROC get_byte
	push bx
		mov bx, read_buff_pos
		cmp bx, read_bytes
		jb get_from_buffer      ; if no buffer read is needed
		
		call read_new_buffer
		
		get_from_buffer:
			mov bx, offset read_buffer
			add bx, read_buff_pos
			mov dl, [bx]
			inc read_buff_pos
			
		inc word ptr[ip_counter]    ; advance instruction pointer
	pop bx
	ret
ENDP get_byte

; Reads new data into read_buff, resets read_buff_pos
; Checks if eof is reached
; Terminates program on error
PROC read_new_buffer
	push ax
	push bx
	push cx
	push dx
		mov ah, 3Fh
		mov bx, input_handle
		mov cx, BUFF_SIZE
		mov dx, offset read_buffer
		int 21h
		jc read_err                ; error while reading
		
		mov read_bytes, ax
		mov read_buff_pos, 0
		
		cmp read_bytes, 0          ; check if eof is reached
		je set_eof
		
		jmp read_new_buffer_end
		
		set_eof:
			mov eof, 1
		jmp read_new_buffer_end
		
		read_err:
			mov ah, 9
			mov dx, offset read_err_str
			int 21h
		jmp exit
		
read_new_buffer_end:
	pop dx
	pop cx
	pop bx
	pop ax
	ret
ENDP read_new_buffer

; Puts an asciiz string to write_buffer
; USING: put_byte
; INPUT: DS:DX - address to asciiz string
PROc put_string
	push bx
	push dx
		mov bx, dx
		put_string_loop:
			mov dl, [bx]
			call put_byte
			inc bx
			cmp byte ptr [bx], 0    ; check if there are more symbols in string
			jne put_string_loop
	pop dx
	pop bx
	ret
ENDP put_string

; Puts a byte to write_buffer. Prints write_buffer when it is full
; USING: print_buffer
; INPUT: DL - byte to put
PROC put_byte
	push bx
		mov bx, write_buff_pos
		cmp bx, BUFF_SIZE
		jb put_to_buffer         ; if buffer is not full
		
		call print_buffer
		
		put_to_buffer:
			mov bx, offset write_buffer
			add bx, write_buff_pos
			mov [bx], dl
			inc write_buff_pos
	pop bx
	ret
ENDP put_byte

; Prints bytes from write_buffer until write_buff_pos, resets write_buff_pos to 0
; Terminates program on error
PROC print_buffer
	push ax
	push bx
	push cx
	push dx
		mov ah, 40h
		mov bx, output_handle
		mov cx, write_buff_pos
		mov dx, offset write_buffer
		int 21h
		jc write_err
		mov write_buff_pos, 0    ; reset position after write
		jmp print_buffer_end
		
		write_err:
			mov ah, 9
			mov dx, offset write_err_str
			int 21h
		jmp exit	

print_buffer_end:	
	pop dx
	pop cx
	pop bx
	pop ax
	ret
ENDP print_buffer

; Prints byte in hex number format
; USING: print_hex_byte
; INPUT: DL - byte to print
PROC print_hex_byte
	push dx
	push cx
		mov dh, dl     ; make a copy
		mov cl, 4
		shr dl, cl     ; set lower nibble to first hex number
		call print_hex_digit
		mov dl, dh     ; return initial value
		call print_hex_digit
	pop cx
	pop dx
	ret
ENDP print_hex_byte

; Prints number in lower byte nibble in hex format 
; USING: put_byte
; INPUT: DL - byte with number in lower nibble
PROC print_hex_digit
	push dx
		and dl, 00001111b    ; set upper nibble to 0
		add dl, 30h
		cmp dl, '9'
		jbe write_byte
		add dl, 7           
		
		write_byte:
			call put_byte
	pop dx
	ret
ENDP print_hex_digit


; ================= FILE PROCEDURES =================

; parse command line parameters
parse_parameters:
	mov si, 80h  ; si - current position from which to parse
	xor bx, bx   ; bx - position in param name to be writen at
	xor cx, cx   ; cx - which param is being parsed. 1 - first, 2 - second
	
	cmp byte ptr es:[si], 0      ; check if any params have been entered
	je parse_param_err
	
	call skip_spaces             ; skip all preceding spaces
	
	mov cx, 1
	mov bx, offset input_file    ; initialize parsing of first parameter
	
	parse_loop:
		cmp cx, 3
		je parse_param_err       ; end if more than 2 params have been entered
		
		cmp es:[si], '?/'            ; check if /? in parameters is found
		je parse_param_err
	
		mov dl, es:[si]
		mov [bx], dl
		
		inc si
		inc bx
		
		cmp byte ptr es:[si], 20h    ; if first parameter has ended
		je second_param              
		cmp byte ptr es:[si], 0Dh    ; if all parameters have ended
		je check_param_count
	jmp parse_loop
		
	second_param:                ; initialize parsing of second parameter
		inc cx
		mov bx, offset output_file
		call skip_spaces
	jmp parse_loop
	
	
	check_param_count:
		cmp cx, 2
		jne parse_param_err      ; if 2 parameters haven't been parsed
		RET                      ; otherwise, return from parse_parameters
	
	skip_spaces:
		inc si
		cmp byte ptr es:[si], 20h
		je skip_spaces
	RET
		
	parse_param_err:
		mov ah, 9
		mov dx, offset help_str
		int 21h
	jmp exit


; open input file and create output file
prepare_files:
	; input file
	mov ah, 3Dh
	mov al, 0
	mov dx, offset input_file          
	int 21h                             
	jc open_err
	mov input_handle, ax
	
	; output file
	mov ah, 3Ch
	mov cx, 6
	mov dx, offset output_file
	int 21h								
	jc create_err
	mov output_handle, ax

	RET
	
	open_err:
		mov ah, 9
		mov dx, offset open_err_str
		int 21h
	jmp exit
	create_err:
		mov ah, 9
		mov dx, offset create_err_str
		int 21h
	jmp exit


; close input and output file
close_files:
	mov ah, 3Eh
	mov bx, input_handle
	int 21h
	jc close_err
	mov bx, output_handle
	int 21h
	jc close_err
	
	RET
	
	close_err:
		mov ah, 9
		mov dx, offset close_err_str
		int 21h
	jmp exit

END start