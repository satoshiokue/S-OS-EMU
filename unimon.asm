;
; S-OS EMUZ80 Monitor
;   Target: EMUZ80 + MEZ80RAM
;   Assembler: The Macro Assembler AS
;
; Modified by Satoshi Okue https://twitter.com/S_Okue
; Version 0.1 2023/4/25
;

;;; 2022.10.10 It was added functions by A.honda
;;; Rev.B01	add trace command
;;; Rev.B02	add disassemble commnand
;;; Rev.B03	add assemble command and PaloAltoTinyBASIC
;;;                and GAME80 interpreter and GAME80 compiler
;;; Rev.B03_1	The monitor corresponded to the SuperMEZ80
;;; Rev.B04	The monitor corresponded to UART XON/XOFF
;;; Rev.B05	add VTL

;;;
;;; Universal Monitor for Zilog Z80
;;;   Copyright (C) 2019,2020,2021 Haruo Asano
;;;

	CPU	Z80

	INCLUDE "S-OS.asm"

;;;
;;; Constants
;;;
CR		EQU	0DH
LF		EQU	0AH
BS		EQU	08H
DEL		EQU	7FH
ESC		EQU	1BH
BUFLEN		EQU	40
NUMLEN		EQU	7
F_bitSize	EQU	8
NO_UPPER	EQU	00000100b
NO_LF		EQU	00000010b
NO_CR		EQU	00000001b

UARTDR		EQU	00H	; UART DATA I/O REGISTOR
UARTCR		EQU	01H	; UART CONTROL I/O REGISTOR
STACKM		EQU	03000H	; monitor stack
STACK		EQU	02FC0H	; user stack
ROM_B		EQU	0000H	; program base address
ENTRY		EQU	0040H	; Entry point
RAM_B		EQU	ENTRY	; work area base address

;;;
;;; ROM area
;;;
start:

	ORG	ROM_B	; (RST 00H)

E_CSTART:
	JP	CSTART
E_WSTART:
	JP	API01
	DB	0008H - $ dup(00H)	; nop
	JP	CONOUT
	DB	0010H - $ dup(00H)	; nop
	JP	CONIN
	DB	0018H - $ dup(00H)	; nop
	JP	CONST
	DB	0020H - $ dup(00H)	; nop
	RET
	DB	0028H - $ dup(00H)	; nop
	RET
	DB	0030H - $ dup(00H)	; nop
	JP	RST30H_IN
	DB	0038H - $ dup(00H)	; nop
	JP	RST38H_IN
	DB	ENTRY - $ dup(00H)	; nop
	;	ORG	ENTRY

;;;
;;; Universal Monitor Z80 Cold start
;;;

CSTART:
	DI
	LD	SP,STACKM	; monitor stask defines STACKM
	LD	HL,RAM_B
	LD	(DSADDR),HL
	LD	(SADDR),HL
	LD	(GADDR),HL
	LD	A,'I'
	LD	(HEXMOD),A

	;; Initialize register value
	XOR	A
	LD	HL,REG_B
	LD	B,REG_E-REG_B
IR0:	LD	(HL),A
	INC	HL
	DJNZ	IR0
	LD	HL,STACK		; user stack define STACK
	LD	(REGSP),HL
	LD	HL,RAM_B
	LD	(REGPC),HL		; set program counter
	LD	B,F_BITSIZE
	LD	A,'.'
	LD	HL,F_BIT
IR00:	LD	(HL),A
	INC	HL
	DJNZ	IR00		; init F_bit string
	XOR	A
	LD	(HL),A		; delimiter

; init dbg work area

	LD	B,DBG_WEND - DBG_WTOP
	LD	HL,DBG_WTOP
	XOR	A

DBG_WINI:
	LD	(HL),A
	INC	HL
	DJNZ	DBG_WINI

	LD	A,'I'
	LD	(TM_MODE),A	; default call_in mode
	LD	A,'N'
	LD	(TP_MODE),A	; default display reg mode
	LD	L,0
	LD	H,L
	LD	(TC_CNT),HL	; clear trace step counter to 0
	XOR	A
	LD	(FEVER_T),A	; clear flag trace forever
	; init bp, tp, gstop address & opcode
	LD	HL,RAM_B
	LD	(TPT1_ADR),HL
	LD	(TPT2_ADR),HL
	LD	(BPT1_ADR),HL
	LD	(BPT2_ADR),HL
	LD	(TMPB_ADR),HL
	LD	A,(HL)
	LD	(TPT1_OP),A
	LD	(TPT2_OP),A
	LD	(BPT1_OP),A
	LD	(BPT2_OP),A
	LD	(TMPB_OP),A

;; Opening message

	LD	HL,OPNMSG
	CALL	STROUT
;	EI

WSTART:
	XOR	A
	LD	(KY_FLG),A	; clear skip LF flag

	LD	HL,PROMPT
	CALL	STROUT
	CALL	GETLIN
	CALL	SKIPSP
	OR	A
	JR	Z,WSTART

	CP	'A'
	JP	Z,DISASSEMBLE
	CP	'D'
	JP	Z,DUMP
	CP	'G'
	JP	Z,GO
	CP	'S'
	JP	Z,SETM

	CP	'L'
	JP	Z,LOADH
	CP	'P'
	JP	Z,SAVEH

	CP	'I'
	JP	Z,PIN
	CP	'O'
	JP	Z,POUT

	CP	'R'
	JP	Z,REG

	CP	'B'
	JP	Z,BRK_CMD	; break point command

	CP	'T'
	JP	Z,TRACE_CMD	; trace point command

	CP	'H'
	JR	Z,COMMAND_HLP	; command help message

	CP	'E'
	JP	Z,EMBASIC	; Start EMBASIC

	CP	'!'
	JP	Z,COLD		; Cold Start S-OS EMUZ80

ERR:
	LD	HL,ERRMSG
	CALL	STROUT
	JR	WSTART

;;
;; Start EMBASIC
;;

EMBASIC:
	OUT	(15H),A
	JP	3000H

;;
;; command help
;;
command_hlp:

	LD	HL,cmd_hlp
	CALL	STROUT
	JR	WSTART

GET_dNUM:
	PUSH	DE
	PUSH	HL
	CALL	GET_NUM
	POP	HL
	POP	DE
	RET

	; get number
	; input HL : string buffer
	;
	; Return
	; CF =1 : Error
	; BC: Calculation result

GET_NUM:
	XOR	A		; Initialize C
	LD	B,A
	LD	C,A		; clear BC

GET_NUM0:
	CALL	SKIPSP		; A <- next char
	OR	A
	RET	Z		; ZF=1, ok! buffer end

	CALL	GET_BI
	RET	C

	PUSH	AF
	EX	AF,AF'		;'AF <> AF: save A
	POP	AF
	CALL	MUL_10		; BC = BC * 10
	RET	C		; overflow error
	EX	AF,AF'		;'AF <> AF: restor A

	PUSH	HL
	LD	D,0
	LD	E,A

	LD	H,B
	LD	L,C
	ADD	HL,DE
	LD	B,H
	LD	C,L		; result: BC = BC * 10 + A
	POP	HL
	RET	C		; overflow error
				; result: BC = BC * 10 + A
	INC	HL
	JR	GET_NUM0
;
; Make binary to A
; ERROR if CF=1
;
GET_BI:
	OR	A
	JR	Z,UP_BI
	CP	'0'
	RET	C

	CP	'9'+1		; ASCII':'
	JR	NC,UP_BI
	SUB	'0'		; Make binary to A
	RET

UP_BI:
	SCF			; Set CF
	RET

;
; multiply by 10
; BC = BC * 10
MUL_10:
	PUSH	HL

	PUSH	BC
	SLA	C
	RL	B		; 2BC
	SLA	C
	RL	B		; 4BC
	POP	HL		; hl = bc
	ADD	HL,BC
	PUSH	HL
	POP	BC		; 5BC
	SLA	C
	RL	B		; 10BC

	POP	HL
	RET			; result : BC = BC * 10
;
; list break point
;
B_LIST:
	LD	A,(BPT1_F)
	OR	A
	JR	Z,B_LIST1

	LD	HL,(BPT1_ADR)
	LD	A,'1'
	CALL	B_MSG_OUT

B_LIST1:
	LD	A,(BPT2_F)
	OR	A
	JP	Z,WSTART

	LD	HL,(BPT2_ADR)
	LD	A,'2'
	CALL	B_MSG_OUT
	JP	WSTART


;;;
;;; break point command
;;;
BRK_CMD:
	INC	HL
	CALL	SKIPSP		; A <- next char
	OR	A
	JR	Z,B_LIST	; only type "B"

	LD	BC,BPT1_F
	CP	'1'
	JR	Z,SET_BP1

	LD	BC,BPT2_F
	CP	'2'
	JR	Z,SET_BP1

	CP	'C'		;clear?
	JR	Z,BP_CLR
	JP	ERR

SET_BP1:
	EX	AF,AF'		;'

	INC	HL
	CALL	SKIPSP		; A <- next char
	OR	A
	JR	Z,BP_LOT 	;; No address input -> list out
	CP	','
	JP	NZ,ERR

	INC	HL
	CALL	SKIPSP
	PUSH	BC
	CALL	RDHEX		; 1st arg.
	POP	HL		; hl <- bc
	JP	C,ERR

	CALL	SETBPADR
	JP	C,ERR
	JP	WSTART


; hl : point of bp flag( bpt1_f or bpt2_f)
; de : break point address

; check ram area, and set berak point
;
SETBPADR:
	LD	A,1
	LD	(HL),A		; set flag
	INC	HL
	LD	A,(DE)		; get opcode
	LD	(HL),A		; save opcode
	INC	HL
	LD	(HL),E 		; set break point low address
	INC	HL
	LD	(HL),D 		; set break point high address
	OR	A		; reset carry
	RET

CHKERR:
	SCF	;SET CARRY
	RET

; clear break point

BP_CLR:
	INC	HL
	CALL	SKIPSP		; A <- next char
	OR	A
	JR	Z,B_ACLR	; all clear

	LD	BC,BPT1_F
	CP	'1'
	JR	Z,BP_CLR1

	LD	BC,BPT2_F
	CP	'2'
	JP	NZ,ERR

BP_CLR1:
	XOR	A
	LD	(BC),A
	JP	WSTART

B_ACLR:
	XOR	A
	LD	BC,BPT1_F
	LD	(BC),A
	LD	BC,BPT2_F
	LD	(BC),A
	JP	WSTART

; when no address input. list out BP
;
; bc : break pointer buffer offset
BP_LOT:
	LD	A,(BC)		; set break point?
	OR	A
	JP	Z,WSTART	; no break point setting

	EX	AF,AF'		;'
	LD	HL,(BPT1_ADR)
	CP	'1'
	JR	Z,L_B2
	LD	HL,(BPT2_ADR)
L_B2:
	CALL	B_MSG_OUT
	JP	WSTART

BP_MSG1:	DB	"BP(",0
BP_MSG2:	DB	"):",0

B_MSG_OUT:
	PUSH	HL
	PUSH	AF
	LD	HL,BP_MSG1
	CALL	STROUT
	POP	AF
	CALL	CONOUT
	LD	HL,BP_MSG2
	CALL	STROUT
	POP	HL
	CALL	HEXOUT4
	CALL	CRLF
	RET

;;;
;;; trace command
;;;
TRACE_CMD:

; T[address][,step��]
; TP[ON | OFF]
; TM[I | S]

; init steps
	PUSH	HL
	LD	L,0
	LD	H,0
	LD	(TC_CNT),HL	; clear trace step counter to 0
	LD	HL,(REGPC)
	LD	(TMPT),HL	; init temp address
	XOR	A
	LD	(FEVER_T),A	; clear flag trace forever

	POP	HL
	INC	HL
	CALL	SKIPSP
	CALL	RDHEX		; 1st arg.
	JP	NC,TADR_CHK
	CALL	SKIPSP
	LD	A,(HL)
	OR	A
	JP	Z,T_OP1		; only 1 step trace, check opcode
	CP	','
	JP	Z,STP_CHK	; steps check

	CP	'P'
	JR	Z,TP_CMD
	CP	'M'
	JP	NZ,ERR

	; tm_cmd

	INC	HL
	CALL	SKIPSP		; A <- next char
DIP_TM:
	LD	HL,TM_MSG_I
	CP	'I'
	JR	Z,SET_TM
	LD	HL,TM_MSG_S
	CP	'S'
	JR	Z,SET_TM
	OR	A
	JP	NZ,ERR

;display T mode
	LD	A,(TM_MODE)
	JR	DIP_TM

;set TM mode and display
SET_TM:
	LD	(TM_MODE),A
	PUSH	HL
	LD	HL,TM_MSG_0
	CALL	STROUT
	POP	HL
	CALL	STROUT
	JP	WSTART

TM_MSG_0:
	DB	"TM mode:<CALL ",0
TM_MSG_I:
	DB	"IN>",CR,LF,0
TM_MSG_S:
	DB	"SKIP>",CR,LF,0

	; tp_cmd
TP_CMD:
	INC	HL
	CALL	SKIPSP		; A <- next char
	OR	A
	JR	NZ,TP_N1
	LD	A,(TP_MODE)
	JR	TP_N2

TP_N1:
	CP	'O'
	JP	NZ,ERR
	INC	HL
	CALL	SKIPSP		; A <- next char

TP_N2:
	LD	HL,TP_MSG_ON
	CP	'N'
	JR	Z,TP_MD_ON

	LD	HL,TP_MSG_OFF
	CP	'F'
	JP	NZ,ERR

TP_MD_ON:
	; set trace mode and display mode
	LD	(TP_MODE),A
	PUSH	HL
	LD	HL,TP_MSG_0
	CALL	STROUT
	POP	HL
	CALL	STROUT
	JP	WSTART

TP_MSG_0:
	DB	"TP MODE: ",0
TP_MSG_ON:
	DB	"ON",CR,LF,0
TP_MSG_OFF:
	DB	"OFF",CR,LF,0

TADR_CHK:
	LD	(TMPT),DE	; set start address
	CALL	SKIPSP		; A <- next char
	OR	A
	JR	Z,T_OP1	; 1step trace
	CP	','
	JR	Z,STP_CHK
	JP	ERR

STP_CHK:
	INC	HL
	CALL	SKIPSP		; A <- next char
	OR	A
	JP	Z,ERR
	CP	'-'
	JR	Z,CHK_FEVRE

; check steps

	CALL	GET_NUM		; get steps to BC
	JP	C,ERR		; number error

T_OP11:
	LD	(TC_CNT), BC	; set trace step counter
	JR	T_OP_CHK

T_OP1:
	LD	BC,1
	JR	T_OP11

CHK_FEVRE:
	INC	HL
	CALL	SKIPSP		; A <- next char
	CP	'1'
	JP	NZ,ERR
	INC	HL
	CALL	SKIPSP		; A <- next char
	OR	A
	JP	NZ,ERR		; not "-1" then error
	LD	A,1
	LD	(FEVER_T),A	; set flag trace forever

T_OP_CHK:
	LD	HL,(TMPT)	; get PC address
	LD	A,(HL)		; get opcode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; branch opecode check
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2 insertion Trace code(TC)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
INSTC2:
	; check 1 byte machine code: branch (RET CC)

	LD	BC,RETCC_TBLE - RETCC_TBLS
	LD	HL,RETCC_TBLS
	CPIR
	JR	NZ,NEXT_BC1	; not RET CC

	; RET CC
	; trace operation:
	;   1. ea = *REGSP; *ea = TC;
	;   2. ea = *REGPC; *(ea+1) = TC;

	; 1
	LD	C,1		; first TC point
	CALL	INSBRK_SP
	JP	C,ERR_TRACE_SEQ

	; 2
	LD	C,2		; second TC point
	CALL	INSBRK_1OP
	JP	C,ERR_TRACE_SEQ

	JP	END_INS_TC

	; check 2 byte machine code: branch (JR CC, Relative Value)
NEXT_BC1:

	LD	BC,JRCC_TBLE - JRCC_TBLS
	LD	HL,JRCC_TBLS
	CPIR
	JR	NZ,NEXT_BC2	; not JR CC

	; JR CC, nn
	; trace operation:
	;   1. ea = *REGPC; *(ea + 2 + *(ea+1)) = TC;
	;   2. ea = *REGPC; *(ea+2) = TC;

	; 1
	LD	C,1		; first TC point
	LD	HL,(TMPT)
	CALL	REL_ADR_C
	CALL	INADR_CHK_AND_WRT
	JP	C,ERR_TRACE_SEQ

	; 2
	LD	C,2		; second TC point
	CALL	INSBRK_2OP
	JP	C,ERR_TRACE_SEQ

	JP	END_INS_TC

	; check 3 byte machine code: branch JP CC, nnnn 16bit literal)

NEXT_BC2:
	LD	BC,JPCC_TBLE - JPCC_TBLS
	LD	HL,JPCC_TBLS
	CPIR
	JR	NZ,NEXT_BC21		; not JP CCC

	; JP CC, nnnn
	; trace operation:
	;   1. ea = *REGPC; *((short *)(ea+1)) = TC;
	;   2. ea = *REGPC; *(ea+3) = TC;

	LD	C,1		; first TC point
	JR	NEXT_BC222

	; check 3 byte machine code: branch (CALL CC, nnnn 16bit literal)

NEXT_BC21:
	LD	BC,CLCC_TBLE - CLCC_TBLS
	LD	HL,CLCC_TBLS
	CPIR
	JR	NZ,INSTC1		; not CALL CCC

	; CALL CC, nnnn
	; trace operation:
	; TM_mode = 'I'
	;   1. ea = *REGPC; *((short *)(ea+1)) = TC;
	;   2. ea = *REGPC; *(ea+3) = TC;
	;
	; TM_mode = 'S'
	;   2. ea = *REGPC; *(ea+3) = TC;

	LD	C,1		; first TC point
	LD	A,(TM_MODE)
	CP	'S'
	JR	Z,NEXT_BC22	; skip insertion 1.

NEXT_BC222:
	; 1. ea = *REGPC; *((char *)(ea+1)) = TC;
	CALL	INSBRK_BRP
	JP	C,ERR_TRACE_SEQ

NEXT_BC221:
	; 2. ea = *REGPC; *(ea+3) = TC;
	LD	C,2		; second TC point
NEXT_BC22:
	CALL	INSBRK_3OP
	JP	C,ERR_TRACE_SEQ

	JP	END_INS_TC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1 insertion Trace code(TC)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
INSTC1:
	; check 1 byte machine code: branch (return)
	CP	0C9H		; RET ?
	JR	NZ,NEXT_BC3	; not RET

	; RET
	; trace operation:
	;   ea = *REGSP; *ea = TC;
	LD	C,1		; first TC point
	CALL	INSBRK_SP
	JP	C,ERR_TRACE_SEQ
	JP	END_INS_TC

	; check RST p
NEXT_BC3:
	LD	BC,RST_TBLE - RST_TBLS
	LD	HL,RST_TBLS
	CPIR
	JR	NZ,NEXT_BC4		; not RST p

	; RST p
	; can't trace: skip trace
	; trace operation:
	;   ea = *REGPC; *(ea+1) = TC;
;	LD	HL,RST_DMSG
;	CALL	STROUT		; message out "DETECT RST OPCODE"
	LD	C,1		; first TC point
	CALL	INSBRK_1OP
	JP	C,ERR_TRACE_SEQ

	JP	END_INS_TC

;RST_DMSG:
;	db	"(RST nn) WILL BE SKIPPED. AND TRACE NEXT OPCODE",CR,LF,00

	; check code 0EDH
NEXT_BC4:
	CP	0EDH		; CODE 0EDH ?
	JR	NZ,NEXT_BC5	; not 0EDH

	INC	HL
	LD	A,(HL)
	CP	45H		; RETN?
	JR	Z,NEXT_BC6	; yes, RETN
	CP	4DH		; RETI?
	JR	NZ,NEXT_BC5	; not RETN

	; trace operation:
	;   ea = *REGSP; *ea = TC;
NEXT_BC6:
	LD	C,1		; first TC point
	CALL	INSBRK_SP
	JR	C,ERR_TRACE_SEQ
	JR	END_INS_TC

	; check JP (HL)
NEXT_BC5:
	LD	HL,(TMPT)
	LD	A,(HL)

	CP	0E9H		; JP (HL) ?
	JR	NZ,NEXT_BC7	; not JP (HL)

	; JP (HL)
	; trace operation:
	;   ea = *REGHL; *ea = TC;
	LD	HL,(REGHL)
DJPHL:
	LD	C,1		; first TC point
	CALL	INADR_CHK_AND_WRT
	JR	C,ERR_TRACE_SEQ
	JR	END_INS_TC

	; check JP (IX)
NEXT_BC7:
	CP	0DDH		; 1st OPOCDE 0DDH ?
	JR	NZ,NEXT_BC8	; no 0DDH
	INC	HL
	LD	A,(HL)
	CP	0E9H		; JP (IX) ?
	JR	NZ,NEXT_BC8	; not JP (IX)

	; JP (IX)
	; trace operation:
	;   ea = *REGIX; *ea = TC;
	LD	HL,(REGIX)
	JR	DJPHL

	; check JP (IY)
NEXT_BC8:
	LD	HL,(TMPT)
	LD	A,(HL)

	CP	0FDH		; 1st OPOCDE 0FDH ?
	JR	NZ,NEXT_BC9	; no 0FDH
	INC	HL
	LD	A,(HL)
	CP	0E9H		; JP (IX) ?
	JR	NZ,NEXT_BC9	; not JP (IX)

	; JP (IY)
	; trace operation:
	;   ea = *REGIY; *ea = TC;
	LD	HL,(REGIY)
	JR	DJPHL

	; check JR relative
NEXT_BC9:
	LD	HL,(TMPT)
	LD	A,(HL)
	CP	18H		; JR relative ?
	JR	NZ,NEXT_BC10	; not JR relative

	; JR Relative
	; trace operation:
	;   ea = *REGPC; *(ea + 2 + *(ea+1)) = TC;
	LD	C,1		; first TC point
	CALL	REL_ADR_C
	CALL	INADR_CHK_AND_WRT
	JR	C,ERR_TRACE_SEQ
	JR	END_INS_TC

	; check JP literal
NEXT_BC10:
	CP	0C3H		; JP literal ?
	JR	NZ,NEXT_BC11	; not JP literal

	; JP literal
	; trace operation:
	; ea = *REGPC; *((char *)(ea+1)) = TC;
	LD	C,1		; first TC point
	CALL	INSBRK_BRP
	JR	C,ERR_TRACE_SEQ
	JR	END_INS_TC

	; check call literal
NEXT_BC11:
	CP	0CDH		; CALL literal ?
	JP	NZ,INS2		; no, check not branch opcode

	; CALL literal
	; trace operation:
	; TM_mode = 'I'
	;   ea = *REGPC; *((short *)(ea+1)) = TC;
	; TM_mode = 'S'
	;   2. ea = *REGPC; *(ea+3) = TC;

	LD	C,1		; first TC point
	LD	A,(TM_MODE)
	CP	'S'
	JR	Z,NEXT_BC111	; yes, TM_mode='S'

	; TM_mode = 'I'
	; ea = *REGPC; *((char *)(ea+1)) = TC;
	CALL	INSBRK_BRP
	JR	C,ERR_TRACE_SEQ
	JR	END_INS_TC

	; TM_mode = 'S'
	; ea = *REGPC; *(ea+3) = TC;
NEXT_BC111:
	CALL	INSBRK_3OP
	JR	C,ERR_TRACE_SEQ

END_INS_TC:
	JP	G0	; go, trace operation

ERR_TRACE_SEQ:
	LD	HL,TERR_MSG
	CALL	STROUT
	JP	WSTART
	RST	38H
;
TERR_MSG:	DB	"Adr ERR",CR,LF,0

;--------------------------------------
; 2 byte machine code branch
; - 2nd byte is Relative address
; - input hl : opecode address
; - output hl : target address
;--------------------------------------
REL_ADR_C:
	INC	HL
	LD	E,(HL)		; e = 2nd operand
	INC	HL		; hl = PC + 2
	LD	D,0FFH
	BIT	7,E		; test msb bit
	JR	NZ,EXP_MSB	;
	LD	D,0
EXP_MSB:
	ADD	HL,DE
	RET

;--------------------------------------
; 1 byte op code, insert TC
; ea = *REGPC; *(ea+1) = TC
;--------------------------------------
INSBRK_1OP:
;	LD	HL,(REGPC)
	LD	HL,(TMPT)
	JR	IB1

;--------------------------------------
; 2 byte op code, insert TC
; ea = *REGPC; *(ea+2) = TC
;--------------------------------------
INSBRK_2OP:
;	LD	HL,(REGPC)
	LD	HL,(TMPT)
	JR	IB2

;--------------------------------------
; 3 byte op code, insert TC
; ea = *REGPC; *(ea+3) = TC;
;--------------------------------------
INSBRK_3OP:
;	LD	HL,(REGPC)
	LD	HL,(TMPT)
IB3:	INC	HL
IB2:	INC	HL
IB1:	INC	HL
	CALL	INADR_CHK_AND_WRT
	RET

;--------------------------------------
; 3 byte op code, insert TC in branch point
; ea = *REGPC; *((char *)(ea+1)) = TC;
;--------------------------------------
INSBRK_BRP:
;	LD	HL,(REGPC)
	LD	HL,(TMPT)
	INC	HL
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	EX	DE,HL
	CALL	INADR_CHK_AND_WRT
	RET

;--------------------------------------
; insert TC at SP
; ea = *REGSP; *ea = TC;
;--------------------------------------
INSBRK_SP:
	LD	DE,(REGSP)
	LD	A,(DE)
	LD	L,A
	INC	DE
	LD	A,(DE)
	LD	H,A		; HL = *SP
	CALL	INADR_CHK_AND_WRT
	RET

;--------------------------------------
; check (HL) is RAM AREA
; insert Trace code at (HL)
;--------------------------------------
INADR_CHK_AND_WRT:
	LD	A,C
	CP	1		;first save?
	JR	NZ,ICKA1
	LD	DE,TPT1_F
	LD	(DE),A		; set trace ON
	LD	(TPT1_ADR),HL
	LD	A,(HL)		; get opcode
	LD	(TPT1_OP),A	; save opcode
	JR	ICKA_END
ICKA1:
	LD	DE,TPT2_F
	LD	(DE),A		; set trace ON
	LD	(TPT2_ADR),HL
	LD	A,(HL)		; get opcode
	LD	(TPT2_OP),A	; save opcode
ICKA_END:
	XOR	A
	RET

NO_RAM_AREA:
	SCF
	RET


;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2 insertion TC TABLE
;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 1 byte machine code: branch (RET CC)
RETCC_TBLS:
	DB	0C0H	; RET	NZ
	DB	0C8H	; RET	Z
	DB	0D0H	; RET	NC
	DB	0D8H	; RET	C
	DB	0E0H	; RET	PO
	DB	0E8H	; RET	PE
	DB	0F0H	; RET	P
	DB	0F8H	; RET	M
RETCC_TBLE:

; 2 byte machine code: branch (JR CC, Relative)
JRCC_TBLS:
	DB	10H	; DJNZ	$
JRCC_TBLS1:
	DB	20H	; JR	NZ,$
	DB	28H	; JR	Z,$
	DB	30H	; JR	NC,$
	DB	38H	; JR	C,$
JRCC_TBLE:

; 3 byte machine code: branch (JP CC, 16bit literal)
JPCC_TBLS:
	DB	0C2H	; JP	NZ,1234H
	DB	0CAH	; JP	Z,1234H
	DB	0D2H	; JP	NC,1234H
	DB	0DAH	; JP	C,1234H
	DB	0E2H	; JP	PO,1234H
	DB	0EAH	; JP	PE,1234H
	DB	0F2H	; JP	P,1234H
	DB	0FAH	; JP	M,1234H
JPCC_TBLE:

; (call 16bit literal)
CLCC_TBLS:
	DB	0C4H	; CALL	NZ,1234H
	DB	0CCH	; CALL	Z,1234H
	DB	0D4H	; CALL	NC,1234H
	DB	0DCH	; CALL	C,1234H
	DB	0E4H	; CALL	PO,1234H
	DB	0ECH	; CALL	PE,1234H
	DB	0F4H	; CALL	P,1234H
	DB	0FCH	; CALL	M,1234H
CLCC_TBLE:

;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1 insertion TC TABLE
;;;;;;;;;;;;;;;;;;;;;;;;;;

; restart
RST_TBLS:
	DB	0C7H	; RST	00H
	DB	0CFH	; RST	08H
	DB	0D7H	; RST	10H
	DB	0DFH	; RST	18H
	DB	0E7H	; RST	20H
	DB	0EFH	; RST	28H
	DB	0F7H	; RST	30H
	DB	0FFH	; RST	38H
RST_TBLE:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; machine code check(except branch)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
INS2:

	; 2byte machine code search
	LD	BC,TWO_OPTBL_E - TWO_OPTBL
	LD	HL,TWO_OPTBL
	CPIR
	JP	Z,MEET_OP2

	; 3byte machine code search
	LD	BC,THREE_OPTBL_E - THREE_OPTBL
	LD	HL,THREE_OPTBL
	CPIR
	JP	Z,MEET_OP3

	; check 0CBH

	; readjust hl
	LD	HL,(TMPT)

	CP	0CBH		; opecode 0CBH?
	JR	Z,MEET_OP2

	; check 0DDH
	CP	0ddh		; opecode 0DDH?
	JR	Z,MEET_DD

	; check 0EDH
	CP	0edh		; opecode 0EDH?
	JR	Z,MEET_ED

	; check 0FDH
	CP	0fdh		; opecode 0FDH?
	JR	Z,MEET_DD

	; 1byte machine code
	JR	MEET_OP1

	; opecode 0DDh
MEET_DD:
	INC	HL
	LD	A,(HL)
	CP	0CBH		; 2nd 0CBH?
	JR	Z,MEET_OP4
	CP	21H		; 2nd 21H?
	JR	Z,MEET_OP4
	CP	22H		; 2nd 22H?
	JR	Z,MEET_OP4
	CP	2AH		; 2nd 2AH?
	JR	Z,MEET_OP4
	CP	36H		; 2nd 36H?
	JR	Z,MEET_OP4

	; 2nd code search
	LD	BC,DD_2NDTBL_E - DD_2NDTBL
	LD	HL,DD_2NDTBL
	CPIR
	JR	Z,MEET_OP2
	JR	MEET_OP3

MEET_ED:
	INC	HL
	LD	A,(HL)
	CP	43H		; 2nd 43H?
	JR	Z,MEET_OP4
	CP	4BH		; 2nd 4BH?
	JR	Z,MEET_OP4
	CP	53H		; 2nd 53H?
	JR	Z,MEET_OP4
	CP	5BH		; 2nd 5BH?
	JR	Z,MEET_OP4
	CP	73H		; 2nd 73H?
	JR	Z,MEET_OP4
	CP	7BH		; 2nd 7BH?
	JR	Z,MEET_OP4
	JR	MEET_OP2

; 1 machine code
MEET_OP1:
	LD	C,1
	CALL	INSBRK_1OP
	JP	C,ERR_TRACE_SEQ
	JP	END_INS_TC

; 2 machine code
MEET_OP2:
	LD	C,1
	CALL	INSBRK_2OP
	JP	C,ERR_TRACE_SEQ
	JP	END_INS_TC
; 3 machine code
MEET_OP3:
	LD	C,1
	CALL	INSBRK_3OP
	JP	C,ERR_TRACE_SEQ
	JP	END_INS_TC

; 4 machine codee
MEET_OP4:
	LD	C,1
;	LD	HL,(REGPC)
	LD	HL,(TMPT)
	INC	HL
	CALL	IB3
	JP	C,ERR_TRACE_SEQ
	JP	END_INS_TC

TWO_OPTBL:	; second byte is 8bitliteral[nn]
LD_R_NN_S:
	DB	06h	; LD	B,nn
	DB	0Eh	; LD	C,nn
	DB	16h	; LD	D,nn
	DB	1Eh	; LD	E,nn
	DB	26h	; LD	H,nn
	DB	2Eh	; LD	L,nn
	DB	36h	; LD	(HL),nn
	DB	3Eh	; LD	A,nn
LD_R_NN_E:

LOG_OP_S:
	DB	0C6h	; ADD	A,nn
	DB	0CEh	; ADC	A,nn
	DB	0DEh	; SBC	A,nn
	DB	0D6h	; SUB	nn
	DB	0E6h	; AND	nn
	DB	0EEh	; XOR	nn
	DB	0F6h	; OR	nn
	DB	0FEh	; CP	nn
	DB	0DBh	; IN	A,(nn)
	DB	0D3h	; OUT	(nn),A
LOG_OP_E:
TWO_OPTBL_E:

THREE_OPTBL:	; 2nd, 3rd byte is 16bitliteral[nnnn]
	DB	01h	; LD	BC,nnnn
	DB	11h	; LD	DE,nnnn
	DB	21h	; LD	HL,nnnn
	DB	31h	; LD	SP,nnnn
THREE_OPTBLE:
	DB	22h	; LD	(nnnn),HL
	DB	32h	; LD	(nnnn),A

	DB	2Ah	; LD	HL,(nnnn)
	DB	3Ah	; LD	A,(nnnn)
THREE_OPTBL_E:

DD_2NDTBL:
	DB	09h	; ADD	IX,BC
	DB	19h	; ADD	IX,DE
	DB	29h	; ADD	IX,IX
	DB	39h	; ADD	IX,SP
DD_2NDTBL1:
	DB	23h	; INC	IX
	DB	2Bh	; DEC	IX
	DB	0E5h	; PUSH	IX
	DB	0E1h	; POP	IX
DD_2NDTBL2:
	DB	0E3h	; EX	(SP),IX
	DB	0F9h	; LD	SP,IX
DD_2NDTBL_E:

;;;
;;; Dump memory
;;;

DUMP:
	INC	HL
	CALL	SKIPSP
	CALL	RDHEX		; 1st arg.
	JR	C,DP0
	;; 1st arg. found
	LD	(DSADDR),DE
	JR	DP00

DP0:	;; No arg. chk

	PUSH	HL
	LD	HL,(DSADDR)
	LD	BC,128
	ADD	HL,BC
	LD	(DEADDR),HL
	POP	HL
	LD	A,(HL)
	OR	A
	JR	Z,DPM_C	; no arg.

DP00:	CALL	SKIPSP
	LD	A,(HL)
	CP	','
	JR	Z,DP1
	OR	A
	JP	NZ,ERR

	;; No 2nd arg.

	LD	HL,128
	ADD	HL,DE
	LD	(DEADDR),HL
	JR	DPM_C

DP1:	INC	HL
	CALL	SKIPSP
	CALL	RDHEX
	JP	C,ERR
	CALL	SKIPSP
	OR	A
	JP	NZ,ERR
	INC	DE
	LD	(DEADDR),DE
DPM_C:	CALL	DPM
	JP	WSTART

	;; DUMP main
DPM:
	LD	HL,(DSADDR)
	LD	A,0F0H
	AND	L
	LD	L,A
	XOR	A
	LD	(DSTATE),A
DPM0:	PUSH	HL
	CALL	DPL
	POP	HL
	LD	BC,16
	ADD	HL,BC
	CALL	CONST
	JR	NZ,DPM1
	LD	A,(DSTATE)
	CP	2
	JR	C,DPM0
	LD	HL,(DEADDR)
	LD	(DSADDR),HL
	RET
DPM1:	LD	(DSADDR),HL
	JP	CONIN

DPL:	; DUMP line
	CALL	HEXOUT4
	PUSH	HL
	LD	HL,DSEP0
	CALL	STROUT
	POP	HL
	LD	IX,LINEBUF
	LD	B,16
DPL0:	CALL	DPB
	DJNZ	DPL0
	LD	HL,DSEP1
	CALL	STROUT
	LD	HL,LINEBUF
	LD	B,16
DPL1:	LD	A,(HL)
	INC	HL
	CP	' '
	JR	C,DPL2
	CP	7FH
	JR	NC,DPL2
	CALL	CONOUT
	JR	DPL3
DPL2:	LD	A,'.'
	CALL	CONOUT
DPL3:	DJNZ	DPL1
	JP	CRLF

DPB:	; Dump byte
	LD	A,' '
	CALL	CONOUT
	LD	A,(DSTATE)
	OR	A
	JR	NZ,DPB2
	; Dump state 0
	LD	A,(DSADDR)	; Low byte
	CP	L
	JR	NZ,DPB0
	LD	A,(DSADDR+1)	; High byte
	CP	H
	JR	Z,DPB1
DPB0:	; Still 0 or 2
	LD	A,' '
	CALL	CONOUT
	CALL	CONOUT
	LD	(IX),A
	INC	HL
	INC	IX
	RET
DPB1:	; Found start address
	LD	A,1
	LD	(DSTATE),A
DPB2:	LD	A,(DSTATE)
	CP	1
	JR	NZ,DPB0
	; Dump state 1
	LD	A,(HL)
	LD	(IX),A
	CALL	HEXOUT2
	INC	HL
	INC	IX
	LD	A,(DEADDR)	; Low byte
	CP	L
	RET	NZ
	LD	A,(DEADDR+1)	; High byte
	CP	H
	RET	NZ
	; Found end address
	LD	A,2
	LD	(DSTATE),A
	RET

;;;
;;; Disassemble
;;;

; A[<address>][,s<steps>|<end address>]

DISASSEMBLE:
	INC	HL
	CALL	SKIPSP
	CALL	RDHEX		; 1st arg.
	JR	NC,GET_DI1

DI_NXT:
	;; No arg. chk
	CALL	SKIPSP
	LD	A,(HL)
	OR	A
	JR	NZ,CHK_DI1	; ',' check

; No arg
	XOR	A
	LD	L,A
	LD	L,10
	LD	(DASM_STPF),A	; set step flag
	LD	(DASM_ED),HL	; set 10 steps
	JR	DISASM_GO

; 1st arg
GET_DI1:
	LD	(DASM_ADR),DE	; save start address
;	INC	HL
	JR	DI_NXT

CHK_DI1:
	CP	','
	JP	NZ,ERR

; check 2nd arg

	INC	HL
	CALL	SKIPSP
	CP	'S'
	JR	NZ,CHK_STPDI

; step arg
	LD	A,1
	LD	(DASM_STPF),A	; set step flag
	INC	HL
	CALL	GET_NUM		; get decimal number to binary
	JP	C,ERR
	LD	(DASM_ED), BC	; set steps
	JR	DISASM_GO

CHK_STPDI:
	CALL	RDHEX		; 2nd arg.
	JP	C,ERR
	LD	(DASM_ED),DE	; set end address
	XOR	A
	LD	(DASM_STPF),A	; clear step flag

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; operation Disassemble
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DISASM_GO:
	CALL	CONST
	JR	Z,DIS_GO1
	CALL	CONIN		;discard key
	JP	WSTART		; exit disasm command

DIS_GO1:
	; get opcode
	CALL	DIS_ANALYSIS
	CALL	MK_ADR_STR	; conout address and machine code
				; *dasm_adr is next opcode address
	LD	HL,ADR_MC_BUF
	CALL	STROUT		; conout disassemble strings
	LD	A,(DASM_STPF)
	OR	A
	JR	NZ,CALC_DIS_STEP

	; *dasm_adr > *dasm_ed ?, yes, finish
	LD	HL,(DASM_ED)
	LD	BC,(DASM_ADR)
	SBC	HL, BC
	JR	NC,DISASM_GO
	JP	WSTART

CALC_DIS_STEP:
	LD	HL,(DASM_ED)
	DEC	HL
	LD	(DASM_ED),HL
	LD	A,H
	OR	L
	JR	NZ,DISASM_GO
	JP	WSTART

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Disassemble and maked strings to user buffer
; input de : user buffer
;       hl : disassemble address
; output de : next MC address
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GET_DISASM_ST:
	LD	(DASM_ADR),HL
	PUSH	DE
	CALL	DIS_ANALYSIS
	CALL	MK_ADR_STR	; conout address and machine code
	POP	DE
	LD	HL,ADR_MC_BUF
	LD	BC,DASM_BE - DASM_BS
	LDIR
	LD	DE,(DASM_ADR)
	LD	A,(MC_SIZE)
	RET

;-------------------------------------------------
; Make address and machine code at adr_mc_buf
; "XXXX XX XX XX XX " (17bytes)
;-------------------------------------------------
MK_ADR_STR:
	LD	HL,ADR_MC_BUF
	LD	DE,(DASM_ADR)
	CALL	HEX4STR		; address XXXX
	CALL	INS_SPCR
	CALL	INS_SPCR
	LD	B,4
	LD	A,(MC_SIZE)
	LD	C,A
MAS_1:	LD	A,(DE)
	INC	DE

	PUSH	DE
	LD	E,A
	CALL	HEX2STR		; MC XX
	CALL	INS_SPCR
	POP	DE
	DEC	B
	JR	Z,MAS_3		; end
	DEC	C
	JR	NZ,MAS_1

MAS_2:	CALL	INS_SPCR
	CALL	INS_SPCR
	CALL	INS_SPCR
	DEC	B
	JR	NZ,MAS_2

MAS_3:	CALL	INS_SPCR
	LD	(DASM_ADR),DE	; set next analysis address
	RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; dis assemble analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DIS_ANALYSIS:

	; pre init string buffer

	LD	A,1
	LD	(MC_SIZE),A
	LD	DE,LDSTR	; insert LD string
	CALL	MKOPCSTR
	LD	HL,DASM_OPRSTR
	CALL	INSPOSTSTR	; CR,LF,0
	LD	HL,(DASM_ADR)
	LD	A,(HL)		; get opcode

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; analysys 1 byte MC
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;  check no operand

	LD	BC,OTH_1OP_E - OTH_1OP_S
	LD	HL,OTH_1OP_S
	CPIR
	JR	NZ,CHK_LD

;  no operand

	LD	HL,OTH_1OP_TBL
	CALL	GET_STRBUFPOINT
	JP	MKOPCSTR

GET_R_NUM:
	AND	38H
	RRCA
	RRCA
	RRCA
	LD	C,A
	LD	B,0		; bc ; register No.
	RET
;
; LD opecode
;

; check LD "A,(BC)", "A,(DE)", "(BC),A", "(DE),A", "SP,HL"

CHK_LD:
	CP	0AH	; LD	A,(BC)
	JR	Z,LD_A_KBCK
	CP	1AH	; LD	A,(DE)
	JR	Z,LD_A_KDEK
	CP	02H	; LD	(BC),A
	JR	Z,LD_KBCK_A
	CP	12H	; LD	(DE),A
	JR	Z,LD_KDEK_A
	CP	0F9H	; LD	SP,HL
	JR	NZ,CHK_LD1

; LD SP, HL
	LD	DE,RNSP
	CALL	MK_RCS		; "SP, "
	JP	HL_CRLF

; LD A, (BC)
LD_A_KBCK:
	LD	DE,RNBC
LD_A_KXXK:
	PUSH	DE
	CALL	A_COLON_SP	; "A, "
	POP	DE
	CALL	MK_KRK		; "(BC)", "(DE)"
	JP	INSPOSTSTR	; CR,LF,0

; LD A, (DE)
LD_A_KDEK:
	LD	DE,RNDE
	JR	LD_A_KXXK

; LD (BC),A
LD_KBCK_A:
	LD	DE,RNBC

LD_KXXK_A:
	CALL	MK_KRKCS	; "(BC), ", "(DE), "
	JP	A_CRLF		; "A",CR,LF

; LD (DE),A
LD_KDEK_A:
	LD	DE,RNDE
	JR	LD_KXXK_A

; check other 1byte LD MC

CHK_LD1:
	CP	40H
	JP	C,CHK_INC
	CP	80H
	JR	NC,CHK_ADD
	CP	76H		; HALT?
	JR	NZ,LD1OP

	LD	DE,HLTSTR	; HALT string
	JP	MKOPCSTR	; de : next string buffer addr

; LD

LD1OP:
	CALL	GET_R_NUM
	CALL	GET_RSTG_OFF
	LD	HL,DASM_OPRSTR
	CALL	MK_RCS		; "REG, "

MK_2NDOPR:
	LD	DE,(DASM_ADR)
	LD	A,(DE)		; get opcode
	AND	07H
	LD	C,A
	LD	B,0

MK_2NDOPR1:
	PUSH	HL		; save copy buffer
	CALL	GET_RSTG_OFF
	POP	HL		; copy buffer
	JP	CPSTR_CRLF	; "REG",CR,LF

; ADD 80H - 87H
; check ADD code

CHK_ADD:
	CP	88H
	JR	NC,CHK_ADC

; ADD
	CALL	ADD_OPSTR
	CALL	A_COLON_SP
	JR	MK_2NDOPR

; ADC 88H - 8FH
; check ADC code

CHK_ADC:
	CP	90H
	JR	NC,CHK_SUB

; ADC
	LD	DE,ADCSTR
	CALL	MKOPCSTR
	CALL	A_COLON_SP
	JR	MK_2NDOPR

; SUB 90H - 97H
; check SUB code

CHK_SUB:
	CP	98H
	JR	NC,CHK_SBC

; SUB
	LD	DE,SUBSTR
	CALL	MKOPCSTR
	JR	MK_2NDOPR

; SBC 98H - 9FH
; check SBC code

CHK_SBC:
	CP	0A0H
	JR	NC,CHK_AND

; SBC
	LD	DE,SBCSTR
	CALL	MKOPCSTR
	CALL	A_COLON_SP
	JR	MK_2NDOPR

; AND 0A0H - 0A7H
; check AND code

CHK_AND:
	CP	0A8H
	JR	NC,CHK_XOR

; AND
	LD	DE,ANDSTR
	CALL	MKOPCSTR
	JR	MK_2NDOPR

; XOR 0A8H - 0AFH
; check XOR code

CHK_XOR:

	CP	0B0H
	JR	NC,CHK_OR

; XOR
	LD	DE,XORSTR
	CALL	MKOPCSTR
	JR	MK_2NDOPR

; OR 0B0H - 0B7H
; check OR code

CHK_OR:
	CP	0B8H
	JR	NC,CHK_CP

; OR
	LD	DE,ORSTR
	CALL	MKOPCSTR
	JR	MK_2NDOPR

; CP 0B8H - 0BFH
; check CP code

CHK_CP:
	CP	0C0H
	JP	NC,CHK_POP_RP

; CP
	LD	DE,CPSTR
	CALL	MKOPCSTR
	JR	MK_2NDOPR

;check INC

CHK_INC:
	LD	BC,INC_OPCDTBLE - INC_OPCDTBLS
	LD	HL,INC_OPCDTBLS
	CPIR
	JR	NZ,CHK_DEC

; INC
	PUSH	AF
	LD	DE,INCSTR
	CALL	MKOPCSTR
	POP	AF

INC_DEC:
	CALL	GET_R_NUM
	JP	MK_2NDOPR1

;check dec

CHK_DEC:
	LD	BC,DEC_OPCDTBLE - DEC_OPCDTBLS
	LD	HL,DEC_OPCDTBLS
	CPIR
	JR	NZ,CHK_INC_RP

; DEC
	PUSH	AF
	LD	DE,DECSTR
	CALL	MKOPCSTR
	POP	AF
	JR	INC_DEC

; check inc rp

CHK_INC_RP:
	LD	BC,INC_RP_E - INC_RP_S
	LD	HL,INC_RP_S
	CPIR
	JR	NZ,CHK_DEC_RP

; INC rp
	LD	DE,INCSTR
	CALL	MKOPCSTR

	LD	HL,INC_DEC_ADDRP
	CALL	MK_STR
	JP	INSPOSTSTR	; CR,LF,0

; check dec rp

CHK_DEC_RP:
	LD	BC,DEC_RP_E - DEC_RP_S
	LD	HL,DEC_RP_S
	CPIR
	JR	NZ,CHK_ADD_RP

; dec rp
	LD	DE,DECSTR
	CALL	MKOPCSTR

	LD	HL,INC_DEC_ADDRP
	CALL	MK_STR
	JP	INSPOSTSTR	; CR,LF,0

;check ADD HL, rp

CHK_ADD_RP:
	LD	BC,ADD_RP_E - ADD_RP_S
	LD	HL,ADD_RP_S
	CPIR
	JR	NZ,CHK_EX

; add hl, rp

	CALL	ADD_OPSTR

	LD	DE,RNHL
	CALL	MK_RCS		; "HL, "

	PUSH	HL
	LD	HL,INC_DEC_ADDRP
	CALL	GET_STRBUFPOINT
	POP	HL
	JP	CPSTR_CRLF	; CR,LF,0

; check POP rp

CHK_POP_RP:
	LD	BC,POP_RP_E - POP_RP_S
	LD	HL,POP_RP_S
	CPIR
	JR	NZ,CHK_PUSH_RP

; POP RP

	LD	DE,POPSTR
	CALL	MKOPCSTR

	LD	HL,POP_PUSHRP
	CALL	MK_STR
	JP	INSPOSTSTR	; CR,LF,0

; check PUSH rp

CHK_PUSH_RP:
	LD	BC,PUSH_RP_E - PUSH_RP_S
	LD	HL,PUSH_RP_S
	CPIR
	JR	NZ,CHK_EX

; PUSH RP

	LD	DE,PUSHSTR
	CALL	MKOPCSTR

	LD	HL,POP_PUSHRP
	CALL	MK_STR
	JP	INSPOSTSTR	; CR,LF,0

; check EX XX, XX

CHK_EX:
	CP	08H		; EX	AF,AF'
	JR	Z,EX_AF_AF
	CP	0E3H		; EX	(SP),HL
	JR	Z,DEX_SP_HL
	CP	0EBH		; EX	DE,HL
	JR	NZ,CHK_1MC_BNH

; EX DE, HL
	LD	DE,RNDE
	CALL	MK_RCS

INS_HL_OPR:
	CALL	HL_CRLF

INS_EX_OPC:
	LD	DE,EXSTR
	JP	MKOPCSTR

; EX AF,AF'
EX_AF_AF:
	LD	DE,RNAF
	CALL	MK_RCS
	LD	DE,RNAFX
	CALL	CPSTR_CRLF
	JR INS_EX_OPC

; EX (SP),HL
DEX_SP_HL:
	LD	DE,RNSP
	CALL	MK_KRKCS
	JR	INS_HL_OPR

; check other one MC code

CHK_1MC_BNH:

; check JP (HL)

	CP	0E9H		; JP (HL) ?
	JR	NZ,CHK_RET

; JP (HL)
	LD	DE,JPSTR
	CALL	MKOPCSTR	; "JP "

	LD	DE,RNHL
	CALL	MK_KRK_1
	JP	INSPOSTSTR

; check RET CC
CHK_RET:
	LD	BC,RETCC_TBLE - RETCC_TBLS
	LD	HL,RETCC_TBLS
	CPIR
	JR	NZ,CHK_RST

; RET CC
; BC : 7 >= BC >= 0

	; make opcode string
	LD	DE,RETSTR
	CALL	MKOPCSTR

	; make operand string

	LD	HL,CC_OPR	; string base
	CALL	MK_STR
	JP	INSPOSTSTR	; CR,LF,0

; check RST p

CHK_RST:
	LD	BC,RST_TBLE - RST_TBLS
	LD	HL,RST_TBLS
	CPIR
	JR	NZ,CHK_2MC	; 2bytes MC

; RST p

	LD	DE,RSTSTR
	CALL	MKOPCSTR	; de : next string buffer addr

; 0 <= BC <= 7
; 7: 00H  (0000 0 111 : 00 000 000)
; 6: 08H  (0000 0 110 : 00 001 000)
; 5: 10H  (0000 0 101 : 00 010 000)
; 4: 18H  (0000 0 100 : 00 011 000)
; 3: 20H  (0000 0 011 : 00 100 000)
; 2: 28H  (0000 0 010 : 00 101 000)
; 1: 30H  (0000 0 001 : 00 110 000)
; 0: 38H  (0000 0 000 : 00 111 000)
;
	LD	A,C
	CPL		; not a
	SLA	A
	SLA	A
	SLA	A
	AND	38H	; a = RST No.

	LD	E,A
	LD	HL,DASM_OPRSTR
	JP	MK_N2CRLF	; "nnH",CR,LF

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; analysys 2 byte MC
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CHK_2MC:

	PUSH	AF
	LD	A,2
	LD	(MC_SIZE),A
	POP	AF

; check LD r, nn

	LD	BC,LD_R_NN_E - LD_R_NN_S
	LD	HL,LD_R_NN_S
	CPIR
	JR	NZ,CHK_LOGOP2

; LD r, nn

	CALL	GET_R_NUM
	CALL	GET_RSTG_OFF
	CALL	MK_RCS		; "REG, "

	LD	DE,(DASM_ADR)
	INC	DE
	LD	A,(DE)		; get nn
	LD	E,A
	CALL	HEX2STR_ASM	; "nnH"
	JP	INSPOSTSTR	; CR,LF,0

; check logical operation with 8 bit literal

CHK_LOGOP2:

	LD	BC,LOG_OP_E - LOG_OP_S
	LD	HL,LOG_OP_S
	CPIR
	JP	NZ,CHK_DJNZ

; logical operation with 8 bit literal


	PUSH	BC
	LD	HL,LOGOP2STR
	CALL	GET_STRBUFPOINT
	CALL	MKOPCSTR		; make op code string
	POP	BC
	LD	A,C

	CP	1
	JR	Z,ACS_KN2K	; insert "A, (nn)"
	OR	A
	JR	Z,INS_KN2K	; insert "(nn),A"
	CP	7
	JR	C,INS_N2CRLF	; insert  "nnH"

; insert "A, "

	CALL	A_COLON_SP

; "nn"
INS_N2CRLF:
	LD	DE,(DASM_ADR)
	INC	DE
	LD	A,(DE)
	LD	E,A		; get nn
	JP	MK_N2CRLF	; "nnH",CR,LF

ACS_KN2K:
	CALL	A_COLON_SP	; "A, "
	CALL	KN2K		; "(nnH)"
	JP	INSPOSTSTR	; CR,LF,0

; "(nn),A"
INS_KN2K:
	LD	HL,DASM_OPRSTR	;operand str buffer
	CALL	KN2K		; "(nnH)"
	CALL	INS_KMR		; " ,"
	JP	A_CRLF		; "A",CR,LF,0

; "(nnH)"
KN2K:
	CALL	INS_KAKKOL	; "("
	LD	DE,(DASM_ADR)
	INC	DE
	LD	A,(DE)		; get nn
	LD	E,A		; e: nn
	CALL	HEX2STR_ASM	; hex strings
	JP	INS_KAKKOR	; ")"


; check DJNZ nn
CHK_DJNZ:
	CP	10H
	JR	NZ,CHK_JRNN

; check DJNZ nn
	LD	DE,DJNZSTR
	JR	JR_N4CRLF

; check jr nn
CHK_JRNN:
	CP	18H
	JR	NZ,CHK_JRCC	; not JR relative

; JR Relative
	LD	DE,JRSTR
JR_N4CRLF:
	CALL	MKOPCSTR	; de : next string buffer addr
	LD	HL,DASM_OPRSTR
	JP	MKREL_STR	; "nnnnH",cr,lf : nnnn : branch address

; JR CC, nn

CHK_JRCC:
	LD	BC,JRCC_TBLE - JRCC_TBLS1
	LD	HL,JRCC_TBLS1
	CPIR
	JR	NZ,chk_3MC	; no, check 3 bnytes MC

; JR CC, nn(Relative Value)

	LD	DE,JRSTR
	CALL	MKOPCSTR
	LD	HL,JRCC_OPR1	; string base
	CALL	MK_STR		; "NZ", "Z", "NC", "C"
	CALL	INS_KMR		; ", "
	JP	MKREL_STR	; "nnnnH",CR,LF

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; analysys 3 byte MC
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CHK_3MC:
	PUSH	AF
	LD	A,3
	LD	(MC_SIZE),A
	POP	AF

	LD	BC,THREE_OPTBLE - THREE_OPTBL
	LD	HL,THREE_OPTBL
	CPIR
	JP	NZ,CHK_LD16

; LD rp, nnnn

	LD	HL,LOGOP3STR
	CALL	GET_STRBUFPOINT
	CALL	MK_RCS		; "Reg, "

GET_N4CRLF:
	CALL	GET_N4
	JP	MK_N4CRLF	; "nnnnH",CR,LF,0

; LD 16bit literal
CHK_LD16:
	CP	22H
	JR	Z,INS_KKHL	; "(nnnnH),HL"
	CP	32H
	JR	Z,INS_KKA	; "(nnnnH),A"
	CP	2AH
	JR	Z,INS_HLKK	; "HL, (nnnnH)"
	CP	3AH
	JP	NZ,CHK_JPN4	; check jp n4

; "A, (nnnnH)"
	CALL	A_COLON_SP	; "A, "
	JR	KN4KCRLF

; "(nnnnH),HL"
INS_KKHL:
	CALL	KN4HK
	JP	HL_CRLF		; "HL",cr,lf

; "(nnnnH),A"
INS_KKA:
	CALL	KN4HK
	JP	A_CRLF		; "A",cr,lf

KN4HK:
	CALL	GET_N4
	JP	INS_KN4KCS	; "(nnnnH), "

GET_N4:
	PUSH	HL
	LD	HL,(DASM_ADR)
	INC	HL
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	POP	HL
	RET

; "HL, (nnnnH)"
INS_HLKK:
	LD	DE,RNHL
	CALL	MK_RCS		; "HL ,"

KN4KCRLF:
	CALL	GET_N4
	CALL	MK_KN4K		; "(nnnnH)"
	JP	INSPOSTSTR	; cr,LF

; check JP literal

CHK_JPN4:
	CP	0C3H		; JP literal ?
	JR	NZ,CHK_CALLN4	; not JP literal

; "JP nnnnH"

	LD	DE,JPSTR
	CALL	MKOPCSTR	; de : next string buffer addr
	JR	GET_N4CRLF

; check call literal

CHK_CALLN4:
	CP	0CDH		; CALL literal ?
	JR	NZ,CHK_JPCC

; "CALL nnnnH",cr,lf

	LD	DE,CALLSTR
	CALL	MKOPCSTR	; de : next string buffer addr
	JR	GET_N4CRLF


; check 3 byte machine code: branch JP CC, nnnn

CHK_JPCC:
	LD	BC,JPCC_TBLE - JPCC_TBLS
	LD	HL,JPCC_TBLS
	CPIR
	JR	NZ,CHK_CALCC

; JP CC, nnnn
; BC : 7 >= BC >= 0

	LD	DE,JPSTR

CC_NNNN:
	CALL	MKOPCSTR	; de : next string buffer addr

	LD	HL,CC_OPR	; string base
	CALL	MK_STR		; de: point (string end) + 1
	CALL	INS_KMR		; " ,"
	JR	GET_N4CRLF	; "nnnnh",CR,lf


; check 3 byte machine code: branch (CALL CC, nnnn 16bit literal)

CHK_CALCC:
	LD	BC,CLCC_TBLE - CLCC_TBLS
	LD	HL,CLCC_TBLS
	CPIR
	JR	NZ,CHK_0CBH

; CALL CC, nnnn
; BC : 7 >= BC >= 0 CALLstr

	LD	DE,CALLSTR
	JR	CC_NNNN

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; OPECODE 0CBH check
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CHK_0CBH:
	LD	HL,(DASM_ADR)
	INC	HL		; 2nd opecode address

	PUSH	AF
	LD	A,2
	LD	(MC_SIZE),A	; set 2bytes MC
	POP	AF

	CP	0CBH
	JP	NZ,CHK_0DDH

	LD	A,(HL)		; a : 2nd opecode
	LD	HL,DASM_OPRSTR	; set operand string buffer

	CP	08H
	JP	C,MK_RLCR
	CP	10H
	JP	C,MK_RRCR
	CP	18H
	JP	C,MK_RLR
	CP	20H
	JP	C,MK_RRR
	CP	28H
	JP	C,MK_SLAR
	CP	30H
	JP	C,MK_SRAR
	CP	38H
	JP	C,OP_ERR
	CP	40H
	JP	C,MK_SRLR
	CP	80H
	JP	C,MK_BITR
	CP	0C0H
	JP	C,MK_RESR

; make SET n, r, SET n, (HL)

	CALL	MK_BITR_STR
	LD	DE,SETSTR
	JP	MKOPCSTR


; make RLC r, RLC (HL)
MK_RLCR:
	CALL	INS_RSTG
	LD	DE,RLCSTR
	JP	MKOPCSTR

; make RRC r, RRC (HL)
MK_RRCR:
	CALL	INS_RSTG
	LD	DE,RRCSTR
	JP	MKOPCSTR

; make RL r, RL (HL)
MK_RLR:
	CALL	INS_RSTG
	LD	DE,RLSTR
	JP	MKOPCSTR

; make RR r, RR (HL)
MK_RRR:
	CALL	INS_RSTG
	LD	DE,RRSTR
	JP	MKOPCSTR

; make SLA r, SLA (HL)
MK_SLAR:
	CALL	INS_RSTG
	LD	DE,SLASTR
	JP	MKOPCSTR

; make SRA r, SRA (HL)
MK_SRAR:
	CALL	INS_RSTG
	LD	DE,SRASTR
	JP	MKOPCSTR

; make SRL r, SRL (HL)
MK_SRLR:
	CALL	INS_RSTG
	LD	DE,SRLSTR
	JP	MKOPCSTR

; make BIT n, r, BIT n, (HL)
MK_BITR:
	CALL	MK_BITR_STR
	LD	DE,BITSTR
	JP	MKOPCSTR

; make RES n, r, RES n, (HL)
MK_RESR:
	CALL	MK_BITR_STR
	LD	DE,RESSTR
	JP	MKOPCSTR

OP_ERR:
	LD	DE,ER_OPMSG
	JP	MKOPCSTR

;-------------------------------------------
; input a : 2nd opecode
;	hl : make string buffer
; make "bit No, r" string to *hl
; (ex) *hl = "1, B"
;-------------------------------------------
MK_BITR_STR:
	PUSH	AF
	CALL	SET_BITNO
	POP	AF

INS_RSTG:
	AND	07H
	LD	C,A		; table offset
	LD	B,0		; bc : string offset
	PUSH	HL
	CALL	GET_RSTG_OFF	; get string address to de
	POP	HL
	JP	CPSTR_CRLF	; "REG",CR,LF,0
;
; input hl : make string buffer
;
SET_BITNO:
	AND	38H
	RRCA
	RRCA
	RRCA
	LD	B,A		; bit number

	LD	A,30H
	OR	B		; a : bit string "0" - "7"
	LD	(HL),A
	INC	HL
	JP	INS_KMR

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; OPECODE 0DDH check
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CHK_0DDH:
	CP	0DDH
	JP	NZ,CHK_0EDH

	LD	DE,RNIX
	LD	(REG_XY),DE	; save index reg string
	LD	DE,ADDIXRP_TBL
	LD	(XY_SRTP),DE

CHK_0DDH_1:

	LD	A,(HL)		; get 2nd opcode

	CP	0E3H		; EX (SP),IX
	JP	Z,INS_KSPKIX
	CP	0E9H		; JP (IX)
	JP	Z,INS_JPIX
	CP	0F9H		; LD SP, IX
	JP	Z,INS_SPIX

	LD	BC,DD_2NDTBL1 - DD_2NDTBL
	LD	HL,DD_2NDTBL
	CPIR
	JP	Z,INS_IXRP

	LD	BC,DD_2NDTBL2 - DD_2NDTBL1
	LD	HL,DD_2NDTBL1
	CPIR
	JP	NZ,CHK_0DD3OP

; INC	IX
; DEC	IX
; PUSH	IX
; POP	IX
	LD	HL,DD_2OPT
	CALL	GET_STRBUFPOINT
	CALL	MKOPCSTR		; inseert OPCODE strings to dasm_OpcStr

	LD	HL,DASM_OPRSTR
	JP	IX_CRLF			; "IX",CR,LF,0

; JP	(IX)
INS_JPIX:
	LD	DE,(REG_XY)
INS_JPIX1:
	PUSH	DE
	LD	DE,JPSTR
	CALL	MKOPCSTR	; de : next string buffer addr

	POP	DE
	CALL	MK_KRK_1
	JP	INSPOSTSTR	; CR,LF,0

; EX	(SP),IX
INS_KSPKIX:
	LD	DE,EXSTR
	CALL	MKOPCSTR	; de : next string buffer addr
	LD	DE,RNSP
	CALL	MK_KRKCS	; "(SP), "
	JP	IX_CRLF		; "IX",cr,lf

; LD	SP,IX
INS_SPIX:
	LD	DE,RNSP
	CALL	MK_RCS		; "SP, "
	JP	IX_CRLF		; "IX",cr,lf

; ADD	IX,BC
; ADD	IX,DE
; ADD	IX,IX
; ADD	IX,SP
INS_IXRP:
	CALL	ADD_OPSTR	; de : next string buffer addr

	LD	DE,(REG_XY)
	CALL	MK_RCS		; "IX, "

	PUSH	HL
	LD	HL,(XY_SRTP)
	CALL	GET_STRBUFPOINT
	POP	HL
	JP	CPSTR_CRLF	; "REG",cr,lf

;
; check 0DD 3bytes MC
;
CHK_0DD3OP:
	PUSH	AF
	LD	A,3
	LD	(MC_SIZE),A	; 2byte machine code
	POP	AF

	LD	BC,DD_LD_TBLE - DD_LD_TBL
	LD	HL,DD_LD_TBL
	CPIR
	JP	Z,DD_LD

	CP	86H
	JP	Z,DD_MIX
	CP	8EH
	JP	Z,DD_MIX1
	CP	9EH
	JP	Z,DD_MIX2

	LD	BC,DD_LOG_TBLE - DD_LOG_TBL
	LD	HL,DD_LOG_TBL
	CPIR
	JP	NZ,CHK_0DD4OP

;
; make "SUB (IX+nn)", "AND (IX+nn)", "XOR (IX+nn)"
;       "OR (IX+nn)",  "CP (IX+nn)"
;      "INC (IX+nn)", "DEC (IX+nn)"
;
	LD	HL,DDLOGTBL
	CALL	GET_STRBUFPOINT
	CALL	MKOPCSTR

MK_KIXPNK:
	LD	DE,(REG_XY)

MK_KIYPNK:
	LD	HL,DASM_OPRSTR
	JP	KIXYPNK_CRLF		; "(IX+nnH)",cr,lf

;
; make "LD (IX+nn), r" or "LD r, (IX+nn)"
;
DD_LD:
	LD	A,C
	CP	7
	JP	NC,DD_LD1

; LD (IX+nn), r
	LD	DE,(REG_XY)
	CALL	KIXYPNK_CS	; make "(IX+nn), "

	PUSH	HL
	LD	HL,DD_LDTBL
	CALL	GET_STRBUFPOINT
	POP	HL
	JP	CPSTR_CRLF	; "A", "L", "H", "E", D", "C", "B"
				; CR,LF,0

; LD r, (IX+nn)
DD_LD1:
	SUB	7
	LD	C,A
	LD	HL,DD_LDTBL
	CALL	GET_STRBUFPOINT

	CALL	MK_RCS		; "REG, "
DD_MIX4:
	LD	DE,(REG_XY)
	JP	KIXYPNK_CRLF	; "(IX+nn)",CR,LF,0


; "ADD A,(IX+nn)"
DD_MIX:
	LD	DE,ADDSTR
DD_MIX3:
	CALL	MKOPCSTR	; "ADD"
	CALL	A_COLON_SP	; "A, "
	JR	DD_MIX4		; "(IX+nn)",cr,lf

;"ADC A,(IX+nn)"
DD_MIX1:
	LD	DE,ADCSTR
	JR	DD_MIX3

;"SBC A,(IX+nn)"
DD_MIX2:
	LD	DE,SBCSTR
	JR	DD_MIX3

;
; check 0DD 4bytes MC
;
CHK_0DD4OP:
	PUSH	AF
	LD	A,4
	LD	(MC_SIZE),A	; 2byte machine code
	POP	AF

	CP	21H
	JP	Z,DD_21
	CP	22H
	JP	Z,DD_22
	CP	2AH
	JP	Z,DD_2A
	CP	36H
	JP	NZ,CHK_DD_CB
; DD 36
; "LD (IX+xx), yy"

	LD	DE,(REG_XY)
	CALL	KIXYPNK_CS	; "(IX+xx), "
	LD	E,D		; e: yy
	JP	MK_N2CRLF	; "0yyH",cr,lf


; "LD IX, nnnn"
DD_21:
	LD	DE,(REG_XY)
	CALL	MK_RCS		; "IX, "
	CALL	GET_NNNN
	JP	MK_N4CRLF	; "nnnnH",cr,lf

; "LD (nnnn), IX"
DD_22:
	CALL	GET_NNNN
	CALL	INS_KN4KCS	; "(nnnnH), "
	JP	IX_CRLF		; "IX",cr,lf

; "LD IX, (nnnn)"
DD_2A:
	LD	DE,(REG_XY)
DD_2A1:
	CALL	MK_RCS		; "IX, "
	CALL	GET_NNNN
	CALL	MK_KN4K		; "(nnnnH)"
	JP	INSPOSTSTR	; cr,lf

CHK_DD_CB
	CP	0CBH
	JP	NZ,OP_ERR

	CALL	GET_NNNN	; d: 3rd OP, e:nn
	LD	A,D

	LD	BC,DD_RT_TBLE - DD_RT_TBLES
	LD	HL,DD_RT_TBLES
	CPIR
	JP	NZ,DD_CB_NN_XX

	LD	HL,DD_RT_STR
	CALL	GET_STRBUFPOINT
	CALL	MKOPCSTR	; RLC, RRC, RL, RR, SLA, SRA, SRL
	JP	MK_KIXPNK	; "(IX+nnH)",cr,lf

; check BIT, RES, SET
DD_CB_NN_XX:

	; check undefine MC
	LD	BC,DD_BIT_OPTBLE - DD_BIT_OPTBL
	LD	HL,DD_BIT_OPTBL
	CPIR
	JP	NZ,OP_ERR

	CP	80H
	JP	C,DD_BIT
	CP	0C0H
	JP	C,DD_RES

; DD_SET
; "SET i, (IX+nn)"

	LD	DE,SETSTR
	CALL	MKOPCSTR

DD_BSR:

	LD	HL,DASM_OPRSTR ; set operand string buffer
	CALL	SET_BITNO	; "i, "
	JP	DD_MIX4		; "(IX+nn)",CR,lf

; DD_BIT
; "BIT i, (IX+nn)"
DD_BIT:
	LD	DE,BITSTR
	CALL	MKOPCSTR
	JR	DD_BSR

; DD_RES
; "RES i, (IX+nn)"
DD_RES:
	LD	DE,RESSTR
	CALL	MKOPCSTR
	JR	DD_BSR

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; OPECODE 0EDH check
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CHK_0EDH:
	CP	0EDH
	JP	NZ,CHK_0FDH

	LD	A,(HL)		; get 2nd opcode

	; check undefine MC
	LD	BC,ED_OP_TBLE - ED_OP_TBL
	LD	HL,ED_OP_TBL
	CPIR
	JP	NZ,OP_ERR

	LD	A,C
	CP	6
	JP	C,ED_4MC
	CP	27
	JP	C,ED_NO_OPR
	CP	30
	JP	C,ED_IM
	JP	Z,ED_LD_AR
	CP	31
	JP	Z,ED_LD_AI
	CP	32
	JP	Z,ED_LD_RA
	CP	33
	JP	Z,ED_LD_IA
	CP	38
	JP	C,ED_ADC
	CP	42
	JP	C,ED_SBC
	CP	49
	JP	C,ED_OUT

; c: 49 - 55 : IN r,(C) (r:B,C,D,E,H,L,A)

	LD	DE,INSTR
	CALL	MKOPCSTR

	LD	A,C
	SUB	49
	LD	C,A
	LD	HL,DD_LDTBL
	CALL	GET_STRBUFPOINT	; get strung buffer
	CALL	MK_RCS		; "REG, "
	LD	DE,KCKSTR
	JP	CPSTR_CRLF	; "(C)",cr,lf

; c: 42 - 48 : OUT (C),r(r:B,C,D,E,H,L,A)
ED_OUT:
	LD	DE,OUTSTR
	CALL	MKOPCSTR

	LD	DE,KCKSTR
	CALL	MK_RCS		; "(C), "

	LD	A,C
	SUB	42
	LD	C,A
	PUSH	HL
	LD	HL,DD_LDTBL

ED_OUT1:
	CALL	GET_STRBUFPOINT	; get strung buffer
	POP	HL
	JP	CPSTR_CRLF	; "REG",cr,lf

; c: 38 - 41 : SBC HL,rr(rr:BC,DE,HL,SP)
ED_SBC:
	LD	DE,SBCSTR
	CALL	MKOPCSTR

	LD	DE,RNHL
	CALL	MK_RCS		; "HL, "

	LD	A,C
	SUB	38
ED_SBC1:
	LD	C,A
	PUSH	HL
	LD	HL,INC_DEC_ADDRP
	JR	ED_OUT1

; c: 34 - 37 : ADC HL,rr(rr:BC,DE,HL,SP)
ED_ADC:
	LD	DE,ADCSTR
	CALL	MKOPCSTR

	LD	DE,RNHL
	CALL	MK_RCS		; "HL, "

	LD	A,C
	SUB	34
	JR	ED_SBC1

; c: 30, 31 : LD A,R ; LD A,I
ED_LD_AR:
	CALL	A_COLON_SP	; "A, "
	LD	DE,RNR
	JP	CPSTR_CRLF	; "R",cr,lf

ED_LD_AI:
	CALL	A_COLON_SP	; "A, "
	LD	DE,RNI
	JP	CPSTR_CRLF	; "I",cr,lf

; c: 32, 33 : LD R,A ; LD I,A
ED_LD_RA:
	LD	DE,RNR
ED_LD_RA1:
	CALL	MK_RCS		; "R, "
	JP	A_CRLF		; "A",cr,lf

ED_LD_IA:
	LD	DE,RNI
	JR	ED_LD_RA1

; c: 27, 28, 29 : IM 2, IM 1, IM 0
ED_IM:
	LD	DE,IMSTR
	CALL	MKOPCSTR

	LD	A,29
	SUB	C
	ADD	A,'0'		; '0', '1', '2'
	LD	HL,DASM_OPRSTR
	LD	(HL),A
	INC	HL
	JP	INSPOSTSTR

; c: 6 - 26 ed_noopr
ED_NO_OPR:
	SUB	6
	LD	C,A
	LD	B,0
	LD	HL,ED_NOOPR
	CALL	GET_STRBUFPOINT		; de : string buffer
	JP	MKOPCSTR
;
; check ED 4 byte MC
;
ED_4MC:
	LD	A,4
	LD	(MC_SIZE),A	; 4byte machine code

	PUSH	BC
	LD	A,C
	LD	HL,ED_RP_STR
	CALL	GET_STRBUFPOINT		; de : string buffer
	POP	BC
	LD	A,C
	CP	3
	JP	C,DD_2A1		; "rr, (nnnnH)",cr,lf
					; rr: BC, DE, SP
; "(nnnnH), rr" rr: BC, DE, SP

	PUSH	DE
	CALL	GET_NNNN
	CALL	INS_KN4KCS	; "(nnnnH), "
	POP	DE
	JP	CPSTR_CRLF	; "REG",cr,lf

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; OPECODE 0FDH check
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CHK_0FDH:

	LD	DE,RNIY
	LD	(REG_XY),DE	; save index reg string
	LD	DE,ADDIYRP_TBL
	LD	(XY_SRTP),DE

	JP	CHK_0DDH_1

;-----------------------------------
; make "(REG+nn), " at dasm_OprStr
;	REG : IX or IY
; input de: RNIX, or RNIY
;-----------------------------------
KIXYPNK_CS:
	LD	HL,DASM_OPRSTR
	CALL	MK_KIXYPNK
	JP	INS_KMR

;-----------------------------------
; make "(REG+nn)",cr,lf
;	REG : IX or IY
; input de: RNIX, or RNIY
;       hl: string buffer
;-----------------------------------
KIXYPNK_CRLF:
	CALL	MK_KIXYPNK
	JP	INSPOSTSTR	; CR,LF,0

;-----------------------------------
; make "(IX+nn)", "(IY+nn)" string
; input de: RNIX, or RNIY
;       hl: string buffer
;-----------------------------------
MK_KIXYPNK:
	CALL	INS_KAKKOL	; "("
	CALL	ST_COPY		; IX
	CALL	GET_NNNN
	LD	A,E		;get number
	CP	80H
	JR	C,NUMPLUS	; number plus
	NEG			; a = not a + 1
	LD	E,A
	CALL	INS_MISR	; -
SV_NNKR:
	CALL	HEX2STR_ASM	; nnh
	JP	INS_KAKKOR	; ")"

NUMPLUS:
	CALL	INS_PLSR	; +
	JR	SV_NNKR

; de : nnnn , or d:yy, e:xx
GET_NNNN:
	PUSH	HL
	LD	HL,(DASM_ADR)
	INC	HL		; 2nd opecode address
	INC	HL		; operand address
	LD	E,(HL)		; get nn
	INC	HL
	LD	D,(HL)		; d:yy use for "LD (IX+xx), yy"
				; d:OP use for 4bytes MC like "DD CB xx OP"
				; de : YYXX : ED OP XX YY
	POP	HL
	RET

;-------------------------------------------------
; hl: massage table
; bc: found operand point
;
; output: copy strings to dasm_OprStr buffer
;         hl = end point of copied string buffer
;-------------------------------------------------
MK_STR:
	CALL	GET_STRBUFPOINT
	LD	HL,DASM_OPRSTR
	JP	ST_COPY

;------------------------------------------------
; get register strings address
; input bc: string buffer offset
; output de: register strings address
;------------------------------------------------
GET_RSTG_OFF:
	LD	HL,DISREGTBL

;-------------------------------------------------
; input
;	bc: found operand point
;	hl: massage table
; output:
;	DE : string point
;-------------------------------------------------
GET_STRBUFPOINT:
	SLA	C
	RL	B		; bc *=2
	ADD	HL, BC		; get operand string point
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	RET

;------------------------------------------------
; insert "nnH",CR,LF to buffer
;input e: value
;      hl: string buffer
;------------------------------------------------
MK_N2CRLF:
	CALL	HEX2STR_ASM
	JP	INSPOSTSTR

;------------------------------------------------
; insert "nnnnH",CR,LF to buffer
;input de: value
;      hl: string buffer
;------------------------------------------------
MK_N4CRLF:
	CALL	HEX4STR_ASM
	JP	INSPOSTSTR

;------------------------------------------------
; insert "(nnnnH), " to buffer dasm_OprStr
;input de: value
;
; make "(nnnnH), " to dasm_OprStr buffer
; output hl: end point of string buffer
;------------------------------------------------
INS_KN4KCS:
	LD	HL,DASM_OPRSTR	;operand str buffer
	CALL	INS_KAKKOL	; "("
	CALL	HEX4STR_ASM
	CALL	INS_KAKKOR	; ")"
	JP	INS_KMR		; ", "

;------------------------------------------------
; insert "(nnnnH)" to buffer
;input de: value
;      hl: string buffer
;------------------------------------------------
MK_KN4K:
	CALL	INS_KAKKOL	; "("
	CALL	HEX4STR_ASM
	JP	INS_KAKKOR	; ")"

;------------------------------------------------
; Make address string from branch relative value
; input hl: string buffer pointer
; output *hl= "nnnnH",cr,lf,0
;------------------------------------------------
MKREL_STR:
	PUSH	HL
	LD	HL,(DASM_ADR)
	CALL	REL_ADR_C	; return hl : target addr
	EX	DE, HL		; hl: buffer, de: addr value
	POP	HL
	CALL	HEX4STR_ASM
	JR	INSPOSTSTR

;------------------------------------------------
;input de: value, hl: save baffer.
;output 4hex chars(asm format)
;	(ex). "0FFFFH"
;             "1234H"
;       hl : hl = hl + 5(or 6); *hl=0
;------------------------------------------------
HEX4STR_ASM:
	LD	A,D
	CP	0A0H
	JR	C,H4SA_1
	LD	A,'0'
	LD	(HL),A
	INC	HL

H4SA_1:
	CALL	HEX4STR
H4SA_2:
	LD	A,'H'
	JP	IKK_1

;------------------------------------------------
;input e: value, hl: save baffer.
;output: 2 hex chars(asm format)
;	(ex). "0FFH"
;             "12H"
;       hl : hl = hl + 3(or 4); *hl=0
;------------------------------------------------
HEX2STR_ASM:
	LD	A,E
	CP	0A0H
	JR	C,H2SA_1
	LD	A,'0'
	LD	(HL),A
	INC	HL

H2SA_1:
	CALL	HEX2STR
	JR	H4SA_2

;------------------------------------------------
;input de: value, hl: save baffer.
;output: 4 hex chras in save buffer
;	 hl = hl + 4
;------------------------------------------------
HEX4STR:
	LD	A,D
	CALL	H2S_N1
	LD	A,D
	CALL	H2S_N2

;------------------------------------------------
;INPUT E: VALUE, HL: SAVE baffer.
;OUTPUT: 2 HEX CHRAS IN Save buffer
;	 HL = HL + 2
;------------------------------------------------
HEX2STR:
	LD	A,E
	CALL	h2s_n1
	LD	A,E
	CALL	h2s_n2
	RET

H2S_N1:
	RRCA
	RRCA
	RRCA
	RRCA
H2S_N2:
	AND	0FH
	OR	30H
	CP	3AH
	JR	C,IKK_1		; '0' to '9'
	ADD	A, 07H		; 'A' to 'F'
	JR	IKK_1

ADD_OPSTR:
	LD	DE,ADDSTR
;-----------------------------------
; make opecode strings
; input de : opecode strings
;-----------------------------------
MKOPCSTR:
	PUSH	AF
	PUSH	BC
	LD	B,8		;tab size
	LD	HL,DASM_OPCSTr
DMKSTR0:
	LD	A,(DE)
	OR	A
	JR	Z,DMKST1
	LD	(HL),A
	INC	HL
	INC	DE
	DEC	B
	JR	DMKSTR0
DMKST1:	LD	A,' '
DMKST2:	LD	(HL),A
	INC	HL
	DEC	B
	JR	NZ,DMKST2
	POP	BC
	POP	AF
	RET

;
; "REG",CR,LF
;
A_CRLF:
	LD	DE,RNA
	JR	CPSTR_CRLF	; "A",CR,LF
;bc_crlf:
;	LD	DE,RNBC
;	JR	cpstr_crlf
;de_crlf:
;	LD	DE,RNDE
;	JR	cpstr_crlf
IX_CRLF:
	LD	DE,(REG_XY)
	JR	CPSTR_CRLF
;sp_crlf:
;	LD	DE,RNSP
;	JR	cpstr_crlf
HL_CRLF:
	LD	DE,RNHL
CPSTR_CRLF:
	CALL	ST_COPY		; "REG"

;-----------------------------------
; insert CR,LF,0(end delimiter)
; input hl : insert buffer
;-----------------------------------
INSPOSTSTR:
	LD	A,CR
	LD	(HL),A		; CR code
	INC	HL
	LD	A,LF
	LD	(HL),A		; LF code
	INC	HL
	XOR	A
	LD	(HL),A		; end delimiter
	RET

MK_KRK_1:
	LD	HL,DASM_OPRSTR
;----------------------------
; insert "(Reg)" to buffer
;
; input de: register string
;	hl: save buffer
;----------------------------
MK_KRK:
	CALL	INS_KAKKOL	; "("
	CALL	ST_COPY		; "REG"
	JR	INS_KAKKOR	; ")"

;----------------------------
; insert "(Reg), " to dasm_OprStr
;
; input de: register string
;----------------------------
MK_KRKCS:
	LD	HL,DASM_OPRSTR
	CALL	INS_KAKKOL	; "("
	CALL	ST_COPY		; "REG"
	CALL	INS_KAKKOR	; ")"
	JR	INS_KMR		; ", "

;----------------------------
; insert "A, " to dasm_OprStr
;----------------------------
A_COLON_SP:
	LD	DE,RNA
;----------------------------
; insert "Reg, " to dasm_OprStr
;
; input de: register string
;----------------------------
MK_RCS:
	LD	HL,DASM_OPRSTR
	CALL	ST_COPY		; "REG"
	JR	INS_KMR		; ", "

;-------------------------------------------------
; copy string to output buffer
; input:
;	de : string point
;	hl : output buffer
;-------------------------------------------------
ST_COPY:
	LD	A,(DE)
	LD	(HL),A
	OR	A
	RET	Z	; return after coping delimiter
			; hl : delimiter position
	INC	HL
	INC	DE
	JR	ST_COPY

;--------------
; insert "("
;--------------
INS_KAKKOL:
	LD	A,'('
	JR	IKK_1

;--------------
; insert ")"
;--------------
INS_KAKKOR:
	LD	A,')'
IKK_1:
	LD	(HL),A
	INC	HL
	RET

;--------------
; insert " "
;--------------
INS_SPCR:
	LD	A,' '
	JR	IKK_1

;--------------
; insert "+"
;--------------
INS_PLSR:
	LD	A,'+'
	JR	IKK_1

;--------------
; insert "-"
;--------------
INS_MISR:
	LD	A,'-'
	JR	IKK_1

;--------------
; insert ", "
;--------------
INS_KMR:
	LD	A,','
	CALL	IKK_1
	JR	INS_SPCR

;------------------------------------------
; Dis assemble tables
;------------------------------------------
oth_1op_s:
	DB	00H	; NOP
	DB	07H	; RLCA
	DB	0FH	; RRCA
	DB	17H	; RLA
	DB	1FH	; RRA
	DB	27H	; DAA
	DB	2FH	; CPL
	DB	37H	; SCF
	DB	3FH	; CCF
	DB	0C9H	; RET
	DB	0D9H	; EXX
	DB	0F3H	; DI
	DB	0FBH	; EI
oth_1op_e:

INC_OPCDTBLS:
	DB	04H	; INC	B
	DB	0CH	; INC	C
	DB	14H	; INC	D
	DB	1CH	; INC	E
	DB	24H	; INC	H
	DB	2CH	; INC	L
	DB	34H	; INC	(HL)
	DB	3CH	; INC	A
INC_OPCDTBLE:

DEC_OPCDTBLS:
	DB	05H	; DEC	B
	DB	0DH	; DEC	C
	DB	15H	; DEC	D
	DB	1DH	; DEC	E
	DB	25H	; DEC	H
	DB	2DH	; DEC	L
	DB	35H	; DEC	(HL)
	DB	3DH	; DEC	A
DEC_OPCDTBLE:

INC_RP_S:
	DB	03H	; INC	BC
	DB	13H	; INC	DE
	DB	23H	; INC	HL
	DB	33H	; INC	SP
INC_RP_E:

DEC_RP_S:
	DB	0BH	; DEC	BC
	DB	1BH	; DEC	DE
	DB	2BH	; DEC	HL
	DB	3BH	; DEC	SP
DEC_RP_E:

ADD_RP_S:
	DB	09H	; ADD	HL,BC
	DB	19H	; ADD	HL,DE
	DB	29H	; ADD	HL,HL
	DB	39H	; ADD	HL,SP
ADD_RP_E:

POP_RP_S:
	DB	0C1H	; POP	BC
	DB	0D1H	; POP	DE
	DB	0E1H	; POP	HL
	DB	0F1H	; POP	AF
POP_RP_E:

PUSH_RP_S:
	DB	0C5H	; PUSH	BC
	DB	0D5H	; PUSH	DE
	DB	0E5H	; PUSH	HL
	DB	0F5H	; PUSH	AF
PUSH_RP_E:

DD_LD_TBL:
	DB	46H	; nn: LD B,(IX+nn)
	DB	4EH	; nn: LD C,(IX+nn)
	DB	56H	; nn: LD D,(IX+nn)
	DB	5EH	; nn: LD E,(IX+nn)
	DB	66H	; nn: LD H,(IX+nn)
	DB	6EH	; nn: LD L,(IX+nn)
	DB	7EH	; nn: LD A,(IX+nn)

	DB	70H	; nn: LD (IX+nn),B
	DB	71H	; nn: LD (IX+nn),C
	DB	72H	; nn: LD (IX+nn),D
	DB	73H	; nn: LD (IX+nn),E
	DB	74H	; nn: LD (IX+nn),H
	DB	75H	; nn: LD (IX+nn),L
	DB	77H	; nn: LD (IX+nn),A
DD_LD_TBLE:

DD_LOG_TBL:
	DB	96H	; nn: SUB (IX+nn)
	DB	0A6H	; nn: AND (IX+nn)
	DB	0AEH	; nn: XOR (IX+nn)
	DB	0B6H	; nn: OR  (IX+nn)
	DB	0BEH	; nn: CP  (IX+nn)
	DB	34H	; nn: INC (IX+nn)
	DB	35H	; nn: DEC (IX+nn)
DD_LOG_TBLE:

DD_RT_TBLES:
	DB	06H	; RLC (IX+nn)
	DB	0EH	; RRC (IX+nn)
	DB	16H	; RL  (IX+nn)
	DB	1EH	; RR  (IX+nn)
	DB	26H	; SLA (IX+nn)
	DB	2EH	; SRA (IX+nn)
	DB	3EH	; SRL (IX+nn)
DD_RT_TBLE:

; USE CHECKING UNDEFINE MC ( DD CB nn XX )
DD_BIT_OPTBL:
	DB	46H	; BIT 0,(IX+12H)
	DB	4EH	; BIT 1,(IX+12H)
	DB	56H	; BIT 2,(IX+12H)
	DB	5EH	; BIT 3,(IX+12H)
	DB	66H	; BIT 4,(IX+12H)
	DB	6EH	; BIT 5,(IX+12H)
	DB	76H	; BIT 6,(IX+12H)
	DB	7EH	; BIT 7,(IX+12H)
	DB	86H	; RES 0,(IX+12H)
	DB	8EH	; RES 1,(IX+12H)
	DB	96H	; RES 2,(IX+12H)
	DB	9EH	; RES 3,(IX+12H)
	DB	0A6H	; RES 4,(IX+12H)
	DB	0AEH	; RES 5,(IX+12H)
	DB	0B6H	; RES 6,(IX+12H)
	DB	0BEH	; RES 7,(IX+12H)
	DB	0C6H	; SET 0,(IX+12H)
	DB	0CEH	; SET 1,(IX+12H)
	DB	0D6H	; SET 2,(IX+12H)
	DB	0DEH	; SET 3,(IX+12H)
	DB	0E6H	; SET 4,(IX+12H)
	DB	0EEH	; SET 5,(IX+12H)
	DB	0F6H	; SET 6,(IX+12H)
	DB	0FEH	; SET 7,(IX+12H)
DD_BIT_OPTBLE:

; USE CHECKING UNDEFINE MC ( ED XX ....)
ED_OP_TBL:
	DB	40H	; 55:IN  B,(C)
	DB	48H	; 54:IN  C,(C)
	DB	50H	; 53:IN  D,(C)
	DB	58H	; 52:IN  E,(C)
	DB	60H	; 51:IN  H,(C)
	DB	68H	; 50:IN  L,(C)
	DB	78H	; 49:IN  A,(C)

	DB	41H	; 48:OUT (C),B
	DB	49H	; 47:OUT (C),C
	DB	51H	; 46:OUT (C),D
	DB	59H	; 45:OUT (C),E
	DB	61H	; 44:OUT (C),H
	DB	69H	; 43:OUT (C),L
	DB	79H	; 42:OUT (C),A

	DB	42H	; 41:SBC HL,BC
	DB	52H	; 40:SBC HL,DE
	DB	62H	; 39:SBC HL,HL
	DB	72H	; 38:SBC HL,SP

	DB	4AH	; 37:ADC HL,BC
	DB	5AH	; 36:ADC HL,DE
	DB	6AH	; 35:ADC HL,HL
	DB	7AH	; 34:ADC HL,SP

	DB	47H	; 33:LD I,A
	DB	4FH	; 32:LD R,A
	DB	57H	; 31:LD A,I
	DB	5FH	; 30:LD A,R

	DB	46H	; 29:IM  0
	DB	56H	; 28:IM  1
	DB	5EH	; 27:IM  2

	DB	44H	; 26:NEG
	DB	45H	; 25:RETN
	DB	4DH	; 24:RETI
	DB	67H	; 23:RRD
	DB	6FH	; 22:RLD
	DB	0A0H	; 21:LDI
	DB	0A1H	; 20:CPI
	DB	0A2H	; 19:INI
	DB	0A3H	; 18:OUTI
	DB	0A8H	; 17:LDD
	DB	0A9H	; 16:CPD
	DB	0AAH	; 15:IND
	DB	0ABH	; 14:OUTD
	DB	0B0H	; 13:LDIR
	DB	0B1H	; 12:CPIR
	DB	0B2H	; 11:INIR
	DB	0B3H	; 10:OTIR
	DB	0B8H	; 09:LDDR
	DB	0B9H	; 08:CPDR
	DB	0BAH	; 07:INDR
	DB	0BBH	; 06:OTDR

	DB	43H	; 05:LD (nnnn),BC
	DB	53H	; 04:LD (nnnn),DE
	DB	73H	; 03:LD (nnnn),SP

	DB	4BH	; 02:LD BC,(nnnn)
	DB	5BH	; 01:LD DE,(nnnn)
	DB	7BH	; 00:LD SP,(nnnn)
ED_OP_TBLE:

RNAF:		DB	"AF",0
RNAFX:		DB	"AF'",0

RETSTR:		DB	"RET",0
JPSTR:		DB	"JP",0
JRSTR:		DB	"JR",0
DJNZSTR:	DB	"DJNZ",0
CALLSTR:	DB	"CALL",0
RSTSTR:		DB	"RST",0
LDSTR:		DB	"LD",0
HLTSTR:		DB	"HALT",0
ADDSTR:		DB	"ADD",0
ADCSTR:		DB	"ADC",0
SUBSTR:		DB	"SUB",0
SBCSTR:		DB	"SBC",0
ANDSTR:		DB	"AND",0
XORSTR:		DB	"XOR",0
ORSTR:		DB	"OR",0
CPSTR:		DB	"CP",0
INCSTR:		DB	"INC",0
DECSTR:		DB	"DEC",0
POPSTR:		DB	"POP",0
PUSHSTR:	DB	"PUSH",0
EXSTR:		DB	"EX",0

EXXSTR:		DB	"EXX",0
NOPSTR:		DB	"NOP",0
RLCASTR:	DB	"RLCA",0
RRCASTR:	DB	"RRCA",0
RLASTR:		DB	"RLA",0
RRASTR:		DB	"RRA",0
DAASTR:		DB	"DAA",0
CPLSTR:		DB	"CPL",0
SCFSTR:		DB	"SCF",0
CCFSTR:		DB	"CCF",0
DISTR:		DB	"DI",0
EISTR:		DB	"EI",0

OUTSTR:		DB	"OUT",0
INSTR:		DB	"IN",0

RLCSTR		DB	"RLC",0
RRCSTR		DB	"RRC",0
RLSTR		DB	"RL",0
RRSTR		DB	"RR",0
SLASTR		DB	"SLA",0
SRASTR		DB	"SRA",0
SRLSTR		DB	"SRL",0
BITSTR		DB	"BIT",0
RESSTR		DB	"RES",0
NEGSTR		DB	"NEG",0
IMSTR		DB	"IM",0
RRDSTR		DB	"RRD",0
RLDSTR		DB	"RLD",0
LDISTR		DB	"LDI",0
CPISTR		DB	"CPI",0
INISTR		DB	"INI",0
OUTISTR		DB	"OUTI",0
LDDSTR		DB	"LDD",0
CPDSTR		DB	"CPD",0
INDSTR		DB	"IND",0
OUTDSTR		DB	"OUTD",0
LDIRSTR		DB	"LDIR",0
CPIRSTR		DB	"CPIR",0
INIRSTR		DB	"INIR",0
OTIRSTR		DB	"OTIR",0
LDDRSTR		DB	"LDDR",0
CPDRSTR		DB	"CPDR",0
INDRSTR		DB	"INDR",0
OTDRSTR		DB	"OTDR",0
RETISTR		DB	"RETI",0
RETNSTR		DB	"RETN",0
;BITSTR		DB	"BIT",0
;RESSTR		DB	"RES",0
SETSTR		DB	"SET",0
COM_ERRM:
ER_OPMSG:	DB	"???",0
KCKSTR		DB	"(C)",0

KRNHL:		DB	"(HL)",0
CC0:		DB	"NZ",0
CC1:		DB	"Z",0
CC2:		DB	"NC",0
CC3:		DB	"C",0
CC4:		DB	"PO",0
CC5:		DB	"PE",0
CC6:		DB	"P",0
CC7:		DB	"M",0

DISREGTBL:	DW	RNB, RNC, RND, RNE, RNH, RNL, KRNHL, RNA
CC_OPR:		DW	CC7, CC6, CC5, CC4, CC3, CC2, CC1, CC0
JRCC_OPR1:	DW	CC3, CC2, CC1, CC0
INC_DEC_ADDRP:	DW	RNSP, RNHL, RNDE, RNBC
POP_PUSHRP:	DW	RNAF, RNHL, RNDE, RNBC

OTH_1OP_TBL:	DW	EISTR, DISTR, EXXSTR, RETSTR, CCFSTR
		DW	SCFSTR, CPLSTR, DAASTR, RRASTR
		DW	RLASTR, RRCASTR, RLCASTR, NOPSTR

LOGOP2STR:	DW	OUTSTR, INSTR, CPSTR, ORSTR, XORSTR
		DW	ANDSTR, SUBSTR, SBCSTR, ADCSTR, ADDSTR

LOGOP3STR	DW	RNSP, RNHL, RNDE, RNBC
ED_RP_STR	DW	RNSP, RNDE, RNBC, RNSP, RNDE, RNBC

DD_2OPT:	DW	POPSTR, PUSHSTR, DECSTR, INCSTR
ADDIXRP_TBL:	DW	RNSP, RNIX, RNDE, RNBC
ADDIYRP_TBL:	DW	RNSP, RNIY, RNDE, RNBC

DDLOGTBL:	DW	DECSTR, INCSTR, CPSTR, ORSTR, XORSTR, ANDSTR, SUBSTR
DD_LDTBL:	DW	RNA, RNL, RNH, RNE, RND, RNC, RNB

ED_NOOPR:
		DW	OTDRSTR, INDRSTR, CPDRSTR, LDDRSTR
		DW	OTIRSTR, INIRSTR, CPIRSTR, LDIRSTR
		DW	OUTDSTR, INDSTR, CPDSTR, LDDSTR
		DW	OUTISTR, INISTR, CPISTR, LDISTR
		DW	RLDSTR, RRDSTR, RETISTR, RETNSTR
		DW	NEGSTR

DD_RT_STR:	DW	SRLSTR, SRASTR, SLASTR, RRSTR
		DW	RLSTR, RRCSTR, RLCSTR

;;;
;;; GO address
;;;

GO:
	LD	DE,(REGPC)
	LD	(GOTMP),DE	; save go tmp go address
	INC	HL
	CALL	SKIPSP
	CALL	RDHEX
	JR	NC,GOSTMP
	OR	A
	JR	Z,G_STPADR
GOSTMP:
	LD	A,D
	LD	(GOTMP),DE	; save going address
G_STPADR:
	CALL	SKIPSP
	LD	A,(HL)
	OR	A
	JR	Z,GO1
	CP	','
	JP	NZ,ERR

; set break point with go command

	INC	HL
	CALL	SKIPSP
	CALL	RDHEX		; 1st arg.
	JP	C,ERR
	LD	HL,TMPB_F	; hl: temp break point buffer
	CALL	SETBPADR
	JP	C,ERR		; address is incorrect

GO1:
	LD	HL,(GOTMP)
	LD	(REGPC),HL	; set go address

G0:
; check Trace mode

	LD	A,(TPT1_F)
	OR	A
	JR	Z,DONOT_TRACE
	LD	HL,(TMPT)
	LD	(REGPC),HL	; set trace address
	LD	HL,TPT1_F
	LD	DE,TPT1_ADR
	CALL	SET_BP
	LD	HL,TPT2_F
	LD	DE,TPT2_ADR
	CALL	SET_BP
	JR	SKP_TBP		; skip set tmp bp and bp, if tracing

; set break point
DONOT_TRACE:
	LD	HL,BPT1_F
	LD	DE,BPT1_ADR
	CALL	SET_BP
	LD	HL,BPT2_F
	LD	DE,BPT2_ADR
	CALL	SET_BP

; check go, break pointer

	LD	HL,TMPB_F
	LD	DE,TMPB_ADR
	CALL	SET_BP

	;; R register adjustment

SKP_TBP:
	LD	HL,(REGSP)
	LD	SP,HL
	LD	HL,(REGPC)
	PUSH	HL
	LD	IX,(REGIX)
	LD	IY,(REGIY)
	LD	HL,(REGAFX)
	PUSH	HL
	LD	BC,(REGBCX)
	LD	DE,(REGDEX)
	LD	HL,(REGHLX)
	EXX
	POP	AF
	EX	AF,AF'		;'
	LD	HL,(REGAF)
	PUSH	HL
	LD	BC,(REGBC)
	LD	DE,(REGDE)
	LD	HL,(REGHL)
	LD	A,(REGI)
	LD	I,A
	LD	A,(REGR)
	LD	R,A
	POP	AF
	RET			; POP PC

SET_BP:
	LD	A,(HL)
	OR	A
	RET	Z
	LD	A,(DE)
	LD	C,A
	INC	DE
	LD	A,(DE)
	LD	B,A	; bc = break point address
	LD	A,0FFH
	LD	(BC),A	; insert RST 38H code
	RET

;;;
;;; SET memory
;;;

SETM:
	INC	HL
	CALL	SKIPSP
	CALL	RDHEX
	JP	C,ERR
	CALL	SKIPSP
	LD	A,(HL)
	OR	A
	JP	NZ,ERR
	LD	A,C
	OR	A
	JR	NZ,SM0
	LD	DE,(SADDR)

SM0:
	EX	DE,HL
SM1:	CALL	HEXOUT4
	PUSH	HL
	LD	HL,DSAP
	CALL	STROUT
	POP	HL
	LD	A,(HL)
	PUSH	HL
	CALL	HEXOUT2
	LD	A,' '
	CALL	CONOUT
	CALL	GETLIN
	CALL	SKIPSP
	LD	A,(HL)
	OR	A
	JR	NZ,SM2
	;; Empty  (Increment address)
	POP	HL
	INC	HL
	LD	(SADDR),HL
	JR	SM1
SM2:	CP	'-'
	JR	NZ,SM3
	;; '-'  (Decrement address)
	POP	HL
	DEC	HL
	LD	(SADDR),HL
	JR	SM1
SM3:	CP	'.'
	JR	NZ,SM4
	POP	HL
	LD	(SADDR),HL
	JP	WSTART
SM4:	CALL	RDHEX
	OR	A
	POP	HL
	JP	Z,ERR
	LD	(HL),E
	INC	HL
	LD	(SADDR),HL	; set value

	; resave opcode for BP command
	LD	DE,(BPT1_ADR)
	LD	A,(DE)
	LD	(BPT1_OP),A
	LD	DE,(BPT2_ADR)
	LD	A,(DE)
	LD	(BPT2_OP),A
	JR	SM1

;;;
;;; LOAD HEX file
;;;

LOADH:
	; clear brk point
	XOR	A
	LD	C,A
	LD	(BPT1_F),A
	LD	(BPT2_F),A
	LD	(L_JPFLG),A
	INC	HL
	CALL	SKIPSP
	CP	'G'
	JR	NZ,lh00
	LD	(L_JPFLG),A
	INC	HL
	JR	LH000
LH00:	CALL	RDHEX
LH000:	CALL	SKIPSP
	OR	A
	JP	NZ,ERR
	LD	A,C
	OR	A
	JR	NZ,LH0
	LD	DE,0		;Offset
LH0:	LD	A,'.'
	CALL	CONOUT
	CALL	CONIN
	CP	'S'
	JR	Z,LHS0
LH1:	CP	':'
	JR	Z,LHI0
LH2:
	;; Skip to EOL
	CP	CR
	JR	Z,LH0
	CP	LF
	JR	Z,LH0
LH3:	CALL	CONIN
	JR	LH2

LHI0:	CALL	HEXIN
	LD	C,A		; Checksum
	LD	B,A		; Length
	CALL	HEXIN
	LD	H,A		; Address H
	ADD	A,C
	LD	C,A
	CALL	HEXIN
	LD	L,A		; Address L
	ADD	A,C
	LD	C,A

	;; Add offset
	ADD	HL,DE
	CALL	SV_1STADDR
	CALL	HEXIN
	LD	(RECTYP),A
	ADD	A,C
	LD	C,A		; Checksum
	LD	A,B
	OR	A
	JR	Z,LHI3
LHI1:	CALL	HEXIN
	PUSH	AF
	ADD	A,C
	LD	C,A		; Checksum
	LD	A,(RECTYP)
	OR	A
	JR	NZ,LHI20
	POP	AF
	LD	(HL),A
	INC	HL
	JR	LHI2
LHI20:	POP	AF
LHI2:	DJNZ	LHI1
LHI3:	CALL	HEXIN
	ADD	A,C
	JR	NZ,LHIE		; Checksum error
	LD	A,(RECTYP)
	OR	A
	JP	Z,LH3

LG_JUMP:
	CALL	CRLF
	LD	A,(L_JPFLG)
	OR	A
	JR	Z,L_END
	LD	HL,(LD_JMP)
	JP	(HL)		; Jump to load address

LHIE:
	LD	HL,IHEMSG
LHSR:
	CALL	STROUT
L_END:
	JP	WSTART


SV_1STADDR:
	LD	A,(L_JPFLG)
	CP	'G'
	RET	NZ
	LD	(LD_JMP),HL
	LD	A,1
	LD	(L_JPFLG),A
	RET

LHS0:
	CALL	CONIN
	LD	(RECTYP),A
	CALL	HEXIN
	LD	B,A		; Length+3
	LD	C,A		; Checksum
	CALL	HEXIN
	LD	H,A
	ADD	A,C
	LD	C,A
	CALL	HEXIN
	LD	L,A
	ADD	A,C
	LD	C,A
	ADD	HL,DE
	CALL	SV_1STADDR
	DEC	B
	DEC	B
	DEC	B
	JR	Z,LHS3
LHS1:
	CALL	HEXIN
	PUSH	AF
	ADD	A,C
	LD	C,A		; Checksum
	LD	A,(RECTYP)
	CP	'1'
	JR	NZ,LHS2
	POP	AF
	LD	(HL),A
	INC	HL
	JR	LHS20
LHS2:	POP	AF
LHS20:	DJNZ	LHS1
LHS3:	CALL	HEXIN
	ADD	A,C
	CP	0FFH
	JR	NZ,LHSE
	LD	A,(RECTYP)
	CP	'7'
	JR	Z,L_END
	CP	'8'
	JR	Z,L_END
	CP	'9'
	JR	Z,LG_JUMP
	JP	LH3
LHSE:
	LD	HL,SHEMSG
	JR	LHSR
;;;
;;; Punch HEX file
;;;

SAVEH:
	INC	HL
	LD	A,(HL)
	CP	'I'
	JR	Z,SH0
	CP	'S'
	JR	NZ,SH1
SH0:	INC	HL
	LD	(HEXMOD),A
SH1:	CALL	SKIPSP
	CALL	RDHEX
	OR	A
	JR	Z,SHE
	PUSH	DE
	POP	IX		; IX = Start address
	CALL	SKIPSP
	LD	A,(HL)
	CP	','
	JR	NZ,SHE
	INC	HL
	CALL	SKIPSP
	CALL	RDHEX		; DE = End address
	OR	A
	JR	Z,SHE
	CALL	SKIPSP
	LD	A,(HL)
	OR	A
	JR	Z,SH2
SHE:	JP	ERR

SH2:	PUSH	IX
	POP	HL
	EX	DE,HL
	INC	HL
	OR	A
	SBC	HL,DE		; HL = Length
SH3:	CALL	SHL00
	LD	A,H
	OR	L
	JR	NZ,SH3
	LD	A,(HEXMOD)
	CP	'I'
	JR	NZ,SH4
	;; End record for Intel HEX
	LD	HL,IHEXER
	CALL	STROUT
	JP	WSTART
SH4:
	;; End record for Motorola S record
	LD	HL,SRECER
	CALL	STROUT
	JP	WSTART

SHL00:
	LD	C,16
	LD	A,H
	OR	A
	JR	NZ,SHL0
	LD	A,L
	CP	C
	JR	NC,SHL0
	LD	C,A
SHL0:
	LD	B,0
	OR	A
	SBC	HL,BC
	LD	B,C
	LD	A,(HEXMOD)
	CP	'I'
	JR	NZ,SHLS
	;; Intel HEX
	LD	A,':'
	CALL	CONOUT
	LD	A,B
	CALL	HEXOUT2		; Length
	LD	C,B		; Checksum
	LD	A,D
	CALL	HEXOUT2
	LD	A,D
	ADD	A,C
	LD	C,A
	LD	A,E
	CALL	HEXOUT2
	LD	A,E
	ADD	A,C
	LD	C,A
	XOR	A
	CALL	HEXOUT2
SHLI0:	LD	A,(DE)
	PUSH	AF
	CALL	HEXOUT2
	POP	AF
	ADD	A,C
	LD	C,A
	INC	DE
	DJNZ	SHLI0
	LD	A,C
	NEG
	CALL	HEXOUT2
	JP	CRLF

SHLS:
	;; Motorola S record
	LD	A,'S'
	CALL	CONOUT
	LD	A,'1'
	CALL	CONOUT
	LD	A,B
	ADD	A,2+1		; DataLength + 2(Addr) + 1(Sum)
	LD	C,A
	CALL	HEXOUT2
	LD	A,D
	CALL	HEXOUT2
	LD	A,D
	ADD	A,C
	LD	C,A
	LD	A,E
	CALL	HEXOUT2
	LD	A,E
	ADD	A,C
	LD	C,A
SHLS0:	LD	A,(DE)
	PUSH	AF
	CALL	HEXOUT2		; Data
	POP	AF
	ADD	A,C
	LD	C,A
	INC	DE
	DJNZ	SHLS0
	LD	A,C
	CPL
	CALL	HEXOUT2
	JP	CRLF

;;;
;;; Port in
;;;

PIN:
	INC	HL
	CALL	SKIPSP
	CALL	RDHEX
	LD	A,C
	OR	A
	JP	Z,ERR		; Port no. missing
	CALL	SKIPSP
	LD	A,(HL)
	OR	A
	JP	NZ,ERR
	LD	B,D
	LD	C,E
	IN	A,(C)
	CALL	HEXOUT2
	CALL	CRLF
	JP	WSTART

;;;
;;; Port out
;;;

POUT:
	INC	HL
	CALL	SKIPSP
	CALL	RDHEX
	LD	A,C
	OR	A
	JP	Z,ERR		; Port no. missing
	PUSH	DE
	POP	IX
	CALL	SKIPSP
	LD	A,(HL)
	CP	','
	JP	NZ,ERR
	INC	HL
	CALL	SKIPSP
	CALL	RDHEX
	LD	A,C
	OR	A
	JP	Z,ERR		; Data missing
	CALL	SKIPSP
	LD	A,(HL)
	OR	A
	JP	NZ,ERR
	PUSH	IX
	POP	BC
	OUT	(C),E
	JP	WSTART

;;;
;;; Register
;;;

REG:
	INC	HL
	CALL	SKIPSP
	OR	A
	JR	NZ,RG0
	CALL	RDUMP
	JP	WSTART
RG0:
	EX	DE,HL
	LD	HL,RNTAB
RG1:
	CP	(HL)
	JR	Z,RG2		; Character match
	LD	C,A
	INC	HL
	LD	A,(HL)
	OR	A
	JR	Z,RGE		; Found end mark
	LD	A,C
	LD	BC,5
	ADD	HL,BC		; Next entry
	JR	RG1
RG2:
	INC	HL
	LD	A,(HL)
	CP	0FH		; Link code
	JR	NZ,RG3
	;; Next table
	INC	HL
	LD	C,(HL)
	INC	HL
	LD	H,(HL)
	LD	L,C
	INC	DE
	LD	A,(DE)
	JR	RG1
RG3:
	OR	A
	JR	Z,RGE		; Found end mark

	LD	C,(HL)		; LD C,A???
	INC	HL
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	PUSH	DE		; Reg storage address
	INC	HL
	LD	A,(HL)
	INC	HL
	LD	H,(HL)
	LD	L,A		; HL: Reg name
	CALL	STROUT
	LD	A,'='
	CALL	CONOUT

	LD	A,C
	AND	07H
	CP	1
	JR	NZ,RG4
	;; 8 bit register
	POP	HL
	LD	A,(HL)
	PUSH	HL
	CALL	HEXOUT2
	JR	RG5
RG4:
	;; 16 bit register
	POP	HL
	PUSH	HL
	INC	HL
	LD	A,(HL)
	CALL	HEXOUT2
	DEC	HL
	LD	A,(HL)
	CALL	HEXOUT2
RG5:
	LD	A,' '
	CALL	CONOUT
	PUSH	BC		; C: reg size
	CALL	GETLIN
	CALL	SKIPSP
	CALL	RDHEX
	OR	A
	JR	Z,RGR
	POP	BC
	POP	HL
	LD	A,C
	CP	1
	JR	NZ,RG6
	;; 8 bit register
	LD	(HL),E
	JR	RG7
RG6:
	;; 16 bit register
	LD	(HL),E
	INC	HL
	LD	(HL),D
RG7:
RGR:
	JP	WSTART
RGE:
	JP	ERR

RDUMP:
	LD	HL,RDTAB
RD0:
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	INC	HL
	LD	A,D
	OR	E
	JP	Z,CRLF		; End
	PUSH	DE
	EX	DE,HL
	CALL	STROUT		; print name of register
	EX	DE,HL
	POP	DE

; flag check
	LD	A,RDSF_H
	CP	D
	JR	NZ,RD101
	LD	A,RDSF_L
	CP	E
	JR	NZ,RD101
	JR	RD20

RD101:
	LD	A,RDSFX_H
	CP	D
	JR	NZ,RD10
	LD	A,RDSFX_L
	CP	E
	JR	Z,RD20

RD10:
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	INC	HL
	LD	A,(HL)
	INC	HL
	EX	DE,HL
	CP	1
	JR	NZ,RD1
	;; 1 byte
	LD	A,(HL)
	CALL	HEXOUT2
	EX	DE,HL
	JR	RD0
RD1:
	;; 2 byte
	INC	HL
	LD	A,(HL)
	CALL	HEXOUT2		; High byte
	DEC	HL
	LD	A,(HL)
	CALL	HEXOUT2		; Low byte
	EX	DE,HL
	JR	RD0

; make flag image string
rd20:
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	INC	HL
	LD	A,(DE)		; get flag values
	PUSH	DE
	PUSH	HL

; make flag image

	LD	HL,F_BIT
	LD	BC,F_BIT_ON
	LD	DE,F_BIT_OFF
	PUSH	AF		; adjustment SP. DO NOT DELETE THIS LINE!

FLG_LOOP:
	POP	AF		; "push af" before loop back
	SLA	A
	JR	NC,FLG_OFF
	PUSH	AF
	LD	A,(BC)
	LD	(HL),A
	POP	AF
	JR	FLG_NXT
FLG_OFF:
	PUSH	AF
	LD	A,(DE)
	LD	(HL),A
	POP	AF

FLG_NXT:
	INC	BC
	INC	DE
	INC	HL
	PUSH	AF
	LD	A,(HL)
	OR	A		;check delimiter
	JR	NZ,FLG_LOOP
	POP	AF		; restore stack position
	LD	HL,F_BIT
	CALL	STROUT		; print flag register for bit imaze
	POP	HL
	POP	DE
	INC	HL
	JR	RD0

F_BIT_ON:	DB	"SZ.H.PNC"
F_BIT_OFF:	DB	"........"

;;;
;;; Other support routines
;;;

STROUT:
	LD	A,(HL)
	AND	A
	RET	Z
	CALL	CONOUT
	INC	HL
	JR	STROUT

; input:  HL
; output: 4 hex_char output console

HEXOUT4:
	LD	A,H
	CALL	HEXOUT2
	LD	A,L

; input:  A
; output: 2 hex_char output console
HEXOUT2:
	PUSH	AF
	RRA
	RRA
	RRA
	RRA
	CALL	HEXOUT1
	POP	AF

; input:  A
; output: 1 hex_char output console
HEXOUT1:
	AND	0FH
	ADD	A,'0'
	CP	'9'+1
	JP	C,CONOUT
	ADD	A,'A'-'9'-1
	JP	CONOUT

HEXIN:
	XOR	A
	CALL	HI0
	RLCA
	RLCA
	RLCA
	RLCA
HI0:
	PUSH	BC
	LD	C,A
	CALL	CONIN
	CP	'0'
	JR	C,HIR
	CP	'9'+1
	JR	C,HI1
	CP	'A'
	JR	C,HIR
	CP	'F'+1
	JR	NC,HIR
	SUB	'A'-'9'-1
HI1:
	SUB	'0'
	OR	C
HIR:
	POP	BC
	RET

CRLF:
	LD	A,CR
	CALL	CONOUT
	LD	A,LF
	JP	CONOUT

CLR_CRT:
	PUSH	HL
	LD	HL,ESC_CRT_CLR
	CALL	STROUT
	POP	HL
	RET

ESC_CRT_CLR:
	DB	01BH
	DB	"[2"
	DB	0

GETLIN:
	LD	HL,LINEBUF
	LD	B,KBUF_LEN

GL0:	; input hl

	PUSH	DE
	PUSH	HL
	LD	E,B	; E: buffer length
	DEC	E	; buffer lenght -1
	LD	B,0

GL00:
	CALL	CONIN
	CP	CR
	JR	Z,GLE
	CP	LF
	JR	Z,GLE
	CP	BS
	JR	Z,GLB
	CP	DEL
	JR	Z,GLB
	CP	'"'
	JR	NZ,GL001
	PUSH	AF
	LD	A,(KY_FLG)
	XOR	NO_UPPER	; toggle UPPER or NO UPPER
	LD	(KY_FLG),A
	POP	AF

GL001:	CP	' '
	JR	C,GL00
	CP	80H
	JR	NC,GL00
	LD	C,A
	LD	A,B
	CP	E		; buffer full check
	JR	NC,GL00		; Too long
	INC	B
	LD	A,C
	CALL	CONOUT
	CP	'a'
	JR	C,GL1
	CP	'z'+1
	JR	NC,GL1

	PUSH	HL
	LD	HL,KY_FLG
	bit	NO_UPPER>>1, (HL)
	JR	NZ,SKIP_UPPER
	AND	0DFH		; make upper code
SKIP_UPPER:
	POP	HL
GL1:
	LD	(HL),A
	INC	HL
	JR	GL00
GLB:
	LD	A,B
	AND	A
	JR	Z,GL00
	DEC	B
	DEC	HL
	LD	A,08H
	CALL	CONOUT
	LD	A,' '
	CALL	CONOUT
	LD	A,08H
	CALL	CONOUT
	JR	GL00
GLE:
	PUSH	HL
	LD	HL,KY_FLG
	BIT	NO_CR>>1, (HL)
	JR	NZ,SKIP_CR
	LD	A,CR
	CALL	CONOUT
SKIP_CR:
	BIT	NO_LF>>1, (HL)
	JR	NZ,SKIP_LF
	LD	A,LF
	CALL	CONOUT
SKIP_LF:
	RES	NO_UPPER>>1,(HL)	; set upper flag
	POP	HL
	LD	(HL),0

	POP	HL
	POP	DE
	RET

SKIPSP:
	LD	A,(HL)
	CP	' '
	RET	NZ
	INC	HL
	JR	SKIPSP

UPPER:
	CP	'a'
	RET	C
	CP	'z'+1
	RET	NC
	ADD	A,'A'-'a'
	RET

RDHEX:
	LD	C,0
	LD	DE,0
RH0:
	LD	A,(HL)
	CP	'0'
	JR	C,RHE
	CP	'9'+1
	JR	C,RH1
	CP	'A'
	JR	C,RHE
	CP	'F'+1
	JR	NC,RHE
	SUB	'A'-'9'-1
RH1:
	SUB	'0'
	RLA
	RLA
	RLA
	RLA
	RLA
	RL	E
	RL	D
	RLA
	RL	E
	RL	D
	RLA
	RL	E
	RL	D
	RLA
	RL	E
	RL	D
	INC	HL
	INC	C
	JR	RH0
RHE:
	LD	A,C
	OR	A
	JR	Z,rhe1
	CP	5
	JR	NC,rhe1
	OR	A	; clear carry
	RET

rhe1:
	SCF		; set carry
	RET

;;;
;;; API Handler
;;:   C : API entory NO.
;;;

RST30H_IN:
	PUSH	HL
	PUSH	BC
	LD	HL,APITBL
	LD	B,0
	ADD	HL,BC
	ADD	HL,BC

	LD	BC,APITBL_E
	OR	A
	PUSH	HL
	SBC	HL, BC
	POP	HL
	JR	NC,NO_API	; request No. is not exist

	LD	B,(HL)
	INC	HL
	LD	H,(HL)
	LD	L,B

	POP	BC
	EX	(SP),HL		; Restore HL, jump address on stack top
NO_OPERATE:
	RET

NO_API:
	POP	BC
	POP	HL
	RET

APITBL:
	DW	CSTART		; 00: CSTART
	DW	API01		; 01: WSTART
	DW	CONOUT		; 02: CONOUT
	DW	STROUT		; 03: STROUT
	DW	CONIN		; 04: CONIN
	DW	CONST		; 05: CONST
	DW	API06		; 06: PSPEC
	DW	HEXOUT4		; 07: CONOUT HEX4bytes: input HL
	DW	HEXOUT2		; 08: CONOUT HEX2bytes: input A
	DW	HEXOUT1		; 09: CONOUT HEX1byte : input A
	DW	CLR_CRT		; 10: Clear screen (ESC+[2)
	DW	GL0		; 11: GET a line.  input HL : input buffer address)
				;                         B : buffer length
				;                  output B : length of strings
	DW	SKIPSP		; 12: SKIP Spase
	DW	CRLF		; 13: CONOUT CRLF
	DW	UPPER		; 14: Lower to UPPER
	DW	RDHEX		; 15: get hex number from chr buffer
				;     input  HL : hex string buffer
				;     output DE : hex number
				;            CF=1 : error, C, A = hex counts(1-4)
	DW	DEC_STR		; 16: get decimal srtings
				; input hl : return strings buffer addr.
				;       de : 16bit binary
	DW	DIV16_8		; 17; division 16bit / 8bit
	DW	MUL_8		; 18: multiply 8bit * 8bit
	DW	NO_OPERATE	; 19: no operation : if RAM12K=0
	DW	GET_DISASM_ST	; 20: dis assemble string
				;     input: HL buffer. need 42bytes
				;     output : DE : next MC address
				;              A  : disassembled MC size
	DW	GET_DNUM	; 21: get number from decimal strings
				;     input HL : string buffer
				;     Return
				;        CF =1 : Error
				;        BC: Calculation result
APITBL_E:

	;; WSTART from API
API01:
	LD	SP,STACKM	; reset SP for monitor

; check stop by bp and trace operation

	LD	A,(TMPB_F)
	OR	A
	JR	Z,WS_CHK1
	LD	HL,TMPB_OP
	CALL	RSTR_TPT
WS_CHK1:
	LD	A,(TPT1_F)
	OR	A
	JR	Z,WS_CHK2
	LD	HL,TPT1_OP
	CALL	RSTR_TPT

WS_CHK2:
	LD	A,(TPT2_F)
	OR	A
	JR	Z,WS_CHK3
	LD	HL,TPT2_OP
	CALL	RSTR_TPT
WS_CHK3:
	LD	A,(BPT2_F)
	OR	A
	JR	Z,WS_CHK4
	LD	HL,BPT2_OP
	CALL	RSTR_BPT
WS_CHK4:
	LD	A,(BPT1_F)
	OR	A
	JR	Z,WS_CHK5
	LD	HL,BPT1_OP
	CALL	RSTR_BPT
WS_CHK5:
	JP	BACKTOMON

	;; PSPEC
API06:
	XOR	A
	RET

;;;
;;; Break Point
;;; trace Point
;;; go, stop point
;;; operation handler
;;
RST38H_IN:
	PUSH	AF
	LD	A,R
	LD	(REGR),A
	LD	A,I
	LD	(REGI),A
	LD	(REGHL),HL
	LD	(REGDE),DE
	LD	(REGBC),BC
	POP	HL
	LD	(REGAF),HL
	EX	AF,AF'		;'
	PUSH	AF
	EXX
	LD	(REGHLX),HL
	LD	(REGDEX),DE
	LD	(REGBCX),BC
	POP	HL
	LD	(REGAFX),HL
	LD	(REGIX),IX
	LD	(REGIY),IY
	POP	HL
	DEC	HL
	LD	(REGPC),HL
	LD	(REGSP),SP

; check bp and trace operation

	LD	SP,STACKM	; reset SP for monitor
	XOR	A
	LD	D,A
	LD	E,A		;clear msg pointer


; check go, end operation
	LD	A,(TMPB_F)
	OR	A
	JR	Z,CHK_BP
	LD	DE,STPBRK_MSG
	LD	HL,TMPB_OP
	CALL	RSTR_TPT

; check set break point

CHK_BP:
	LD	A,(BPT2_F)
	OR	A
	JR	Z,BP_CHK1
	LD	DE,STPBRK_MSG
	LD	HL,BPT2_OP
	CALL	RSTR_BPT

BP_CHK1:
	LD	A,(BPT1_F)
	OR	A
	JR	Z,TP_CHK1
	LD	DE,STPBRK_MSG
	LD	HL,BPT1_OP
	CALL	RSTR_BPT

; check trace operation
TP_CHK1:
	LD	A,(TPT1_F)
	OR	A
	JR	Z,TP_CHK2
	LD	DE,TRACE_MSG
	LD	HL,TPT1_OP
	CALL	RSTR_TPT

TP_CHK2:
	LD	A,(TPT2_F)
	OR	A
	JR	Z,BP_CHK_END
	LD	DE,TRACE_MSG
	LD	HL,TPT2_OP
	CALL	RSTR_TPT

BP_CHK_END:
	LD	A,D
	OR	E
	JR	NZ,NO_RST38_MSG

	; set RST 38H message
	LD	DE,RST38MSG

NO_RST38_MSG:
	LD	A,(DE)		; get first char of message
	CP	'T'		; trace ?
	JR	Z,CHK_NTRACE
	EX	DE, HL
	CALL	STROUT

	;; R register adjustment

	CALL	RDUMP
	CALL	DIS_CALL	; list disassemble
	JR	BACKTOMON	; goto WBOOT

;
; check continue trace operation
;
CHK_NTRACE:
	LD	A,(TP_MODE)
	CP	'F'		; chk
	JR	Z,SKP_RMSG

;NO_TRACE:
	EX	DE, HL
	CALL	STROUT

	;; R register adjustment

	CALL	RDUMP
	CALL	DIS_CALL	; list disassemble

SKP_RMSG:
	CALL	CONST
	JR	Z,T_NO_KY	; no key in
	CALL	CONIN
	CP	03H		; chk CTL+C
	JR	NZ,T_NO_KY

	; stop_trace
BACKTOMON:
	XOR	A
	LD	(FEVER_T),A	; clear forever flag
	LD	H,A
	LD	L,A
	LD	(TC_CNT),HL
	JP	WSTART

	; check trace forever
T_NO_KY:
	LD	A,(FEVER_T)
	OR	A
	JP	NZ,REPEAT_TRACE
	LD	HL,(TC_CNT)
	DEC	HL
	LD	(TC_CNT),HL
	LD	A,L
	OR	H
	JR	NZ,REPEAT_TRACE
	JP	WSTART

REPEAT_TRACE:
	LD	HL,(REGPC)
	LD	(TMPT),HL
	JP	T_OP_CHK

	; HL=buffer point
RSTR_TPT:
	PUSH	HL
	LD	A,(HL)
	INC	HL
	LD	C,(HL)
	INC	HL
	LD	B,(HL)
	LD	(bc),A		; restor OP CODE
	POP	HL
	XOR	A
	DEC	HL
	LD	(HL),A		; clear trace flag
	RET

	; HL=buffer point
RSTR_BPT:
	LD	A,(HL)
	INC	HL
	LD	C,(HL)
	INC	HL
	LD	B,(HL)
	LD	(bc),A		; restor OP CODE
	RET

DIS_CALL:
	LD	HL,(REGPC)
	LD	(DASM_ADR),HL	; set disasm address
	CALL	DIS_ANALYSIS
	CALL	MK_ADR_STR	; conout address and machine code
	LD	HL,ADR_MC_BUF
	CALL	STROUT		; conout disassemble strings
	LD	HL,(REGPC)
	LD	(DASM_ADR),HL	; restrore disasm address
	RET

RST38MSG:
	DB	"RST 38H",CR,LF,0
STPBRK_MSG:
	DB	"Breaked",CR,LF,0
TRACE_MSG:
	DB	"Traced",CR,LF,0

;
; make decimal string
;
; input HL : output string buffer
;       DE : 16bit binary
;
; output (HL) : decimal strings

DEC_STR:
	PUSH	AF
	PUSH	BC
	PUSH	DE
	PUSH	HL
	PUSH	IX

	PUSH	HL
	POP	IX		; ix: save buffer top address

	EX	DE, HL		; hl: 16bit binary, de: buffer
	PUSH	HL		; save 16bit binary
	LD	HL,5
	ADD	HL,DE		; hl = buffer + 5
	XOR	A
	LD	(HL),A
	EX	DE, HL		; de: buffer + 5, hl : buffer
	POP	HL		; hl : 16bit binary
	LD	BC,1

LOOP_DEC:
	LD	A,10
	CALL	DIV16_8
	OR	30H
	DEC	DE
	LD	(DE),A
	INC	C
	LD	A,H
	OR	L
	JR	NZ,LOOP_DEC

	LD	A,C
	CP	6
	JR	Z,END_DEC

	PUSH	IX
	POP	HL		; hl : buffer top address
	EX	DE, HL
	LDIR

END_DEC:
	POP	IX
	POP	HL
	POP	DE
	POP	BC
	POP	AF
	RET


; DIV 16bit / 8 bit
; input
;	HL, A
; output
;	result = HL, mod = A

DIV16_8:
	PUSH	BC
	PUSH	DE

	LD	C,A
	LD	B,15
	XOR	A
	ADD	HL, HL
	RLA
	SUB	C
	JR	C,D16_MINUS_BEFORE
	ADD	HL, HL
	INC	L

D16_PLUS:
	RLA
	SUB	C
	JR	C,D16_MINUS_AFTER

D16_PLUS_AFTER:
	ADD	HL, HL
	INC	L
	DJNZ	D16_PLUS
	JP	D16_END

D16_MINUS_BEFORE:
	ADD	HL, HL

D16_MINUS:
	RLA
	ADD	A, C
	JR	C,D16_PLUS_AFTER

D16_MINUS_AFTER:
	ADD	HL, HL
	DJNZ	D16_MINUS
	ADD	A,C
D16_END:
	POP	DE
	POP	BC
	RET

;
; input : HL / DE
; output : quotient HL
;	   remainder DE

DIV16:
	LD	(DIV16_NA),HL
	LD	(DIV16_NB),DE

	XOR	A
	LD	(DIV16_NC),A
	LD	(DIV16_NC+1),A
	LD	(DIV16_ND),A
	LD	(DIV16_ND+1),A
	LD	B,16

DIV16_X2:
	LD	HL,DIV16_NC
	SLA	(HL)
	INC	HL
	RL	(HL)

	LD	HL,DIV16_NA
	SLA	(HL)
	INC	HL
	RL	(HL)
	LD	HL,DIV16_ND
	RL	(HL)
	INC	HL
	RL	(HL)

	LD	HL,(DIV16_NB)
	LD	E,L
	LD	D,H
	LD	HL,(DIV16_ND)
	XOR	A
	SBC	HL, DE
	JR	NC,DIV16_X0
	JR	DIV16_X1

DIV16_X0:
	LD	(DIV16_ND),HL

	LD	A,(DIV16_NC)
	OR	1
	LD	(DIV16_NC),A

DIV16_X1:
	DJNZ	DIV16_X2

	LD	HL,(DIV16_NC)
	LD	DE,(DIV16_ND)
	RET

; 8bit * 8bit : ans = 16bit
; input A , BC
; output HL

MUL_8:
	PUSH	AF
	PUSH	BC
	OR	A		; clear carry
	JR	ST_MUL8

LOOP_M8:
	SLA	C
	RL	B

ST_MUL8:
	RRA
	JR	NC,LOOP_M8
	ADD	HL, BC
	JR	NZ,LOOP_M8
	POP	BC
	POP	AF
	RET

;------------------------
	ORG	2400H
;------------------------

;;;
;;; Messages
;;;

CMD_HLP:
	DB	"H :Command Help",CR,LF
	DB	"B[1|2[,<adr>]] :Set or List Break Point",CR,LF
	DB	"BC[1|2] :Clear Break Point",CR,LF
	DB	"D[<adr>[,<eadr>]] :Dump Memory",CR,LF
	DB	"A[<adr>][,s<steps>|<adr>] :DisAssemble",CR,LF
	DB	"G[<adr>][,<stop adr>] :Go and Stop",CR,LF
	DB	"I<port> :Port Input",CR,LF
	DB	"L[G|<offset>] :Load HexFile (and GO)",CR,LF
	DB	"O<port>,<data> :Port Output",CR,LF
	DB	"P[I|S] :Punch HexFile(I:Intel,S:Motorola)",CR,LF
	DB	"R[<reg>] :Set or Dump register",CR,LF
	DB	"S[<adr>] :Set Memory",CR,LF
	DB	"T[<adr>][,<steps>|-1] : Trace command",CR,LF
	DB	"TM[I|S] :Trace Option for CALL",CR,LF
	DB	"TP[ON|OFF] :Trace Print Mode",CR,LF
	DB	"E :EMBASIC",CR,LF
	DB	"! :S-OS EMUZ80",CR,LF,0

OPNMSG:
	DB	CR,LF
	DB	"S-OS EMUZ80 Monitor",CR,LF,0

PROMPT:
	DB	"] ",00H

IHEMSG:
	DB	"Error ihex",CR,LF,0
SHEMSG:
	DB	"Error srec",CR,LF,0
ERRMSG:
	DB	"Error",CR,LF,0

DSEP0:
	DB	" :",0
DSEP1:
	DB	" | ",0

DSAP	DB	"  ", 0

IHEXER:
        DB	":00000001FF",CR,LF,0
SRECER:
        DB	"S9030000FC",CR,LF,0

	;; Register dump table
RDTAB:
	DW	RDSA,REGAF+1
	DB	1
	DW	RDSBC,REGBC
	DB	2
	DW	RDSDE,REGDE
	DB	2
	DW	RDSHL,REGHL
	DB	2
	DW	RDSF,REGAF
	DB	1

	DW	RDSIX,REGIX
	DB	2
	DW	RDSIY,REGIY
	DB	2

	DW	RDSAX,REGAFX+1
	DB	1
	DW	RDSBCX,REGBCX
	DB	2
	DW	RDSDEX,REGDEX
	DB	2
	DW	RDSHLX,REGHLX
	DB	2
	DW	RDSFX,REGAFX
	DB	1

	DW	RDSSP,REGSP
	DB	2
	DW	RDSPC,REGPC
	DB	2
	DW	RDSI,REGI
	DB	1
	DW	RDSR,REGR
	DB	1

	DW	0,0
	DB	0

RDSA:	DB	"A =",0
RDSBC:	DB	" BC =",0
RDSDE:	DB	" DE =",0
RDSHL:	DB	" HL =",0

RDSF:	DB	" F =",0

RDSF_H	equ	RDSF >> 8
RDSF_L	equ	RDSF & 0FFH


RDSIX:	DB	" IX=",0
RDSIY:	DB	" IY=",0
RDSAX:	DB	CR,LF,"A'=",0
RDSBCX:	DB	" BC'=",0
RDSDEX:	DB	" DE'=",0
RDSHLX:	DB	" HL'=",0

RDSFX:	DB	" F'=",0

RDSFX_H	EQU	RDSFX >> 8
RDSFX_L	EQU	RDSFX & 0FFH

RDSSP:	DB	" SP=",0
RDSPC:	DB	" PC=",0
RDSI:	DB	" I=",0
RDSR:	DB	" R=",0

RNTAB:
	DB	'A',0FH		; "A?"
	DW	RNTABA,0
	DB	'B',0FH		; "B?"
	DW	RNTABB,0
	DB	'C',0FH		; "C?"
	DW	RNTABC,0
	DB	'D',0FH		; "D?"
	DW	RNTABD,0
	DB	'E',0FH		; "E?"
	DW	RNTABE,0
	DB	'F',0FH		; "F?"
	DW	RNTABF,0
	DB	'H',0FH		; "H?"
	DW	RNTABH,0
	DB	'I',0FH		; "I?"
	DW	RNTABI,0
	DB	'L',0FH		; "L?"
	DW	RNTABL,0
	DB	'P',0FH		; "P?"
	DW	RNTABP,0
	DB	'R',1		; "R"
	DW	REGR,RNR
	DB	'S',0FH		; "S?"
	DW	RNTABS,0

	DB	0,0		; End mark

RNTABA:
	DB	0,1		; "A"
	DW	REGAF+1,RNA
	DB	27H,1		; "A'"
;;	DB	'\'',1		; "A'"
	DW	REGAFX+1,RNAX

	DB	0,0

RNTABB:
	DB	0,1		; "B"
	DW	REGBC+1,RNB
	DB	27H,1		; "B'"
;;	DB	'\'',1		; "B'"
	DW	REGBCX+1,RNBX
	DB	'C',0FH		; "BC?"
	DW	RNTABBC,0

	DB	0,0		; End mark

RNTABBC:
	DB	0,2		; "BC"
	DW	REGBC,RNBC
	DB	27H,2		; "BC'"
;;	DB	'\'',2		; "BC'"
	DW	REGBCX,RNBCX

	DB	0,0

RNTABC:
	DB	0,1		; "C"
	DW	REGBC,RNC
	DB	27H,1		; "C'"
;;	DB	'\'',1		; "C'"
	DW	REGBCX,RNCX

	DB	0,0

RNTABD:
	DB	0,1		; "D"
	DW	REGDE+1,RND
	DB	27H,1		; "D'"
;;	DB	'\'',1		; "D'"
	DW	REGDEX+1,RNDX
	DB	'E',0FH		; "DE?"
	DW	RNTABDE,0

	DB	0,0

RNTABDE:
	DB	0,2		; "DE"
	DW	REGDE,RNDE
	DB	27H,2		; "DE'"
;;	DB	'\'',2		; "DE'"
	DW	REGDEX,RNDEX

	DB	0,0

RNTABE:
	DB	0,1		; "E"
	DW	REGDE,RNE
	DB	27H,1		; "E'"
;;	DB	'\'',1		; "E'"
	DW	REGDEX,RNEX

	DB	0,0

RNTABF:
	DB	0,1		; "F"
	DW	REGAF,RNF
	DB	27H,1		; "F'"
;;	DB	'\'',1		; "F'"
	DW	REGAFX,RNFX

	DB	0,0

RNTABH:
	DB	0,1		; "H"
	DW	REGHL+1,RNH
	DB	27H,1		; "H'"
;;	DB	'\'',1		; "H'"
	DW	REGHLX+1,RNHX
	DB	'L',0FH		; "HL?"
	DW	RNTABHL,0

	DB	0,0

RNTABHL:
	DB	0,2		; "HL"
	DW	REGHL,RNHL
	DB	27H,2		; "HL'"
;;	DB	'\'',2		; "HL'"
	DW	REGHLX,RNHLX

	DB	0,0

RNTABL:
	DB	0,1		; "L"
	DW	REGHL,RNL
	DB	27H,1		; "L'"
;;	DB	'\'',1		; "L'"
	DW	REGHLX,RNLX

	DB	0,0

RNTABI:
	DB	0,1		; "I"
	DW	REGI,RNI
	DB	'X',2		; "IX"
	DW	REGIX,RNIX
	DB	'Y',2		; "IY"
	DW	REGIY,RNIY

	DB	0,0

RNTABP:
	DB	'C',2		; "PC"
	DW	REGPC,RNPC

	DB	0,0

RNTABS:
	DB	'P',2		; "SP"
	DW	REGSP,RNSP

	DB	0,0

RNA:	DB	"A",00H
RNBC:	DB	"BC",00H
RNB:	DB	"B",00H
RNC:	DB	"C",00H
RNDE:	DB	"DE",00H
RND:	DB	"D",00H
RNE:	DB	"E",00H
RNHL:	DB	"HL",00H
RNH:	DB	"H",00H
RNL:	DB	"L",00H
RNF:	DB	"F",00H
RNAX:	DB	"A'",00H
RNBCX:	DB	"BC'",00H
RNBX:	DB	"B'",00H
RNCX:	DB	"C'",00H
RNDEX:	DB	"DE'",00H
RNDX:	DB	"D'",00H
RNEX:	DB	"E'",00H
RNHLX:	DB	"HL'",00H
RNHX:	DB	"H'",00H
RNLX:	DB	"L'",00H
RNFX:	DB	"F'",00H
RNIX:	DB	"IX",00H
RNIY:	DB	"IY",00H
RNSP:	DB	"SP",00H
RNPC:	DB	"PC",00H
RNI:	DB	"I",00H
RNR:	DB	"R",00H

;;;
;;; Console drivers
;;;

CONIN:
	IN	A,(UARTCR)
	BIT	0,A
	JR	Z,CONIN
	IN	A,(UARTDR)
	RET

CONST:
	IN	A,(UARTCR)
	BIT	0,A
	RET

CONOUT:
	PUSH	AF
PCST1:	IN	A,(UARTCR)
	BIT	1,A
	JR	Z,PCST1
	POP	AF
	OUT	(UARTDR),A
	RET

;;
;; Work Area
;;

DSADDR:		DS	2	; Dump start address
DEADDR:		DS	2	; Dump end address
DSTATE:		DS	1	; Dump state
GADDR:		DS	2	; Go address
SADDR:		DS	2	; Set address
HEXMOD:		DS	1	; HEX file mode
RECTYP:		DS	1	; Record type
SIZE:		DS	1	; I/O Size 00H,'W','S'

REG_B:
REGAF:		DS	2
REGBC:		DS	2
REGDE:		DS	2
REGHL:		DS	2
REGAFX:		DS	2	; Register AF'
REGBCX:		DS	2
REGDEX:		DS	2
REGHLX:		DS	2	; Register HL'
REGIX:		DS	2
REGIY:		DS	2
REGSP:		DS	2
REGPC:		DS	2
REGI:		DS	1
REGR:		DS	1
REG_E:

L_JPFLG		DS	1	; L command jp flag
LD_JMP		DS	2	; jump address after load

; DIV 16 /16 buffer
DIV16_NA:	DS	2
DIV16_NB:	DS	2
DIV16_NC:	DS	2
DIV16_ND:	DS	2

; go command temp start address
GOTMP:		DS	2

; trace mode switch
TP_MODE:	DS	1	; N: display on, F: display off
TM_MODE:	DS	1	; 'S':skip call, 'I':trace CALL IN
TC_CNT:		DS	2	; numbers of step
TMPT:		DS	2	; save temp buffer
FEVER_T:	DS	1	; flag trace forever

; break, trace point work area
DBG_WTOP	EQU	$
TPT1_F:		DS	1
TPT1_OP:	DS	1	; save trace point1 opcode
TPT1_ADR:	DS	2
TPT2_F:		DS	1
TPT2_OP:	DS	1	; save trace point2 opcode (for branch)
TPT2_ADR:	DS	2

; BREAK POINT WORK AREA
BPT1_F:		DS	1
BPT1_OP:	DS	1
BPT1_ADR:	DS	2
BPT2_F:		DS	1
BPT2_OP:	DS	1
BPT2_ADR:	DS	2

TMPB_F:		DS	1
TMPB_OP:	DS	1
TMPB_ADR:	DS	2
DBG_WEND	EQU	$

F_BIT:		DS	F_BITSIZE+1
KY_FLG:		DS	1	; b00000ULC ( U: UPPER L: LF C: CR )
				; U=0: Lower To Upper
				; L=0: End of line is added LF
				; C=0: End of line is added CR
;::::::::::::::::::::::::::
; DISASEM work area
;;;;;;;;;;;;;;;;;;;;;;;;;;;
DASM_ED:	DS	2
DASM_STPF:	DS	1
DASM_ADR:	DS	2
REG_XY:		DS	2	; RNIX or RNIY
XY_SRTP		DS	2	; strings pointer

MC_SIZE:	DS	1

;;;;;;;;;;;;;;;;;;;
; union area
;;;;;;;;;;;;;;;;;;;

; DidAsm string buffer
; total 42 bytes
DASM_BS:
ADR_MC_BUF:	DS	19
DASM_OPCSTR:	DS	8
DASM_OPRSTR:	DS	15 - NUMLEN
NUM_STRING:	DS	NUMLEN	; max 65536 or 0FFFFH + null( max 7bytes )
DASM_BE:

	END