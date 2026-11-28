	.data
	.text
	.globl	_doname
_doname:
	push	bp
	mov	bp,sp
	push	si
	push	di
	sub	sp,#LF1
	mov	di,4(bp)
	or	di,di
	bne	L56
	mov	bx,*8(bp)
	mov	(bx),*0
	mov	*2(bx),*0
	mov	ax,*0
	br	L51
L56:
	cmp	_dbgflag,*0
	beq	L57
	push	*6(bp)
	push	*2(di)
	mov	ax,#L59
	push	ax
	call	_printf
	add	sp,*6
	mov	ax,#__iob+8
	push	ax
	call	_fflush
	pop	cx
L57:
	test	*6(di),*7
	ble	L61
	mov	bx,*8(bp)
	mov	ax,*8(di)
	mov	dx,*10(di)
	mov	(bx),ax
	mov	*2(bx),dx
	mov	ax,*6(di)
	and	ax,*7
	cmp	ax,*3
	bne	L10000
	mov	ax,*1
	br	L10001
L10000:
	mov	ax,*0
L10001:
	br	L51
L61:
	mov	*-6(bp),*0
	mov	*-22(bp),*0
	mov	*-20(bp),*0
	mov	*-42(bp),*0
	mov	*-44(bp),*0
	push	*2(di)
	call	_exists
	pop	cx
	mov	*-26(bp),ax
	mov	*-24(bp),dx
	mov	*-30(bp),*0
	mov	*-28(bp),*0
	mov	*-10(bp),*0
	mov	ax,*1
	mov	cl,*0
	shl	ax,cl
	and	ax,#7
	and	*6(di),#~7
	or	*6(di),ax
	mov	-376(bp),*0
	mov	ax,*4(di)
	mov	*-46(bp),ax
L64:
	cmp	*-46(bp),*0
	beq	L63
	mov	bx,*-46(bp)
	mov	si,*2(bx)
L67:
	or	si,si
	beq	L66
	mov	ax,(si)
	mov	*-32(bp),ax
	push	si
	call	_expand
	pop	cx
L65:
	mov	si,*-32(bp)
	br	L67
L66:
L62:
	mov	bx,*-46(bp)
	mov	ax,(bx)
	mov	*-46(bp),ax
	br	L64
L63:
	mov	ax,*4(di)
	mov	*-46(bp),ax
L71:
	cmp	*-46(bp),*0
	beq	L70
	mov	*-14(bp),*0
	mov	*-12(bp),*0
	mov	bx,*-46(bp)
	mov	si,*2(bx)
L74:
	or	si,si
	beq	L73
	lea	ax,*-18(bp)
	push	ax
	mov	ax,*6(bp)
	inc	ax
	push	ax
	push	*2(si)
	call	_doname
	add	sp,*6
	add	*-6(bp),ax
	cmp	_dbgflag,*0
	beq	L75
	push	*-16(bp)
	push	*-18(bp)
	mov	bx,*2(si)
	push	*2(bx)
	mov	ax,#L76
	push	ax
	call	_printf
	add	sp,*8
L75:
	mov	ax,*-18(bp)
	mov	dx,*-16(bp)
	cmp	dx,*-12(bp)
	blt	L77
	bgt	L10002
	cmp	ax,*-14(bp)
	blos	L77
L10002:
	mov	ax,*-18(bp)
	mov	dx,*-16(bp)
	mov	*-14(bp),ax
	mov	*-12(bp),dx
L77:
	mov	ax,*-26(bp)
	mov	dx,*-24(bp)
	cmp	dx,*-16(bp)
	bgt	L78
	blt	L10003
	cmp	ax,*-18(bp)
	bhis	L78
L10003:
	mov	bx,*2(si)
	push	*2(bx)
	push	-376(bp)
	call	_appendq
	add	sp,*4
	mov	-376(bp),ax
L78:
L72:
	mov	si,(si)
	br	L74
L73:
	mov	ax,*6(di)
	mov	cx,*3
	sar	ax,cl
	and	ax,*7
	cmp	ax,*2
	bne	L79
	mov	bx,*-46(bp)
	cmp	*4(bx),*0
	beq	L80
	mov	ax,*-26(bp)
	mov	dx,*-24(bp)
	cmp	dx,*-12(bp)
	blt	L10004
	bgt	L10005
	cmp	ax,*-14(bp)
	blo	L10004
L10005:
	cmp	*-24(bp),*0
	bne	L10006
	cmp	*-26(bp),*0
	bne	L10006
L10007:
	cmp	*-12(bp),*0
	bne	L10008
	cmp	*-14(bp),*0
	beq	L10004
L10008:
L10006:
	mov	bx,*-46(bp)
	cmp	*2(bx),*0
	bne	L81
L10004:
	mov	ax,_okdel
	mov	*-8(bp),ax
	mov	_okdel,*0
	push	*2(di)
	mov	ax,#L83
	push	ax
	call	_setvar
	add	sp,*4
	push	-376(bp)
	call	_mkqlist
	pop	cx
	push	ax
	mov	ax,#L84
	push	ax
	call	_setvar
	add	sp,*4
	mov	-376(bp),*0
	cmp	_questfl,*0
	bne	L85
	mov	bx,*-46(bp)
	push	*4(bx)
	call	_docom
	pop	cx
	add	*-6(bp),ax
L85:
	mov	ax,*0
	push	ax
	mov	ax,#L87
	push	ax
	call	_setvar
	add	sp,*4
	mov	ax,*-8(bp)
	mov	_okdel,ax
	call	_prestim
	mov	*-30(bp),ax
	mov	*-28(bp),dx
	mov	*-10(bp),*1
L81:
L80:
	br	L88
L79:
	mov	bx,*-46(bp)
	cmp	*4(bx),*0
	beq	L89
	cmp	*-44(bp),*0
	beq	L90
	push	*2(di)
	mov	ax,#L92
	push	ax
	mov	ax,#__iob+16
	push	ax
	call	_fprintf
	add	sp,*6
	br	L93
L90:
	mov	bx,*-46(bp)
	mov	ax,*4(bx)
	mov	*-44(bp),ax
L93:
L89:
	mov	ax,*-14(bp)
	mov	dx,*-12(bp)
	cmp	dx,*-20(bp)
	blt	L94
	bgt	L10009
	cmp	ax,*-22(bp)
	blos	L94
L10009:
	mov	ax,*-14(bp)
	mov	dx,*-12(bp)
	mov	*-22(bp),ax
	mov	*-20(bp),dx
L94:
L88:
L69:
	mov	bx,*-46(bp)
	mov	ax,(bx)
	mov	*-46(bp),ax
	br	L71
L70:
	mov	ax,_sufflis
	mov	*-46(bp),ax
L97:
	cmp	*-46(bp),*0
	beq	L96
	mov	bx,*-46(bp)
	mov	ax,*2(bx)
	mov	*-34(bp),ax
L100:
	cmp	*-34(bp),*0
	beq	L99
	mov	bx,*-34(bp)
	mov	bx,*2(bx)
	mov	ax,*2(bx)
	mov	-372(bp),ax
	lea	ax,-250(bp)
	push	ax
	push	-372(bp)
	push	*2(di)
	call	_suffix
	add	sp,*6
	or	ax,ax
	beq	L102
	mov	ax,*0
	push	ax
	mov	ax,*0
	push	ax
	lea	ax,-350(bp)
	push	ax
	mov	ax,#L103
	push	ax
	lea	ax,-250(bp)
	push	ax
	call	_concat
	add	sp,*6
	push	ax
	call	_srchdir
	add	sp,*6
	mov	ax,_sufflis
	mov	*-48(bp),ax
L106:
	cmp	*-48(bp),*0
	beq	L105
	mov	bx,*-48(bp)
	mov	ax,*2(bx)
	mov	*-36(bp),ax
L109:
	cmp	*-36(bp),*0
	beq	L108
	mov	bx,*-36(bp)
	mov	bx,*2(bx)
	mov	ax,*2(bx)
	mov	-374(bp),ax
	lea	ax,-370(bp)
	push	ax
	push	-372(bp)
	push	-374(bp)
	call	_concat
	add	sp,*6
	push	ax
	call	_srchnam
	pop	cx
	mov	*-38(bp),ax
	or	ax,ax
	beq	L110
	lea	ax,-150(bp)
	push	ax
	push	-374(bp)
	lea	ax,-250(bp)
	push	ax
	call	_concat
	add	sp,*6
	push	ax
	call	_srchnam
	pop	cx
	mov	*-40(bp),ax
	or	ax,ax
	beq	L110
	lea	ax,*-14(bp)
	push	ax
	mov	ax,*6(bp)
	inc	ax
	push	ax
	push	*-40(bp)
	call	_doname
	add	sp,*6
	add	*-6(bp),ax
	mov	ax,*-26(bp)
	mov	dx,*-24(bp)
	cmp	dx,*-12(bp)
	bgt	L111
	blt	L10010
	cmp	ax,*-14(bp)
	bhis	L111
L10010:
	mov	bx,*-40(bp)
	push	*2(bx)
	push	-376(bp)
	call	_appendq
	add	sp,*4
	mov	-376(bp),ax
L111:
	cmp	_dbgflag,*0
	beq	L112
	push	*-12(bp)
	push	*-14(bp)
	mov	bx,*-40(bp)
	push	*2(bx)
	mov	ax,#L113
	push	ax
	call	_printf
	add	sp,*8
L112:
	mov	ax,*-14(bp)
	mov	dx,*-12(bp)
	cmp	dx,*-20(bp)
	blt	L114
	bgt	L10011
	cmp	ax,*-22(bp)
	blos	L114
L10011:
	mov	ax,*-14(bp)
	mov	dx,*-12(bp)
	mov	*-22(bp),ax
	mov	*-20(bp),dx
L114:
	lea	ax,-250(bp)
	push	ax
	mov	ax,#L115
	push	ax
	call	_setvar
	add	sp,*4
	lea	ax,-150(bp)
	push	ax
	call	_copys
	pop	cx
	push	ax
	mov	ax,#L116
	push	ax
	call	_setvar
	add	sp,*4
	mov	bx,*-38(bp)
	mov	ax,*4(bx)
	mov	*-50(bp),ax
L119:
	cmp	*-50(bp),*0
	beq	L118
	mov	bx,*-50(bp)
	mov	ax,*4(bx)
	mov	*-42(bp),ax
	or	ax,ax
	beq	L120
	br	L118
L120:
L117:
	mov	bx,*-50(bp)
	mov	ax,(bx)
	mov	*-50(bp),ax
	br	L119
L118:
	br	L121
L110:
L107:
	mov	bx,*-36(bp)
	mov	ax,(bx)
	mov	*-36(bp),ax
	br	L109
L108:
L104:
	mov	bx,*-48(bp)
	mov	ax,(bx)
	mov	*-48(bp),ax
	br	L106
L105:
L102:
L98:
	mov	bx,*-34(bp)
	mov	ax,(bx)
	mov	*-34(bp),ax
	br	L100
L99:
L95:
	mov	bx,*-46(bp)
	mov	ax,(bx)
	mov	*-46(bp),ax
	br	L97
L96:
L121:
	cmp	*-6(bp),*0
	bne	L122
	mov	ax,*-26(bp)
	mov	dx,*-24(bp)
	cmp	dx,*-20(bp)
	blt	L10012
	bgt	L10013
	cmp	ax,*-22(bp)
	blo	L10012
L10013:
	cmp	*-24(bp),*0
	bne	L122
	cmp	*-26(bp),*0
	bne	L122
L10014:
	cmp	*-20(bp),*0
	bne	L122
	cmp	*-22(bp),*0
	bne	L122
L10015:
L10012:
	cmp	*-20(bp),*0
	blt	L10016
	bgt	L10017
	cmp	*-22(bp),*0
	blos	L10016
L10017:
	mov	ax,*-22(bp)
	mov	dx,*-20(bp)
	br	L10018
L10016:
	call	_prestim
L10018:
	mov	*-26(bp),ax
	mov	*-24(bp),dx
	push	*2(di)
	mov	ax,#L123
	push	ax
	call	_setvar
	add	sp,*4
	push	-376(bp)
	call	_mkqlist
	pop	cx
	push	ax
	mov	ax,#L124
	push	ax
	call	_setvar
	add	sp,*4
	cmp	*-44(bp),*0
	beq	L125
	push	*-44(bp)
	call	_docom
	pop	cx
	add	*-6(bp),ax
	br	L126
L125:
	cmp	*-42(bp),*0
	beq	L127
	push	*-42(bp)
	call	_docom
	pop	cx
	add	*-6(bp),ax
	br	L128
L127:
	mov	ax,*6(di)
	mov	cx,*3
	sar	ax,cl
	test	ax,*7
	bne	L129
	mov	ax,#L130
	push	ax
	call	_srchnam
	pop	cx
	mov	*-38(bp),ax
	or	ax,ax
	beq	L131
	push	*2(di)
	mov	ax,#L132
	push	ax
	call	_setvar
	add	sp,*4
	mov	bx,*-38(bp)
	mov	ax,*4(bx)
	mov	*-50(bp),ax
L135:
	cmp	*-50(bp),*0
	beq	L134
	mov	bx,*-50(bp)
	mov	ax,*4(bx)
	mov	*-42(bp),ax
	or	ax,ax
	beq	L136
	push	*-42(bp)
	call	_docom
	pop	cx
	add	*-6(bp),ax
	br	L134
L136:
L133:
	mov	bx,*-50(bp)
	mov	ax,(bx)
	mov	*-50(bp),ax
	br	L135
L134:
	br	L137
L131:
	cmp	_keepgoi,*0
	beq	L138
	push	*2(di)
	mov	ax,#L139
	push	ax
	call	_printf
	add	sp,*4
	inc	*-6(bp)
	br	L140
L138:
	push	*2(di)
	mov	ax,#L142
	push	ax
	call	_fatal1
	add	sp,*4
L140:
L137:
L129:
L128:
L126:
	mov	ax,*0
	push	ax
	mov	ax,#L143
	push	ax
	call	_setvar
	add	sp,*4
	cmp	_noexfla,*0
	bne	L10019
	push	*2(di)
	call	_exists
	pop	cx
	mov	*-26(bp),ax
	mov	*-24(bp),dx
	cmp	dx,*0
	bne	L144
	cmp	ax,*0
	bne	L144
L10020:
L10019:
	call	_prestim
	mov	*-26(bp),ax
	mov	*-24(bp),dx
L144:
	br	L145
L122:
	cmp	*-6(bp),*0
	beq	L146
	cmp	*6(bp),*0
	bne	L146
	push	*2(di)
	mov	ax,#L147
	push	ax
	call	_printf
	add	sp,*4
	br	L148
L146:
	cmp	_questfl,*0
	bne	L149
	cmp	*6(bp),*0
	bne	L149
	cmp	*-10(bp),*0
	bne	L149
	push	*2(di)
	mov	ax,#L150
	push	ax
	call	_printf
	add	sp,*4
L149:
L148:
L145:
	cmp	_questfl,*0
	beq	L151
	cmp	*6(bp),*0
	bne	L151
	cmp	_ndocoms,*0
	ble	L10021
	mov	ax,*-1
	br	L10022
L10021:
	mov	ax,*0
L10022:
	push	ax
	call	_exit
	pop	cx
L151:
	cmp	*-6(bp),*0
	beq	L10023
	mov	ax,*3
	br	L10024
L10023:
	mov	ax,*2
L10024:
	mov	cl,*0
	shl	ax,cl
	and	ax,#7
	and	*6(di),#~7
	or	*6(di),ax
	mov	ax,*-30(bp)
	mov	dx,*-28(bp)
	cmp	dx,*-24(bp)
	blt	L153
	bgt	L10025
	cmp	ax,*-26(bp)
	blos	L153
L10025:
	mov	ax,*-30(bp)
	mov	dx,*-28(bp)
	mov	*-26(bp),ax
	mov	*-24(bp),dx
L153:
	mov	ax,*-26(bp)
	mov	dx,*-24(bp)
	mov	*8(di),ax
	mov	*10(di),dx
	mov	bx,*8(bp)
	mov	ax,*-26(bp)
	mov	dx,*-24(bp)
	mov	(bx),ax
	mov	*2(bx),dx
	mov	ax,*-6(bp)
	br	L51
L51:
	lea	sp,*-4(bp)
	pop	di
	pop	si
	pop	bp
	ret
	LF1 = 372
	.data
	.text
	.globl	_docom
_docom:
	push	bp
	mov	bp,sp
	push	si
	push	di
	sub	sp,#LF2
	inc	_ndocoms
	cmp	_questfl,*0
	beq	L156
	mov	ax,*0
	br	L154
L156:
	cmp	_touchfl,*0
	beq	L157
	mov	ax,#L158
	push	ax
	call	_varptr
	pop	cx
	mov	bx,ax
	mov	ax,*4(bx)
	mov	*-6(bp),ax
	cmp	_silflag,*0
	bne	L159
	push	*-6(bp)
	mov	ax,#L160
	push	ax
	call	_printf
	add	sp,*4
L159:
	cmp	_noexfla,*0
	bne	L161
	push	*-6(bp)
	mov	ax,*1
	push	ax
	call	_touch
	add	sp,*4
L161:
	br	L163
L157:
L166:
	cmp	*4(bp),*0
	beq	L165
	lea	ax,-2510(bp)
	push	ax
	mov	bx,*4(bp)
	push	*2(bx)
	call	_subst
	add	sp,*4
	mov	ax,_ignerr
	mov	*-8(bp),ax
	mov	*-10(bp),*0
	lea	ax,-2510(bp)
	mov	*-6(bp),ax
L169:
	mov	bx,*-6(bp)
	cmpb	(bx),*45
	beq	L10026
	mov	bx,*-6(bp)
	cmpb	(bx),*64
	bne	L168
L10026:
	mov	bx,*-6(bp)
	cmpb	(bx),*45
	bne	L170
	mov	*-8(bp),*1
	br	L171
L170:
	mov	*-10(bp),*1
L171:
L167:
	inc	*-6(bp)
	br	L169
L168:
	push	*-10(bp)
	push	*-8(bp)
	push	*-6(bp)
	call	_docom1
	add	sp,*6
	or	ax,ax
	beq	L173
	cmp	*-8(bp),*0
	bne	L173
	cmp	_keepgoi,*0
	beq	L174
	mov	ax,*1
	br	L154
L174:
	mov	ax,*0
	push	ax
	call	_fatal
	pop	cx
L173:
L164:
	mov	bx,*4(bp)
	mov	ax,(bx)
	mov	*4(bp),ax
	br	L166
L165:
L163:
	mov	ax,*0
	br	L154
L154:
	lea	sp,*-4(bp)
	pop	di
	pop	si
	pop	bp
	ret
	LF2 = 2506
	.data
	.text
	.globl	_docom1
_docom1:
	push	bp
	mov	bp,sp
	push	si
	push	di
	sub	sp,#LF3
	mov	di,4(bp)
	cmpb	(di),*0
	bne	L177
	mov	ax,*0
	br	L176
L177:
	cmp	_silflag,*0
	bne	L178
	cmp	*8(bp),*0
	beq	L10027
	cmp	_noexfla,*0
	beq	L178
L10027:
	push	di
	cmp	_noexfla,*0
	beq	L10028
	mov	ax,#L180
	br	L10029
L10028:
	mov	ax,_prompt
L10029:
	push	ax
	mov	ax,#L179
	push	ax
	call	_printf
	add	sp,*6
	mov	ax,#__iob+8
	push	ax
	call	_fflush
	pop	cx
L178:
	cmp	_noexfla,*0
	beq	L181
	mov	ax,*0
	br	L176
L181:
	push	*6(bp)
	push	di
	call	_dosys
	add	sp,*4
	mov	si,ax
	or	ax,ax
	beq	L183
	mov	ax,si
	mov	cx,*8
	sar	ax,cl
	or	ax,ax
	beq	L184
	mov	ax,si
	mov	cx,*8
	sar	ax,cl
	push	ax
	mov	ax,#L185
	push	ax
	call	_printf
	add	sp,*4
	br	L186
L184:
	push	si
	mov	ax,#L187
	push	ax
	call	_printf
	add	sp,*4
L186:
	cmp	*6(bp),*0
	beq	L188
	mov	ax,#L189
	push	ax
	call	_printf
	pop	cx
	br	L190
L188:
	mov	ax,#L191
	push	ax
	call	_printf
	pop	cx
L190:
	mov	ax,#__iob+8
	push	ax
	call	_fflush
	pop	cx
L183:
	mov	ax,si
	br	L176
L176:
	lea	sp,*-4(bp)
	pop	di
	pop	si
	pop	bp
	ret
	LF3 = 0
	.data
	.text
	.globl	_expand
_expand:
	push	bp
	mov	bp,sp
	push	si
	push	di
	sub	sp,#LF4
	mov	di,4(bp)
	mov	bx,*2(di)
	mov	ax,*2(bx)
	mov	*-6(bp),ax
	mov	si,*-6(bp)
L195:
	mov	bx,si
	inc	si
	movb	ax,(bx)
	cbw
	br	L197
L198:
	br	L192
L199:
L200:
L201:
	push	(di)
	mov	ax,*1
	push	ax
	push	*-6(bp)
	call	_srchdir
	add	sp,*6
	mov	*-8(bp),ax
	or	ax,ax
	beq	L202
	mov	ax,*-8(bp)
	mov	(di),ax
	mov	*2(di),*0
L202:
	br	L192
L197:
	cmp	ax,#0
	beq	L198
	cmp	ax,#42
	beq	L199
	cmp	ax,#63
	beq	L200
	cmp	ax,#91
	beq	L201
L196:
L193:
	br	L195
L194:
L192:
	lea	sp,*-4(bp)
	pop	di
	pop	si
	pop	bp
	ret
	LF4 = 4
	.data
L59:
	.byte	100,111,110,97,109,101,40,37
	.byte	115,44,37,100,41,10,0
L76:
	.byte	84,73,77,69,40,37,115,41
	.byte	61,37,108,100,10,0
L83:
	.byte	64,0
L84:
	.byte	63,0
L87:
	.byte	64,0
L92:
	.byte	84,111,111,32,109,97,110,121
	.byte	32,99,111,109,109,97,110,100
	.byte	32,108,105,110,101,115,32,102
	.byte	111,114,32,96,37,115,39,10
	.byte	0
L103:
	.byte	42,0
L113:
	.byte	84,73,77,69,40,37,115,41
	.byte	61,37,108,100,10,0
L115:
	.byte	42,0
L116:
	.byte	60,0
L123:
	.byte	64,0
L124:
	.byte	63,0
L130:
	.byte	46,68,69,70,65,85,76,84
	.byte	0
L132:
	.byte	60,0
L139:
	.byte	68,111,110,39,116,32,107,110
	.byte	111,119,32,104,111,119,32,116
	.byte	111,32,109,97,107,101,32,37
	.byte	115,10,0
L142:
	.byte	32,68,111,110,39,116,32,107
	.byte	110,111,119,32,104,111,119,32
	.byte	116,111,32,109,97,107,101,32
	.byte	37,115,0
L143:
	.byte	64,0
L147:
	.byte	96,37,115,39,32,110,111,116
	.byte	32,114,101,109,97,100,101,32
	.byte	98,101,99,97,117,115,101,32
	.byte	111,102,32,101,114,114,111,114
	.byte	115,10,0
L150:
	.byte	96,37,115,39,32,105,115,32
	.byte	117,112,32,116,111,32,100,97
	.byte	116,101,46,10,0
L158:
	.byte	64,0
L160:
	.byte	116,111,117,99,104,40,37,115
	.byte	41,10,0
L179:
	.byte	37,115,37,115,10,0
L180:
	.byte	0
L185:
	.byte	42,42,42,32,69,114,114,111
	.byte	114,32,99,111,100,101,32,37
	.byte	100,0
L187:
	.byte	42,42,42,32,84,101,114,109
	.byte	105,110,97,116,105,111,110,32
	.byte	99,111,100,101,32,37,100,0
L189:
	.byte	32,40,105,103,110,111,114,101
	.byte	100,41,10,0
L191:
	.byte	10,0
