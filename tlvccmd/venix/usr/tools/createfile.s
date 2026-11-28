        .data
        .text
        .globl  _main
_main:
        push    bp
        mov     bp,sp
        push    si
        push    di
        sub     sp,#LF1
        mov     *-6(bp),*0
L19:
        cmp     *-6(bp),#255
        bhi     L20
        mov     ax,*1
        push    ax
        lea     ax,*-6(bp)
        push    ax
        mov     ax,*1
        push    ax
        call    _write
        add     sp,*6
        inc     *-6(bp)
        br      L19
L20:
L18:
        lea     sp,*-4(bp)
        pop     di
        pop     si
        pop     bp
        ret
        LF1 = 2
        .data
