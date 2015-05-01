subroutine jelira(nomlu, catr, ival, cval)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "asterf_types.h"
#include "jeveux_private.h"
#include "asterfort/codent.h"
#include "asterfort/jjallc.h"
#include "asterfort/jjcroc.h"
#include "asterfort/jjlide.h"
#include "asterfort/jjlirs.h"
#include "asterfort/jjvern.h"
#include "asterfort/utmess.h"
    character(len=*), intent(in) :: nomlu, catr
    character(len=*), intent(out), optional :: cval
    integer, intent(out), optional :: ival
!     ------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
!     ------------------------------------------------------------------
    integer :: isstat
    common /iconje/  isstat
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: iadmi, ib, ibacol, ibnum, ic, id, ipgcex
    integer :: is, iss, iu, iuu, iva, ixdeso, ixiadd
    integer :: ixiadm, ixlong, ixlono, ixluti, ixnom, ixnum, jcara
    integer :: jdate, jdocu, jgenr, jhcod, jiadd, jiadm, jlong
    integer :: jlono, jltyp, jluti, jmarq, jorig, jrnom, jtype
    integer :: ltypi, n
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
!
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
    character(len=4) :: kstat
    common /kstaje/  kstat
!     ------------------------------------------------------------------
    character(len=2) :: dn2
    character(len=5) :: classe
    character(len=8) :: nomfic, kstout, kstini
    common /kficje/  classe    , nomfic(n) , kstout(n) , kstini(n) ,&
     &                 dn2(n)
!
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
!
    integer :: ipgc, kdesma(2), lgd, lgduti, kposma(2), lgp, lgputi
    common /iadmje/  ipgc,kdesma,   lgd,lgduti,kposma,   lgp,lgputi
!     ------------------------------------------------------------------
    integer :: ivnmax, iddeso, idiadd, idiadm, idnom, idlong, idlono, idluti
    integer :: idnum
    parameter    ( ivnmax = 0 , iddeso = 1 ,idiadd = 2 , idiadm = 3 ,&
     &                          idnom  = 5 ,             idlong = 7 ,&
     &             idlono = 8 , idluti = 9 ,idnum  = 10 )
!     ------------------------------------------------------------------
    character(len=32) :: nom32, noml32
    character(len=33) :: cva
    character(len=1) :: genri
    character(len=8) :: catrlu
    integer :: lcv, icre, iret
    aster_logical :: lconst, lconti, llong, lluti, lcol
! DEB ------------------------------------------------------------------
!
    ipgcex = ipgc
    noml32 = nomlu
    catrlu = catr
!
    iva = -1
    cva = ' '
    lcv = 0
    icre = 0
    call jjvern(noml32, icre, iret)
!
    ic = iclas
    if (iret .eq. 0) then
        call utmess('F', 'JEVEUX_26', sk=noml32(1:24))
    else if (iret .eq. 1) then
        lcol = .false.
        ic = iclaos
        id = idatos
        lconst = .true.
        lconti = .true.
        if (noml32(25:32) .ne. '        ') then
            call utmess('F', 'JEVEUX1_09', sk=noml32(1:24))
        endif
    else
        lcol = .true.
        ipgc = -2
        ic = iclaco
        call jjallc(ic, idatco, 'L', ibacol)
        if (noml32(25:32) .ne. '        ') then
            iret = 3
            call jjcroc(noml32(25:32), icre)
            if (idatoc .eq. 0) then
                call utmess('F', 'JEVEUX_30', sk=noml32(1:24))
            endif
        endif
        ixdeso = iszon ( jiszon + ibacol + iddeso )
        id = ixdeso
        ixiadd = iszon ( jiszon + ibacol + idiadd )
        lconti = ( ixiadd .eq. 0 )
        ixlong = iszon ( jiszon + ibacol + idlong )
        ixlono = iszon ( jiszon + ibacol + idlono )
        ixluti = iszon ( jiszon + ibacol + idluti )
        ixnom = iszon ( jiszon + ibacol + idnom )
        ixnum = iszon ( jiszon + ibacol + idnum )
        lconst = (ixlong .eq. 0 )
        if (iret .eq. 2) then
            if (catrlu .eq. 'ACCES   ') then
                if (ixnom .ne. 0) then
                    nom32 = rnom ( jrnom(ic) + ixnom )
                    cva = 'NO'
                    lcv = 2
                    if (index (nom32,'$$') .eq. 0) then
                        cva = cva(1:2)//' '//nom32(1:24)
                        lcv = lcv + 1 + 24
                    endif
                else
                    cva = 'NU'
                    lcv = 2
                endif
            else if (catrlu .eq. 'STOCKAGE') then
                if (ixiadd .ne. 0) then
                    nom32 = rnom ( jrnom(ic) + ixiadd )
                    cva = 'DISPERSE'
                    lcv = 8
                    if (index (nom32,'$$') .eq. 0) then
                        cva = cva(1:8)//' '//nom32(1:24)
                        lcv = lcv + 1 + 24
                    endif
                else
                    cva = 'CONTIG'
                    lcv = 6
                endif
            else if (catrlu .eq. 'MODELONG') then
                if (.not. lconst) then
                    nom32 = rnom ( jrnom(ic) + ixlong )
                    cva = 'VARIABLE'
                    lcv = 8
                    if (index (nom32,'$$') .eq. 0) then
                        cva = cva(1:8)//' '//nom32(1:24)
                        lcv = lcv + 1 + 24
                    endif
                else
                    cva = 'CONSTANT'
                    lcv = 8
                endif
            else if (catrlu .eq. 'LONT    ' .and. lconti) then
                iva = lono ( jlono(ic) + ixdeso )
            else if (catrlu .eq. 'NMAXOC  ') then
                iva = iszon ( jiszon + ibacol + ivnmax )
            else if (catrlu .eq. 'NUTIOC  ') then
                if (ixnom .gt. 0) then
                    iva = luti ( jluti(ic) + ixnom )
                else if (ixnum .gt. 0) then
                    ibnum = iadm ( jiadm(ic) + 2*ixnum-1 )
                    iva = iszon ( jiszon + ibnum - 1 + 2 )
                endif
            else if (catrlu .eq. 'NOMUTI  ') then
                if (ixnom .gt. 0) then
                    iva = luti ( jluti(ic) + ixnom )
                endif
            endif
        endif
    endif
    if (iva .ge. 0 .or. lcv .gt. 0) goto 100
    genri = genr ( jgenr(ic) + id )
    ltypi = ltyp ( jltyp(ic) + id )
    llong = ( catrlu(4:6) .eq. 'MAX' )
    lluti = ( catrlu(4:6) .eq. 'UTI' )
    if ((genri .ne. 'N' .and. catrlu(1:3).eq. 'NOM') .or.&
        (index('CRS',genri).eq.0 .and. catrlu(1:3).eq. 'NOL') .or.&
        (genri .ne. 'R' .and. catrlu(1:3).eq. 'NOC') .or.&
        (index('EV' ,genri).eq.0 .and. catrlu(1:4).eq. 'LONM') .or.&
        (index('EV' ,genri).eq.0 .and. catrlu(1:4).eq. 'LONU')) then
        call utmess('F', 'JEVEUX1_10', sk=genri)
    endif
!
    if (catrlu .eq. 'CLAS    ') then
        cva = classe ( ic : ic )
        lcv = 1
    else if (catrlu .eq. 'GENR    ') then
        cva = genri
        lcv = len(genr(1))
    else if (catrlu(1:4) .eq. 'TYPE') then
        cva = type ( jtype(ic) + id )
        lcv = len(type(1))
        if (cva .eq. 'K') then
            if (catrlu .eq. 'TYPELONG') then
                call codent(ltypi, 'G', cva(2:4))
                lcv = len(type(1))+2
            endif
        endif
    else if (catrlu .eq. 'LTYP    ') then
        iva = ltypi
    else if (catrlu .eq. 'DOCU    ') then
        cva = docu ( jdocu(ic) + id )
        lcv = len(docu(1))
    else if (catrlu .eq. 'DATE    ') then
        iva = date ( jdate(ic) + id )
    else if (catrlu .eq. 'XOUS    ') then
        if (iret .eq. 1) then
            cva = 'S'
        else
            cva = 'X'
        endif
        lcv = 1
    else
        if (llong .and. lconst) then
            if (catrlu .eq. 'LONMAX  ' .or. catrlu .eq. 'NOMMAX  ') then
                iva = long ( jlong(ic) + id )
            endif
        else if (llong .and. iret .eq. 3) then
            ib = jiszon + iadm ( jiadm(ic) + 2*ixlong-1 ) - 1 + idatoc
            if (catrlu .eq. 'LONMAX  ' .or. catrlu .eq. 'NOMMAX  ') then
                iva = iszon (ib)
            endif
        else if (lluti .and. lconst) then
            if (catrlu .eq. 'LONUTI  ' .or. catrlu .eq. 'NOMUTI  ') then
                iva = luti ( jluti(ic) + id )
            endif
        else if (lluti .and. iret .eq. 3) then
            ib = jiszon + iadm ( jiadm(ic) + 2*ixluti-1 ) - 1 + idatoc
            if (catrlu .eq. 'LONUTI  ') then
                iva = iszon (ib)
            endif
        else if (catrlu.eq.'LONO    ') then
            if (lconti .or. lconst) then
                iva = lono( jlono(ic) + id )
            else
                ib = jiszon + iadm ( jiadm(ic) + 2*ixlono-1 ) - 1 + idatoc
                iva = iszon (ib)
            endif
        else if (catrlu .eq. 'IADD    ') then
            if (lconti) then
                iva = iadd ( jiadd(ic) + 2*id-1 )
            else
                ib = jiszon + iadm (jiadm(ic) + 2*ixiadd-1) - 1 + 2* idatoc-1
                iva = iszon(ib)
            endif
        else if (catrlu .eq. 'LADD    ') then
            if (lconti) then
                iva = iadd ( jiadd(ic) + 2*id )
            else
                ib = jiszon + iadm (jiadm(ic) + 2*ixiadd-1) - 1 + 2* idatoc
                iva = iszon(ib)
            endif
        else if (catrlu .eq. 'IADM    ') then
            if (lconti) then
                iva = iadm ( jiadm(ic) + 2*id-1 )
            else
                ixiadm = iszon ( jiszon + ibacol + idiadm )
                ib = jiszon + iadm (jiadm(ic) + 2*ixiadm-1) - 1 + 2* idatoc-1
                iva = iszon(ib)
            endif
        else if (catrlu .eq. 'USAGE   ') then
            lcv = 3
            if (lconti) then
                iadmi = iadm ( jiadm(ic) + 2*id-1 )
                if (iadmi .eq. 0) then
                    cva = 'X X'
                else
                    call jjlirs(iadmi, ic, id, iuu, iss)
                    iu = iuu / isstat
                    is = iss / isstat
                    cva = kstat(iu:iu)//' '//kstat(is:is)
                endif
            else
                ixiadm = iszon ( jiszon + ibacol + idiadm )
                ib = jiszon + iadm(jiadm(ic)+2*ixiadm-1) -1 + 2* idatoc-1
                iadmi = iszon(ib)
                if (iadmi .ne. 0) then
                    call jjlirs(iadmi, ic, idatoc, iuu, iss)
                    iu = iuu / isstat
                    is = iss / isstat
                    cva = kstat(iu:iu)//' '//kstat(is:is)
                else
                    cva = 'X X'
                endif
            endif
        else
            call utmess('F', 'JEVEUX1_04', sk=catrlu)
        endif
    endif
100 continue
    if (lcv .ne. 0) then
        if (present(cval)) then
            cval = cva(1:lcv)
        endif
    else
        if (present(ival)) then
            ival = iva
        endif
    endif
!
    if (lcol) then
        call jjlide('JELIBE', noml32(1:24), 2)
    endif
    ipgc = ipgcex
!
end subroutine
