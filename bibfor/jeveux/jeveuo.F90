subroutine jeveuo(nomlu, cel, jctab)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: j-pierre.lefebvre at edf.fr
    implicit none
!
    include 'jeveux.h'
    include 'jeveux_private.h'
    include 'asterfort/jjallc.h'
    include 'asterfort/jjalty.h'
    include 'asterfort/jjcroc.h'
    include 'asterfort/jjvern.h'
    include 'asterfort/jxlocs.h'
    include 'asterfort/u2mesk.h'
    integer :: jctab
    character(len=*) :: nomlu, cel
!     ==================================================================
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ibacol, iblono, inat, inatb, ixdeso, ixiadd, ixlono
    integer :: jcara, jdate, jdocu, jgenr, jhcod, jiadd, jiadm
    integer :: jlong, jlono, jltyp, jluti, jmarq, jorig, jrnom
    integer :: jtype, lonoi, ltypi, n
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
!
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
    integer :: numatr
    common /idatje/  numatr
!     ------------------------------------------------------------------
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
    integer :: izr, izc, izl, izk8, izk16, izk24, izk32, izk80
    equivalence    (izr,zr),(izc,zc),(izl,zl),(izk8,zk8),(izk16,zk16),&
     &               (izk24,zk24),(izk32,zk32),(izk80,zk80)
! ----------------------------------------------------------------------
    character(len=1) :: genri, typei, kcel
    character(len=8) :: noml8
    character(len=32) :: noml32
    integer :: icre, iret
    integer :: iddeso, idiadd, idlono
    parameter    (  iddeso = 1 , idiadd = 2  ,&
     &               idlono = 8   )
!     ==================================================================
    noml32 = nomlu
    noml8 = noml32(25:32)
    kcel = cel
    if (kcel .ne. 'L' .and. kcel .ne. 'E') then
        call u2mesk('F', 'JEVEUX1_27', 1, kcel)
    endif
!
    icre = 0
    call jjvern(noml32, icre, iret)
    inat = iret
    inatb = iret
    goto ( 10 , 20 , 30 ) ,iret+1
10  continue
! ----   IRET = 0
    call u2mesk('F', 'JEVEUX_26', 1, noml32(1:24))
    goto 100
20  continue
! ----   IRET = 1
    genri = genr( jgenr(iclaos) + idatos )
    typei = type( jtype(iclaos) + idatos )
    ltypi = ltyp( jltyp(iclaos) + idatos )
    if (genri .eq. 'N') then
        call u2mesk('F', 'JEVEUX1_20', 1, noml32)
    endif
    goto 100
30  continue
! ----   IRET = 2
    call jjallc(iclaco, idatco, cel, ibacol)
    ixiadd = iszon ( jiszon + ibacol + idiadd )
    ixdeso = iszon ( jiszon + ibacol + iddeso )
    if (noml8 .eq. '$$XATR  ') then
        ixlono = numatr
        iblono = iadm ( jiadm(iclaco) + 2*ixlono-1 )
        genri = genr ( jgenr(iclaco) + ixlono )
        ltypi = ltyp ( jltyp(iclaco) + ixlono )
        lonoi = lono ( jlono(iclaco) + ixlono ) * ltypi
        call jxlocs(zi, genri, ltypi, lonoi, iblono,&
                    .false., jctab)
        goto 1000
    else
        if (noml8 .ne. '        ') then
            inat = 3
            call jjcroc(noml8, icre)
!            ------ CAS D'UN OBJET DE COLLECTION  ------
            if (ixiadd .ne. 0) inatb = 3
        else
            if (ixiadd .ne. 0) then
!            ----------- COLLECTION DISPERSEE
                call u2mesk('F', 'JEVEUX1_21', 1, noml32)
            endif
        endif
        genri = genr( jgenr(iclaco) + ixdeso )
        typei = type( jtype(iclaco) + ixdeso )
        ltypi = ltyp( jltyp(iclaco) + ixdeso )
    endif
100  continue
    call jjalty(typei, ltypi, cel, inatb, jctab)
    if (inat .eq. 3 .and. ixiadd .eq. 0) then
        ixlono = iszon ( jiszon + ibacol + idlono )
        if (ixlono .gt. 0) then
            iblono = iadm ( jiadm(iclaco) + 2*ixlono-1 )
            lonoi = iszon(jiszon+iblono-1+idatoc+1) - iszon(jiszon+ iblono-1+idatoc )
            if (lonoi .gt. 0) then
                jctab = jctab + (iszon(jiszon+iblono-1+idatoc) - 1)
            else
                call u2mesk('F', 'JEVEUX1_22', 1, noml32)
            endif
        else
            jctab = jctab + long(jlong(iclaco)+ixdeso) * (idatoc-1)
        endif
    endif
1000  continue
end subroutine
