subroutine jerazo(nomlu, ni, i1)
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
    include 'jeveux.h'
    include 'jeveux_private.h'
    include 'asterfort/jjallc.h'
    include 'asterfort/jjalty.h'
    include 'asterfort/jjcroc.h'
    include 'asterfort/jjvern.h'
    include 'asterfort/jxlocs.h'
    include 'asterfort/u2mesk.h'
    character(len=*) :: nomlu
    integer :: ni, i1
! ----------------------------------------------------------------------
!     REMISE A "ZERO" DU SEGMENT DE VALEURS ASSOCIE A UN OBJET JEVEUX
! IN  NI    : NOMBRE DE VALEURS A REINITIALISER
! IN  I1    : INDICE DE LA PREMIERE VALEUR
! IN  NOMLU : NOM DE L'OBJET JEVEUX
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
!-----------------------------------------------------------------------
    integer :: i, ibacol, iblono, icre, inat, inatb, iret
    integer :: ixdeso, ixiadd, ixlono, j1, j2, jcara, jctab
    integer :: jdate, jdocu, jgenr, jhcod, jiadd, jiadm, jini
    integer :: jlong, jlono, jltyp, jluti, jmarq, jorig, jrnom
    integer :: jtype, lonoi, ltypi, n
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
! ----------------------------------------------------------------------
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
! ----------------------------------------------------------------------
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
!
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
!
    integer :: numatr
    common /idatje/  numatr
! -------------------------------------------------
    character(len=32) :: noml32
    character(len=8) :: noml8
    character(len=1) :: typei, genri
! ----------------------------------------------------------------------
    integer :: iddeso, idiadd, idlono
    parameter    (  iddeso = 1 ,idiadd = 2  ,&
     &               idlono = 8   )
! ----------------------------------------------------------------------
    noml32 = nomlu
    noml8 = noml32(25:32)
!
    icre = 0
    call jjvern(noml32, icre, iret)
    inat = iret
    inatb = iret
    goto ( 1010 , 1020 , 1030 ) ,iret+1
! ----     IRET = 0
1010  continue
    call u2mesk('F', 'JEVEUX_26', 1, noml32(1:24))
    goto 100
! ----     IRET = 1
1020  continue
    genri = genr( jgenr(iclaos) + idatos )
    typei = type( jtype(iclaos) + idatos )
    ltypi = ltyp( jltyp(iclaos) + idatos )
    if (genri .eq. 'N') then
        call u2mesk('F', 'JEVEUX1_20', 1, noml32)
    endif
    goto 100
! ----     IRET = 2
1030  continue
    call jjallc(iclaco, idatco, 'E', ibacol)
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
    call jjalty(typei, ltypi, 'E', inatb, jctab)
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
!
    jini = jctab + i1 - 1
    j1 = 0
    j2 = ni - 1
    if (typei .eq. 'I') then
        do 20 i = j1, j2
            zi(jini+i) = 0
20      continue
    else if (typei .eq. 'R') then
        do 30 i = j1, j2
            zr(jini+i) = 0.d0
30      continue
    else if (typei .eq. 'C') then
        do 40 i = j1, j2
            zc(jini+i) = (0.d0,0.d0)
40      continue
    else if (typei .eq. 'L') then
        do 50 i = j1, j2
            zl(jini+i) = .false.
50      continue
    else if (typei .eq. 'K') then
        if (ltypi .eq. 8) then
            do 60 i = j1, j2
                zk8(jini+i) = ' '
60          continue
        else if (ltypi .eq. 16) then
            do 61 i = j1, j2
                zk16(jini+i) = ' '
61          continue
        else if (ltypi .eq. 24) then
            do 62 i = j1, j2
                zk24(jini+i) = ' '
62          continue
        else if (ltypi .eq. 32) then
            do 63 i = j1, j2
                zk32(jini+i) = ' '
63          continue
        else if (ltypi .eq. 80) then
            do 64 i = j1, j2
                zk80(jini+i) = ' '
64          continue
        endif
    endif
!
end subroutine
