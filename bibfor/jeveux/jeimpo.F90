subroutine jeimpo(unit, nomlu, mess)
! person_in_charge: j-pierre.lefebvre at edf.fr
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
! ----------------------------------------------------------------------
! IMPRIME LE CONTENU D'UN OBJET JEVEUX
!
! IN  UNIT  : NUMERO D'UNITE LOGIQUE ASSOCIE AU FICHIER D'IMPRESSION
! IN  NOMLU : NOM DE L'OBJET JEVEUX A IMPRIMER
! IN  MESS  : MESSAGE D'INFORMATION IMPRIME
! ----------------------------------------------------------------------
    implicit none
    include 'jeveux_private.h'
    include 'asterfort/jjallc.h'
    include 'asterfort/jjalty.h'
    include 'asterfort/jjcroc.h'
    include 'asterfort/jjimpo.h'
    include 'asterfort/jjlide.h'
    include 'asterfort/jjvern.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    integer :: unit
    character(len=*) :: nomlu, mess
!     ------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
!     -----------------------------------------------------------------
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: iadmex, iadmi, ibdeso, ibiadd, ibiadm, iblono, ideci
    integer :: inat, ipgcex, ixiadm, ixlono, jcara, jdate, jdocu
    integer :: jgenr, jhcod, jiadd, jiadm, jlong, jlono, jltyp
    integer :: jluti, jmarq, jorig, jrnom, jtype, k, n
    integer :: nbmax
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
!
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
!
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
    integer :: ipgc, kdesma(2), lgd, lgduti, kposma(2), lgp, lgputi
    common /iadmje/  ipgc,kdesma,   lgd,lgduti,kposma,   lgp,lgputi
    integer :: numec
    common /inumje/  numec
!     ------------------------------------------------------------------
    character(len=32) :: noml32
    character(len=1) :: genri, typei
    integer :: icre, iret, jctab, ltypi, lonoi, iaddi(2)
    integer :: ibacol, ixiadd, ixdeso
    logical :: lconst, lcol
    real(kind=8) :: rb
!     ------------------------------------------------------------------
    integer :: ivnmax, iddeso, idiadd, idiadm, idlong, idlono
    parameter    ( ivnmax = 0 , iddeso = 1 , idiadd = 2 , idiadm = 3 ,&
     &                              idlong = 7 ,&
     &               idlono = 8   )
! DEB ------------------------------------------------------------------
    ipgcex = ipgc
    ipgc = -2
    noml32 = nomlu
!
    lcol = .false.
    icre = 0
    call jjvern(noml32, icre, iret)
!
    if (iret .eq. 0) then
        call u2mesk('A', 'JEVEUX_26', 1, noml32(1:24))
        goto 9999
    else if (iret .eq. 1) then
!
! ----  CAS D'UN OBJET SIMPLE
!
        inat = 1
        iadmi = iadm ( jiadm(iclaos) + 2*idatos-1 )
        iadmex = iadmi
        genri = genr ( jgenr(iclaos) + idatos )
        typei = type ( jtype(iclaos) + idatos )
        ltypi = ltyp ( jltyp(iclaos) + idatos )
        lonoi = lono ( jlono(iclaos) + idatos ) * ltypi
        if (iadmex .eq. 0) then
            iaddi(1) = iadd ( jiadd(iclaos) + 2*idatos-1 )
            iaddi(2) = iadd ( jiadd(iclaos) + 2*idatos )
            if (iaddi(1) .eq. 0) then
                call u2mesk('A', 'JEVEUX_27', 1, noml32(1:24))
                goto 9999
            endif
            call jjalty(typei, ltypi, 'L', 1, jctab)
            iadmi = iadm ( jiadm(iclaos) + 2*idatos-1 )
        endif
        ideci = 0
        call jjimpo(unit, iadmi, ideci, 0, genri,&
                    typei, ltypi, lonoi, mess)
        if (iadmex .eq. 0) then
            call jjlide('JEIMPO', noml32, inat)
        endif
    else if (iret .eq. 2) then
!
! ----- CAS D'UNE COLLECTION
!
        lcol = .true.
        call jjallc(iclaco, idatco, 'L', ibacol)
        if (noml32(25:32) .eq. '        ') then
            inat = 2
        else
            call jjcroc(noml32(25:32), icre)
            inat = 3
        endif
    endif
    if (inat .eq. 2) then
!
! ----- CAS D'UNE COLLECTION ENTIERE
!
        ixiadd = iszon ( jiszon + ibacol + idiadd )
        ixiadm = iszon ( jiszon + ibacol + idiadm )
        ixdeso = iszon ( jiszon + ibacol + iddeso )
        genri = genr( jgenr(iclaco) + ixdeso )
        typei = type( jtype(iclaco) + ixdeso )
        ltypi = ltyp( jltyp(iclaco) + ixdeso )
        if (ixiadd .eq. 0) then
!
! ------- COLLECTION CONTIGUE
!
            iadmi = iadm ( jiadm(iclaco) + 2*ixdeso-1 )
            iaddi(1) = iadd ( jiadd(iclaco) + 2*ixdeso-1 )
            iaddi(2) = iadd ( jiadd(iclaco) + 2*ixdeso )
            iadmex = iadmi
            if (iadmex .eq. 0) then
                if (iaddi(1) .eq. 0) then
                    call u2mesk('A', 'JEVEUX_28', 1, noml32(1:24))
                    goto 9999
                endif
                call jjalty(typei, ltypi, 'L', 2, jctab)
                iadmi = iadm ( jiadm(iclaco) + 2*ixdeso-1 )
            endif
            lonoi = lono( jlono(iclaco) + ixdeso ) * ltypi
            ideci = 0
            call jjimpo(unit, iadmi, ideci, -1, genri,&
                        typei, ltypi, lonoi, mess)
            if (iadmex .eq. 0) then
                call jjlide('JEIMPO', noml32, inat)
            endif
        else
!
! ------- COLLECTION DISPERSEE
!
            nbmax = iszon ( jiszon + ibacol + ivnmax )
            ibiadm = iadm ( jiadm(iclaco) + 2*ixiadm-1 )
            ibiadd = iadm ( jiadm(iclaco) + 2*ixiadd-1 )
            ideci = 0
            do 10 k = 1, nbmax
                iadmi = iszon(jiszon + ibiadm - 1 + 2*k-1 )
                if (iadmi .eq. 0) then
                    iaddi(1) = iszon(jiszon + ibiadd - 1 + 2*k-1 )
                    iaddi(2) = iszon(jiszon + ibiadd - 1 + 2*k )
                    if (iaddi(1) .eq. 0) goto 10
                    call jjalty(typei, ltypi, 'L', 3, jctab)
                    iadmi = iszon(jiszon + ibiadm - 1 + 2*k-1 )
                endif
                ixlono = iszon ( jiszon + ibacol + idlono )
                if (ixlono .eq. 0) then
                    lonoi = lono ( jlono(iclaco) + ixdeso ) * ltypi
                else
                    iblono = iadm ( jiadm(iclaco) + 2*ixlono-1 )
                    lonoi = iszon ( jiszon + iblono - 1 + k ) * ltypi
                endif
                call jjimpo(unit, iadmi, ideci, k, genri,&
                            typei, ltypi, lonoi, mess)
                numec = k
                call jjlide('JEIMPO', noml32//'$$XNUM  ', 2)
10          continue
        endif
        call jjlide('JEIMPO', noml32, inat)
    else if (inat .eq. 3) then
!       ------ CAS D'UN OBJET DE COLLECTION  ------
        ixiadd = iszon ( jiszon + ibacol + idiadd )
        ixiadm = iszon ( jiszon + ibacol + idiadm )
        ixdeso = iszon ( jiszon + ibacol + iddeso )
        ixlono = iszon ( jiszon + ibacol + idlono )
        genri = genr( jgenr(iclaco) + ixdeso )
        typei = type( jtype(iclaco) + ixdeso )
        ltypi = ltyp( jltyp(iclaco) + ixdeso )
        if (ixiadd .eq. 0) then
!           ----------- COLLECTION CONTIGUE
            lconst = ( iszon ( jiszon + ibacol + idlong ) .eq. 0 )
            ibdeso = iadm ( jiadm(iclaco) + 2*ixdeso-1 )
            iaddi(1) = iadd ( jiadd(iclaco) + 2*ixdeso-1 )
            iaddi(2) = iadd ( jiadd(iclaco) + 2*ixdeso )
            iadmex = ibdeso
            if (iadmex .eq. 0) then
                if (iaddi(1) .eq. 0) then
                    call u2mesg('A', 'JEVEUX_29', 1, noml32(1:24), 1,&
                                idatoc, 0, rb)
                    goto 9999
                endif
                call jjalty(typei, ltypi, 'L', 2, jctab)
                ibdeso = iadm ( jiadm(iclaco) + 2*ixdeso-1 )
            endif
            if (lconst) then
                lonoi = lono ( jlono(iclaco) + ixdeso ) * ltypi
                lonoi = lonoi / iszon ( jiszon + ibacol + ivnmax )
                iadmi = ibdeso
                ideci = ( idatoc - 1 ) * lonoi
            else
                iblono = iadm ( jiadm(iclaco) + 2*ixlono-1 )
                lonoi = ltypi * (iszon(jiszon+iblono-1+idatoc+1) - iszon(jiszon+iblono-1+idatoc )&
                        )
                iadmi = ibdeso
                ideci = (ltypi*(iszon(jiszon+iblono-1+idatoc)-1))
            endif
            call jjimpo(unit, iadmi, ideci, idatoc, genri,&
                        typei, ltypi, lonoi, mess)
            if (iadmex .eq. 0) then
                call jjlide('JEIMPO', noml32, inat)
            endif
        else
!
! -------- COLLECTION DISPERSEE
!
            ibiadm = iadm ( jiadm(iclaco) + 2*ixiadm-1 )
            ibiadd = iadm ( jiadm(iclaco) + 2*ixiadd-1 )
            iadmi = iszon(jiszon + ibiadm - 1 + 2*idatoc-1 )
            iadmex = iadmi
            ideci = 0
            if (iadmex .eq. 0) then
                iaddi(1) = iszon(jiszon + ibiadd - 1 + 2*idatoc-1 )
                iaddi(2) = iszon(jiszon + ibiadd - 1 + 2*idatoc )
                if (iaddi(1) .eq. 0) then
                    call u2mesg('A', 'JEVEUX_29', 1, noml32(1:24), 1,&
                                idatoc, 0, rb)
                    goto 9999
                endif
                call jjalty(typei, ltypi, 'L', inat, jctab)
                iadmi = iszon(jiszon + ibiadm - 1 + 2*idatoc-1 )
            endif
            ixlono = iszon ( jiszon + ibacol + idlono )
            if (ixlono .eq. 0) then
                lonoi = lono( jlono(iclaco) + ixdeso ) * ltypi
            else
                iblono = iadm ( jiadm(iclaco) + 2*ixlono-1 )
                lonoi = iszon ( jiszon + iblono + idatoc - 1 ) * ltypi
            endif
            call jjimpo(unit, iadmi, ideci, idatoc, genri,&
                        typei, ltypi, lonoi, mess)
            if (iadmex .eq. 0) then
                call jjlide('JEIMPO', noml32, inat)
            endif
        endif
    endif
9999  continue
    if (lcol) then
        call jjlide('JEIMPO', noml32(1:24), 2)
    endif
    ipgc = ipgcex
! FIN ------------------------------------------------------------------
end subroutine
