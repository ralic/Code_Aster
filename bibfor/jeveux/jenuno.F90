subroutine jenuno(nomlu, nomo)
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
    include 'jeveux_private.h'
    include 'asterfort/assert.h'
    include 'asterfort/jjallc.h'
    include 'asterfort/jjlide.h'
    include 'asterfort/jjvern.h'
    include 'asterfort/jxveuo.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    character(len=*) :: nomlu, nomo
! ----------------------------------------------------------------------
! RENVOIE LE NOM ASSOCIE A UN IDENTIFICATEUR
!
! IN  NOMLU  : NOM DE LA COLLECTION OU DU REPERTOIRE
!              L' APPEL DOIT S'EFFECTUER PAR L'INTERMEDIAIRE DE JEXNUM
! IN  NOMO   : NOM DANS LE REPERTOIRE
!
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
! ----------------------------------------------------------------------
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: iadmex, iadmi, ibacol, ideco, idenom, ipgcex, ixnom
    integer :: jcara, jctab, jdate, jdocu, jgenr, jhcod, jiadd
    integer :: jiadm, jlong, jlono, jltyp, jluti, jmarq, jorig
    integer :: jrnom, jtype, k, kadm, lnom, lutii, n
    integer :: nk
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
!
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
! ----------------------------------------------------------------------
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
    integer :: ipgc, kdesma(2), lgd, lgduti, kposma(2), lgp, lgputi
    common /iadmje/  ipgc,kdesma,   lgd,lgduti,kposma,   lgp,lgputi
    integer :: numec
    common /inumje/  numec
! ----------------------------------------------------------------------
    integer :: ideno, ilnom
    parameter      ( ideno=2,ilnom=3 )
! ----------------------------------------------------------------------
    integer :: idnom
    parameter    ( idnom  = 5 )
! ----------------------------------------------------------------------
    character(len=32) :: noml32
    character(len=1) :: genri
    integer :: icre, iret, itab, vali(2)
    real(kind=8) :: valr
! DEB ------------------------------------------------------------------
    ipgcex = ipgc
    ipgc = -2
!
    icre = 0
    noml32 = nomlu
    call jjvern(noml32, icre, iret)
!
    if (iret .eq. 0) then
        call u2mesk('F', 'JEVEUX_25', 1, noml32(1:24))
    else
        if (iret .eq. 1) then
!
! ------- OBJET DE TYPE REPERTOIRE
!
            genri = genr ( jgenr(iclaos) + idatos )
            if (genri .ne. 'N') then
                call u2mesk('F', 'JEVEUX1_12', 1, noml32)
            endif
            lutii = luti ( jluti(iclaos) + idatos )
            if (lutii .lt. numec .or. numec .le. 0) then
                vali(1) = lutii
                vali(2) = numec
                call u2mesg('F', 'JEVEUX1_13', 1, noml32, 2,&
                            vali, 0, valr)
            endif
            iadmi = iadm ( jiadm(iclaos) + 2*idatos-1 )
            iadmex = iadmi
            if (iadmex .eq. 0) then
                call jxveuo('L', itab, iret, jctab)
                iadmi = iadm ( jiadm(iclaos) + 2*idatos-1 )
            endif
            kadm = iadmi
            idenom = iszon ( jiszon + kadm - 1 + ideno )
            lnom = iszon ( jiszon + kadm - 1 + ilnom )
            ideco = (kadm - 1) * lois + idenom + lnom * (numec - 1)
            nk = min ( len(nomo) , lnom )
            nomo = ' '
            do 10 k = 1, nk
                nomo(k:k) = k1zon ( jk1zon + ideco + k )
10          continue
            if (iadmex .eq. 0) then
                call jjlide('JENUNO', noml32, iret)
            endif
        else if (iret .eq. 2) then
!
! ------- REPERTOIRE DE COLLECTION
!
            call jjallc(iclaco, idatco, 'L', ibacol)
            ixnom = iszon ( jiszon + ibacol + idnom )
            if (ixnom .eq. 0) then
                call u2mesk('F', 'JEVEUX1_14', 1, noml32)
            endif
            lutii = luti ( jluti(iclaco) + ixnom )
            if (lutii .lt. numec .or. numec .le. 0) then
                vali(1) = lutii
                vali(2) = numec
                call u2mesg('F', 'JEVEUX1_13', 1, noml32, 2,&
                            vali, 0, valr)
            endif
            iadmi = iadm ( jiadm(iclaco) + 2*ixnom-1 )
            kadm = iadmi
            idenom = iszon ( jiszon + kadm - 1 + ideno )
            lnom = iszon ( jiszon + kadm - 1 + ilnom )
            ideco = (kadm - 1) * lois + idenom + lnom*(numec - 1)
            nomo = ' '
            do 20 k = 1, min ( len(nomo) , lnom )
                nomo(k:k) = k1zon ( jk1zon + ideco + k )
20          continue
            call jjlide('JENUNO', nomlu(1:24), 2)
        else
            call assert(.false.)
        endif
    endif
    ipgc = ipgcex
! FIN ------------------------------------------------------------------
end subroutine
