subroutine nmvmpm(compor, icodma, itemp, temp, e,&
                  xnu, loi346)
!
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
!
    implicit none
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
    integer :: icodma, itemp
    real(kind=8) :: temp, loi346(21)
    character(len=16) :: compor(*)
!
!    - FONCTION REALISEE: LECTURE DES COMPORTEMENTS VMIS_POUTRE
!      ECRO_LINE ET ECRO_FLEJOU ET ECRO_LINE_FO ET VMIS_POUTRE_FO
!
!    - ARGUMENTS IN:
!      ICODMA : ADRESSE MATERIAU CODE
!      ITEMP  : PRESENCE OU NON D'UN CHAMP DE TEMPERATURE
!      TEMPP  : VALEUR DE LA TEMPERATURE
!    - ARGUMENTS OUT:
!      E , XNU : COEFFICIENTS ELASTIQUES
!      LOI346  : TABLEAU DE CARACTERISTIQUES
!
!     VARIABLES NECESSAIRE AUX LOIS DE COMPORTEMENT
!
    real(kind=8) :: zero
    parameter (zero = 0.0d0)
!
    real(kind=8) :: xnp, xmpy, xmey, xay, xby, xmpz, xmez, xmpx, xaz, xbz, su
    real(kind=8) :: sy, e, ep, puiss, xnu
    integer :: numloi, nbpar
    real(kind=8) :: valpar, valres(10)
    integer :: icodre(10), kpg, spt
    character(len=8) :: nopar, nomre2(4), nomre3(10), nomre4(2), fami, poum
!
!     POUR LES MESSAGES
    real(kind=8) :: valrm(2)
    character(len=15) :: valkm(5)
!
!     DATA NOMRE2/'EP','SY','SU','PUISS'/
    data nomre2 /'EP','SY','SU','PUISS'/
!     DATA NOMRE3/'NP','MEY','MPY','CAY','CBY','MEZ','MPZ','CAZ','CBZ',
    data nomre3 /'NP','MEY','MPY','CAY','CBY','MEZ','MPZ','CAZ','CBZ',&
     &            'MPX'/
!     DATA NOMRE4/'D_SIGM_EPSI','SY'/
    data nomre4 /'D_SIGM_E','SY'/
!
!
    if (compor(1) .eq. 'ELAS') then
        loi346(1) = 0
        goto 9999
    endif
!
    if (itemp .eq. 0) then
        nbpar = 0
        nopar = ' '
        valpar = 0.d0
    else
        nbpar = 1
        nopar = 'TEMP'
        valpar = temp
    endif
!
    numloi = 0
    xmpy = zero
    xmey = zero
    xay = zero
    xby = zero
    xmpz = zero
    xmez = zero
    xmpx = zero
    xaz = zero
    xbz = zero
    xnp = zero
    ep = zero
    sy = zero
    su = zero
    puiss= zero
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
!     LE TYPE D'ECROUISSAGE
    if (compor(1) .eq. 'VMIS_POU_LINE') then
        numloi = 1
        call rcvalb(fami, kpg, spt, poum, icodma,&
                    ' ', 'ECRO_LINE', nbpar, nopar, valpar,&
                    2, nomre4, valres, icodre, 1)
        ep = valres(1)
        sy = valres(2)
        if (ep .ge. e) then
            valkm(1) = 'EP'
            valkm(2) = 'E'
            valkm(3) = 'VMIS_POU_LINE'
            valkm(4) = 'ECRO_LINE'
            valrm(1) = ep
            valrm(2) = e
            call utmess('F', 'ALGORITH8_80', nk=4, valk=valkm, nr=2,&
                        valr=valrm)
        endif
!
    else if (compor(1) .eq. 'VMIS_POU_FLEJOU') then
        numloi = 2
        call rcvalb(fami, kpg, spt, poum, icodma,&
                    ' ', 'ECRO_FLEJOU', nbpar, nopar, valpar,&
                    4, nomre2, valres, icodre, 1)
        ep = valres(1)
        sy = valres(2)
        su = valres(3)
        puiss = valres(4)
!
        if (sy .ge. su) then
            valkm(1) = 'SY'
            valkm(2) = 'SU'
            valkm(3) = 'VMIS_POU_FLEJOU'
            valkm(4) = 'ECRO_FLEJOU'
            valrm(1) = sy
            valrm(2) = su
            call utmess('F', 'ALGORITH8_80', nk=4, valk=valkm, nr=2,&
                        valr=valrm)
        endif
        if (ep .ge. e) then
            valkm(1) = 'EP'
            valkm(2) = 'E'
            valkm(3) = 'VMIS_POU_FLEJOU'
            valkm(4) = 'ECRO_FLEJOU'
            valrm(1) = ep
            valrm(2) = e
            call utmess('F', 'ALGORITH8_80', nk=4, valk=valkm, nr=2,&
                        valr=valrm)
        endif
    endif
    if (numloi .eq. 0) then
        valkm(1) = 'VMIS_POU_LINE'
        valkm(2) = 'ECRO_LINE'
        valkm(3) = 'VMIS_POU_FLEJOU'
        valkm(4) = 'ECRO_FLEJOU'
        valkm(5) = 'VMIS_POUTRE'
        call utmess('F', 'ALGORITH8_81', nk=5, valk=valkm)
    endif
!
!     CALCUL DU EP : MODULE PLASTIQUE TANGENT
    ep = e*ep/(e-ep)
!
!     NP,MEY,CAY,...,MPX
    call rcvalb(fami, kpg, spt, poum, icodma,&
                ' ', 'VMIS_POUTRE', nbpar, nopar, valpar,&
                10, nomre3, valres, icodre, 1)
    xnp = valres(1)
    xmey = valres(2)
    xmpy = valres(3)
    xay = valres(4)
    xby = valres(5)
    xmez = valres(6)
    xmpz = valres(7)
    xaz = valres(8)
    xbz = valres(9)
    xmpx = valres(10)
!
    if (xmey .gt. xmpy) then
        valkm(1) = 'MEY'
        valkm(2) = 'MPY'
        if (numloi .eq. 1) then
            valkm(3) = 'VMIS_POU_LINE'
        else
            valkm(3) = 'VMIS_POU_FLEJOU'
        endif
        valkm(4) = 'VMIS_POUTRE'
        valrm(1) = xmey
        valrm(2) = xmpy
        call utmess('F', 'ALGORITH8_80', nk=4, valk=valkm, nr=2,&
                    valr=valrm)
    endif
    if (xmez .gt. xmpz) then
        valkm(1) = 'MEZ'
        valkm(2) = 'MPZ'
        if (numloi .eq. 1) then
            valkm(3) = 'VMIS_POU_LINE'
        else
            valkm(3) = 'VMIS_POU_FLEJOU'
        endif
        valkm(4) = 'VMIS_POUTRE'
        valrm(1) = xmez
        valrm(2) = xmpz
        call utmess('F', 'ALGORITH8_80', nk=4, valk=valkm, nr=2,&
                    valr=valrm)
    endif
!     VARIABLES NECESSAIRES A LA LOI DE COMPORTEMENT PLASTIQUE
!     XNP,XMPY,XMEY,XAY,XBY,XMPZ,XMEZ,XMPX,XAZ,XBZ,
!     SU, SY , E , EP , PUISS, AA,XIY,XIZ,XJX,XNU
    loi346(1 ) = numloi
    loi346(1+ 1 ) = xmpy
    loi346(1+ 2 ) = xmey
    loi346(1+ 3 ) = xay
    loi346(1+ 4 ) = xby
    loi346(1+ 5 ) = xmpz
    loi346(1+ 6 ) = xmez
    loi346(1+ 7 ) = xmpx
    loi346(1+ 8 ) = xaz
    loi346(1+ 9 ) = xbz
    loi346(1+10 ) = su
    loi346(1+11 ) = sy
    loi346(1+12 ) = e
    loi346(1+13 ) = ep
    loi346(1+14 ) = puiss
!     LOI346(1+15 ) = AA
!     LOI346(1+16 ) = XIY
!     LOI346(1+17 ) = XIZ
!     LOI346(1+18 ) = XJX
    loi346(1+19 ) = xnu
    loi346(1+20 ) = xnp
!
9999  continue
end subroutine
