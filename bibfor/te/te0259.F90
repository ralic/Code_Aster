subroutine te0259(option, nomte)
! aslint: disable=
    implicit none
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterfort/matrot.h"
#include "asterfort/pogyro.h"
#include "asterfort/rcvalb.h"
#include "asterfort/upletr.h"
#include "asterfort/utmess.h"
#include "asterfort/utpalg.h"
!
    character(len=16) :: option, nomte
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!       'MECA_GYRO': CALCUL DE LA MATRICE D'AMORTISSEMENT GYROSCOPIQUE
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!       'MECA_POU_D_E'  : POUTRE DROITE D'EULER       (SECTION VARIABLE)
!       'MECA_POU_D_T'  : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
!       'MECA_POU_D_EM' : POUTRE DROITE MULTIFIBRE D EULER (SECT. CONST)
!       'MECA_POU_D_TG' : POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!       'MECA_POU_D_TGM': POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!                         MULTI-FIBRES (SECTION CONSTANTE)
!
!
    integer :: nddl, nbres, i
    parameter (nbres=3)
    real(kind=8) :: valres(nbres)
    integer :: codres(nbres)
    character(len=8) :: nompar, nomres(nbres), fami, poum
    real(kind=8) :: pgl(3, 3), klv(78), klw(78), mlv(105)
    real(kind=8) :: e, rho
    real(kind=8) :: valpar, xnu, zero
    integer :: imate, lmat, lorien, lsect
    integer :: nbpar, nc, nno, kpg, spt
!     ------------------------------------------------------------------
    data nomres/'E','RHO','NU'/
!     ------------------------------------------------------------------
    zero = 0.d0
!     ------------------------------------------------------------------
!
!     --- CARACTERISTIQUES DES ELEMENTS
!
    if (nomte .eq. 'MECA_POU_D_E' .or. nomte .eq. 'MECA_POU_D_T' .or. nomte .eq.&
        'MECA_POU_D_EM') then
        nno = 2
        nc = 6
        elseif (nomte.eq.'MECA_POU_D_TG' .or. nomte.eq.'MECA_POU_D_TGM'&
    ) then
        nno = 2
        nc = 7
    else
        call utmess('F', 'ELEMENTS2_42', sk=nomte)
    endif
!
    nddl = nc*nno
!
!     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
!
    call jevech('PMATERC', 'L', imate)
!
    nbpar = 0
    nompar = ' '
    valpar = zero
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'ELAS', nbpar, nompar, valpar,&
                nbres, nomres, valres, codres, 1)
    e = valres(1)
    rho = valres(2)
    xnu = valres(3)
!
    call jevech('PCAGNPO', 'L', lsect)
!
    call jevech('PMATUNS', 'E', lmat)
!
!     --- RECUPERATION DES ORIENTATIONS ---
!
    call jevech('PCAORIE', 'L', lorien)
!
!     --- CALCUL DE LA MATRICE GYROSCOPIQUE LOCALE ---
    call pogyro(nomte, rho, xnu, zi(imate), klv,&
                78)
!
    call matrot(zr(lorien), pgl)
!  CHANGEMENT DE BASE : LOCAL -> GLOBAL
    call utpalg(nno, 6, pgl, klv, klw)
!
    if (nomte .eq. 'MECA_POU_D_TG' .or. nomte .eq. 'MECA_POU_D_TGM') then
        do 100 i = 1, 21
            mlv(i) = klw(i)
100      continue
        do 102 i = 22, 28
            mlv(i) = 0.d0
102      continue
        do 104 i = 29, 34
            mlv(i) = klw(i-7)
104      continue
        mlv(35) = 0.d0
        do 106 i = 36, 42
            mlv(i) = klw(i-8)
106      continue
        mlv(43) = 0.d0
        do 108 i = 44, 51
            mlv(i) = klw(i-9)
108      continue
        mlv(52) = 0.d0
        do 110 i = 53, 61
            mlv(i) = klw(i-10)
110      continue
        mlv(62) = 0.d0
        do 112 i = 63, 72
            mlv(i) = klw(i-11)
112      continue
        mlv(73) = 0.d0
        do 114 i = 74, 84
            mlv(i) = klw(i-12)
114      continue
        mlv(85) = 0.d0
        do 116 i = 86, 91
            mlv(i) = klw(i-13)
116      continue
        do 118 i = 92, 105
            mlv(i) = 0.d0
118      continue
    endif
!
! CONSITUER UNE MATRICE PLEINE A PARTIR DE LA TRIANGULAIRE SUPERIEURE
!
    if (nomte .eq. 'MECA_POU_D_TG' .or. nomte .eq. 'MECA_POU_D_TGM') then
        call upletr(nddl, zr(lmat), mlv)
    else
        call upletr(nddl, zr(lmat), klw)
    endif
!
end subroutine
