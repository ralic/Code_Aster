subroutine telamb(angmas, lambt, tlambt, aniso, ndim)
    implicit none
! ======================================================================
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
! --- DEFINITION DU TENSEUR DE LAMBT DANS LE REPERE GLOBAL -------------
! --- CAS ISOTROPE OU ISOTROPE TRANSVERSE ------------------------------
! ======================================================================
#include "asterc/r8pi.h"
    integer :: aniso, ndim
    real(kind=8) :: angmas(3), lambt(4)
    real(kind=8) :: tlambt(ndim, ndim),lambti(3,3)
    real(kind=8) :: passag(3,3),work(3,3),tk2(3,3)
! ======================================================================
! --- INITIALISATION DU TENSEUR ----------------------------------------
! ======================================================================
    call matini(3, 3, 0.d0, work)
    call matini(3, 3, 0.d0, tk2)
    call matini(ndim, ndim, 0.d0, tlambt)
    call matini(3, 3, 0.d0, lambti)
    if (aniso .eq. 0) then
! ======================================================================
! --- CAS ISOTROPE -----------------------------------------------------
! --- CALCUL DU TENSEUR LAMBT DANS LE REPERE GLOBAL --------------------
! ======================================================================
        tlambt(1,1)=lambt(1)
        tlambt(2,2)=lambt(1)
        if(ndim.eq.3)then
         tlambt(3,3)=lambt(1)
        endif
    else if (aniso.eq.1) then
! ======================================================================
! --- CAS ISOTROPE TRANSVERSE 3D------------------------------------------
! --- CALCUL DU TENSEUR DE CONDUCTIVITE DANS LE REPERE GLOBAL ----------
! ======================================================================
        lambti(1,1)=lambt(2)
        lambti(2,2)=lambt(2)
        lambti(3,3)=lambt(3)
! Recupération de la matrice de passage du local au global
        if(ndim.eq.3)then
         call matrot(angmas,passag)
         call utbtab('ZERO', 3, 3, lambti, passag,work, tk2)
         tlambt = tk2
        endif
    else if (aniso.eq.2) then
! ======================================================================
! --- CAS ORTHOTROPE 2D ------------------------------------------
! --- CALCUL DU TENSEUR DE CONDUCTIVITE DANS LE REPERE GLOBAL ----------
! ======================================================================
        lambti(1,1)=lambt(2)
        lambti(2,2)=lambt(4)
        call matrot(angmas,passag)
        call utbtab('ZERO', 3, 3, lambti, passag,work, tk2)
        tlambt(1,1)= tk2(1,1)
        tlambt(2,2)= tk2(2,2)
        tlambt(1,2)= tk2(1,2)
        tlambt(2,1)= tk2(2,1)
!
    endif
!
end subroutine
