subroutine rupmat(fami, kpg, ksp, imat, vim,&
                  lgpg, e, sigd)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! =================================================================
    implicit none
#include "asterfort/rcvalb.h"
    integer :: kpg, ksp, imat, lgpg, cerr(1), i
    real(kind=8) :: e, vim(*), sigd(6), coef(1)
    character(len=*) :: fami
! =================================================================
!   IN  FAMI   :  FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!     KPG,KSP : NUMERO DU (SOUS)POINT DE GAUSS
!     IMAT    : ADRESSE DU MATERIAU CODE
!     VIM     : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
!     LGPG  : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
!           CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
!   OUT E    :  MODULE D YOUNG DEGRADE PAR UN COEF DONNE MATERIAU
!     SIGD   :  CHAMPS DE CONTRAINTES DES ELE. ENDOMMAGES
! =================================================================
    if (vim (lgpg) .lt. 0.5d0) then
        goto 999
    endif
!
    call rcvalb(fami, kpg, ksp, '+', imat,&
                ' ', 'CRIT_RUPT', 0, ' ', [0.d0],&
                1, 'COEF', coef, cerr, 1)
!
    e = e /coef(1)
!
    do 100 i = 1, 6
        sigd(i)=0.d0
100  end do
!
999  continue
end subroutine
