subroutine fmdevi(nbfonc, nbptot, sigm, dev)
    implicit none
!
#include "jeveux.h"
    integer :: nbfonc, nbptot
    real(kind=8) :: sigm(nbfonc*nbptot)
    real(kind=8) :: dev(nbfonc*nbptot)
!     ------------------------------------------------------------------
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
!     -----------------------------------------------------------------
!     NBFONC  : IN  : NOMBRE DE FONCTIONS (6 EN 3D 4 EN 2D)
!     NBPTOT  : IN  : NOMBRE DE PAS DE TEMPS DE CALCUL
!     SIGM    : IN  : VECTEUR DES CONTRAINTES EN TOUS LES PAS DE TEMPS
!     DEV     : OUT : VECTEUR DU DEVIATEUR DES CONTRAINTES
!     -----------------------------------------------------------------
!     ------------------------------------------------------------------
    real(kind=8) :: ph
    integer :: i, idec, j
!-----------------------------------------------------------------------
!
!------- CALCUL DU DEVIATEUR -------
!
    do 10 i = 1, nbptot
        idec = (i-1)*nbfonc
        ph = (sigm(idec+1)+sigm(idec+2)+sigm(idec+3))/3.d0
        do 20 j = 1, nbfonc
            dev(idec+j) = sigm(idec+j)
            if (j .le. 3) then
                dev(idec+j)=dev(idec+j)-ph
            endif
20      continue
10  end do
!
end subroutine
