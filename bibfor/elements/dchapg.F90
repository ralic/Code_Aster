subroutine dchapg(sig1, sig2, npg, nbsig, decha)
    implicit none
#include "asterc/r8prem.h"
#include "asterfort/norsig.h"
    integer :: npg, nbsig
    real(kind=8) :: sig1(*), sig2(*), decha(*)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!     BUT:
!       CALCUL DE L'INDICATEUR LOCAL DE DECHARGE DECHA
!       I = (NORME(SIG2) - NORME(SIG1))/NORME(SIG2)
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   SIG1     : CONTRAINTES INSTANT +
! IN   SIG2     : CONTRAINTES INSTANT -
! IN   NPG      : NOMBRE DE POINT DE GAUSS
!
!      SORTIE :
!-------------
! OUT  DECHA    : INDICATEUR DE DECHARGE AU POINTS DE GAUSS
!
! ......................................................................
!
    integer :: igau
!
    real(kind=8) :: zero, un, norm1, norm2, zernor
!
! ----------------------------------------------------------------------
!
    zero = 0.0d0
    un = 1.0d0
    zernor = 10.0d0*r8prem()
!
    do 10 igau = 1, npg
!      CALCUL DU SECOND INVARIANT DU TENSEUR DES CONTRAINTES :
!
        norm1 = norsig(sig1(1+ (igau-1)*nbsig),nbsig)
        norm2 = norsig(sig2(1+ (igau-1)*nbsig),nbsig)
!
!     DANS LE CAS OU NORME(SIG2) = 0 :
!     SI NORME(SIG1) = 0, ON MET L'INDICATEUR A 0
!     SINON IL Y A EU DECHARGE ET ON MET L'INDICATEUR A -1 :
!
        if (norm2 .le. zernor) then
            if (norm1 .le. zernor) then
                decha(igau) = zero
            else
                decha(igau) = -un
            endif
        else
            decha(igau) = (norm2-norm1)/norm2
        endif
10  end do
!
end subroutine
