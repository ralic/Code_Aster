subroutine dxefro(ne, t2iu, edgle, edglc)
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterfort/utbtab.h"
#include "asterfort/pmavec.h"
    integer, intent(in) :: ne
    real(kind=8), intent(in) :: t2iu(2, 2)
    real(kind=8), intent(in) :: edgle(*)
    real(kind=8), intent(out) :: edglc(*)
!     ------------------------------------------------------------------
!     PASSAGE DES EFFORTS OU DEFORMATIONS GENERALISES DU REPERE
!     INTRINSEQUE DE L'ELEMENT AU REPERE LOCAL DE LA COQUE
!     ------------------------------------------------------------------
!     IN  NE    I      NOMBRE DE POINTS A TRAITER
!     IN  T2IU  R 2,2  MATRICE DE PASSAGE INTRINSEQUE - UTILISATEUR
!     IN  EDGLE R  8   NXX NYY NXY MXX MYY MXY QX QY
!     OUT EDGLC R  8   NXX NYY NXY MXX MYY MXY QX QY
!  OU IN  EDGLE R  8   EXX EYY EXY KXX KYY KXY GAX GAY
!     OUT EDGLE R  8   EXX EYY EXY KXX KYY KXY GAX GAY
!     
!     ATTENTION : LES DEFORMATIONS GENERALISEES SONT ATTENDUES SOUS LA FORME
!     EXX EYY EXY ET NON PAS SOUS LA FORME EXX EYY 2EXY.
!
!  REMARQUE : ON PEUT APPELER CETTE ROUTINE AVEC LE MEME TABLEAU EN E/S
!     ------------------------------------------------------------------
    real(kind=8) :: nle(2,2), mle(2,2), qle(2)
    real(kind=8) :: nlc(2,2), mlc(2,2), qlc(2)
    real(kind=8) ::t2ui(2,2), xab(2,2)
    integer :: i
!
!   Transposée de t2iu pour changement de repère des efforts tranchants
    t2ui(1,1) = t2iu(1,1)
    t2ui(2,1) = t2iu(1,2)
    t2ui(1,2) = t2iu(2,1)
    t2ui(2,2) = t2iu(2,2)
!
    do i = 1, ne
        nle(1,1) = edgle(1+8*(i-1))
        nle(2,1) = edgle(3+8*(i-1))
        nle(1,2) = edgle(3+8*(i-1))
        nle(2,2) = edgle(2+8*(i-1))
!
        mle(1,1) = edgle(4+8*(i-1))
        mle(2,1) = edgle(6+8*(i-1))
        mle(1,2) = edgle(6+8*(i-1))
        mle(2,2) = edgle(5+8*(i-1))
!
        qle(1)   = edgle(7+8*(i-1))
        qle(2)   = edgle(8+8*(i-1))
!
        call utbtab('ZERO', 2, 2, nle, t2iu, xab, nlc)
        call utbtab('ZERO', 2, 2, mle, t2iu, xab, mlc)
        call pmavec('ZERO', 2, t2ui, qle, qlc)
!
        edglc(1+8*(i-1)) = nlc(1,1)
        edglc(2+8*(i-1)) = nlc(2,2)
        edglc(3+8*(i-1)) = nlc(2,1)
!
        edglc(4+8*(i-1)) = mlc(1,1)
        edglc(5+8*(i-1)) = mlc(2,2)
        edglc(6+8*(i-1)) = mlc(2,1)
!
        edglc(7+8*(i-1)) = qlc(1)
        edglc(8+8*(i-1)) = qlc(2)
    end do
end subroutine
