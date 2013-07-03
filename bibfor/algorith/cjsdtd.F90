subroutine cjsdtd(mod, q, dtddq)
    implicit none
!       ================================================================
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
!     ------------------------------------------------------------------
!     CALCUL DE LA DERIVEE DU TENSEUR TD PAR RAPPORT A Q
!     TD = DEVIATEUR( DET(Q) * INVERSE(Q) )
!     ------------------------------------------------------------------
!     IN   MOD      :  MODELISATION
!          Q        :  TENSEUR (6 COMPOSANTES)
!     OUT  DTDDQ    :  TENSEUR RESULTAT (6 COMPOSANTES)
!          DTDDQ(I,J) = D TD(I) / D Q(J)
!     ------------------------------------------------------------------
!
#include "asterfort/u2mess.h"
    integer :: ndt, ndi
    real(kind=8) :: q(6), dtddq(6, 6)
    real(kind=8) :: zero, deux, trois, quatre, rc2
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    parameter     ( zero   = 0.d0   )
    parameter     ( deux   = 2.d0   )
    parameter     ( trois  = 3.d0   )
    parameter     ( quatre = 4.d0   )
!
    character(len=8) :: mod
!
    common /tdim/   ndt, ndi
!
!-----------------------------------------------------------------------
!
!
!------------------------------
! ATTENTION A LA PRISE EN COMPTE DE COEFFICIENTS 1/DEUX ET RC2 EN
!           FACTEUR DE CERTAINS TERMES
!
!
    rc2 = sqrt(2.d0)
!
!
!
! - MODELISATION 3D:
!
    if (mod(1:2) .eq. '3D') then
!
        dtddq(1,1) = ( - q(2) - q(3) ) / trois
        dtddq(2,1) = ( - q(2) + deux*q(3) ) / trois
        dtddq(3,1) = ( deux*q(2) - q(3) ) / trois
        dtddq(4,1) = zero
        dtddq(5,1) = zero
        dtddq(6,1) = - q(6)
!
!
        dtddq(1,2) = ( - q(1) + deux*q(3) ) / trois
        dtddq(2,2) = ( - q(1) - q(3) ) / trois
        dtddq(3,2) = ( deux*q(1) - q(3) ) / trois
        dtddq(4,2) = zero
        dtddq(5,2) = - q(5)
        dtddq(6,2) = zero
!
!
        dtddq(1,3) = ( - q(1) + deux*q(2) ) / trois
        dtddq(2,3) = ( deux*q(1) - q(2) ) / trois
        dtddq(3,3) = ( - q(1) - q(2) ) / trois
        dtddq(4,3) = - q(4)
        dtddq(5,3) = zero
        dtddq(6,3) = zero
!
!
        dtddq(1,4) = deux * q(4) / trois / deux
        dtddq(2,4) = deux * q(4) / trois / deux
        dtddq(3,4) = - quatre * q(4) / trois / deux
        dtddq(4,4) = - q(3)
        dtddq(5,4) = q(6) / rc2
        dtddq(6,4) = q(5) / rc2
!
!
        dtddq(1,5) = deux * q(5) / trois / deux
        dtddq(2,5) = - quatre * q(5) / trois / deux
        dtddq(3,5) = deux * q(5) / trois / deux
        dtddq(4,5) = q(6) / rc2
        dtddq(5,5) = - q(2)
        dtddq(6,5) = q(5) / rc2
!
!
        dtddq(1,6) = - quatre * q(6) / trois / deux
        dtddq(2,6) = deux * q(6) / trois / deux
        dtddq(3,6) = deux * q(6) / trois / deux
        dtddq(4,6) = q(5) / rc2
        dtddq(5,6) = q(4) / rc2
        dtddq(6,6) = - q(1)
!
!
!
! - MODELISATION 2D : D_PLAN ET AXIS
!
        else if ( mod(1:6) .eq. 'D_PLAN' .or. mod(1:4) .eq. 'AXIS'&
    ) then
!
!
        dtddq(1,1) = ( - q(2) - q(3) ) / trois
        dtddq(2,1) = ( - q(2) + deux*q(3) ) / trois
        dtddq(3,1) = ( deux*q(2) - q(3) ) / trois
        dtddq(4,1) = zero
!
!
        dtddq(1,2) = ( - q(1) + deux*q(3) ) / trois
        dtddq(2,2) = ( - q(1) - q(3) ) / trois
        dtddq(3,2) = ( deux*q(1) - q(3) ) / trois
        dtddq(4,2) = zero
!
!
        dtddq(1,3) = ( - q(1) + deux*q(2) ) / trois
        dtddq(2,3) = ( deux*q(1) - q(2) ) / trois
        dtddq(3,3) = ( - q(1) - q(2) ) / trois
        dtddq(4,3) = - q(4)
!
!
        dtddq(1,4) = deux * q(4) / trois / deux
        dtddq(2,4) = deux * q(4) / trois / deux
        dtddq(3,4) = - quatre * q(4) / trois / deux
        dtddq(4,4) = - q(3)
!
!
!
!
!
    else if (mod(1:6) .eq. 'C_PLAN' .or. mod(1:2) .eq. '1D') then
        call u2mess('F', 'ALGORITH2_15')
    endif
!
end subroutine
