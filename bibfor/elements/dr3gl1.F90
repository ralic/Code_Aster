subroutine dr3gl1(p, ag, al)
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
!--------------------------------------------------------
! ELEMENT SHB8-PS A.COMBESCURE, S.BAGUET INSA LYON 2003 /
!-------------------------------------------------------
!     ------------------------------------------------------------------
!          CHANGEMENT DE REPERE 3D  GLOBAL => LOCAL        H. BUNG 06-97
!     ------------------------------------------------------------------
!          P     : MATRICE DE PASSAGE  LOCAL => GENERAL
!                  DETERMINEE PAR LA ROUTINE DR3P
!          AG    : MATRICE DANS LE REPERE GENERAL
!          AL    : MATRICE DANS LE REPERE LOCAL
    implicit none
!---   VARIABLES GLOBALES
#include "asterfort/r8inir.h"
    real(kind=8) :: p(3, 3), ag(3, 3), al(3, 3)
!---   VARIABLES LOCALES
    integer :: l
    real(kind=8) :: ss
!
!---     ON CALCULE   (AL) = (P)T * (AG) * (P)
!      CALL ZDANUL(AL,9)
    call r8inir(9, 0.d0, al, 1)
    do 10 l = 1, 3
!---  M = 1 ---
        ss=ag(1,1)*p(1,l)+ag(1,2)*p(2,l)+ag(1,3)*p(3,l)
!   - K=1,3 -
        al(1,l)=al(1,l)+p(1,1)*ss
        al(2,l)=al(2,l)+p(1,2)*ss
        al(3,l)=al(3,l)+p(1,3)*ss
!---  M = 2 ---
        ss=ag(2,1)*p(1,l)+ag(2,2)*p(2,l)+ag(2,3)*p(3,l)
!   - K=1,3 -
        al(1,l)=al(1,l)+p(2,1)*ss
        al(2,l)=al(2,l)+p(2,2)*ss
        al(3,l)=al(3,l)+p(2,3)*ss
!---  M = 3 ---
        ss=ag(3,1)*p(1,l)+ag(3,2)*p(2,l)+ag(3,3)*p(3,l)
!   - K=1,3 -
        al(1,l)=al(1,l)+p(3,1)*ss
        al(2,l)=al(2,l)+p(3,2)*ss
        al(3,l)=al(3,l)+p(3,3)*ss
10  end do
!
end subroutine
