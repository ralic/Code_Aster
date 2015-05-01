subroutine dfuuss(nmnbn, nmplas, nmdpla, nmprox, bend,&
                  dfuu)
!
    implicit none
!
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!======================================================================
!
!     CALUL DES DIRECTIONS DE L ECOULEMENT DES DEFORMATIONS PLASTIQUES
!
! IN  NMNBN : FORCE - BACKFORCE
! IN  NMPLAS : MOMENTS LIMITES DE PLASTICITE
! IN  NMDPLA : DERIVEES DES MOMENTS LIMITES DE PLASTICITE
! IN  NMPROX : NMPROX > 0 : NBN DANS ZONE DE CRITIQUE
! IN  BEND : FLEXION POSITIVE (1) OU NEGATIVE (2)
!
! OUT DFUU : DIRECTIONS DE L ECOULEMENT DES DEFORMATIONS PLASTIQUES
!
#include "asterfort/dfplgl.h"
    integer :: bend, nmprox(2)
!
    real(kind=8) :: dfuu(*), nmnbn(6), nmplas(2, 3), nmdpla(2, 2)
!
    if (nmprox(bend) .gt. 0) then
        if (bend .eq. 1) then
            dfuu(1) = -nmdpla(bend,1)
            dfuu(2) = -nmdpla(bend,2)
            dfuu(4) = 1.d0
            dfuu(5) = 1.d0
        else
            dfuu(1) = nmdpla(bend,1)
            dfuu(2) = nmdpla(bend,2)
            dfuu(4) = -1.d0
            dfuu(5) = -1.d0
        endif
!
        dfuu(3) = 0.d0
        dfuu(6) = 0.d0
    else
!
!     CALCUL LE GRADIENT DU CRITERE DE PLASICITE
        call dfplgl(nmnbn, nmplas, nmdpla, bend, dfuu)
!
    endif
!
end subroutine
