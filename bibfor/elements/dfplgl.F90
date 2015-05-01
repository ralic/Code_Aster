subroutine dfplgl(nmnbn, nmplas, nmdpla, bend, dfpl)
!
    implicit none
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
!     CALCUL LE GRADIENT DU CRITERE DE PLASICITE
!
! IN  NMNBN : FORCE - BACKFORCE
! IN  NMPLAS : MOMENTS LIMITES DE PLASTICITE
! IN  NMDPLA : DERIVEES DES MOMENTS LIMITES DE PLASTICITE
! IN  BEND : FLEXION POSITIVE (1) OU NEGATIVE (2)
!
! OUT DFPL : GRADIENT DU CRITERE DE PLASICITE
!
!
    integer :: bend
!
    real(kind=8) :: dfpl(*), nmnbn(6), nmplas(2, 3), nmdpla(2, 2)
!
    dfpl(4) = -(nmnbn(5)-nmplas(bend,2))
    dfpl(5) = -(nmnbn(4)-nmplas(bend,1))
    dfpl(6) = 2.d0*nmnbn(6)
    dfpl(1) = -nmdpla(bend,1)*dfpl(4)
    dfpl(2) = -nmdpla(bend,2)*dfpl(5)
    dfpl(3) = 0.d0*nmnbn(6)
!
end subroutine
