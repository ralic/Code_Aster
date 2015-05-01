subroutine dfplas(mom, plamom, df)
    implicit none
    real(kind=8) :: df(3), mom(3), plamom(3)
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
! ======================================================================
!-----------------------------------------------------------------------
!     BUT : CALCUL DU GRADIENT DE LA FONCTION SEUIL
!
! IN  R  MOM     : MOMENT - MOMENT DE RAPPEL (M-BACKM)
!     R  PLASMO  : MOMENTS LIMITES ELASTIQUES DANS LE REPERE ORTHOTROPE
!
! OUT R  DF      : GRADIENT DE LA FONCTION SEUIL
!-----------------------------------------------------------------------
!
    df(1)=-(mom(2)-plamom(2))
    df(2)=-(mom(1)-plamom(1))
    df(3)=2.d0*mom(3)
!
end subroutine
