subroutine hplass(nmnbn, nmplas, nmdpla, nmddpl, bend,&
                  hplas)
!
    implicit none
!
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     CALCUL LA MATRICE HESSIENNE DU CRITERE DE PLASTICITE
!
! IN  NMNBN : MOMENT DE RAPPEL
! IN  NMPLAS : MOMENTS LIMITES DE PLASTICITE
! IN  NMDPLA : DERIVEES DES MOMENTS LIMITES DE PLASTICITE
! IN  NMDDPL : DERIVEES SECONDES DES MOMENTS LIMITES DE PLASTICITE
! IN  BEND : SIGNE DE LA FLEXION (1 POSITIVE, 2 NEGATIVE)
!
! OUT HPLAS : MATRICE HESSIENNE DU CRITERE DE PLASTICITE
!
    include 'asterfort/r8inir.h'
    integer :: bend
!
    real(kind=8) :: hplas(6, *), nmnbn(6), nmplas(2, 3)
    real(kind=8) :: nmdpla(2, 2), nmddpl(2, 2)
!
    call r8inir(6*6, 0.d0, hplas, 1)
!
    hplas(1,1) = nmddpl(bend,1)*(nmnbn(5)-nmplas(bend,2))
    hplas(2,2) = nmddpl(bend,2)*(nmnbn(4)-nmplas(bend,1))
    hplas(2,1) = -nmdpla(bend,1)*nmdpla(bend,2)
    hplas(1,2) = hplas(2,1)
    hplas(5,1) = nmdpla(bend,1)
    hplas(1,5) = nmdpla(bend,1)
    hplas(4,2) = nmdpla(bend,2)
    hplas(2,4) = nmdpla(bend,2)
    hplas(4,5) = -1.d0
    hplas(5,4) = -1.d0
    hplas(6,6) = 2.d0
!
end subroutine
