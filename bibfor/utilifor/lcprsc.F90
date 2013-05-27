subroutine lcprsc(x, y, p)
    implicit none
!       ----------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!       ----------------------------------------------------------------
!       PRODUIT SCALAIRE DE 2 VECTEURS P = <X Y>
!       UTILISABLE COMME PRODUIT TENSORIEL CONTRACTE 2 FOIS A CONDITION
!       QUE LES TENSEURS SOIT REPRESENTES SOUS FORME DE VECTEURS V :
!       V = ( V1 V2 V3 SQRT(2)*V12 SQRT(2)*V13 SQRT(2)*V23 )
!       IN  X      :  VECTEUR
!       IN  Y      :  VECTEUR
!       OUT P      :  SCALAIRE RESULTAT
!       ----------------------------------------------------------------
    integer :: n, nd
    real(kind=8) :: x(6), y(6), p
    common /tdim/   n , nd
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
    p = 0.d0
    do 1 i = 1, n
        p = p + x(i)*y(i)
 1  continue
end subroutine
