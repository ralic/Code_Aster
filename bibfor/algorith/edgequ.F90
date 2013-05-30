function edgequ(ndimsi, tens, ani)
!
    implicit none
    real(kind=8) :: edgequ
!
!
    include 'asterc/r8prem.h'
    include 'asterfort/u2mess.h'
    integer :: ndimsi
    real(kind=8) :: tens(ndimsi), ani(6, 6)
!
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
!
! ----------------------------------------------------------------------
!     MODELE VISCOPLASTIQUE SANS SEUIL DE EDGAR
!     CALCUL DU TENSEUR EQUIVALENT AU SENS DE HILL
!     EQUI = DSQRT(TENS(I)*ANI(I,J)*TENS(J))
! IN  DIM  : DIMENSION DE L'ESPACE
! IN  TENS : TENSEUR
! IN  ANI  : MATRICE D ANISOTROPIE DE HILL
!
! OUT EQUI    : TENSEUR EQUIVALENT
! ----------------------------------------------------------------------
!
    integer :: i, j
    real(kind=8) :: equi, pdtsca(6)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    data     pdtsca/1.d0,1.d0,1.d0,2.d0,2.d0,2.d0/
!
    equi = 0.d0
    do 5 i = 1, ndimsi
        do 10 j = 1, ndimsi
            equi=equi+pdtsca(i)*tens(i)*ani(i,j)*pdtsca(j)*tens(j)
10      continue
 5  end do
!
    if (equi .gt. 0.d0) then
        equi = sqrt(equi)
    else
        if (abs(equi) .lt. r8prem()) then
            equi = 0.d0
        else
            call u2mess('F', 'COMPOR1_71')
        endif
    endif
!
    edgequ = equi
!
end function
