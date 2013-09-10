subroutine dmdepv(rho, fsat, tbiot, dmdeps)

! ======================================================================
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
! --- CALCUL DES DERIVEES DES APPORTS MASSIQUES PAR RAPPORT A LA -------
! --- DEFORMATION VOLUMIQUE --------------------------------------------
! ======================================================================
    implicit none
!
    integer :: i
    real(kind=8) :: rho, fsat, tbiot(6), dmdeps(6)
    real(kind=8) :: rac2
    rac2=sqrt(2.d0)
    do 10 i = 1, 3
        dmdeps(i) = rho*tbiot(i)*fsat
10  end do
    do 20 i = 4, 6
        dmdeps(i) = rho*tbiot(i)*fsat*rac2
20  end do
! ======================================================================
end subroutine
