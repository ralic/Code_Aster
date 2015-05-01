subroutine matran(nbm, s0, z0, omega, omegad,&
                  trans)
!
! ********************************************************************
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
! ********************************************************************
! DESCRIPTION : ALGORITHME D'INTEGRATION TEMPORELLE
! -----------    CALCUL DE LA MATRICE DE TRANSFERT
!
! ****************** DECLARATION DES VARIABLES ***********************
!
    implicit none
!
! ARGUMENTS
! ---------
    integer :: nbm
    real(kind=8) :: omega(*), omegad(*), trans(2, 2, *)
    complex(kind=8) :: s0(*), z0(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: i
!
! ****************** DEBUT DU CODE EXECUTABLE ************************
!
    do 10 i = 1, nbm
!
        trans(1,1,i) = dimag(s0(i)*z0(i))/omegad(i)
        trans(1,2,i) = dimag(omega(i)*omega(i)*dconjg(z0(i)))/omegad( i)
        trans(2,1,i) = dimag(z0(i))/omegad(i)
        trans(2,2,i) = dimag(s0(i)*dconjg(z0(i)))/omegad(i)
!
10  end do
!
end subroutine
