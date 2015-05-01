subroutine ionfixe_3d(alfeq, sfeq, csh, csheff, temp,&
                      nasol, ssol, alsol, alpal, cash)
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
! ======================================================================
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!      provient de rsi_3d : 
!     calcul des quantités d'ions fixés dans les csh à l'équilibre
!=====================================================================
    implicit none
    real(kind=8) :: alfeq
    real(kind=8) :: sfeq
    real(kind=8) :: csh
    real(kind=8) :: csheff
    real(kind=8) :: temp
    real(kind=8) :: nasol
    real(kind=8) :: ssol
    real(kind=8) :: alsol
    real(kind=8) :: alpal
    real(kind=8) :: cash
!     les alfeq contienne les cash de façon a nnuler la vitesse
!     liberation si alf=cash quand il ny aen pls en solution
    alfeq=csheff*(1.d0-dexp(-alsol/alpal))+cash
    sfeq=2.067d0*csh*ssol*(temp**0.2124d0)*(nasol**0.2004d0)
!      print*, 'sfeq', sfeq, 'nasol', nasol      
end subroutine
