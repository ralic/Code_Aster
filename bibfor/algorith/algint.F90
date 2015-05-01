subroutine algint(nbm, vitg, vitg0, depg, depg0,&
                  zin, trans, omegad, s0)
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
! ======================================================================
!-----------------------------------------------------------------------
! DESCRIPTION : CALCUL DU VECTEUR D'ETAT A L'INSTANT N+1 PAR
! -----------   UNE METHODE INTEGRALE
!
!               APPELANTS : CALCMI, CALTRA
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    integer :: nbm
    real(kind=8) :: vitg(*), vitg0(*), depg(*), depg0(*)
    complex(kind=8) :: zin(*)
    real(kind=8) :: trans(2, 2, *), omegad(*)
    complex(kind=8) :: s0(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: i
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC   DIMAG
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    do 10 i = 1, nbm
        vitg(i) = trans(1,1,i)*vitg0(i) + trans(1,2,i)*depg0(i) + dimag((s0(i)/omegad(i))*zin(i))
        depg(i) = trans(2,1,i)*vitg0(i) + trans(2,2,i)*depg0(i) + dimag(zin(i)/omegad(i))
10  end do
!
! --- FIN DE ALGINT.
end subroutine
