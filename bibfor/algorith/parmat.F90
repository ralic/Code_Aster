subroutine parmat(nbm, dt, amor, puls, pulsd,&
                  s0, z0, sr0, za1, za2,&
                  za3)
    implicit none
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! ======================================================================
!-----------------------------------------------------------------------
! DESCRIPTION : CALCUL DES PARAMETRES (ALGO METHODE INTEGRALE)
! -----------
!               APPELANTS : CALCMI, CALTRA
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    integer :: nbm
    real(kind=8) :: dt, amor(*), puls(*), pulsd(*)
    complex(kind=8) :: s0(*), z0(*), sr0(*), za1(*), za2(*), za3(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: i
    real(kind=8) :: ksi0
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC   DCMPLX, EXP, SQRT
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    do 10 i = 1, nbm
!
        ksi0 = amor(i)/(2.0d0*puls(i))
        if (ksi0 .ge. 1.0d0) then
            ksi0 = 0.99d0
        else if (ksi0.le.-1.0d0) then
            ksi0 = -0.99d0
        endif
        pulsd(i) = puls(i) * sqrt( 1.0d0 - ksi0*ksi0 )
        s0(i) = dcmplx( -ksi0*puls(i), pulsd(i) )
        sr0(i) = s0(i) * dt
        z0(i) = exp( sr0(i) )
!
        za1(i) = (z0(i)-1.0d0) / s0(i)
        za2(i) = (1.0d0/sr0(i)) - (1.0d0/(z0(i)-1.0d0))
        za3(i) = 1.0d0 - za2(i)
!
10  end do
!
! --- FIN DE PARMAT.
end subroutine
