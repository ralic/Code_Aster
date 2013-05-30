subroutine parmtr(np4, nfour, nbm, ttrans, amor,&
                  puls, pulsd, s0, z0, omegaf,&
                  za4, za5)
    implicit none
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
! ======================================================================
!-----------------------------------------------------------------------
! DESCRIPTION : CALCUL DES PARAMETRES (ALGO METHODE INTEGRALE)
! -----------   POUR PASSAGE DU TRANSITOIRE
!
!               APPELANT : CALTRA
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    include 'asterfort/u2mess.h'
    integer :: np4, nfour, nbm
    real(kind=8) :: ttrans, amor(*), puls(*), pulsd(*)
    complex(kind=8) :: s0(*), z0(*)
    real(kind=8) :: omegaf(*)
    complex(kind=8) :: za4(np4, *), za5(np4, *)
!
! VARIABLES LOCALES
! -----------------
    integer :: i, j, n2m1
    real(kind=8) :: ksi0, zero
    complex(kind=8) :: zomega
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC   DCMPLX, EXP, SQRT
!
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    zero = 0.0d0
    n2m1 = (nfour/2) - 1
!
    do 10 i = 1, nbm
        ksi0 = amor(i)/(2.0d0*puls(i))
        if (ksi0 .ge. 1.0d0) then
            ksi0 = 0.99d0
            call u2mess('A', 'ALGORITH9_74')
        else if (ksi0.le.-1.0d0) then
            ksi0 = -0.99d0
            call u2mess('A', 'ALGORITH9_75')
        endif
        pulsd(i) = puls(i) * sqrt( 1.0d0 - ksi0*ksi0 )
        s0(i) = dcmplx( -ksi0*puls(i), pulsd(i) )
        z0(i) = exp( s0(i)*ttrans )
!
        do 11 j = 1, n2m1
            zomega = dcmplx( zero, omegaf(j) )
            za4(j,i) = (z0(i) - exp(zomega*ttrans)) / (s0(i) - zomega)
            za5(j,i) = (z0(i) - exp(-zomega*ttrans)) / (s0(i) + zomega)
11      continue
!
10  end do
!
! --- FIN DE PARMTR.
end subroutine
