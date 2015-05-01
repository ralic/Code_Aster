subroutine intftr(np4, nfour, nbm, za4, za5,&
                  aa, bb, zitr)
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
! DESCRIPTION : CALCUL DE L'INTEGRALE I(N+1), TERME DE FORCAGE EXPLICITE
! -----------   DECOMPOSITION EN SERIE DE FOURIER
!
!               APPELANT : CALTRA
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    integer :: np4, nfour, nbm
    complex(kind=8) :: za4(np4, *), za5(np4, *)
    real(kind=8) :: aa(np4, *), bb(np4, *)
    complex(kind=8) :: zitr(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: i, j, n2m1
    real(kind=8) :: zero, un
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC   DCMPLX
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    n2m1 = (nfour/2) - 1
    zero = 0.0d0
    un = 1.0d0
!
    do 10 i = 1, nbm
        zitr(i) = dcmplx(zero,zero)
        do 11 j = 1, n2m1
            zitr(i) = zitr(i) + (0.5d0*aa(j,i)*(za4(j,i)+za5(j,i))) - (0.5d0*dcmplx(zero,un)*bb(j&
                      &,i)*(za4(j,i)-za5(j,i)))
11      continue
10  end do
!
! --- FIN DE INTFTR.
end subroutine
