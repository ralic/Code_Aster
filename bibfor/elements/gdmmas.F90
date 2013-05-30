subroutine gdmmas(kp, nno, pjacob, en, grani,&
                  rot0, mass)
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
! FONCTION: POUR UN ELEMENT DE POUTRE EN GRAND DEPLACEMENT, CALCULE LA
!           MATRICE DE MASSE EN POSITION DE REFERENCE.
!
!     IN  : KP        : NUMERO DU POINT DE GAUSS
!           NNO       : NOMBRE DE NOEUDS
!           PJACOB    : POIDS * JACOBIEN
!           EN        : FONCTIONS DE FORME
!           GRANI     : DIAGONALE DU TENSEUR D'INERTIE EN AXES LOCAUX
!                       POUR LES 3 1ERES COMPOSANTES, RHO*A POUR LA 4EME
!           ROT0      : MATRICE DE ROTATION DES AXES PRINCIPAUX D'INERT.
!                       AU POINT DE GAUSS DANS LA POSITION DE REFERENCE,
!                       PAR RAPPORT AUX AXES GENERAUX
!
!     OUT : MASS      : MATRICE DE MASSE (CUMUL DES CONTRIBUTIONS DES
!                       POINTS DE GAUSS)
! ------------------------------------------------------------------
    implicit none
    include 'asterfort/cumuma.h'
    include 'asterfort/promat.h'
    real(kind=8) :: en(3, 2), grani(4), rot0(3, 3), mass(18, 18), imas(6, 6)
    real(kind=8) :: iro(3, 3), amat1(3, 3)
!
!-----------------------------------------------------------------------
    integer :: i, j, kp, nno
    real(kind=8) :: coef, pjacob, zero
!-----------------------------------------------------------------------
    zero = 0.d0
    do 2 j = 1, 6
        do 1 i = 1, 6
            imas(i,j) = zero
 1      end do
 2  end do
!
    do 3 i = 1, 3
        imas(i,i) = grani(4)
 3  end do
!
    do 5 j = 1, 3
        do 4 i = 1, 3
            amat1(i,j) = grani(i) * rot0(j,i)
 4      end do
 5  end do
    call promat(rot0, 3, 3, 3, amat1,&
                3, 3, 3, iro)
!
    do 7 j = 1, 3
        do 6 i = 1, 3
            imas(3+i,3+j) = iro(i,j)
 6      end do
 7  end do
!
    do 52 j = 1, nno
        do 51 i = 1, nno
            coef = pjacob * en(i,kp) * en(j,kp)
            call cumuma(i, j, imas, coef, mass)
51      end do
52  end do
end subroutine
