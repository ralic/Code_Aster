subroutine cale(ndim, fd, id, fdm, fdmt,&
                prodf, edpn1)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "asterfort/lctr2m.h"
#include "asterfort/matinv.h"
#include "asterfort/pmat.h"
#include "asterfort/r8inir.h"
    integer :: ndim, i, j
    real(kind=8) :: fd(3, 3), det
!
! ----------------------------------------------------------------
!   NDIM    : DIMENSION DE L'ESPACE
!   FD      : TENSEUR DE DEFORMATION ENTRE CONFIGURATION T- ET T+
!   ID     :  MATRICE IDENTITE DE DIMENSION 3
!
!   SORTIE:
!   FDM     : INVERSE DU TENSEUR DEFORMATION ENTRE CONFIG T- ET T+
!   FDMT    : TRANSPOSEE DE INVERSE DU TENSEUR DE DEFORMATION
!             ENTRE CONFIGURATION T- ET T+
!   PRODF   : TENSEUR PRODUIT ENTRE FDMT ET FDM
!   EDPN1   :
!
!-----------------------------------------------------------------
!
!       CALCUL DE EDPN1
!
!-----------------------------------------------------------------
    real(kind=8) :: id(3, 3), fdm(3, 3)
    real(kind=8) :: fdmt(3, 3), prodf(3, 3)
    real(kind=8) :: edpn1(3, 3)
!
!----------------------INTIALISATION DES MATRICES ----------------
    call r8inir(9, 0.d0, fdm, 1)
    call r8inir(9, 0.d0, fdmt, 1)
    call r8inir(9, 0.d0, prodf, 1)
    call r8inir(9, 0.d0, edpn1, 1)
!
!--------------------------------CALCUL DE e_(n+1)----------------
!
!      CALCUL DE L'INVERSE DE FD = FDM : SUBROUTINE INVERSE
    call matinv('S', 3, fd, fdm, det)
!
!      CALCUL DE LA MATRICE TRANSPOSEE DE L'INVERSE DE FD :
!      SUBROUTINE TRANSPOSEE
    call lctr2m(3, fdm, fdmt)
!       DO 70 I = 1,3
!       DO 80 J = 1,3
!        FDMT(I,J) = FDM(J,I)
! 80     CONTINUE
! 70    CONTINUE
!
    call pmat(3, fdmt, fdm, prodf)
!
!      CALCUL DE e_(N+1)
    do 75 i = 1, 3
        do 85 j = 1, 3
            edpn1(i,j) = 0.5d0*(id(i,j)-prodf(i,j))
85      continue
75  continue
end subroutine
