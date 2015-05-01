subroutine kcoude(nbrddl, poids, b, c, k)
    implicit none
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
! ......................................................................
!
!    - FONCTION REALISEE:  CALCUL DE LA MATRICES K
!
!    - ARGUMENTS :
!         ENTREE :     NBRDDL  : NBRE DES DDL
!                      POIDS   : PRODUITS DES DIFFERENTS POIDS
!                                D'INTEGRATION ET DU JACOBIEN
!                      B       : MATRICE DE DEFORMATION
!                      C       : MATRICE DE COMPORTEMENT
!         SORTIE :     K       : MATRICE DE RIGIDITE
!
! ......................................................................
!
    integer :: i, j, nbrddl
!JMP      PARAMETER (NBRDDL=63)
    real(kind=8) :: b(4, nbrddl), c(4, 4), k(nbrddl, nbrddl), poids
    real(kind=8) :: c11b1i, c12b1i, c13b1i, c21b2i, c31b3i, c32b3i, c22b2i
    real(kind=8) :: c23b2i
    real(kind=8) :: c33b3i, c44b4i, cb1j, cb2j, cb3j
!
!
! CALCUL DE LA MATRICE DE RIGIDITE K
!
!
    do 10 i = 1, nbrddl
        c11b1i=c(1,1)* b(1,i)
        c12b1i=c(1,2)* b(1,i)
        c13b1i=c(1,3)* b(1,i)
        c21b2i=c(2,1)* b(2,i)
        c31b3i=c(3,1)* b(3,i)
        c32b3i=c(3,2)* b(3,i)
        c22b2i=c(2,2)* b(2,i)
        c23b2i=c(2,3)* b(2,i)
        c33b3i=c(3,3)* b(3,i)
        c44b4i=c(4,4)* b(4,i)
        cb1j=c11b1i+c21b2i+c31b3i
        cb2j=c22b2i+c12b1i+c32b3i
        cb3j=c13b1i+c23b2i+c33b3i
        do 20 j = 1, i
            k(i,j) = k(i,j) +( cb1j*b(1,j)+cb2j*b(2,j)+cb3j*b(3,j)+ c44b4i*b(4,j) ) *poids
            k(j,i) = k(i,j)
20      continue
10  end do
!
! FIN DU CALCUL DE LA MATRICE DE RIGIDITE
!
!
!
end subroutine
