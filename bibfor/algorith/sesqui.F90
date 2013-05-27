subroutine sesqui(mat, vect, ndim, normec)
    implicit   none
    integer :: ndim
    complex(kind=8) :: mat(*), vect(ndim), normec
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
!
!  BUT:  < NORME CARREE >
!
!   CETTE ROUTINE CALCULE LA NORME D'UNE VECTEUR COMPLEXE AU SENS DU
!   PRODUIT SCALAIRE COMPLEXE DEFINI PAR LA MATRICE COMPLEXE MAT
!
!-----------------------------------------------------------------------
!
! MAT      /I/: MATRICE COMPLEXE DEFINISSANT LE PRODUIT SCALAIRE
! VECT     /I/: VECTEUR COMPLEXE A NORMER
! NDIM     /I/: DIMENSION DU VECTEUR ET DE LA MATRICE
! NOR      /O/: NORME CARREE DU VECTEUR
!
!-----------------------------------------------------------------------
    integer :: i, j, idiag, jdiag
!-----------------------------------------------------------------------
!
    normec = dcmplx(0.d0,0.d0)
    do 10 i = 1, ndim
        idiag = i*(i-1)/2+1
        do 20 j = 1, ndim
            if (j .ge. i) then
                jdiag = j*(j-1)/2+1
                normec = normec + vect(j)*mat(jdiag+j-i)*dconjg(vect( i))
            else
                normec = normec + vect(j)*dconjg(mat(idiag+i-j))* dconjg(vect(i))
            endif
20      continue
10  continue
!
end subroutine
