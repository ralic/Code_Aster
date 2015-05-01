subroutine amppr(amat, nb1, nb2, bmat, n1,&
                 n2, i, j)
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
    implicit none
!
!***********************************************************************
!    P. RICHARD     DATE 12/03/91
!-----------------------------------------------------------------------
!  BUT:  AJOUTER UNE MATRICE PLEINE REELLE A UNE MATRICEPLEINE REELLE
!            A UNE MATRICE PLEINE REELLE
!-----------------------------------------------------------------------
!
! AMAT     /M/: MATRICE RECEPTRICE
! NB1      /I/: NB DE LIGNES DE LA MATRICE RECEPTRICE
! NB2      /I/: NB DE COLONNES DE LA MATRICE RECEPTRICE
! BMAT     /M/: MATRICE PLEINE A AJOUTER
! N1       /I/: NB DE LIGNE DE LA MATRICE A AJOUTER
! N2       /I/: NB DE COLONNE DE LA MATRICE A AJOUTER
! I        /I/: INDICE DU PREMIER TERME DANS RECEPTRICE
! J        /I/: INDICE DE COLONNE TERME  DANS RECEPTRICE
!
!-----------------------------------------------------------------------
!
    real(kind=8) :: amat(nb1, nb2), bmat(n1, n2)
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ideb, ifin, ii, iideb, iifin, j
    integer :: jdeb, jfin, jj, jjdeb, jjfin, n1, n2
    integer :: nb1, nb2
!-----------------------------------------------------------------------
    jdeb=j
    jfin=min(j+n2-1,nb2)
    if (jfin .lt. jdeb) goto 9999
    jjdeb=jdeb-j+1
    jjfin=jfin-j+1
!
    ideb=i
    ifin=min(i+n1-1,nb1)
    if (ifin .lt. ideb) goto 9999
    iideb=ideb-i+1
    iifin=ifin-i+1
!
    do 10 ii = iideb, iifin
        do 20 jj = jjdeb, jjfin
            amat(i+ii-1,j+jj-1)=amat(i+ii-1,j+jj-1)+bmat(ii,jj)
20      continue
10  end do
!
9999  continue
end subroutine
