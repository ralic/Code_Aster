subroutine klgcou(nno, nbrddl, pgl1, pgl2, pgl3,&
                  pgl4, k)
! aslint: disable=W1306
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
!
! PASSAGE DE LA MATRICE K DU REPERE LOCAL AU REPERE GLOBAL
! POUR LES DDL DE POUTRE, ON LAISSE LES DDL DE COQUE DANS
! LE REPERE LOCAL.  ELEMENT COURBE
! ENTREE        : NNO = NBRE DE NOEUDS
!                 PGLI = MATRICES DE PASSAGE NOEUD I
! ENTREE-SORTIE : K   = MATRICE DE RIGIDITE
!
    integer :: i, j, l, nno, nbrddl, m
!JMP      PARAMETER          (NBRDDL=63)
    real(kind=8) :: k(nbrddl, nbrddl), p(nbrddl, nbrddl)
    real(kind=8) :: pgl1(3, 3), pgl2(3, 3), pgl3(3, 3), pgl4(3, 3)
    real(kind=8) :: ktemp(nbrddl, nbrddl)
!
!
!  INITIALISATION A L'IDENTITE DE LA MATRICE DE PASSAGE P
!
    do 10, i=1, nbrddl
    do 20 j = 1, nbrddl
        if (i .eq. j) then
            p(i,j)=1.d0
        else
            p(i,j)=0.d0
        endif
20  continue
    10 end do
!
!  REMPLISSAGE DES DE BLOC DE LA MATRICE P CORRESPONDANT AUX DDL
!  DE POUTRE (UX, UY, UZ, TETAX, TETAY, ET TETAZ) PAR LA MATRICE
!  DE PASSAGE (3*3) PGL.
!
    l=1
    m=(l-1)*nbrddl/nno
    do 40, i=1,3
    do 50, j=1,3
    p(m+i,m+j)=pgl1(i,j)
    p(m+3+i,m+3+j)=pgl1(i,j)
50  continue
40  continue
    l=2
    m=(l-1)*nbrddl/nno
    do 41, i=1,3
    do 51, j=1,3
    p(m+i,m+j)=pgl2(i,j)
    p(m+3+i,m+3+j)=pgl2(i,j)
51  continue
41  continue
    l=3
    m=(l-1)*nbrddl/nno
    do 42, i=1,3
    do 52, j=1,3
    p(m+i,m+j)=pgl3(i,j)
    p(m+3+i,m+3+j)=pgl3(i,j)
52  continue
42  continue
!
    if (nno .eq. 4) then
!
        l=4
        m=(l-1)*nbrddl/nno
        do 43 i = 1, 3
            do 53 j = 1, 3
                p(m+i,m+j)=pgl4(i,j)
                p(m+3+i,m+3+j)=pgl4(i,j)
53          continue
43      continue
!
    endif
!
!
! INITIALISATION A ZERO DE LA MATRICE KTEMP
!
    do 60, i=1,nbrddl
    do 70, j=1,nbrddl
    ktemp(i,j)=0.d0
70  continue
    60 end do
!
! CALCUL DE KTEMP = PRODUIT (TRANSPOSEE P) * K
!
    do 80, i=1,nbrddl
    do 90, j=1,nbrddl
    do 100, l=1,nbrddl
    ktemp(i,j)=ktemp(i,j)+p(l,i)*k(l,j)
100  continue
90  continue
    80 end do
!
!  INITIALISATION A ZERO DE LA MATRICE K
!
    do 110, i=1,nbrddl
    do 120, j=1,nbrddl
    k(i,j)=0.d0
120  continue
110  continue
!
! CALCUL DE K = PRODUIT KTEMP * P = (TRANSPOSEE P) * K * P
!
    do 130, i=1,nbrddl
    do 140, j=1,nbrddl
    do 150, l=1,nbrddl
    k(i,j)=k(i,j)+ktemp(i,l)*p(l,j)
150  continue
140  continue
130  continue
!
end subroutine
