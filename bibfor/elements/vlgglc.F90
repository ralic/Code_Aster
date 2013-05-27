subroutine vlgglc(nno, nbrddl, pgl1, pgl2, pgl3,&
                  pgl4, v, code, p, vtemp)
    implicit none
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! PASSAGE D'UN VECTEUR V DU REPERE GLOBAL AU REPERE LOCAL
! OU INVERSEMENT. ON AGIT UNIQUEMENT SUR LES DDL DE POUTRE,
! LES DDL DE COQUE RESTENT INCHANGES.***ELEMENT COURBE***
!
    include 'asterfort/u2mesk.h'
    integer :: i, j, l, nno, nbrddl, m
!JMP      PARAMETER          (NBRDDL=63)
    real(kind=8) :: v(nbrddl), p(nbrddl, nbrddl)
    real(kind=8) :: pgl1(3, 3), pgl2(3, 3), pgl3(3, 3), pgl4(3, 3)
    real(kind=8) :: vtemp(nbrddl)
    character(len=2) :: code
!  ENTREE :NNO  = NBRE DE NOEUDS
!          PGL  = MATRICE DE PASSAGE
!          CODE = GL POUR UN PASSAGE GLOBAL -> LOCAL
!                 LG POUR UN PASSAGE LOCAL  -> GLOBAL
! ENTREE-SORTIE : V
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
!
    l=2
    m=(l-1)*nbrddl/nno
    do 41, i=1,3
    do 51, j=1,3
    p(m+i,m+j)=pgl2(i,j)
    p(m+3+i,m+3+j)=pgl2(i,j)
51  continue
41  continue
!
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
! INITIALISATION A ZERO DU VECTEUR VTEMP
!
    do 60, i=1,nbrddl
    vtemp(i) = 0.d0
    60 end do
!
!  CAS D'UN PASSAGE LOCAL -> GLOBAL
!
    if (code .eq. 'LG') then
!
! CALCUL DE VTEMP = PRODUIT (TRANSPOSEE P) * V
!
        do 70, i=1,nbrddl
        do 90, l=1,nbrddl
        vtemp(i)=vtemp(i)+p(l,i)*v(l)
90      continue
        70     end do
!
    else if (code.eq.'GL') then
!
! CALCUL DE VTEMP = P * V
!
        do 100, i=1,nbrddl
        do 110, l=1,nbrddl
        vtemp(i)=vtemp(i)+p(i,l)*v(l)
110      continue
100      continue
!
    else
        call u2mesk('F', 'ELEMENTS4_58', 1, code)
    endif
!
! STOCKAGE DE VTEMP DANS V
!
    do 120, i=1,nbrddl
    v(i) = vtemp(i)
    120 end do
!
end subroutine
