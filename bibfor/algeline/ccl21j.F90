subroutine ccl21j(fronti, frontj, frn, j, l,&
                  n, n1, t1, t2)
! person_in_charge: olivier.boiteau at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! VERSION COMPLEXE DE COL21J
!     VERSION MODIFIEE POUR L' APPEL A CGEMV (PRODUITS MATRICE VECTEUR)
!     LE STOCKAGE DES COLONNES DE LA FACTORISEE EST MODIFIE
    implicit none
    integer :: n, n1, k
    complex(kind=8) :: fronti(*), frontj(*), t1(n), t2(n), frn(*)
!
    integer :: i, j, l, ll, ic1, ic2, id1, id2, jd1, jd
    ic1 = 3
    ic2 = ic1 + n
    jd1 = 0
    ll = l
    do 120 k = 1, n1
        id1 = ic1
        id2 = ic2
        do 110 i = 1, ll
            jd1 = jd1 + 1
            frontj(jd1) = frontj(jd1) - t1(k)*fronti(id1) - t2(k)* fronti(id2)
            id1 = id1 + 1
            id2 = id2 + 1
110      continue
        ll = ll - 1
        ic1 = ic1 + 1
        ic2 = ic2 + 1
        jd1 = jd1 + 2*j + k
120  end do
    jd1 = 0
    do 140 k = n1 + 1, l
        id1 = ic1
        id2 = ic2
        jd = jd1
        do 130 i = 1, ll
            jd = jd + 1
            frn(jd) = frn(jd) - t1(k)*fronti(id1)
            id1 = id1 + 1
130      continue
!
        do 131 i = 1, ll
            jd1 = jd1 + 1
            frn(jd1) = frn(jd1) - t2(k)*fronti(id2)
            id2 = id2 + 1
131      continue
        ll = ll - 1
        ic1 = ic1 + 1
        ic2 = ic2 + 1
140  end do
end subroutine
