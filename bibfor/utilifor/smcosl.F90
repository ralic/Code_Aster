subroutine smcosl(trc, ind, a, b, x,&
                  nbhist)
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
!......................................................................C
!    SYSTEME MATRICIEL POUR LE CALCUL DES COORDONNEES BARYCENTRIQUES   C
!......................................................................C
    implicit none
    real(kind=8) :: a(6, 6), b(6), trc((3*nbhist), 5), x(5), zero, un
    integer :: ind(6), nbhist, j, i
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    zero=0.d0
    un=1.d0
    do 20 j = 1, 6
        b(j)=zero
        do 10 i = 1, 6
            a(j,i)=zero
10      continue
20  end do
!
! CONSTRUCTION MATRICE A
!
    do 40 i = 1, 3
        do 30 j = 1, 6
            a(i,j)=trc(ind(j),i)
30      continue
40  end do
    do 50 j = 1, 6
        a(4,j)=trc(ind(j),4)/x(4)
        a(5,j)=trc(ind(j),5)/x(5)
        a(6,j)=un
50  end do
!
! CONSTRUCTION SECOND MEMBRE
!
    do 60 i = 1, 3
        b(i)= x(i)
60  end do
    b(4)=un
    b(5)=un
    b(6)=un
end subroutine
