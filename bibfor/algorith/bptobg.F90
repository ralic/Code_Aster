subroutine bptobg(m, n, p)
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
    implicit none
    real(kind=8) :: m(6), n(6), p(3, 3)
! ----------------------------------------------------------------------
! CHANGEMENT DE BASE DE LA BASE PROPRE A LA BASE INITIALE DONT LES
!    VECTEURS PROPRES SONT RANGES DANS P POUR DES MATRICES SYMETRIQUES
!
! IN  M       : MATRICE DANS BP
! IN  P       : MATRICE DE PASSAGE B->BP
! OUT N       : MATRICE M DANS B
! ----------------------------------------------------------------------
    integer :: i, j, t(3, 3)
    real(kind=8) :: temp
    n(1)=0.d0
    n(2)=0.d0
    n(3)=0.d0
    n(4)=0.d0
    n(5)=0.d0
    n(6)=0.d0
    t(1,1)=1
    t(1,2)=4
    t(1,3)=5
    t(2,1)=4
    t(2,2)=2
    t(2,3)=6
    t(3,1)=5
    t(3,2)=6
    t(3,3)=3
    do 10 i = 1, 3
        do 11 j = 1, 3
            temp=m(t(i,j))
            n(1)=n(1)+p(1,i)*p(1,j)*temp
            n(2)=n(2)+p(2,i)*p(2,j)*temp
            n(3)=n(3)+p(3,i)*p(3,j)*temp
            n(4)=n(4)+p(1,i)*p(2,j)*temp
            n(5)=n(5)+p(1,i)*p(3,j)*temp
            n(6)=n(6)+p(2,i)*p(3,j)*temp
11      continue
10  end do
end subroutine
