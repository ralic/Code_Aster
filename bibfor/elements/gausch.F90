subroutine gausch(npgxyz, xpg, ypg, zpg, hpg)
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
!......................................................................C
!......................................................................C
!                                                                      C
! BUT: CALCUL DES POIDS ET POINTS DE GAUSS POUR DES POLYNOMES          C
!      NON HOMOGENE A TROIS VARIABLES                                  C
!                                                                      C
! ENTREES  ---> NPGXZ(I)    : NOMBRE DE POINTS DE GAUSS DANS           C
!                             LA DIRECTION "I"                         C
!                                                                      C
! SORTIES  <--- XPG,YPG,ZPG : COORDONNEES DES POINTS DE GAUSS          C
!          <--- HPG         : POIDS DES POINTS DE GAUSS                C
!                                                                      C
!......................................................................C
!......................................................................C
!
    implicit     none
    integer :: npgxyz(3)
    real(kind=8) :: xpg(1), ypg(1), zpg(1), hpg(1)
!----------------------------------------------------------------------
    integer :: npari, i, j, k, npi
    real(kind=8) :: a(4), h(4), coord(3, 4), hpgxyz(3, 4)
!----------------------------------------------------------------------
!
    do 10 i = 1, 3
!
        if (npgxyz(i) .eq. 2) then
!
            npari = 2
            a(1) = -1.d0/(sqrt(3.d0))
            a(2) = -a(1)
            h(1) = 1.d00
            h(2) = 1.d00
!
        else if (npgxyz(i).eq.3) then
!
            npari = 3
            a(1) = -sqrt(3.d0/5.d0)
            a(2) = 0.d00
            a(3) = -a(1)
            h(1) = 5.d0/9.d0
            h(2) = 8.d0/9.d0
            h(3) = h(1)
!
        else if (npgxyz(i).eq.4) then
!
            npari = 4
            a(1) = -sqrt((3.d0+2.d0*sqrt(6.d0/5.d0))/7.d0)
            a(2) = -sqrt((3.d0-2.d0*sqrt(6.d0/5.d0))/7.d0)
            a(3) = -a(2)
            a(4) = -a(1)
            h(1) = .5d0-1.d0/(6.d0*sqrt(6.d0/5.d0))
            h(2) = .5d0+1.d0/(6.d0*sqrt(6.d0/5.d0))
            h(3) = h(2)
            h(4) = h(1)
!
        endif
!
        do 20 j = 1, npari
            coord(i,j) = a(j)
            hpgxyz(i,j) = h(j)
20      continue
10  end do
    npi = 0
    do 30 i = 1, npgxyz(1)
        do 30 j = 1, npgxyz(2)
            do 30 k = 1, npgxyz(3)
                npi = npi + 1
                xpg( npi ) = coord(1,i)
                ypg( npi ) = coord(2,j)
                zpg( npi ) = coord(3,k)
                hpg( npi ) = hpgxyz(1,i)*hpgxyz(2,j)*hpgxyz(3,k)
30          continue
!
!------------------------------------------------------------
end subroutine
