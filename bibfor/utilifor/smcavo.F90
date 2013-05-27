subroutine smcavo(x, ind, nbhist, trc)
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
! RECHERCHE DES PLUS PROCHES VOISINS DE X PARMI LES POINTS CONSTRUITS  C
!......................................................................C
    implicit none
    real(kind=8) :: sdx, trc((3*nbhist), 5), x(5), d(6), dx(5)
    integer :: ind(6), nbhist, invois, i, j, n
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    do 10 i = 1, 6
        ind(i)=0
        d(i)=10.0d25
10  end do
    do 70 n = 1, (3*nbhist)
        do 20 i = 1, 3
            dx(i)=(x(i)-trc(n,i))**2
20      continue
        do 30 i = 4, 5
            dx(i)=((x(i)-trc(n,i))/x(i))**2
30      continue
        sdx=0.d0
        do 40 i = 1, 5
            sdx=sdx+dx(i)
40      continue
        if (sdx .ge. d(6)) then
            goto 70
        else
            do 50 i = 6, 1, -1
                if (sdx .lt. d(i)) then
                    invois=i
                endif
50          continue
            if (invois .eq. 6) then
                d(6)=sdx
                ind(6)=n
            else
                do 60 j = 6, invois+1, -1
                    d(j)=d(j-1)
                    ind(j)=ind(j-1)
60              continue
                d(invois)=sdx
                ind(invois)=n
            endif
        endif
70  end do
end subroutine
