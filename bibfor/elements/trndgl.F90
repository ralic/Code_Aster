subroutine trndgl(nbx, vectn, vectpt, deplg, depll,&
                  rotfic)
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
    implicit none
!
    include 'jeveux.h'
    integer :: nbx
    real(kind=8) :: vectn(9, 3), vectpt(9, 2, 3)
    real(kind=8) :: deplg(*), depll(*), t(3, 3), rotfic(*)
    integer :: i, i1, i2, ib, j
!-----------------------------------------------------------------------
!
    do 10 ib = 1, nbx
!
!     RESTITUTION DE LA MATRICE DE PASSAGE
!
        do 15 i = 1, 2
            do 20 j = 1, 3
                t(i,j)=vectpt(ib,i,j)
20          end do
15      end do
        t(3,1)=vectn (ib,1)
        t(3,2)=vectn (ib,2)
        t(3,3)=vectn (ib,3)
!
        i1=5*(ib-1)
        i2=6*(ib-1)
!
!     LES TERMES DE TRANSLATION
!
        if (ib .le. nbx-1) then
            depll(i1+1)=deplg(i2+1)
            depll(i1+2)=deplg(i2+2)
            depll(i1+3)=deplg(i2+3)
!
!     LES TERMES DE ROTATION (2 SEULEMENT)
!
            depll(i1+4)=t(1,1)*deplg(i2+4)+t(1,2)*deplg(i2+5) +t(1,3)*&
            deplg(i2+6)
!
            depll(i1+5)=t(2,1)*deplg(i2+4)+t(2,2)*deplg(i2+5) +t(2,3)*&
            deplg(i2+6)
!
            rotfic(ib)=t(3,1)*deplg(i2+4)+t(3,2)*deplg(i2+5) +t(3,3)*&
            deplg(i2+6)
        else
            depll(i1+1)=t(1,1)*deplg(i2+1)+t(1,2)*deplg(i2+2) +t(1,3)*&
            deplg(i2+3)
!
            depll(i1+2)=t(2,1)*deplg(i2+1)+t(2,2)*deplg(i2+2) +t(2,3)*&
            deplg(i2+3)
!
            rotfic(ib)=t(3,1)*deplg(i2+1)+t(3,2)*deplg(i2+2) +t(3,3)*&
            deplg(i2+3)
        endif
!
10  end do
!
end subroutine
