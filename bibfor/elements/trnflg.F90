subroutine trnflg(nbx, vectpt, vecl, vecg)
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
    implicit none
!
    integer :: nbx
    real(kind=8) :: vecl(*), vecg(*), vectpt(9, 3, 3)
!
!     CONSTRUCTION DE LA MATRICE VECG = PLGT * VECL
!
!-----------------------------------------------------------------------
    integer :: i1, ib
!-----------------------------------------------------------------------
    do 10 ib = 1, nbx
!
        i1=6*(ib-1)
!
!     LES TERMES DE FORCE
!
        if (ib .le. nbx-1) then
            vecg(i1+1)=vecl(i1+1)
            vecg(i1+2)=vecl(i1+2)
            vecg(i1+3)=vecl(i1+3)
!
!     LES TERMES DE MOMENT = TPI * MLOCAL  (  TPI = TI  )
!
            vecg(i1+4)=vectpt(ib,1,1)*vecl(i1+4)+vectpt(ib,2,1)*vecl(&
            i1+5) +vectpt(ib,3,1)*vecl(i1+6)
            vecg(i1+5)=vectpt(ib,1,2)*vecl(i1+4)+vectpt(ib,2,2)*vecl(&
            i1+5) +vectpt(ib,3,2)*vecl(i1+6)
            vecg(i1+6)=vectpt(ib,1,3)*vecl(i1+4)+vectpt(ib,2,3)*vecl(&
            i1+5) +vectpt(ib,3,3)*vecl(i1+6)
        else
            vecg(i1+1)=vectpt(ib,1,1)*vecl(i1+1)+vectpt(ib,2,1)*vecl(&
            i1+2) +vectpt(ib,3,1)*vecl(i1+3)
            vecg(i1+2)=vectpt(ib,1,2)*vecl(i1+1)+vectpt(ib,2,2)*vecl(&
            i1+2) +vectpt(ib,3,2)*vecl(i1+3)
            vecg(i1+3)=vectpt(ib,1,3)*vecl(i1+1)+vectpt(ib,2,3)*vecl(&
            i1+2) +vectpt(ib,3,3)*vecl(i1+3)
        endif
10  end do
!
end subroutine
