subroutine utvtsv(raz, n, s, v, vtsv)
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
    character(len=*) :: raz
    integer :: n
    real(kind=8) :: s(*), v(*), vtsv
!     ------------------------------------------------------------------
!     PRODUIT VT . S . V - S CARREE SYMETRIQUE - V VECTEUR
!     ------------------------------------------------------------------
!IN   K4  RAZ  'ZERO' : ON FAIT VTSV = 0.   + VT*S*V
!              'CUMU' : ON FAIT VTSV = VTSV + VT*S*V
!IN   I   N    ORDRE DE S ET V
!IN   R   S    MATRICE S           (N*(N+1)/2)
!IN   R   V    VECTEUR V           (N)
!OUT  R   VTSV PRODUIT VT . S . V
!     ------------------------------------------------------------------
    character(len=4) :: raz2
    integer :: i, ik, j, k, l, k1, k2
!-----------------------------------------------------------------------
    k1(i,j)=j*(j-1)/2+i
    k2(i,j)=i*(i-1)/2+j
!-----------------------------------------------------------------------
    raz2=raz
    if (raz2 .eq. 'ZERO') vtsv = 0.d0
!
    do 25 k = 1, n
        do 20 l = 1, n
            ik = k1(k,l)
            if (k .gt. l) ik = k2(k,l)
            vtsv = vtsv + s(ik) * v(k) * v(l)
20      continue
25  end do
end subroutine
