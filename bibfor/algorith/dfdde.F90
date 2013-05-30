subroutine dfdde(eps, endo, ndim, lambda, mu,&
                 dfde)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
    include 'asterfort/diago3.h'
    include 'asterfort/r8inir.h'
    integer :: ndim
    real(kind=8) :: eps(6), lambda, mu
    real(kind=8) :: dfde(6), endo
!
! ----------------------------------------------------------------------
!     LOI DE COMPORTEMENT ENDO_ORTH_BETON
!     CALCUL DE LA DERIVEE DE LA FORCE THERMODYNAMIQUE(ENDO COMPRESSION)
!     PAR RAPPORT A LA DEFORMATION:DFD/DEPS
!
!     FD=(1-ENDO)(LAMBDA/2*(TR(E).H(-TR(E)))**2+MU*TR(E-**2))-ECROD*ENDO
!     IN  NDIM      : DIMENSION 3(3D) OU 2(2D)
!     IN  EPS        : DEFORMATION
!     IN  ENDO     : ENDOMMAGEMENT DE COMPRESSION
!     IN  LAMBDA   : /
!     IN  MU       : / COEFFICIENTS DE LAME
!     OUT DFDE      : DFD/DEPS
! ----------------------------------------------------------------------
!
    real(kind=8) :: treps, rtemp, rac2
    real(kind=8) :: tu(6), vpe(3)
    real(kind=8) :: valeps(3), veceps(3, 3), phid
    integer :: i, j, k, t(3, 3)
!
    t(1,1)=1
    t(1,2)=4
    t(1,3)=5
    t(2,1)=4
    t(2,2)=2
    t(2,3)=6
    t(3,1)=5
    t(3,2)=6
    t(3,3)=3
!
    rac2=sqrt(2.d0)
!
!
    phid = 2.d0*(1.d0-endo)
!
    call r8inir(6, 0.d0, dfde, 1)
!
    treps=0.d0
    treps=eps(1)+eps(2)+eps(3)
!
    if (treps .lt. 0.d0) then
        do 8 i = 1, ndim
            dfde(t(i,i))=dfde(t(i,i))+phid*lambda*treps
 8      continue
    endif
!
    call diago3(eps, veceps, valeps)
    call r8inir(3, 0.d0, vpe, 1)
!
    do 819 i = 1, ndim
        if (valeps(i) .lt. 0.d0) then
            vpe(i)=valeps(i)
        else
            vpe(i)=0.d0
        endif
819  end do
!
    call r8inir(6, 0.d0, tu, 1)
    do 20 i = 1, ndim
        do 21 j = i, ndim
            do 22 k = 1, ndim
                tu(t(i,j))=tu(t(i,j))+veceps(i,k)*vpe(k)*veceps(j,k)
22          continue
21      continue
20  end do
!
    do 23 i = 1, ndim
        do 24 j = i, ndim
            if (j .eq. i) then
                rtemp=1.d0
            else
                rtemp=rac2
            endif
            dfde(t(i,j))=dfde(t(i,j))+2.d0*mu*phid*tu(t(i,j))*rtemp
24      continue
23  end do
!
!
end subroutine
