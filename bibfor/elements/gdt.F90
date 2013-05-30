subroutine gdt(teta, amat)
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
!
! FONCTION: LE VECTEUR-ROTATION ENTRE L'INSTANT INSTAM ET L'ITERATION
!           K-1 DE L'INSTANT INSTAP EST TETA. ON DONNE UN INCREMENT DE
!           VECTEUR-ROTATION DTETA A PARTIR DE L'ITERATION PRECEDENTE.
!           GDT CALCULE LA MATRICE AMAT QUI, MULTIPLIEE PAR DTETA, DONNE
!           LA PARTIE PRINCIPALE DE L'ACCROISSEMENT CORRESPONDANT DE
!           TETA.
!
!     IN  : TETA      : VECTEUR-ROTATION
!
!     OUT : AMAT      : MATRICE DE LINEARISATION
! ------------------------------------------------------------------
    implicit none
    include 'asterc/r8prem.h'
    include 'asterfort/antisy.h'
    include 'blas/ddot.h'
    real(kind=8) :: teta(3), eu(3), amat(3, 3), amat1(3, 3)
!
!-----------------------------------------------------------------------
    integer :: i, j
    real(kind=8) :: anor, anors2, coef, deux, epsil1, epsil2
    real(kind=8) :: prosca, un, zero
!-----------------------------------------------------------------------
    zero = 0.d0
    epsil1= 1.d-4
    epsil2= r8prem( )**2
    un = 1.d0
    deux = 2.d0
!
    prosca=ddot(3,teta,1,teta,1)
    anor = sqrt (prosca)
    anors2 = anor / deux
    if (anors2 .gt. epsil1) then
        coef = anors2 / tan(anors2)
    else
        coef = un + anors2**2/3.d0
        coef = un / coef
    endif
!
    do 1 i = 1, 3
        if (anors2 .le. epsil2) then
            eu(i) = zero
        else
            eu(i) = teta(i) / anor
        endif
 1  end do
    call antisy(teta, un, amat1)
    do 12 j = 1, 3
        do 11 i = 1, 3
            amat(i,j) = eu(i)*eu(j)*(un-coef) - amat1(i,j)/deux
11      end do
        amat(j,j) = amat(j,j) + coef
12  end do
end subroutine
