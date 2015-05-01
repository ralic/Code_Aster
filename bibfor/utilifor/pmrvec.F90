subroutine pmrvec(cumul, n, m, a, x,&
                  y)
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
    implicit none
    character(len=*) :: cumul
    integer :: n, m
    real(kind=8) :: a(n, m), x(m), y(n)
!       ----------------------------------------------------------------
!       PRODUIT D'UNE MATRICE RECTANGLE PLEINE PAR UN VECTEUR
!          Y(N) = 0.  + A(N,M)*X(M)
!       OU Y(N) = Y(N)+ A(N,M)*X(M)
!       ----------------------------------------------------------------
! IN    N, M  : I  :   DIMENSIONS DE LA MATRICE ET DES VECTEURS X ET Y
! IN    A(N,M): R  :   MATRICE REELLE
! IN    X(M)  : R  :   VECTEUR REEL
! IN    CUMUL : K* :   ON CUMULE OU NON DANS LE VECTEUR RESULTAT Y
!       CUMUL = 'ZERO' ON MET Y A ZERO AVANT DE COMMENCER
!       CUMUL = 'CUMU' ON ACCUMULE DANS Y
! OUT   Y(N)  : R  :   VECTEUR REEL
!       ----------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, j
!-----------------------------------------------------------------------
    if (cumul .eq. 'ZERO') then
        do 1 i = 1, n
            y(i) = 0.d0
 1      continue
    endif
!
    do 3 j = 1, m
        do 2 i = 1, n
            y(i) = y(i) + a(i,j) * x(j)
 2      continue
 3  continue
end subroutine
