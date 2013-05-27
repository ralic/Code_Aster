subroutine i3efk2(fk, nbeval, r, s, evalfk)
    implicit none
!
    real(kind=8) :: fk(4, *), r(*), s(*), evalfk(3, *)
    integer :: nbeval
!
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     EVALUATION EN NBEVAL COUPLES (R,S) DE FK
!     ------------------------------------------------------------------
! IN  FK     : R : TABLE(1..4,1..2) DES COEF DE FK
! IN  NBEVAL : R : NOMBRE D' EVALUTAION DEMANDE
! IN  R      : R : TABLE(1..NBEVAL) DES VALEUR DE LA COORDO DE REF 1
! IN  S      : R : TABLE(1..NBEVAL) DES VALEUR DE LA COORDO DE REF 2
! OUT EVALFK : R : TABLE(1..3,1..NBEVAL) DES VALEUR DE FK
!     ------------------------------------------------------------------
!
    integer :: i
    real(kind=8) :: r1, s1
!
!======================================================================
!
    do 10, i = 1, nbeval, 1
    r1 = r(i)
    s1 = s(i)
    evalfk(1,i) = fk(1,1) + fk(2,1)*r1 + s1*(fk(3,1) + fk(4,1)*r1)
    evalfk(2,i) = fk(1,2) + fk(2,2)*r1 + s1*(fk(3,2) + fk(4,2)*r1)
    evalfk(3,i) = fk(1,3) + fk(2,3)*r1 + s1*(fk(3,3) + fk(4,3)*r1)
    10 end do
end subroutine
