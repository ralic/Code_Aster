subroutine caldto(s6, fkooh, msns, dtods)
    implicit none
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jean-michel.proix at edf.fr
!     ----------------------------------------------------------------
!
!     MONOCRISTAL : calcul de la derivee de Tau en GDEF : dTau/dS_ab
!     IN  S6    :  CONTRAINTES NOTATION VOIGT
!     IN  FKOOH :  INVERSE TENSEUR HOOKE
!     IN  MSNS  :  MS * NS
!     OUT DTODS :  dTau/dS
!
#include "asterfort/r8inir.h"
#include "asterfort/tnsvec.h"
#include "blas/dcopy.h"
    integer :: i, j, k, l, ind(3, 3), a, b, m, n
    real(kind=8) :: s6(6), fkooh(6, 6), msns(3, 3), dtods(3, 3)
    real(kind=8) :: s(3, 3), l4(3, 3, 3, 3)
    real(kind=8) :: mus(3, 3), l4s(3, 3)
    data ind/1,4,5,4,2,6,5,6,3/
!     ----------------------------------------------------------------
!     CONSTUCTION DU TENSEUR INVERSE DE HOOKE D'ORDRE 4
!
    do 1 i = 1, 3
        do 1 j = 1, 3
            do 1 k = 1, 3
                do 1 l = 1, 3
                    l4(i,j,k,l)=fkooh(ind(i,j),ind(k,l))
 1              continue
!
    call tnsvec(6, 3, s, s6, 1.d0)
    call dcopy(9, msns, 1, dtods, 1)
!
    call r8inir(9, 0.d0, mus, 1)
!
!     CALCUL DU TERME  2(Lambda**-1)*mu*S
    do 2 m = 1, 3
        do 2 n = 1, 3
            do 2 k = 1, 3
                mus(m,n)=mus(m,n)+msns(m,k)*s(k,n)
 2          continue
!
    do 3 a = 1, 3
        do 3 b = 1, 3
            do 3 m = 1, 3
                do 3 n = 1, 3
                    dtods(a,b)=dtods(a,b)+2.d0*l4(a,b,m,n)*mus(m,n)
 3              continue
!
!     CALCUL DU TERME  2(LAMBDA**-1*S)*MU
    call r8inir(9, 0.d0, l4s, 1)
    do 4 a = 1, 3
        do 4 k = 1, 3
            do 4 m = 1, 3
                do 4 n = 1, 3
                    l4s(a,k)=l4s(a,k)+l4(a,k,m,n)*mus(m,n)
 4              continue
!
    do 5 a = 1, 3
        do 5 b = 1, 3
            do 5 k = 1, 3
                dtods(a,b)=dtods(a,b)+2.d0*l4s(a,k)*mus(k,b)
 5          continue
!
end subroutine
