subroutine deflg2(gn, lamb, logl, pes, feta,&
                  xi, me)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
    implicit none
!     CALCUL DES DES TERMES NECESSAIRES
!     AU POST TRAITEMENT DES CONTRAINTES
!     SUIVANT ARTICLE MIEHE APEL LAMBRECHT CMAME 2002
! ----------------------------------------------------------------------
!     IN   GN    directions propres du tenseur F
!     IN   LAMB  valeurs propres du tenseur F
!     IN   LOGL  log des valeurs propres du tenseur F
!     OUT  PES   tenseur P symetrise, pour le passage de T a PK2
!     OUT  FETA  utilitaires pour DEFLG3 : f_i=-2/lambda_i**2 puis eta
!     OUT  XI    utilitaires pour DEFLG3 : xi_ij
!     OUT  ME    utilitaires pour DEFLG3 : tenseur M d'ordre 4
! ----------------------------------------------------------------------
#include "asterc/r8miem.h"
#include "asterfort/r8inir.h"
#include "asterfort/symt46.h"
    real(kind=8) :: gn(3, 3), lamb(3), logl(3)
    real(kind=8) :: di(3), theta(3, 3)
    real(kind=8) :: pe(3, 3, 3, 3), me(3, 3, 3, 3), xi(3, 3), feta(4), pes(6, 6)
    integer :: nbvec, i, icas, j, k, l, a, b
! ----------------------------------------------------------------------
!
    nbvec = 3
!
    call r8inir(4, 0.d0, feta, 1)
    do 14 i = 1, 3
        di(i)=1.d0/lamb(i)
        feta(i)=-2.d0/lamb(i)/lamb(i)
14  end do
!
    if (abs(lamb(1)-lamb(2)) .lt. r8miem()) then
        if (abs(lamb(1)-lamb(3)) .lt. r8miem()) then
            icas=123
        else
            icas=12
        endif
    else
        if (abs(lamb(1)-lamb(3)) .lt. r8miem()) then
            icas=13
        else if (abs(lamb(2)-lamb(3)).lt.r8miem()) then
            icas=23
        else
            icas=1
        endif
    endif
!
    call r8inir(9, 0.d0, theta, 1)
    call r8inir(9, 0.d0, xi, 1)
    if (icas .eq. 1) then
        theta(1,2)=(logl(1)-logl(2))/(lamb(1)-lamb(2))
        theta(2,1)=(logl(2)-logl(1))/(lamb(2)-lamb(1))
        theta(3,2)=(logl(3)-logl(2))/(lamb(3)-lamb(2))
        theta(2,3)=(logl(2)-logl(3))/(lamb(2)-lamb(3))
        theta(1,3)=(logl(1)-logl(3))/(lamb(1)-lamb(3))
        theta(3,1)=(logl(3)-logl(1))/(lamb(3)-lamb(1))
        xi(1,2)= (theta(1,2) - 0.5d0*di(2)) / (lamb(1)-lamb(2))
        xi(2,1)= (theta(2,1) - 0.5d0*di(1)) / (lamb(2)-lamb(1))
        xi(3,2)= (theta(3,2) - 0.5d0*di(2)) / (lamb(3)-lamb(2))
        xi(2,3)= (theta(2,3) - 0.5d0*di(3)) / (lamb(2)-lamb(3))
        xi(1,3)= (theta(1,3) - 0.5d0*di(3)) / (lamb(1)-lamb(3))
        xi(3,1)= (theta(3,1) - 0.5d0*di(1)) / (lamb(3)-lamb(1))
        do 15 i = 1, 3
            do 16 j = 1, 3
                do 17 k = 1, 3
                    if ((j.ne.i) .and. (j.ne.k) .and. (k.ne.i)) then
                        feta(4)=feta(4)+ logl(i)*0.5d0/(lamb(i)-lamb(&
                        j))/(lamb(i)-lamb(k))
                    endif
17              continue
16          continue
15      continue
    else if (icas.eq.12) then
        theta(1,2)=di(1)/2.d0
        theta(2,1)=di(1)/2.d0
        theta(1,3)=(logl(1)-logl(3))/(lamb(1)-lamb(3))
        theta(3,1)=(logl(3)-logl(1))/(lamb(3)-lamb(1))
        theta(3,2)=(logl(3)-logl(2))/(lamb(3)-lamb(2))
        theta(2,3)=(logl(2)-logl(3))/(lamb(2)-lamb(3))
        xi(1,2)= feta(1)/8.d0
        xi(2,1)= feta(1)/8.d0
        xi(3,2)= (theta(3,2) - 0.5d0*di(2)) / (lamb(3)-lamb(2))
        xi(2,3)= (theta(2,3) - 0.5d0*di(3)) / (lamb(2)-lamb(3))
        xi(1,3)= (theta(1,3) - 0.5d0*di(3)) / (lamb(1)-lamb(3))
        xi(3,1)= (theta(3,1) - 0.5d0*di(1)) / (lamb(3)-lamb(1))
        feta(4)= xi(3,1)
    else if (icas.eq.13) then
        theta(1,2)=(logl(1)-logl(2))/(lamb(1)-lamb(2))
        theta(2,1)=(logl(2)-logl(1))/(lamb(2)-lamb(1))
        theta(1,3)=di(1)/2.d0
        theta(3,1)=di(1)/2.d0
        theta(3,2)=(logl(3)-logl(2))/(lamb(3)-lamb(2))
        theta(2,3)=(logl(2)-logl(3))/(lamb(2)-lamb(3))
        xi(1,2)= (theta(1,2) - 0.5d0*di(2)) / (lamb(1)-lamb(2))
        xi(2,1)= (theta(2,1) - 0.5d0*di(1)) / (lamb(2)-lamb(1))
        xi(3,2)= (theta(3,2) - 0.5d0*di(2)) / (lamb(3)-lamb(2))
        xi(2,3)= (theta(2,3) - 0.5d0*di(3)) / (lamb(2)-lamb(3))
        xi(1,3)= feta(1)/8.d0
        xi(3,1)= feta(1)/8.d0
        feta(4)= xi(1,2)
    else if (icas.eq.23) then
        theta(1,2)=(logl(1)-logl(2))/(lamb(1)-lamb(2))
        theta(2,1)=(logl(2)-logl(1))/(lamb(2)-lamb(1))
        theta(1,3)=(logl(1)-logl(3))/(lamb(1)-lamb(3))
        theta(3,1)=(logl(3)-logl(1))/(lamb(3)-lamb(1))
        theta(3,2)=di(1)/2.d0
        theta(2,3)=di(1)/2.d0
        xi(1,2)= (theta(1,2) - 0.5d0*di(2)) / (lamb(1)-lamb(2))
        xi(2,1)= (theta(2,1) - 0.5d0*di(1)) / (lamb(2)-lamb(1))
        xi(3,2)= feta(2)/8.d0
        xi(2,3)= feta(2)/8.d0
        xi(1,3)= (theta(1,3) - 0.5d0*di(3)) / (lamb(1)-lamb(3))
        xi(3,1)= (theta(3,1) - 0.5d0*di(1)) / (lamb(3)-lamb(1))
        feta(4)= xi(2,3)
    else if (icas.eq.123) then
        theta(1,2)=di(1)/2.d0
        theta(2,1)=di(1)/2.d0
        theta(1,3)=di(1)/2.d0
        theta(3,1)=di(1)/2.d0
        theta(3,2)=di(1)/2.d0
        theta(2,3)=di(1)/2.d0
        xi(1,2)= feta(2)/8.d0
        xi(2,1)= feta(2)/8.d0
        xi(3,2)= feta(2)/8.d0
        xi(2,3)= feta(2)/8.d0
        xi(1,3)= feta(2)/8.d0
        xi(3,1)= feta(2)/8.d0
        feta(4)= feta(2)/8.d0
    endif
!
!
    call r8inir(81, 0.d0, me, 1)
!
!     calcul de M_E (Lagrangien) pour chaque direction propre
    do 25 i = 1, nbvec
        do 26 j = 1, nbvec
            do 27 a = 1, 3
                do 28 b = 1, 3
                    me(a,b,i,j)=gn(a,i)*gn(b,j)+gn(a,j)*gn(b,i)
28              continue
27          continue
26      continue
25  end do
!
    call r8inir(81, 0.d0, pe, 1)
!
    do 29 i = 1, 3
        do 30 k = 1, 3
            do 31 l = 1, 3
                do 32 a = 1, 3
                    do 33 b = 1, 3
                        pe(k,l,a,b)=pe(k,l,a,b)+ di(i)*gn(k,i)*gn(l,i)&
                        *me(a,b,i,i)/2.d0
33                  continue
32              continue
31          continue
30      continue
29  end do
    do 34 i = 1, 3
        do 35 j = 1, 3
            if (i .ne. j) then
                do 36 k = 1, 3
                    do 37 l = 1, 3
                        do 38 a = 1, 3
                            do 39 b = 1, 3
                                pe(k,l,a,b)=pe(k,l,a,b)+ theta(i,j)*&
                                gn(k,i)*gn(l,j)*me(a,b,i,j)
39                          continue
38                      continue
37                  continue
36              continue
            endif
35      continue
34  end do
!
    call symt46(pe, pes)
!
end subroutine
