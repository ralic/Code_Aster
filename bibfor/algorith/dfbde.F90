subroutine dfbde(dim, b, e, deuxmu, lambda,&
                 dsidep)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/dfpdf.h"
#include "asterfort/diago3.h"
#include "asterfort/r8inir.h"
    integer :: dim
    real(kind=8) :: b(6), e(6), deuxmu, lambda
    real(kind=8) :: dsidep(6, 6)
! ----------------------------------------------------------------------
!     LOI DE COMPORTEMENT ENDO_ORTH_BETON
!     CALCUL DE LA DERIVEE DE LA FORCE THERMODYNAMIQUE (ENDO TRACTION)
!     PAR RAPPORT A LA DEFORMATION:DFB/DEPS
!
!     FB=-LAMBDA.TR(EB).H(TR(EB))-MU/2*((BE+EB)_+*E + E*(BE+EB)_+)
!        +ECROB*(I-B)
!     IN  DIM      : DIMENSION 3(3D) OU 2(2D)
!     IN  E        : DEFORMATION
!     IN  B        : TENSEUR D ENDOMMAGEMENT DE TRACTION
!     IN  LAMBDA   : /
!     IN  DEUXMU   : / COEFFICIENTS DE LAME
!     OUT DSIDEP      : DFB/DEPS
! ----------------------------------------------------------------------
!
    integer :: i, j, k, l, t(3, 3)
    integer :: p, q, ik, pq, rs
    real(kind=8) :: rtemp2
    real(kind=8) :: rtemp3, rtemp4, rac2, kron(3, 3), dbede(6, 6), mtemp(6, 6)
    real(kind=8) :: beeb(6), beebp(6), vecbeb(3, 3), valbeb(3)
    real(kind=8) :: f1b(6, 6), f2b(6, 6)
    real(kind=8) :: a(6, 6), treb, c(6, 6), trec
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
!
    kron(1,1)=1.d0
    kron(1,2)=0.d0
    kron(1,3)=0.d0
    kron(2,1)=0.d0
    kron(2,2)=1.d0
    kron(2,3)=0.d0
    kron(3,1)=0.d0
    kron(3,2)=0.d0
    kron(3,3)=1.d0
!
    rac2=sqrt(2.d0)
!
    call r8inir(36, 0.d0, dbede, 1)
    call r8inir(36, 0.d0, c, 1)
    call r8inir(36, 0.d0, dsidep, 1)
    call r8inir(36, 0.d0, a, 1)
    call r8inir(6, 0.d0, beeb, 1)
    call r8inir(36, 0.d0, f1b, 1)
    call r8inir(36, 0.d0, f2b, 1)
!
!-----CALCUL DE D(BE+EB)/DE------------------------------------
    rtemp3 = 0.d0
    rtemp4 = 0.d0
    do 20 i = 1, dim
        do 21 j = i, dim
            do 22 k = 1, dim
                do 23 l = 1, dim
                    if (i .eq. j) then
                        rtemp3=1.d0
                    else
                        rtemp3=rac2
                    endif
                    if (k .eq. l) then
                        rtemp4=1.d0
                    else
                        rtemp4=1.d0/rac2
                    endif
                    dbede(t(i,j),t(k,l))=dbede(t(i,j),t(k,l))+(kron(k,&
                    i)* b(t(l,j))+kron(j,l)*b(t(i,k)))*rtemp3*rtemp4
23              continue
                beeb(t(i,j))=beeb(t(i,j))+ b(t(i,k))*e(t(k,j))+e(t(i,&
                k))*b(t(k,j))
22          continue
21      continue
20  end do
!
!
    call diago3(beeb, vecbeb, valbeb)
    do 822 i = 1, 3
        if (valbeb(i) .lt. 0.d0) then
            valbeb(i)=0.d0
        endif
822  continue
!
    call r8inir(6, 0.d0, beebp, 1)
!
    do 823 i = 1, 3
        do 824 j = i, 3
            do 825 k = 1, 3
                beebp(t(i,j))=beebp(t(i,j))+vecbeb(i,k)*valbeb(k)*&
                vecbeb(j,k)
825          continue
824      continue
823  continue
!
!
!
!
!----------------------------------------------------------------------
!
!-----CALCUL DE F2B----------------------------------------------------
!
!
    call dfpdf(6, beeb, mtemp)
!
    do 240 ik = 1, 6
        do 241 pq = 1, 6
            do 242 rs = 1, 6
                a(ik,pq)=a(ik,pq)+mtemp(ik,rs)*dbede(rs,pq)
242          continue
241      continue
240  end do
!
    rtemp2 = 0.d0
    rtemp3 = 0.d0
    rtemp4 = 0.d0
    do 250 i = 1, 3
        do 251 j = i, 3
            do 252 k = 1, 3
                do 253 l = 1, 6
                    if (i .eq. j) then
                        rtemp2=1.d0
                    else
                        rtemp2=rac2
                    endif
                    if (k .eq. j) then
                        rtemp3=1.d0
                    else
                        rtemp3=1.d0/rac2
                    endif
                    if (k .eq. i) then
                        rtemp4=1.d0
                    else
                        rtemp4=1.d0/rac2
                    endif
                    f2b(t(i,j),l)=f2b(t(i,j),l)+(a(t(i,k),l)*e(t(k,j))&
                    * rtemp4+e(t(i,k))*a(t(k,j),l)*rtemp3)*rtemp2
253              continue
252          continue
251      continue
250  end do
!
!
    rtemp2 = 0.d0
    rtemp3 = 0.d0
    do 260 i = 1, 3
        do 261 j = i, 3
            do 262 p = 1, 3
                do 263 q = 1, 3
                    if (i .eq. j) then
                        rtemp2=1.d0
                    else
                        rtemp2=rac2
                    endif
                    if (p .eq. q) then
                        rtemp3=1.d0
                    else
                        rtemp3=1.d0/rac2
                    endif
                    f2b(t(i,j),t(p,q))=f2b(t(i,j),t(p,q)) +(kron(j,q)*&
                    beebp(t(i,p)) +kron(i,p)*beebp(t(j,q))) *rtemp2*&
                    rtemp3
263              continue
262          continue
261      continue
260  end do
!
!-----------------------------------------------------------------------
!
!-----CALCUL DE F1B-----------------------------------------------------
!
    treb=(beeb(1)+beeb(2)+beeb(3))/2.d0
!
    if (treb .lt. 0) then
        trec=0.d0
    else
        trec=1.d0
    endif
!
!
    rtemp2 = 0.d0
    rtemp3 = 0.d0
    do 120 i = 1, 3
        do 121 j = i, 3
            do 122 p = 1, 3
                do 123 q = 1, 3
                    if (i .eq. j) then
                        rtemp2=1.d0
                    else
                        rtemp2=rac2
                    endif
                    if (p .eq. q) then
                        rtemp3=1.d0
                    else
                        rtemp3=1.d0/rac2
                    endif
                    f1b(t(i,j),t(p,q))=f1b(t(i,j),t(p,q))+trec*b(t(p,&
                    q)) *e(t(i,j))*rtemp2*rtemp3
123              continue
122          continue
121      continue
120  end do
!
!
    rtemp2 = 0.d0
    rtemp3 = 0.d0
    do 160 i = 1, 3
        do 161 j = i, 3
            do 162 p = 1, 3
                do 163 q = 1, 3
                    if (i .eq. j) then
                        rtemp2=1.d0
                    else
                        rtemp2=rac2
                    endif
                    if (p .eq. q) then
                        rtemp3=1.d0
                    else
                        rtemp3=1.d0/rac2
                    endif
                    f1b(t(i,j),t(p,q))= f1b(t(i,j),t(p,q))+trec*treb&
                    *kron(i,p)*kron(j,q)*rtemp2*rtemp3
163              continue
162          continue
161      continue
160  end do
!
!
!----CALCUL DE FB-------------------------------------------------------
!
    do 900 i = 1, 6
        do 901 j = 1, 6
            dsidep(i,j)=-lambda*f1b(i,j)-deuxmu/4.d0*f2b(i,j)
901      continue
900  end do
!
end subroutine
