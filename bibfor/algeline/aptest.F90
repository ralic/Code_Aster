subroutine aptest(nk, imata, itest, cbd)
    implicit none
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: nk, imata, itest
    complex(kind=8) :: cbd(100)
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     SUBROUTINE THAT IMPLEMENTS CANONICAL TESTS FOR APM012 (IMPR_STURM)
!     THIS IS ONLY SEP (STANDARD EIGENVALUE PROBLEM) FOR DEBUGGING.
!     ------------------------------------------------------------------
! IN NK     : IN  : SIZE OF THE CANONICAL SEP
! IN IMATA  : IN  : JEVEUX ADRESS OF THE MATRIX OF THE SEP IN ZC
! IN ITEST  : IN  : NUMBER OF THE CANONICAL TEST
! OUT CBD   : C16 : EIGENVALUES OF THE SEP
!     ------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
!
!
    integer :: ifm, niv, nk2, n1, i
    real(kind=8) :: valm, valkk, vala, valb, valome, raux1, raux2, rauxx, rauxy
    real(kind=8) :: pi
    complex(kind=8) :: czero, cun
!
!   --- MISCELLANEOUS ---
    call jemarq()
    call infniv(ifm, niv)
    czero=dcmplx(0.d0,0.d0)
    cun=dcmplx(1.d0,0.d0)
    pi=r8pi()
!
!   --- DATA OF THE CANONICAL SEP
    nk2=nk*nk
    do 5 i = 1, nk2
        zc(imata+i-1)=czero
 5  end do
    write(ifm,*)'*** WARNING WARNING WARNING WARNING WARNING ***'
    if (itest .eq. 0) then
!   --- TEST 1: MATRIX WITH EIGENVALUES (-3/2;+/- SQRT(61)/2) (NK=2) ---
        cbd(1)=dcmplx(-3.d0/2.d0,sqrt(61.d0)/2.d0)
        cbd(2)=dcmplx(-3.d0/2.d0,-sqrt(61.d0)/2.d0)
        zc(imata+(1-1)*nk+1-1)=1.d0*cun
        zc(imata+(1-1)*nk+2-1)=3.d0*cun
!
        zc(imata+(2-1)*nk+1-1)=3.d0*cun
        zc(imata+(2-1)*nk+2-1)=-4.d0*cun
        write(ifm,*)'BASIC TEST 0'
    else if (itest.eq.1) then
!   --- TEST 1: DIAGONAL MATRIX WITH EIGENVALUES 0.00, -0.2, 30 ---
!   --- AND -400 (NK=4)                                         ---
        cbd(1)=0.0d0*cun
        cbd(2)=-0.2d0*cun
        cbd(3)=30.0d0*cun
        cbd(4)=-400.d0*cun
        zc(imata+(1-1)*nk+1-1)=cbd(1)
        zc(imata+(2-1)*nk+2-1)=cbd(2)
        zc(imata+(3-1)*nk+3-1)=cbd(3)
        zc(imata+(4-1)*nk+4-1)=cbd(4)
        write(ifm,*)'BASIC TEST 1'
    else if (itest.eq.2) then
!   --- TEST 2 MATRIX (FOR NK=4) WITH EIGENVALUES: 4, 4, 2 AND 1 ---
!   --- NON SYMETRICAL AND REAL (CF. THEODOR/LASCAUX P606, NK=4) ---
        cbd(1)=1.d0*cun
        cbd(2)=2.d0*cun
        cbd(3)=4.d0*cun
        cbd(4)=4.d0*cun
        zc(imata+(1-1)*nk+1-1)=-11.98d0*cun
        zc(imata+(1-1)*nk+2-1)=20.61d0*cun
        zc(imata+(1-1)*nk+3-1)=-10.63d0*cun
        zc(imata+(1-1)*nk+4-1)=14.95d0*cun
!
        zc(imata+(2-1)*nk+1-1)=-8.521d0*cun
        zc(imata+(2-1)*nk+2-1)=15.44d0*cun
        zc(imata+(2-1)*nk+3-1)=-6.15d0*cun
        zc(imata+(2-1)*nk+4-1)=7.885d0*cun
!
        zc(imata+(3-1)*nk+1-1)=-5.483d0*cun
        zc(imata+(3-1)*nk+2-1)=9.465d0*cun
        zc(imata+(3-1)*nk+3-1)=-2.088d0*cun
        zc(imata+(3-1)*nk+4-1)=4.590d0*cun
!
        zc(imata+(4-1)*nk+1-1)=-6.298d0*cun
        zc(imata+(4-1)*nk+2-1)=9.351d0*cun
        zc(imata+(4-1)*nk+3-1)=-5.390d0*cun
        zc(imata+(4-1)*nk+4-1)=9.573d0*cun
        write(ifm,*)'BASIC TEST 2'
    else if (itest.eq.3) then
!   --- TEST 3 MATRIX OF SIMPLE SPRING-MASS-DAMPER SYSTEM ---
!   --- (CF. JUNG/KIM/LEE 2001, NK=20)                    ---
        valm=1.d0
        valkk=1.d0
        vala=0.05d0
        valb=0.5d0
        valome=2.d0*sqrt(valm/valkk)
        n1=nk/2
! MODIFIER LA TAILLE FIXEE A 100 DU VECTEUR CBD (NE PAS OUBLIER APM012)
        if (n1 .gt. 100) ASSERT(.false.)
        do 7 i = 1, n1
            raux1=valome*sin(pi*0.5d0*(2*i-1)/(2*n1+1))
            raux2=0.5d0*((vala/raux1)+valb*raux1)
            rauxx=-raux2*raux1
            rauxy=raux1*sqrt(1-(raux2*raux2))
            zc(imata+(2*i-2)*nk+2*i-2)=dcmplx(rauxx,rauxy)
            zc(imata+(2*i-1)*nk+2*i-1)=dcmplx(rauxx,-rauxy)
            cbd(2*i-1)=dcmplx(rauxx,rauxy)
            cbd(2*i) =dcmplx(rauxx,-rauxy)
 7      continue
        write(ifm,*)'BASIC TEST 3'
    else if (itest.eq.4) then
!   --- TEST 4 MATRIX OF SIMPLE SPRING-MASS SYSTEM         ---
!   --- (CF. ASTER TEST SDLD02A, NK=10)                    ---
        cbd(1)=40000.d0*cun
        cbd(2)=20000.d0*cun
        cbd(3)=10000.d0*cun
        cbd(4)=1000.d0*cun
        cbd(5)=1.d0*cun
        cbd(6)=0.01d0*cun
        cbd(7)=-0.001d0*cun
        cbd(8)=100000.d0*cun
        cbd(9)=100000.d0*cun
        cbd(10)=100000.d0*cun
        do 8 i = 1, nk
            zc(imata+(i-1)*nk+i-1)=cbd(i)
 8      continue
        write(ifm,*)'BASIC TEST 4'
    else
        ASSERT(.false.)
    endif
    write(ifm,*)'*** WARNING WARNING WARNING WARNING WARNING ***'
    call jedema()
end subroutine
