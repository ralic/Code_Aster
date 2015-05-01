subroutine prlgma(mati, sina, cosa, sinb, cosb,&
                  sing, cosg, matf)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! DESCRIPTION : PASSAGE REPERE LOCAL -> REPERE GLOBAL
! -----------
!               APPELANT : MDCHOE
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
#include "asterfort/prmama.h"
#include "asterfort/utmess.h"
    real(kind=8) :: mati(3, 3), sina, cosa, sinb, cosb, sing, cosg, matf(3, 3)
!
! VARIABLES LOCALES
! -----------------
    integer :: i, l, k, j, ier, iprod
    real(kind=8) :: rx(3, 3), ry(3, 3), rz(3, 3), rzyx(3, 3), p(3), temp(3, 3)
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC  ABS
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL   PRMAMA
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    rz(1,1) = 1.0d0
    ry(2,2) = rz(1,1)
    rx(3,3) = ry(2,2)
    rx(1,1) = cosa
    rx(1,2) = sina
    rx(2,1) = -sina
    rx(2,2) = cosa
    rx(3,2) = 0.0d0
    rx(3,1) = rx(3,2)
    rx(2,3) = rx(3,1)
    rx(1,3) = rx(2,3)
    ry(1,1) = cosb
    ry(1,3) = -sinb
    ry(3,1) = sinb
    ry(3,3) = cosb
    ry(3,2) = 0.0d0
    ry(2,3) = ry(3,2)
    ry(2,1) = ry(2,3)
    ry(1,2) = ry(2,1)
    rz(2,2) = cosg
    rz(2,3) = sing
    rz(3,2) = -sing
    rz(3,3) = cosg
    rz(3,1) = 0.0d0
    rz(2,1) = rz(3,1)
    rz(1,3) = rz(2,1)
    rz(1,2) = rz(1,3)
!
    do 10 l = 1, 3
        do 10 k = 1, 3
            if (abs(rx(k,l)) .lt. 1.0d-06) rx(k,l) = 0.0d0
            if (abs(ry(k,l)) .lt. 1.0d-06) ry(k,l) = 0.0d0
            if (abs(rz(k,l)) .lt. 1.0d-06) rz(k,l) = 0.0d0
10      continue
!
    do 20 j = 1, 3
        do 20 i = 1, 3
            rzyx(i,j) = 0.0d0
            do 20 k = 1, 3
                p(k) = 0.0d0
                do 21 l = 1, 3
                    p(k) = p(k) + rz(i,l)*ry(l,k)
21              continue
                rzyx(i,j) = rzyx(i,j) + p(k)*rx(k,j)
20          continue
!
    iprod = 1
    ier = 0
    call prmama(iprod, mati, 3, 3, 3,&
                rzyx, 3, 3, 3, temp,&
                3, 3, 3, ier)
    if (ier .ne. 0) then
        call utmess('F', 'ALGORITH10_2')
    endif
!
!                                     SINA = -SINA
! POUR LA TRANSFORMATION INVERSE  =>  SINB = -SINB
! ------------------------------      SING = -SING
!
    rx(1,2)= -sina
    rx(2,1)=  sina
!
    ry(1,3)=  sinb
    ry(3,1)= -sinb
!
    rz(2,3)= -sing
    rz(3,2)=  sing
!
    do 30 l = 1, 3
        do 30 k = 1, 3
            if (abs(rx(k,l)) .lt. 1.0d-06) rx(k,l) = 0.0d0
            if (abs(ry(k,l)) .lt. 1.0d-06) ry(k,l) = 0.0d0
            if (abs(rz(k,l)) .lt. 1.0d-06) rz(k,l) = 0.0d0
30      continue
!
    do 40 j = 1, 3
        do 40 i = 1, 3
            rzyx(i,j) = 0.0d0
            do 40 k = 1, 3
                p(k) = 0.0d0
                do 41 l = 1, 3
                    p(k) = p(k) + rx(i,l)*ry(l,k)
41              continue
                rzyx(i,j) = rzyx(i,j) + p(k)*rz(k,j)
40          continue
!
    ier = 0
    call prmama(iprod, rzyx, 3, 3, 3,&
                temp, 3, 3, 3, matf,&
                3, 3, 3, ier)
    if (ier .ne. 0) then
        call utmess('F', 'ALGORITH10_2')
    endif
!
! --- FIN DE PRLGMA.
end subroutine
