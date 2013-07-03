subroutine nuainr(method, np1, nx1, nc1, ic1,&
                  nuax1, nual1, nuav1, x2, dref,&
                  val2)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/mgauss.h"
#include "asterfort/u2mesk.h"
    character(len=*) :: method
    integer :: nx1, np1, ic1, nc1
    real(kind=8) :: nuax1(*), nuav1(*), x2(nx1), dref, dref2, val2
    logical :: nual1(*)
!
!  BUT : INTERPOLER LA VALEUR VAL2 DU NUAGE NUAG1 SUR LE POINT DE
!        COORDONNEES X2
!
! IN  METHOD   : METHODE D'INTERPOLATION: 'NUAGE_DEG_0' OU 'NUAGE_DEG_1'
! IN  NP1      : NOMBRE DE POINTS DU NUAGE NUAG1
! IN  NX1      : NOMBRE DE COORDONNEES DES POINTS DE NUAG1 (ET X2)
! IN  NC1      : NOMBRE DE CMPS DES POINTS DE NUAG1
! IN  IC1      : NUMERO DE LA CMP A INTERPOLER DANS NUAG1
! IN  NUAX1    : OBJET .NUAX  DE NUAG1
! IN  NUAL1    : OBJET .NUAL  DE NUAG1
! IN  NUAV1    : OBJET .NUAV  DE NUAG1
! IN  X2       : COORDONNEES DU POINT QU L'ON CHERCHE A INTERPOLER
! IN  DREF     : DISTANCE DE REFERENCE POUR LA METHODE D'INTERPOLATION
! OU  VAL2     : VALEUR INTERPOLEE
!
! VARIABLES LOCALES :
    integer :: ip1, ix1, i, j, iret
    real(kind=8) :: k0(1, 1), k1(2, 2), k2(3, 3), k3(4, 4), f(4)
    real(kind=8) :: w, x1, y1, z1, v1, d, det
    character(len=16) :: meth2
!
! PARAMETRES DE L'EXPONENTIELLE DONNANT LE POIDS DES POINTS :
    real(kind=8) :: alpha, beta
    data alpha,beta/.2d0,0.75d0/
!
! -DEB
!
!     -- MISE A ZERO DE K ET F :
!     -------------------------
    if (nx1 .eq. 1) then
        do 52,i = 1,nx1+1
        do 53,j = 1,nx1+1
        k1(i,j)= 0.d0
53      continue
52      continue
    else if (nx1.eq.2) then
        do 54,i = 1,nx1+1
        do 55,j = 1,nx1+1
        k2(i,j)= 0.d0
55      continue
54      continue
    else if (nx1.eq.3) then
        do 56,i = 1,nx1+1
        do 57,j = 1,nx1+1
        k3(i,j)= 0.d0
57      continue
56      continue
    endif
!
    do 58,i = 1,nx1+1
    f(i)= 0.d0
    58 end do
!
    dref2=dref*alpha
!
    if (method .eq. 'NUAGE_DEG_0') then
!     ------------------------
        k0(1,1) = 0.d0
        f(1) = 0.d0
!
        if (nx1 .eq. 1) then
            do 1,ip1 = 1,np1
            if (.not.nual1((ip1-1)*nc1+ic1)) goto 1
            d=(nuax1((ip1-1)*1+1)-x2(1))**2
            w=exp(-(d/dref2)**beta)
            v1 = nuav1((ip1-1)*nc1+ic1)
            k0(1,1) = k0(1,1) + w
            f(1) = f(1) + w*v1
 1          continue
!
        else if (nx1.eq.2) then
            do 2,ip1 = 1,np1
            if (.not.nual1((ip1-1)*nc1+ic1)) goto 2
            d=(nuax1((ip1-1)*2+1)-x2(1))**2 +(nuax1((ip1-1)*2+2)-&
                x2(2))**2
            w=exp(-(d/dref2)**beta)
            v1 = nuav1((ip1-1)*nc1+ic1)
            k0(1,1) = k0(1,1) + w
            f(1) = f(1) + w*v1
 2          continue
!
        else if (nx1.eq.3) then
            do 3,ip1 = 1,np1
            if (.not.nual1((ip1-1)*nc1+ic1)) goto 3
            d=(nuax1((ip1-1)*3+1)-x2(1))**2 +(nuax1((ip1-1)*3+2)-&
                x2(2))**2 +(nuax1((ip1-1)*3+3)-x2(3))**2
            w=exp(-(d/dref2)**beta)
            v1 = nuav1((ip1-1)*nc1+ic1)
            k0(1,1) = k0(1,1) + w
            f(1) = f(1) + w*v1
 3          continue
        endif
!
!
        val2 = f(1)/k0(1,1)
!
!
    else if (method.eq.'NUAGE_DEG_1') then
!     -----------------------------
        if (nx1 .eq. 1) then
!       --------------
            do 11,ip1 = 1,np1
            if (.not.nual1((ip1-1)*nc1+ic1)) goto 11
            d=(nuax1((ip1-1)*1+1)-x2(1))**2
            w=exp(-(d/dref2)**beta)
!
            x1 = nuax1((ip1-1)*1+1)
            v1 = nuav1((ip1-1)*nc1+ic1)
            k1(1,1) = k1(1,1) +w
            k1(1,2) = k1(1,2) +w*x1
            k1(2,2) = k1(2,2) +w*x1*x1
            f(1) = f(1) + w*v1
            f(2) = f(2) + w*v1*x1
11          continue
            k1(2,1)=k1(1,2)
!
        else if (nx1.eq.2) then
!       ------------------
            do 21,ip1 = 1,np1
            if (.not.nual1((ip1-1)*nc1+ic1)) goto 21
            d=(nuax1((ip1-1)*2+1)-x2(1))**2 +(nuax1((ip1-1)*2+2)-&
                x2(2))**2
            w=exp(-(d/dref2)**beta)
!
            x1 = nuax1((ip1-1)*2+1)
            y1 = nuax1((ip1-1)*2+2)
            v1 = nuav1((ip1-1)*nc1+ic1)
            k2(1,1) = k2(1,1) + w
            k2(1,2) = k2(1,2) + w*x1
            k2(1,3) = k2(1,3) + w*y1
            k2(2,2) = k2(2,2) + w*x1*x1
            k2(2,3) = k2(2,3) + w*x1*y1
            k2(3,3) = k2(3,3) + w*y1*y1
            f(1) = f(1) + w*v1
            f(2) = f(2) + w*v1*x1
            f(3) = f(3) + w*v1*y1
21          continue
            k2(2,1) = k2(1,2)
            k2(3,1) = k2(1,3)
            k2(3,2) = k2(2,3)
!
        else if (nx1.eq.3) then
!       ------------------
            do 31,ip1 = 1,np1
            if (.not.nual1((ip1-1)*nc1+ic1)) goto 31
            d=(nuax1((ip1-1)*3+1)-x2(1))**2 +(nuax1((ip1-1)*3+2)-&
                x2(2))**2 +(nuax1((ip1-1)*3+3)-x2(3))**2
            w=exp(-(d/dref2)**beta)
!
            x1 = nuax1((ip1-1)*3+1)
            y1 = nuax1((ip1-1)*3+2)
            z1 = nuax1((ip1-1)*3+3)
            v1 = nuav1((ip1-1)*nc1+ic1)
            k3(1,1) = k3(1,1) + w
            k3(1,2) = k3(1,2) + w*x1
            k3(1,3) = k3(1,3) + w*y1
            k3(1,4) = k3(1,4) + w*z1
            k3(2,2) = k3(2,2) + w*x1*x1
            k3(2,3) = k3(2,3) + w*x1*y1
            k3(2,4) = k3(2,4) + w*x1*z1
            k3(3,3) = k3(3,3) + w*y1*y1
            k3(3,4) = k3(3,4) + w*y1*z1
            k3(4,4) = k3(4,4) + w*z1*z1
            f(1) = f(1) + w*v1
            f(2) = f(2) + w*v1*x1
            f(3) = f(3) + w*v1*y1
            f(4) = f(4) + w*v1*z1
31          continue
            k3(2,1) = k3(1,2)
            k3(3,1) = k3(1,3)
            k3(3,2) = k3(2,3)
            k3(4,1) = k3(1,4)
            k3(4,2) = k3(2,4)
            k3(4,3) = k3(3,4)
        endif
        if (nx1 .eq. 1) then
            call mgauss('NFVP', k1, f, 2, 2,&
                        1, det, iret)
        else if (nx1.eq.2) then
            call mgauss('NFVP', k2, f, 3, 3,&
                        1, det, iret)
        else if (nx1.eq.3) then
            call mgauss('NFVP', k3, f, 4, 4,&
                        1, det, iret)
        endif
!
        val2 = f(1)
        do 50,ix1 = 1,nx1
        val2 = val2 + f(ix1+1)*x2(ix1)
50      continue
!
    else
        meth2 = method
        call u2mesk('F', 'UTILITAI2_60', 1, meth2)
    endif
!
!
end subroutine
