function dimge1(ige1, ige2)
    implicit none
    integer :: dimge1
#include "asterfort/assert.h"
    integer :: ige1, ige2
! ----------------------------------------------------------------------
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
! ======================================================================
!     -- SERT AU CALCUL DE DIM_GEOM D'UN LIGREL
!    IN:
!       IGE1 : VALEUR DE DIM_GEOM (/1/2/3/120/023/103/123)
!       IGE2 : VALEUR DE DIM_GEOM (/1/2/3/120/023/103/123)
!    OUT:
!       DIMGE1   : "CUMUL" DE IGE1 ET IGE2
!
! ----------------------------------------------------------------------
    integer :: i1(3), i2(3), i3(3), k
! DEB ------------------------------------------------------------------
!
!     -- SI IGE1 (OU IGE2) = 0, C'EST FACILE :
    if (ige1 .eq. 0) then
        dimge1=ige2
        goto 9999
    endif
    if (ige2 .eq. 0) then
        dimge1=ige1
        goto 9999
    endif
!
!     -- ON DECODE IGE1 DANS I1 :
    do 1, k=1,3
    i1(k)=0
    1 end do
    if (ige1 .gt. 10) then
        i1(1)=ige1/100
        i1(3)=mod(ige1,10)
        i1(2)=(mod(ige1,100) -i1(3))/10
    else
        ASSERT(ige1.ge.0.and.ige1.le.3)
        if (ige1 .eq. 1) i1(1)=1
        if (ige1 .eq. 2) i1(2)=2
        if (ige1 .eq. 3) i1(3)=3
    endif
!
!     -- ON DECODE IGE2 DANS I2 :
    do 2, k=1,3
    i2(k)=0
    2 end do
    if (ige2 .gt. 10) then
        i2(1)=ige2/100
        i2(3)=mod(ige2,10)
        i2(2)=(mod(ige2,100) -i2(3))/10
    else
        ASSERT(ige2.ge.0.and.ige2.le.3)
        if (ige2 .eq. 1) i2(1)=1
        if (ige2 .eq. 2) i2(2)=2
        if (ige2 .eq. 3) i2(3)=3
    endif
!
!     -- ON CALCULE I3 :
    do 3, k=1,3
    ASSERT(i1(k).eq.0 .or. i1(k).eq.k)
    ASSERT(i2(k).eq.0 .or. i2(k).eq.k)
    i3(k)=max(i1(k),i2(k))
    3 end do
!
!     -- ON RECODE LE RESULTAT :
    if ((i3(1)+i3(2)+i3(3)) .gt. 1) then
        dimge1=100*i3(1)
        dimge1=dimge1+10*i3(2)
        dimge1=dimge1+1*i3(3)
    else
        if (i3(1) .eq. 1) dimge1=1
        if (i3(2) .eq. 1) dimge1=2
        if (i3(3) .eq. 1) dimge1=3
    endif
!
9999  continue
!
end function
