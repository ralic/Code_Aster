function dcargu(c)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! CALCUL DE L'ARGUMENT D'UN NOMBRE COMPLEXE
! PAR CONVENTION ON AFFECTE 0.D0 SI C = (0.D0,0.D0)
!-----------------------------------------------------------------------
!  IN : C : NOMBRE COMPLEXE DONT ON VEUT CALCULER L'ARGUMENT
!-----------------------------------------------------------------------
    include 'asterc/r8pi.h'
    real(kind=8) :: dcargu
    complex(kind=8) :: c
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    real(kind=8) :: pi
!-----------------------------------------------------------------------
    pi = r8pi()
!
    if (dble(c) .eq. 0.d0) then
        if (dimag(c) .gt. 0.d0) then
            dcargu = pi/2.d0
        else if (dimag(c).lt.0.d0) then
            dcargu = -pi/2.d0
        else
            dcargu = 0.d0
        endif
    else if (dble(c).gt.0.d0) then
        dcargu = dble(atan2(dimag(c),dble(c)))
    else if (dble(c).lt.0.d0) then
        dcargu = dble(atan2(dimag(c),dble(c))) + pi
    endif
    if (dcargu .lt. 0.d0) dcargu = dcargu + 2.d0*pi
!
end function
