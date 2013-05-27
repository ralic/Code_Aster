function gasdev()
    implicit none
    real(kind=8) :: gasdev
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     GENERATEUR DE VARIABLE ALEATOIRE DE LOI GAUSSIENNE
! ----------------------------------------------------------------------
    include 'asterc/getran.h'
    integer :: iset
    real(kind=8) :: fac, gset, rsq, v1, v2, u, v
    save    iset, gset
    data    iset /0/
! DEB ------------------------------------------------------------------
!
    if (iset .eq. 0) then
 1      continue
        call getran(u)
        v1 = 2d0*u-1d0
        call getran(v)
        v2 = 2d0*v-1d0
        rsq = v1**2+v2**2
        if (rsq .ge. 1.d0 .or. rsq .eq. 0.d0) goto 1
        fac = sqrt(-2d0*log(rsq)/rsq)
        gset = v1*fac
        gasdev = v2*fac
        iset = 1
    else
        gasdev = gset
        iset = 0
    endif
!
end function
