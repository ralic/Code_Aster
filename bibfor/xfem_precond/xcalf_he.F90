function xcalf_he(he,lsn)
!-----------------------------------------------------------------------
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
!
!-----------------------------------------------------------------------
! BUT : FONCTION SIGNE HEAVISIDE
!-----------------------------------------------------------------------
!
! ARGUMENTS :
!------------
!   - HE  : SIGNE HEAVISIDE
!   - LSN : 
!
!-----------------------------------------------------------------------
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
!
    real(kind=8) :: xcalf_he, he
    real(kind=8) :: lsn, rbid
!
!-----------------------------------------------------------------------
!
    xcalf_he=he
!
    rbid=lsn
!
!    if ( he .eq. 0.d0 ) goto 99
!
!    if (lsn .ne. 0) then
!     if ((lsn*he) .gt. 0) then
!       xcalf_he = 0.d0
!     else
!       xcalf_he = -sign(1.d0,lsn)
!       xcalf_he = 2.d0*he
!     endif
!    else 
!     if (he .gt. 0) then
!       xcalf_he = 2.d0
!     else
!       xcalf_he = 0.d0
!     endif
!    endif
!
!    xcalf_he=he
!
!99   continue
end function
