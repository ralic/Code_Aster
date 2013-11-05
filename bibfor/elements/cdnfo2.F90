subroutine cdnfo2(mater, kfonc, xx, dn, fxx,&
                  ier)
!
    implicit none
! ======================================================================
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
#include "asterfort/rcvale.h"
    integer :: dn, ier
    character(len=8) :: kfonc, kaux, mater, k8b
    real(kind=8) :: xx, fxx, wfxx(1)
    integer :: icodr2(1)
    character(len=16) :: phenom
!
    phenom = 'GLRC_DAMAGE'
    k8b = 'X '
    ier = 0
    if (dn .eq. 0) then
!
        call rcvale(mater, phenom, 1, k8b, [xx],&
                    1, kfonc, wfxx, icodr2(1), 1)
    else if (dn .eq. 1) then
!
        write (kaux,'(A1,A7)') 'D',kfonc(1:7)
        call rcvale(mater, phenom, 1, k8b, [xx],&
                    1, kaux, wfxx, icodr2(1), 1)
    else if (dn .eq. 2) then
!
        write (kaux,'(A2,A6)') 'DD',kfonc(1:6)
        call rcvale(mater, phenom, 1, k8b, [xx],&
                    1, kaux, wfxx, icodr2(1), 1)
    else
        ier = 3
    endif
    if (ier .eq. 0) then
        fxx = wfxx(1)
    endif
!
end subroutine
