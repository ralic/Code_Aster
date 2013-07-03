subroutine irmifr(ifmis, freq, ifreq, nfreq, ic)
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
    implicit none
#include "asterc/r8prem.h"
    integer :: ifmis, ifreq, nfreq, ic
    real(kind=8) :: freq
!
    character(len=72) :: texte
    real(kind=8) :: a(3)
!
!-----------------------------------------------------------------------
    integer :: i, j
!-----------------------------------------------------------------------
    rewind ifmis
    read(ifmis,'(A72)') texte
    read(ifmis,'(A72)') texte
    ic = 1
    nfreq = 0
 1  continue
    nfreq = nfreq + 1
    read(ifmis,'(A72)') texte
    if (texte(1:3) .eq. 'CHA') then
        nfreq = nfreq -1
    else
        goto 1
    endif
    rewind ifmis
    read(ifmis,'(A72)') texte
    read(ifmis,'(A72)') texte
    do 3 i = 1, nfreq
        read(ifmis,*) (a(j),j=1,3)
        if (freq .le. (a(1)*1.0001d0)) then
            ifreq = i
            if (i .gt. 1 .and. freq .lt. (a(1)*0.9999d0)) then
                ifreq = ifreq-1
            endif
            if (freq .le. r8prem( )) ic = 2
            if (i .eq. 1 .and. nfreq .eq. 1) ic = 0
            if (i .eq. nfreq .and. freq .ge. (a(1)*0.9999d0)) then
                ic = 0
                ifreq = nfreq
            endif
            goto 4
        endif
 3  end do
    ifreq = nfreq
    ic = 0
 4  continue
end subroutine
