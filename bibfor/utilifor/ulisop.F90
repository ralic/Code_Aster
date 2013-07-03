function ulisop(unit, name)
    implicit none
    integer :: ulisop
#include "asterfort/u2mesk.h"
#include "asterfort/ulinit.h"
    integer :: unit
    character(len=16) :: name
!     ------------------------------------------------------------------
!     RETOURNE LE NOM LOCAL NAME (DDNAME) ASSOCIE A L'UNITE LOGIQUE
!     ------------------------------------------------------------------
!
! OUT  NAME     : CH*16 : NOM "LOCALE" ASSOCIE AU NUMERO D'UNITE LOGIQUE
!                         FORTRAN
!      ULISOP   : IS    : 0 L'UNITE LOGIQUE N'EST PAS OUVERTE
!                         UNIT SINON
! IN   UNIT     : IS    : NUMERO D'UNITE LOGIQUE ASSOCIE A "NAME"
!
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: j-pierre.lefebvre at edf.fr
!
    integer :: mxf
    parameter       (mxf=100)
    character(len=1) :: typefi(mxf), accefi(mxf), etatfi(mxf), modifi(mxf)
    character(len=16) :: ddname(mxf)
    character(len=255) :: namefi(mxf)
    integer :: first, unitfi(mxf), nbfile
    common/ asgfi1 / first, unitfi      , nbfile
    common/ asgfi2 / namefi,ddname,typefi,accefi,etatfi,modifi
!
    character(len=8) :: k8bid
    integer :: i, ival
!
    if (first .ne. 17111990) call ulinit()
!
    if (unit .lt. 0) then
        write(k8bid,'(I4)') unit
        call u2mesk('F', 'UTILITAI5_9', 1, k8bid)
    endif
    name = '?'
    ival = 0
    do 1 i = 1, nbfile
        if (unitfi(i) .eq. unit .and. etatfi(i) .eq. 'O') then
            name = ddname(i)
            ival = i
            goto 2
        endif
 1  end do
 2  continue
    ulisop = ival
end function
