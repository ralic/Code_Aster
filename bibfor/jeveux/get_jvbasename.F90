subroutine get_jvbasename(bas_, numext, path)
    implicit none
!
    character(len=*), intent(in) :: bas_
    integer, intent(in) :: numext
    character(len=*), intent(out) :: path
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
! aslint: disable=W1303
! for the path name
! person_in_charge: j-pierre.lefebvre at edf.fr
!
! Return the path name to the file of the base of class `bas_`
! `bas_` is one of 'globale', 'volatile', 'elembase'
!
! ----------------------------------------------------------------------
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/lxlgut.h"
#include "asterfort/lxmins.h"
#include "asterfort/utmess.h"
!
    character(len=128) :: repglo, repvol
    common /banvje/ repglo, repvol
    integer :: lrepgl, lrepvo
    common /balvje/ lrepgl,lrepvo
!
    integer :: nchar
    character(len=4) :: base
    character(len=8) :: fname
    character(len=512) :: dir, nom512
!
    base = bas_
    call lxmins(base)
    fname = base//'.'

    ASSERT(numext >= -2)
    if (numext > 0) then
        call codent(numext, 'G', fname(6:7))
    elseif (numext == -1) then
        fname(6:7) = '?'
    elseif (numext == -2) then
        fname(6:7) = '*'
    endif

!TODO Use environment variables for glob & vola directories
    dir = '.'
    if (base .eq. 'glob') then
        dir = repglo(1:lrepgl)
    else if (base .eq. 'vola') then
        dir = repvol(1:lrepvo)
    else if (base .eq. 'elem') then
        call get_environment_variable('ASTER_ELEMENTSDIR', dir, nchar)
        if (nchar > 512 - 9) then
            call utmess('F', 'JEVEUX_3', sk='ASTER_ELEMENTSDIR', si=512 - 9)
        elseif (nchar == 0) then
            dir = '.'
            nchar = 1
        endif
    endif
    nom512 = dir(1:lxlgut(dir))//'/'//fname
    ASSERT(len(path) >= lxlgut(nom512))
    path = nom512
end subroutine
