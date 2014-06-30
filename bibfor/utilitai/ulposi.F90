subroutine ulposi(unit, posi, ierr)
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
! person_in_charge: j-pierre.lefebvre at edf.fr
    implicit none
#include "asterfort/utmess.h"
    character(len=*) :: posi
    integer :: unit, ierr
!     ------------------------------------------------------------------
!
!     POSITIONNEMENT DANS UN FICHIER ET VERIFICATION DE L'ETAT
!     APRES OPEN
!
! IN  : UNIT   : NUMERO D'UNITE LOGIQUE
!       POSI   : N = LE FICHIER EST ECRASE (NEW)
!                O = ON NE FAIT RIEN (ASSIS/OLD)
!                A = ON SE PLACE EN FIN DE FICHIER (APPEND)
! OUT : IERR   : CODE RETOUR D'ERREUR (OK  = 0)
!
!     ------------------------------------------------------------------
    character(len=16) :: kacc
    character(len=4) :: k4b
    character(len=1) :: k1
    character(len=24) :: valk(2)
    integer :: ios, iend
    logical(kind=1) :: lop, lnom
!     ------------------------------------------------------------------
!
    ierr = 100
    k1 = posi
    write(k4b,'(I2)') unit
!
    inquire(unit=unit, opened=lop, named=lnom, access=kacc)
    if (lop) then
        if (kacc .ne. 'SEQUENTIAL') then
            ierr=101
            valk(1) = kacc
            valk(2) = k4b
            call utmess('E', 'UTILITAI5_24', nk=2, valk=valk)
        else
            if (.not. lnom) then
                ierr=102
                call utmess('E', 'UTILITAI5_25', sk=k4b)
            endif
        endif
    else
        ierr=103
        call utmess('E', 'UTILITAI5_26', sk=k4b)
    endif
!
    if (posi .eq. 'N') then
        rewind (unit=unit, iostat=ios)
        if (ios .eq. 0) then
            ierr = 0
        else
            ierr = 104
            call utmess('E', 'UTILITAI5_27', sk=k4b)
        endif
    else if (posi .eq. 'O') then
        ierr = 0
    else if (posi .eq. 'A') then
!       POSITIONNEMENT EN FIN DE FICHIER
!
201      continue
        iend=0
        if (iend .le. 0) then
            read (unit,*,end=301)
            goto 201
        endif
301      continue
        ierr = 0
        backspace unit
    else
        ierr = 105
        valk(1) = k1
        valk(2) = k4b
        call utmess('E', 'UTILITAI5_28', nk=2, valk=valk)
    endif
!
end subroutine
