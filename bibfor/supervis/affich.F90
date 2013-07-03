subroutine affich(nomfic, texte)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
#include "asterc/isjvup.h"
#include "asterfort/iunifi.h"
#include "asterfort/uldefi.h"
    character(len=*) :: texte
    character(len=*) :: nomfic
    integer :: ifm, ier
    logical :: ouvert
!     ----------------------------------------------------------------
    ouvert = .true.
    ifm = iunifi (nomfic)
!
! --- SI JEVEUX N'EST PAS DISPONIBLE (PAS INITIALISE OU FERME)
!     ON SE CONTENTE DU WRITE BRUT
!
    if (isjvup() .eq. 0) then
!
        write(ifm,'(A)') texte
!
    else
!        LE FICHIER EST-IL OUVERT ?
        inquire ( unit=ifm, opened=ouvert, iostat=ier)
        if (ier .eq. 0 .and. .not.ouvert) then
            call uldefi(ifm, ' ', ' ', 'A', 'A',&
                        'O')
        endif
!
        write(ifm,'(A)') texte
!
        if (.not. ouvert) then
            call uldefi(-ifm, ' ', ' ', 'A', 'A',&
                        'O')
        endif
    endif
!
end subroutine
