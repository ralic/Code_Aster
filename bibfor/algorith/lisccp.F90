subroutine lisccp(phenom, lischa)
!
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
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lisico.h"
#include "asterfort/lislch.h"
#include "asterfort/lislco.h"
#include "asterfort/lisnnb.h"
#include "asterfort/utmess.h"
    character(len=16) :: phenom
    character(len=19) :: lischa
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! VERIFICATION COMPATIBILITE CHARGE/PHENOMENE
!
! ----------------------------------------------------------------------
!
!
! IN  PHENOM : TYPE DE PHENOMENE (MECANIQUE, THERMIQUE, ACOUSTIQUE)
! IN  LISCHA : SD LISTE DES CHARGES
!
! ----------------------------------------------------------------------
!
    integer :: ichar, nbchar
    integer :: genrec
    character(len=8) :: phecha, charge
    aster_logical :: lok
    aster_logical :: lveac, lveag, lveas
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! - NOMBRE DE CHARGES
!
    call lisnnb(lischa, nbchar)
    if (nbchar .eq. 0) goto 999
!
! - BOUCLE SUR LES CHARGES
!
    do ichar = 1, nbchar
        lok = .false.
!
! ----- CODE DU GENRE DE LA CHARGE
!
        call lislco(lischa, ichar, genrec)
        lveac = lisico('VECT_ASSE_CHAR',genrec)
        lveag = lisico('VECT_ASSE_GENE',genrec)
        lveas = lisico('VECT_ASSE' ,genrec)
!
! ----- PHENOMENE DE LA CHARGE
!
        if (lveac .or. lveas .or. lveag) then
            phecha = ' '
        else
            call lislch(lischa, ichar, charge)
            call dismoi('TYPE_CHARGE', charge, 'CHARGE', repk=phecha)
        endif
!
        if (phenom .eq. 'MECANIQUE') then
            if ((phecha(1:4).eq.'MECA') .or. (phecha(1:4).eq.'CIME') .or.&
                (lveac.or.lveas.or.lveag)) then
                lok = .true.
            endif
        else if (phenom.eq.'THERMIQUE') then
            if ((phecha(1:4).eq.'THER') .or. (phecha(1:4).eq.'CITH')) then
                lok = .true.
            endif
        else if (phenom.eq.'ACOUSTIQUE') then
            if ((phecha(1:4).eq.'ACOU') .or. (phecha(1:4).eq.'CIAC')) then
                lok = .true.
            endif
        else
            ASSERT(.false.)
        endif
!
        if (.not.lok) then
            call utmess('F', 'CHARGES5_4', sk=charge)
        endif
    end do
!
999 continue
!
    call jedema()
end subroutine
