subroutine liscom(nomo, lischa)
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
    implicit     none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lisico.h"
#include "asterfort/lislch.h"
#include "asterfort/lislco.h"
#include "asterfort/lisnnb.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    character(len=19) :: lischa
    character(len=8) :: nomo
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! VERIFICATION DE LA COHERENCE DES MODELES
!
! ----------------------------------------------------------------------
!
!
! IN  NOMO   : NOM DU MODELE
! IN  LISCHA : SD LISTE DES CHARGES
!
!
!
!
    integer :: ichar, nbchar
    character(len=8) :: modch2, charge, modch1
    integer :: ibid, iret, codcha
    logical :: lveag, lveas
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- NOMBRE DE CHARGES
!
    call lisnnb(lischa, nbchar)
    if (nbchar .eq. 0) goto 999
!
! --- VERIF. PREMIERE CHARGE
!
    ichar = 1
    call lislch(lischa, ichar, charge)
    call lislco(lischa, ichar, codcha)
    lveag = lisico('VECT_ASSE_GENE',codcha)
    lveas = lisico('VECT_ASSE' ,codcha)
    if (nomo .ne. ' ') then
        if (.not.lveag .and. .not.lveas) then
            call dismoi('F', 'NOM_MODELE', charge, 'CHARGE', ibid,&
                        modch1, iret)
            if (modch1 .ne. nomo) call u2mesk('F', 'CHARGES5_5', 1, charge)
        endif
    endif
!
! --- BOUCLE SUR LES CHARGES
!
    do 10 ichar = 2, nbchar
        call lislch(lischa, ichar, charge)
        call lislco(lischa, ichar, codcha)
        lveag = lisico('VECT_ASSE_GENE',codcha)
        lveas = lisico('VECT_ASSE' ,codcha)
        if (nomo .ne. ' ') then
            if (.not.lveag .and. .not.lveas) then
                call dismoi('F', 'NOM_MODELE', charge, 'CHARGE', ibid,&
                            modch2, iret)
                if (modch1 .ne. modch2) call u2mess('F', 'CHARGES5_6')
            endif
        endif
10  end do
!
999  continue
!
    call jedema()
end subroutine
