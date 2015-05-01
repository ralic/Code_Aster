subroutine nmacin(fonact, matass, deppla, cncind)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmpcin.h"
    integer :: fonact(*)
    character(len=19) :: matass, deppla, cncind
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE
!
! BUT : ACTUALISER LES CHARGES CINEMATIQUES DE FACON A CALCULER
!       UNE CORRECTION PAR RAPPORT AU DEPLACEMENT COURANT (DEPPLA)
!
! ----------------------------------------------------------------------
!
!
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  MATASS : SD MATRICE ASSEMBLEE
! IN  DEPPLA : DEPLACEMENT COURANT
! OUT CNCIND : CHAMP DES INCONNUES CINEMATIQUES CORRIGE PAR DEPPLA
!
!
!
!
!
    integer :: neq, i
    aster_logical :: lcine
    integer, pointer :: ccid(:) => null()
    real(kind=8), pointer :: cind(:) => null()
    real(kind=8), pointer :: depla(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- FONCTIONNALITES ACTIVEES
!
    lcine = isfonc(fonact,'DIRI_CINE')
!
    if (lcine) then
        call jelira(cncind(1:19)//'.VALE', 'LONMAX', ival=neq)
        call nmpcin(matass)
        call jeveuo(matass(1:19)//'.CCID', 'L', vi=ccid)
        call jeveuo(deppla(1:19)//'.VALE', 'L', vr=depla)
        call jeveuo(cncind(1:19)//'.VALE', 'E', vr=cind)
!
! ---   CONSTRUCTION DU CHAMP CNCINE QUI RENDRA
! ---   DEPPLA CINEMATIQUEMENT ADMISSIBLE
!
        do 10 i = 1, neq
            if (ccid(i) .eq. 1) then
                cind(i) = cind(i)-depla(i)
            endif
 10     continue
!
    endif
!
    call jedema()
end subroutine
