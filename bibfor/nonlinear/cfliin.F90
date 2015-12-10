subroutine cfliin(noma, ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/cfecrd.h"
#include "asterfort/cfimp1.h"
#include "asterfort/cfinal.h"
#include "asterfort/cfinnl.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
!
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
    character(len=8), intent(in) :: noma
    type(NL_DS_Contact), intent(in) :: ds_contact
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISCRETE - ALGORITHME)
!
! LIAISONS INITIALES
!
! ----------------------------------------------------------------------
!
! IN  NOMA   : NOM DU MAILLAGE
! In  ds_contact       : datastructure for contact management
!
    integer :: ifm, niv
    character(len=24) :: clreac
    integer :: jclrea
    aster_logical :: reageo, reapre
    integer :: nbliac, llf, llf1, llf2
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ......... DETECTION DES LIAISONS INITIALES'
    endif
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    clreac = ds_contact%sdcont_solv(1:14)//'.REAL'
    call jeveuo(clreac, 'L', jclrea)
!
! --- PARAMETRES DE REACTUALISATION
!
    reageo = zl(jclrea-1+1)
    reapre = zl(jclrea-1+3)
!
! --- NOMBRE DE LIAISONS INITIALES
!
    call cfinnl(ds_contact, reageo, nbliac, llf,&
                llf1, llf2)
!
! --- ACTIVATION DES LIAISONS INITIALES
!
    call cfinal(ds_contact, reapre, reageo, nbliac,&
                llf, llf1, llf2)
!
! --- STOCKAGE DES VARIABLES DE CONTROLE DU CONTACT
!
    call cfecrd(ds_contact%sdcont_solv, 'NBLIAC', nbliac)
    call cfecrd(ds_contact%sdcont_solv, 'LLF', llf)
    call cfecrd(ds_contact%sdcont_solv, 'LLF1', llf1)
    call cfecrd(ds_contact%sdcont_solv, 'LLF2', llf2)
!
! --- AFFICHAGES
!
    if (niv .ge. 2) then
        call cfimp1('INI', noma, ds_contact%sdcont_defi, ds_contact%sdcont_solv, ifm)
    endif
!
    call jedema()
!
end subroutine
