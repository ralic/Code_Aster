subroutine nmadir(numedd, fonact, ds_contact, veasse, vediri,&
                  cndiri)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assvec.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/nmasco.h"
#include "asterfort/nmdebg.h"
#include "asterfort/vtaxpy.h"
#include "asterfort/vtzero.h"
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
    integer :: fonact(*)
    character(len=24) :: numedd
    character(len=19) :: veasse(*)
    character(len=19) :: vediri, cndiri
    type(NL_DS_Contact), intent(in) :: ds_contact
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! ASSEMBLAGE DU VECTEUR DES REACTIONS D'APPUI BT.LAMBDA
!
! ----------------------------------------------------------------------
!
! IN  NUMEDD : NOM DE LA NUMEROTATION
! In  ds_contact       : datastructure for contact management
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  VEDIRI : VECT_ELEM DES REACTIONS D'APPUIS
! OUT CNDIRI : VECT_ASSE DES REACTIONS D'APPUIS
!
!
!
!
    integer :: ifm, niv
    character(len=1) :: base
    character(len=19) :: cncont
    aster_logical :: lcont
!
! ----------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... ASSEMBLAGE DES REAC. APPUI'
    endif
!
! --- INITIALISATIONS
!
    base = 'V'
    cncont = '&&CNCHAR.DUMM'
    lcont = isfonc(fonact,'CONTACT')
    call vtzero(cndiri)
    call vtzero(cncont)
!
! --- CONTRIBUTIONS DU CONTACT
!
    if (lcont) then
        call nmasco('CNDIRI', fonact, ds_contact, veasse, cncont)
    endif
!
! --- ASSEMBLAGE DES REACTIONS D'APPUI
!
    call assvec(base, cndiri, 1, vediri, [1.d0],&
                numedd, ' ', 'ZERO', 1)
!
! --- CONTRIBUTIONS DU CONTACT
!
    if (lcont) then
        call vtaxpy(1.d0, cncont, cndiri)
    endif
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        call nmdebg('VECT', cndiri, 6)
    endif
!
end subroutine
