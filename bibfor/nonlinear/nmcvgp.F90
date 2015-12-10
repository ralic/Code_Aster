subroutine nmcvgp(sddisc    , nume_inst, sderro, valinc, fonact,&
                  ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/nmacto.h"
#include "asterfort/nmeceb.h"
#include "asterfort/nmevcv.h"
#include "asterfort/nmevev.h"
#include "asterfort/nmleeb.h"
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
    integer, intent(in) :: fonact(*)
    character(len=19), intent(in) :: sddisc
    character(len=19), intent(in) :: valinc(*)
    integer, intent(in) :: nume_inst
    character(len=24), intent(in) :: sderro
    type(NL_DS_Contact), intent(in) :: ds_contact
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! ETAT DE LA CONVERGENCE DU PAS DE TEMPS
!
! ----------------------------------------------------------------------
!
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  NUMINS : NUMERO D'INSTANT
! IN  SDERRO : SD GESTION DES ERREURS
! IN  VALINC : VARIABLE CHAPEAU INCREMENTS DES VARIABLES
! IN  FONACT : FONCTIONNALITES ACTIVEES
! In  ds_contact       : datastructure for contact management
!
    integer :: ievdac
    character(len=4) :: etfixe
!
! ----------------------------------------------------------------------
!
    call nmleeb(sderro, 'FIXE', etfixe)
!
! --- SI PAS DE CONVERGENCE DU POINT FIXE -> TRANSFERT ETAT DE LA BOUCLE
!
    if (etfixe .ne. 'CONV') then
        if (etfixe .eq. 'STOP') then
            call nmeceb(sderro, 'INST', 'STOP')
        else if (etfixe.eq.'ERRE') then
            call nmeceb(sderro, 'INST', 'ERRE')
        else
            ASSERT(.false.)
        endif
        goto 99
    endif
!
! --- EVALUATION CONVERGENCE PAS DE TEMPS
!
    call nmevcv(sderro, fonact, 'INST')
!
! --- DETECTION DU PREMIER EVENEMENT DECLENCHE
!
    call nmevev(sddisc, nume_inst, valinc, sderro, ds_contact,&
                'INST')
!
! --- UN EVENEMENT SE DECLENCHE
!
    call nmacto(sddisc, ievdac)
    if (ievdac .gt. 0) then
        call nmeceb(sderro, 'INST', 'EVEN')
    endif
!
99  continue
!
end subroutine
