subroutine nmcvgf(sddisc, sderro, valinc, ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/nmacto.h"
#include "asterfort/nmeceb.h"
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
    character(len=19), intent(in) :: sddisc
    character(len=19), intent(in) :: valinc(*)
    character(len=24), intent(in) :: sderro
    type(NL_DS_Contact), intent(in) :: ds_contact
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! ETAT DE LA CONVERGENCE POINT FIXE
!
! ----------------------------------------------------------------------
!
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDERRO : SD GESTION DES ERREURS
! IN  VALINC : VARIABLE CHAPEAU INCREMENTS DES VARIABLES
! In  ds_contact       : datastructure for contact management
!
    integer :: ievdac, nume_inst
    character(len=4) :: etnewt, etfixe
!
! ----------------------------------------------------------------------
!
    nume_inst = -1
!
! --- ETAT DE LA BOUCLE DE NEWTON
!
    call nmleeb(sderro, 'NEWT', etnewt)
!
! --- SI PAS DE CONVERGENCE DE NEWTON -> TRANSFERT ETAT DE LA BOUCLE
!
    if (etnewt .ne. 'CONV') then
        if (etnewt .eq. 'STOP') then
            call nmeceb(sderro, 'FIXE', 'STOP')
        else if (etnewt.eq.'ERRE') then
            call nmeceb(sderro, 'FIXE', 'ERRE')
        else
            ASSERT(.false.)
        endif
        goto 99
    endif
!
! --- ETAT DE LA BOUCLE DE POINT FIXE
!
    call nmleeb(sderro, 'FIXE', etfixe)
!
! --- ERREUR FATALE -> ON NE PEUT RIEN FAIRE
!
    if (etfixe .eq. 'STOP') then
        goto 99
    endif
!
! --- DETECTION DU PREMIER EVENEMENT DECLENCHE
!
    call nmevev(sddisc, nume_inst, valinc, sderro, ds_contact,&
                'FIXE')
!
! --- UN EVENEMENT SE DECLENCHE
!
    call nmacto(sddisc, ievdac)
    if (ievdac .gt. 0) then
        call nmeceb(sderro, 'FIXE', 'EVEN')
    endif
!
99  continue
!
end subroutine
