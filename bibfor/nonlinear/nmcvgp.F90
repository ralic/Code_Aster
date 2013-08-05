subroutine nmcvgp(sddisc, numins, sderro, valinc, fonact,&
                  defico, resoco)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmacto.h"
#include "asterfort/nmeceb.h"
#include "asterfort/nmevcv.h"
#include "asterfort/nmevev.h"
#include "asterfort/nmleeb.h"
    integer :: fonact(*)
    character(len=19) :: sddisc, valinc(*)
    integer :: numins
    character(len=24) :: sderro
    character(len=24) :: defico, resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! ETAT DE LA CONVERGENCE DU PAS DE TEMPS
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  NUMINS : NUMERO D'INSTANT
! IN  SDERRO : SD GESTION DES ERREURS
! IN  VALINC : VARIABLE CHAPEAU INCREMENTS DES VARIABLES
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE RESOLUTION DU CONTACT
!
!
!
!
    integer :: ievdac
    character(len=4) :: etfixe
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ETAT DE LA BOUCLE DE POINT FIXE
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
    call nmevev(sddisc, numins, valinc, sderro, defico,&
                resoco, 'INST')
!
! --- UN EVENEMENT SE DECLENCHE
!
    call nmacto(sddisc, ievdac)
    if (ievdac .gt. 0) call nmeceb(sderro, 'INST', 'EVEN')
!
99  continue
!
    call jedema()
end subroutine
