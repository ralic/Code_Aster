subroutine nmcvgf(sddisc, sderro, valinc, defico, resoco)
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
#include "asterfort/nmevev.h"
#include "asterfort/nmleeb.h"
    character(len=19) :: sddisc, valinc(*)
    character(len=24) :: sderro
    character(len=24) :: defico, resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! ETAT DE LA CONVERGENCE POINT FIXE
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDERRO : SD GESTION DES ERREURS
! IN  VALINC : VARIABLE CHAPEAU INCREMENTS DES VARIABLES
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE RESOLUTION DU CONTACT
!
!
!
!
    integer :: ievdac, numins
    character(len=4) :: etnewt, etfixe
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    numins = -1
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
            call assert(.false.)
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
    if (etfixe .eq. 'STOP') goto 99
!
! --- DETECTION DU PREMIER EVENEMENT DECLENCHE
!
    call nmevev(sddisc, numins, valinc, sderro, defico,&
                resoco, 'FIXE')
!
! --- UN EVENEMENT SE DECLENCHE
!
    call nmacto(sddisc, ievdac)
    if (ievdac .gt. 0) call nmeceb(sderro, 'FIXE', 'EVEN')
!
99  continue
!
    call jedema()
end subroutine
