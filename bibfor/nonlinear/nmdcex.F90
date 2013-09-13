subroutine nmdcex(sddisc, insref, durdec, ievdac, deltac,&
                  retdex)
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/diinst.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmdcdc.h"
#include "asterfort/nmdecc.h"
#include "asterfort/nmdecv.h"
#include "asterfort/nmfinp.h"
#include "asterfort/utmess.h"
    character(len=19) :: sddisc
    integer :: ievdac, retdex
    real(kind=8) :: durdec, insref, deltac
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (GESTION DES EVENEMENTS - DECOUPE)
!
! EXTENSION DE LA DECOUPE AUX INSTANTS SUIVANTS - MANUEL
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  INSREF : INSTANT AU-DELA DUQUEL ON ETEND LA DECOUPE
! IN  DURDEC : DUREEE DE L'EXTENSION DE LA DECOUPE
! IN  DELTAC : INCREMENT DE TEMPS CIBLE
! IN  IEVDAC : INDICE DE L'EVENEMENT ACTIF
! OUT RETDEC : CODE RETOUR DECOUPE
!     0 - ECHEC DE LA DECOUPE
!     1 - ON A DECOUPE
!     2 - PAS DE DECOUPE
!
!
!
!
    integer :: numins
    logical :: lstop
    real(kind=8) :: instam, instap, deltat, insfin
    real(kind=8) :: dtmin, ratio
    real(kind=8) :: valr(2)
    integer :: nbrpas
    logical :: ldeco
    character(len=4) :: typdec
    character(len=24) :: nomlis
    character(len=16) :: optdec
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    optdec = ' '
    ldeco = .false.
    nbrpas = -1
    typdec = 'DELT'
    ratio = 1.d0
    nomlis = '&&NMDCEX.NOMLIS'
    retdex = 0
    ASSERT(durdec.gt.0.d0)
    insfin = insref+durdec
!
! --- AFFICHAGE
!
    valr(1) = insref
    valr(2) = durdec
    call utmess('I', 'SUBDIVISE_13', nr=2, valr=valr)
!
    numins = 1
!
10  continue
!
! ----- INFORMATIONS SUR LE PAS DE TEMPS
!
    instam = diinst(sddisc,numins-1)
    instap = diinst(sddisc,numins)
    deltat = instap-instam
    if ((instam.ge.insref) .and. (instam.le.insfin)) then
        if (deltat .gt. deltac) then
            if (instap .gt. insfin) then
                optdec = 'DEGRESSIF'
                ratio = (instap-insfin)/deltat
            else
                optdec = 'UNIFORME'
            endif
!
! --------- DECOUPE
!
            call nmdecc(nomlis, .false., optdec, deltat, instam,&
                        ratio, typdec, nbrpas, deltac, dtmin,&
                        retdex)
            if (retdex .eq. 0) goto 999
            if (retdex .eq. 2) goto 888
!
! --------- VERIFICATIONS DE LA DECOUPE
!
            call nmdecv(sddisc, numins, ievdac, dtmin, retdex)
            if (retdex .eq. 0) goto 999
!
! --------- MISE A JOUR DES SD APRES DECOUPE
!
            call nmdcdc(sddisc, numins, nomlis, nbrpas)
            ldeco = .true.
888          continue
            call jedetr(nomlis)
        endif
    endif
!
    call nmfinp(sddisc, numins, lstop)
    if (lstop) goto 99
    numins = numins + 1
    goto 10
!
99  continue
!
    if (ldeco) then
        retdex = 1
    else
        retdex = 2
    endif
!
999  continue
!
    if (retdex .eq. 0) then
!
    else if (retdex.eq.1) then
        call utmess('I', 'SUBDIVISE_14', sr=insfin)
    else if (retdex.eq.2) then
        call utmess('I', 'SUBDIVISE_15', sr=insref)
    else
        ASSERT(.false.)
    endif
!
    call jedema()
end subroutine
