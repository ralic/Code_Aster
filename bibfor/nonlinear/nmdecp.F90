subroutine nmdecp(sddisc, iterat, ievdac, typdec, nbrpas,&
                  deltac, ratio, optdec, ldcext, durdec,&
                  retdec)
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmdcae.h"
#include "asterfort/nmdcco.h"
#include "asterfort/utdidt.h"
    character(len=19) :: sddisc
    integer :: ievdac, iterat, retdec
    integer :: nbrpas
    aster_logical :: ldcext
    real(kind=8) :: ratio, deltac, durdec
    character(len=4) :: typdec
    character(len=16) :: optdec
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (GESTION DES EVENEMENTS - DECOUPE)
!
! PARAMETRES DE DECOUPE
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  ITERAT : NUMERO D'ITERATION DE NEWTON
! IN  IEVDAC : INDICE DE L'EVENEMENT ACTIF
! OUT RATIO  : RATIO DU PREMIER PAS DE TEMPS
! OUT TYPDEC : TYPE DE DECOUPE
!              'SUBD' - SUBDIVISION PAR UN NOMBRE DE PAS DONNE
!              'DELT' - SUBDIVISION PAR UN INCREMENT DONNE
! OUT NBRPAS : NOMBRE DE PAS DE TEMPS
! OUT DELTAC : INCREMENT DE TEMPS CIBLE
! OUT OPTDEC : OPTION DE DECOUPE
!     'UNIFORME'   - DECOUPE REGULIERE ET UNIFORME
!     'PROGRESSIF' - DECOUPE EN DEUX ZONES, UN PAS LONG+ UNE SERIE
!                    DE PAS UNIFORMES
!     'DEGRESSIF'  - DECOUPE EN DEUX ZONES, UNE SERIE DE PAS
!                    UNIFORMES + UN PAS LONG
! OUT RETDEC : CODE RETOUR DECOUPE
!     0 - ECHEC DE LA DECOUPE
!     1 - ON A DECOUPE
!     2 - PAS DE DECOUPE
! OUT LDCEXT : .TRUE. SI ON DOIT CONTINUER LA DECOUPE
! OUT DURDEC : DUREEE DE LA DECOUPE APRES (SI LDCEXT =.TRUE.)
!
!
!
!
    integer :: ibid
    real(kind=8) :: r8bid
    character(len=16) :: subaut
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    retdec = 0
    optdec = ' '
    ratio = 0.d0
    nbrpas = -1
    deltac = -1.d0
    typdec = ' '
    optdec = ' '
    ldcext = .false.
    durdec = -1.d0
!
! --- TYPE DE DECOUPAGE AUTO
!
    call utdidt('L', sddisc, 'ECHE', ievdac, 'SUBD_METHODE_AUTO',&
                r8bid, ibid, subaut)
!
! --- PARAMETRES SUIVANT DECOUPE
!
    if (subaut .eq. 'EXTRAPOLE') then
        call nmdcae(sddisc, iterat, typdec, nbrpas, ratio,&
                    optdec, retdec)
    else if (subaut.eq.'COLLISION') then
        call nmdcco(sddisc, ievdac, typdec, nbrpas, deltac,&
                    ratio, optdec, retdec, ldcext, durdec)
    else
        ASSERT(.false.)
    endif
!
    call jedema()
end subroutine
