subroutine nmdcco(sddisc, i_event_acti, typdec, nbrpas, deltac,&
                  ratio , optdec      , retdec, ldcext, subdur)
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
    implicit none
#include "asterf_types.h"
#include "asterfort/utdidt.h"
    character(len=19) :: sddisc
    integer :: i_event_acti, nbrpas, retdec
    real(kind=8) :: ratio, deltac, subdur
    aster_logical :: ldcext
    character(len=4) :: typdec
    character(len=16) :: optdec
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (GESTION DES EVENEMENTS - DECOUPE)
!
! PARAMETRES DE DECOUPE - COLLISION
!
! ----------------------------------------------------------------------
!
! In  sddisc           : datastructure for time discretization
! In  i_event_acti     : index of active event
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
! OUT SUBDUR : DUREEE DE LA DECOUPE APRES (SI LDCEXT =.TRUE.)
!
!
!
!
    real(kind=8) :: subins
!
! ----------------------------------------------------------------------
!
    retdec = 0
    ratio = 1.d0
    nbrpas = -1
    optdec = 'UNIFORME'
    typdec = ' '
    ldcext = .false.
    subdur = -1.d0
!
! --- PARAMETRES
!
    call utdidt('L', sddisc, 'ECHE', 'SUBD_INST', index_ = i_event_acti,&
                valr_ = subins)
    call utdidt('L', sddisc, 'ECHE', 'SUBD_DUREE', index_ = i_event_acti,&
                valr_ = subdur)
    typdec = 'DELT'
    deltac = subins
    ldcext = .true.
    retdec = 1

end subroutine
