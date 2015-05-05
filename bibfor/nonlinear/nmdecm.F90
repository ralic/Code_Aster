subroutine nmdecm(sddisc, i_event_acti, nomlis, instam, deltat,&
                  nbrpas, dtmin       , retdec)
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
    implicit     none
#include "asterc/r8gaem.h"
#include "asterfort/assert.h"
#include "asterfort/nmdecc.h"
#include "asterfort/utdidt.h"
    integer :: i_event_acti
    character(len=19) :: sddisc
    character(len=24) :: nomlis
    integer :: nbrpas
    real(kind=8) :: instam, deltat, dtmin
    integer :: retdec
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (GESTION DES EVENEMENTS - DECOUPE)
!
! DECOUPE DU PAS DE TEMPS - CAS MANUEL
!
! ----------------------------------------------------------------------
!
!
! In  sddisc           : datastructure for time discretization
! In  i_event_acti     : index of active event
! IN  NOMLIS : NOM DE LA LISTE DES INSTANTS A AJOUTER
! IN  INSTAM : INSTANT AU DEBUT
! IN  DELTAT : INCREMENT DE TEMPS COURANT
! IN  TYPDEC : TYPE DE DECOUPE
!     'SUBD' - SUBDIVISION PAR UN NOMBRE DE PAS DONNE
!     'DELT' - SUBDIVISION PAR UN INCREMENT DONNE
! OUT NBRPAS : NOMBRE DE PAS DE TEMPS
! OUT DTMIN  : INTERVALLE DE TEMPS MINIMAL SUR LA LISTE CREEE
! OUT RETDEC : CODE RETOUR DECOUPE
!     0 - ECHEC DE LA DECOUPE
!     1 - ON A DECOUPE
!     2 - PAS DE DECOUPE
!
!
!
!
    real(kind=8) :: ratio, deltac
    character(len=16) :: optdec
    character(len=4) :: typdec
!
! ----------------------------------------------------------------------
!
    dtmin = r8gaem()
    retdec = 0
    ratio = 1.d0
    nbrpas = -1
    deltac = -1.d0
    optdec = 'UNIFORME'
    typdec = 'SUBD'
!
! --- DONNEES
!
    if (typdec .eq. 'SUBD') then
        call utdidt('L', sddisc, 'ECHE', 'SUBD_PAS', index_ = i_event_acti, &
                    vali_ = nbrpas)
    else
        ASSERT(.false.)
    endif
!
! --- CONSTRUCTION DE LA LISTE DES INSTANTS
!
    call nmdecc(nomlis, .true._1, optdec, deltat, instam,&
                ratio, typdec, nbrpas, deltac, dtmin,&
                retdec)
!
end subroutine
