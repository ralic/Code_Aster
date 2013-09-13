subroutine dflldc(mcfact, iechec, dtmin, even, submet,&
                  subaut, pasmin, nbrpas, niveau, subins,&
                  subdur)
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
!
    implicit none
#include "asterfort/assert.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/utmess.h"
    character(len=16) :: mcfact, even
    integer :: iechec
    real(kind=8) :: pasmin, dtmin, subins, subdur
    character(len=16) :: submet, subaut
    integer :: nbrpas, niveau
!
! ----------------------------------------------------------------------
!
! OPERATEUR DEFI_LIST_INST
!
! LECTURE DES PARAMETRES DE L'ACTION DE TYPE DECOUPE DU PAS DE TEMPS
!
! ----------------------------------------------------------------------
!
!
! IN  MCFACT : MOT-CLEF FACTEUR POUR LIRE L'ECHEC
! IN  IECHEC : NUMERO OCCURRENCE ECHEC
! IN  EVEN   : NOM DE L'EVENEMENT
! IN  DTMIN  : INCREMENT MINIMUM DANS LA LISTE D'INSTANT
! OUT SUBMET : TYPE DE SUBDIVISION
! OUT SUBAUT : TYPE DE SUBDIVISION AUTOMATIQUE
! OUT PASMIN : VALEUR DE SUBD_PAS_MINI
! OUT NBRPAS : VALEUR DE SUBD_PAS
! OUT NIVEAU : VALEUR DE SUBD_NIVEAU
! OUT SUBINS : VALEUR DE SUBD_INST
! OUT SUBDUR : VALEUR DE SUBD_DUREE
!
! ----------------------------------------------------------------------
!
    integer :: iret
!
! ----------------------------------------------------------------------
!
!
!
! --- INITIALISATIONS
!
    submet = ' '
    subaut = ' '
    niveau = 0
    nbrpas = 4
    pasmin = 0.d0
    subins = 0.d0
    subdur = 0.d0
!
! --- TYPE DE LA DECOUPE
!
    call getvtx(mcfact, 'SUBD_METHODE', iocc=iechec, scal=submet, nbret=iret)
!
! --- OPTIONS
!
    call getvr8(mcfact, 'SUBD_PAS_MINI', iocc=iechec, scal=pasmin, nbret=iret)
    if (pasmin .gt. dtmin) then
        call utmess('F', 'DISCRETISATION_2')
    endif
    if (submet .eq. 'MANUEL') then
        call getvis(mcfact, 'SUBD_NIVEAU', iocc=iechec, scal=niveau, nbret=iret)
        call getvis(mcfact, 'SUBD_PAS', iocc=iechec, scal=nbrpas, nbret=iret)
        if (nbrpas .lt. 2) ASSERT(.false.)
    else if (submet.eq.'AUTO') then
        if (even .eq. 'COLLISION') then
            call getvr8(mcfact, 'SUBD_INST', iocc=iechec, scal=subins, nbret=iret)
            call getvr8(mcfact, 'SUBD_DUREE', iocc=iechec, scal=subdur, nbret=iret)
            subaut = 'COLLISION'
        else
            subaut = 'EXTRAPOLE'
        endif
    else
        ASSERT(.false.)
    endif
!
end subroutine
