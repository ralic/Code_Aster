subroutine dfllac(mcfact, iechec, dtmin, even, action,&
                  submet, subaut, pasmin, nbrpas, niveau,&
                  pcplus, cmmaxi, prcoll, ducoll)
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
!
    implicit none
#include "asterfort/assert.h"
#include "asterfort/dfllae.h"
#include "asterfort/dflldc.h"
#include "asterfort/dfllin.h"
#include "asterfort/getvtx.h"
    character(len=16) :: mcfact, even
    integer :: iechec
    real(kind=8) :: pasmin, dtmin
    character(len=16) :: submet, action, subaut
    integer :: nbrpas, niveau
    real(kind=8) :: pcplus, cmmaxi, prcoll, ducoll
!
! ----------------------------------------------------------------------
!
! OPERATEUR DEFI_LIST_INST
!
! LECTURE DES PARAMETRES DE L'ACTION
!
! ----------------------------------------------------------------------
!
! IN  MCFACT : MOT-CLEF FACTEUR POUR LIRE L'ECHEC
! IN  IECHEC : NUMERO OCCURRENCE ECHEC
! IN  EVEN   : NOM DE L'EVENEMENT
! IN  DTMIN  : INCREMENT MINIMUM DANS LA LISTE D'INSTANT
! OUT ACTION : ACTION
! OUT SUBMET : TYPE DE SUBDIVISION
! OUT SUBAUT : TYPE DE SUBDIVISION AUTOMATIQUE
! OUT PASMIN : VALEUR DE SUBD_PAS_MINI
! OUT NBRPAS : VALEUR DE SUBD_PAS
! OUT NIVEAU : VALEUR DE SUBD_NIVEAU
! OUT PCPLUS : VALEUR DE PCENT_ITER_PLUS
! OUT CMMAXI : VALEUR DE COEF_MULT_MAXI
! OUT PRCOLL : VALEUR DE PREC_COLLISION
! OUT DUCOLL : VALEUR DE DUREE_COLLISION
!
! ----------------------------------------------------------------------
!
    integer :: iret
!
! ----------------------------------------------------------------------
!
    action = ' '
!
! --- ACTION
!
    call getvtx(mcfact, 'ACTION', iocc=iechec, scal=action, nbret=iret)
    if (action .eq. 'ARRET') then
! ----- PAS D'OPTIONS
    else if (action.eq.'DECOUPE') then
        call dflldc(mcfact, iechec, dtmin, even, submet,&
                    subaut, pasmin, nbrpas, niveau, prcoll,&
                    ducoll)
    else if (action.eq.'ITER_SUPPL') then
        call dfllae(mcfact, iechec, pcplus)
        call dflldc(mcfact, iechec, dtmin, even, submet,&
                    subaut, pasmin, nbrpas, niveau, prcoll,&
                    ducoll)
    else if (action.eq.'ADAPT_COEF_PENA') then
        call dfllin(mcfact, iechec, cmmaxi)
    else if (action.eq.'AUTRE_PILOTAGE') then
        call dflldc(mcfact, iechec, dtmin, even, submet,&
                    subaut, pasmin, nbrpas, niveau, prcoll,&
                    ducoll)
    else if (action.eq.'CONTINUE') then
! ----- PAS D'OPTIONS
    else
        ASSERT(.false.)
    endif
!
end subroutine
