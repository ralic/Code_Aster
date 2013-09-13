subroutine nmrepc(sddisc, solveu, ievdac, retrpc)
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
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utdidt.h"
#include "asterfort/utmess.h"
    character(len=19) :: sddisc, solveu
    integer :: ievdac, retrpc
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - ACTIONS)
!
! GESTION DE L'ACTION REAC_PRECOND
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SOLVEU : SD SOLVEUR
! IN  IEVDAC : INDICE DE L'EVENEMENT ACTIF
! OUT RETRPC : CODE RETOUR REACTUALISATION PRECONDTIONNEUR
!               0 ECHEC  : ON A DEJA REACTUALISE LE PRECONDITIONNEUR
!               1 SUCCES : ON REACTUALISE LE PRECONDITIONNEUR
!
!
    real(kind=8) :: r8bid
    integer :: ireapc, jslvi
    character(len=8) :: k8bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    retrpc = 0
!
! --- PARAMETRES
!
    call utdidt('L', sddisc, 'ECHE', ievdac, 'ESSAI_REAC_PRECOND',&
                r8bid, ireapc, k8bid)
!
! --- L'UTILISATEUR UTILISE UN PRECONDITIONNEUR SIMPLE PRECISION
! --- ET SOUHAITE LE REACTUALISER EN CAS D'ECHEC A LA RESOLUTION
!
    if (ireapc .eq. 1) then
!
! ----- ON A DEJA REACTUALISE LE PRECONDITIONNEUR : ON DECOUPE
!
        ireapc = 1
        retrpc = 0
        call utmess('I', 'MECANONLINE10_40')
    else if (ireapc.eq.0) then
!
! ----- ON RETENTE EN REACTUALISANT LE PRECONDITIONNEUR
!
        ireapc = 1
        call jeveuo(solveu//'.SLVI', 'E', jslvi)
        zi(jslvi-1+5) = 0
        retrpc = 1
        call utmess('I', 'MECANONLINE10_41')
    else
        ASSERT(.false.)
    endif
!
! --- SAUVEGARDE INFO
!
    call utdidt('E', sddisc, 'ECHE', ievdac, 'ESSAI_REAC_PRECOND',&
                r8bid, ireapc, k8bid)
!
    call jedema()
end subroutine
