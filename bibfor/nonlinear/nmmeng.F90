subroutine nmmeng(fonact)
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
    implicit      none
#include "jeveux.h"
#include "asterfort/alfeti.h"
#include "asterfort/detmat.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: fonact(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME PRINCIPAL)
!
! NETTOYAGE FIN DE MECA_NON_LINE
!
! ----------------------------------------------------------------------
!
!
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
!
!
!
!
    character(len=24) :: opt, k24bid
    character(len=19) :: k19bid
    integer :: ibid
    real(kind=8) :: r8bid
    logical :: lfeti
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- DESTRUCTION DE TOUTES LES MATRICES CREEES
!
    call detmat()
!
! --- SI FETI, NETTOYAGE DES OBJETS TEMPORAIRES
! --- NETTOYAGE DES SD FETI SI NECESSAIRE (SUCCESSION DE CALCULS
! --- DECOUPLES)
! --- ET INITIALISATION NUMERO D'INCREMENT
!
    lfeti = isfonc(fonact,'FETI')
    if (lfeti) then
        opt='NETTOYAGE_SDT'
        call alfeti(opt, k19bid, k19bid, k19bid, k19bid,&
                    ibid, r8bid, k24bid, r8bid, ibid,&
                    k24bid, k24bid, k24bid, k24bid, ibid,&
                    k24bid, k24bid, ibid)
    endif
!
    call jedema()
!
end subroutine
