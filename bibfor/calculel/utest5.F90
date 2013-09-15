subroutine utest5(cham19, nomail, nocmp, tbtxt, refi,&
                  refr, refc, typres, epsi, crit,&
                  ific, llab)
    implicit none
#include "asterfort/dismoi.h"
#include "asterfort/utchca.h"
#include "asterfort/utites.h"
#include "asterfort/utmess.h"
    integer :: refi, ific
    real(kind=8) :: refr, epsi
    character(len=*) :: cham19, nomail, typres, nocmp, crit
    character(len=16) :: tbtxt(2)
    complex(kind=8) :: refc
    logical :: llab
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
! IN  : CHAM19 : NOM DE LA CARTE DONT ON DESIRE VERIFIER 1 COMPOSANTE
! IN  : NOMAIL : NOM DE LA MAILLE A TESTER
! IN  : NOCMP  : NOM DU DDL A TESTER
! IN  : TBTXT  : (1)=REFERENCE, (2)=LEGENDE
! IN  : REFR   : VALEUR REELLE ATTENDUE
! IN  : REFI   : VALEUR REELLE ATTENDUE
! IN  : REFC   : VALEUR COMPLEXE ATTENDUE
! IN  : CRIT   : 'RELATIF' OU 'ABSOLU'(PRECISION RELATIVE OU ABSOLUE).
! IN  : EPSI   : PRECISION ESPEREE
! IN  : IFIC   : NUMERO LOGIQUE DU FICHIER DE SORTIE
! IN  : LLAB   : FLAG D IMPRESSION DES LABELS
! OUT : IMPRESSION SUR LISTING
! ----------------------------------------------------------------------
    integer :: vali, ibid, ie, ier
    real(kind=8) :: valr
    complex(kind=8) :: valc
    character(len=8) :: nomma
    character(len=3) :: ssigne
    character(len=4) :: tych
!     ------------------------------------------------------------------
    call dismoi('F', 'TYPE_CHAMP', cham19, 'CHAMP', ibid,&
                tych, ie)
    if (tych .ne. 'CART') then
        call utmess('F', 'CALCULEL3_90', sk=cham19)
    endif
!
    call dismoi('F', 'NOM_MAILLA', cham19, 'CARTE', ibid,&
                nomma, ie)
!
    call utchca(cham19, nomma, nomail, nocmp, typres,&
                valr, vali, valc, ier)
    if (ier .ne. 0) then
        write (ific,*) 'NOOK '
    else
        ssigne='NON'
        call utites(tbtxt(1), tbtxt(2), typres, 1, [refi],&
                    [refr], [refc], vali, valr, valc,&
                    epsi, crit, ific, llab, ssigne)
    endif
!
end subroutine
