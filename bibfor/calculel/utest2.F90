subroutine utest2(cham19, nomail, nonoeu, nupo, nusp,&
                  ivari, nocmp, nbref, tbtxt, refi,&
                  refr, refc, typres, epsi, crit,&
                  ific, llab, ssigne)
    implicit none
#include "asterfort/dismoi.h"
#include "asterfort/utch19.h"
#include "asterfort/utites.h"
    integer :: nbref, refi(nbref), nupo, ivari, ific, nusp
    real(kind=8) :: refr(nbref), epsi
    character(len=*) :: cham19, nomail, nonoeu, typres, nocmp, crit, ssigne
    character(len=16) :: tbtxt(2)
    complex(kind=8) :: refc(nbref)
    logical :: llab
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
! IN  : CHAM19 : NOM DU CHAM_ELEM DONT ON DESIRE VERIFIER 1 COMPOSANTE
! IN  : NOMAIL : NOM DE LA MAILLE A TESTER
! IN  : NONOEU : NOM D'UN NOEUD (POUR LES CHAM_ELEM "AUX NOEUDS").
!                 (SI CE NOM EST BLANC : ON UTILISE NUPO)
! IN  : NUPO   : NUMERO DU POINT A TESTER SUR LA MAILLE NOMAIL
! IN  : NUSP   : NUMERO DU SOUS_POINT A TESTER SUR LA MAILLE NOMAIL
!                (SI NUSP=0 : IL N'Y A PAS DE SOUS-POINT)
! IN  : IVARI   : NUMERO DE LA CMP (POUR VARI_R)
! IN  : NOCMP  : NOM DU DDL A TESTER SUR LE POINT NUPO
! IN  : NBREF  : NOMBRE DE VALEURS DE REFERENCE
! IN  : TBTXT  : (1)=REFERENCE, (2)=LEGENDE
! IN  : REFR   : VALEUR REELLE ATTENDUE SUR LE DDL DU POINT.
! IN  : REFC   : VALEUR COMPLEXE ATTENDUE SUR LE DDL DU POINT.
! IN  : CRIT   : 'RELATIF' OU 'ABSOLU'(PRECISION RELATIVE OU ABSOLUE).
! IN  : EPSI   : PRECISION ESPEREE
! IN  : IFIC   : NUMERO LOGIQUE DU FICHIER DE SORTIE
! IN  : LLAB   : FLAG D IMPRESSION DE LABELS
! OUT : IMPRESSION SUR LISTING
! ----------------------------------------------------------------------
    integer :: vali, ibid, ie, ier
    real(kind=8) :: valr
    complex(kind=8) :: valc
    character(len=8) :: nomma
!     ------------------------------------------------------------------
!
    call dismoi('F', 'NOM_MAILLA', cham19, 'CHAM_ELEM', ibid,&
                nomma, ie)
!
    call utch19(cham19, nomma, nomail, nonoeu, nupo,&
                nusp, ivari, nocmp, typres, valr,&
                valc, vali, ier)
    if (ier .ne. 0) then
        write (ific,*) 'NOOK '
    else
        call utites(tbtxt(1), tbtxt(2), typres, nbref, refi,&
                    refr, refc, vali, valr, valc,&
                    epsi, crit, ific, llab, ssigne)
    endif
!
end subroutine
