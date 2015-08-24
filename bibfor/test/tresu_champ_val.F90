subroutine tresu_champ_val(cham19, nomail, nonoeu, nupo, nusp,&
                           ivari, nocmp, nbref, tbtxt, refi,&
                           refr, refc, typres, epsi, crit,&
                           llab, ssigne, ignore, compare)
    implicit none
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/utch19.h"
#include "asterfort/tresu_print_all.h"
    character(len=*), intent(in) :: cham19
    character(len=*), intent(in) :: nomail
    character(len=*), intent(in) :: nonoeu
    integer, intent(in) :: nupo
    integer, intent(in) :: nusp
    integer, intent(in) :: ivari
    character(len=*), intent(in) :: nocmp
    integer, intent(in) :: nbref
    character(len=16), intent(in) :: tbtxt(2)
    integer, intent(in) :: refi(nbref)
    real(kind=8), intent(in) :: refr(nbref)
    complex(kind=8), intent(in) :: refc(nbref)
    character(len=*), intent(in) :: typres
    real(kind=8), intent(in) :: epsi
    character(len=*), intent(in) :: crit
    aster_logical, intent(in) :: llab
    character(len=*), intent(in) :: ssigne
    aster_logical, intent(in), optional :: ignore
    real(kind=8), intent(in), optional :: compare
! ----------------------------------------------------------------------
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
! IN  : LLAB   : FLAG D IMPRESSION DE LABELS
! OUT : IMPRESSION SUR LISTING
! ----------------------------------------------------------------------
    integer :: vali, ier
    real(kind=8) :: valr
    complex(kind=8) :: valc
    character(len=8) :: nomma
    aster_logical :: skip
    real(kind=8) :: ordgrd
!     ------------------------------------------------------------------
!
    skip = .false.
    if (present(ignore)) then
        skip = ignore
    endif
!
    ordgrd = 1.d0
    if (present(compare)) then
        ordgrd = compare
    endif
!
    call dismoi('NOM_MAILLA', cham19, 'CHAM_ELEM', repk=nomma)
!
    call utch19(cham19, nomma, nomail, nonoeu, nupo,&
                nusp, ivari, nocmp, typres, valr,&
                valc, vali, ier)
    ASSERT( ier .eq. 0 )

    call tresu_print_all(tbtxt(1), tbtxt(2), llab, typres, nbref,&
                         crit, epsi, ssigne, refr, valr,&
                         refi, vali, refc, valc, ignore=skip,&
                         compare=ordgrd)
!
end subroutine
