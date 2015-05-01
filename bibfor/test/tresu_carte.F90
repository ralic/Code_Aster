subroutine tresu_carte(cham19, nomail, nocmp, tbtxt, refi,&
                       refr, refc, typres, epsi, crit,&
                       ific, llab, ignore, compare)
    implicit none
#include "asterf_types.h"
#include "asterfort/dismoi.h"
#include "asterfort/utchca.h"
#include "asterfort/tresu_print_all.h"
#include "asterfort/utmess.h"
    character(len=*), intent(in) :: cham19
    character(len=*), intent(in) :: nomail
    character(len=*), intent(in) :: nocmp
    character(len=16), intent(in) :: tbtxt(2)
    integer, intent(in) :: refi
    real(kind=8), intent(in) :: refr
    complex(kind=8), intent(in) :: refc
    character(len=*), intent(in) :: typres
    real(kind=8), intent(in) :: epsi
    character(len=*), intent(in) :: crit
    integer, intent(in) :: ific
    aster_logical, intent(in) :: llab
    aster_logical, intent(in), optional :: ignore
    real(kind=8), intent(in), optional :: compare
! person_in_charge: jacques.pellet at edf.fr
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
    integer :: vali, ier
    real(kind=8) :: valr
    complex(kind=8) :: valc
    character(len=8) :: nomma
    character(len=4) :: tych
    aster_logical :: skip
    real(kind=8) :: ordgrd
!     ------------------------------------------------------------------
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
    call dismoi('TYPE_CHAMP', cham19, 'CHAMP', repk=tych)
    if (tych .ne. 'CART') then
        call utmess('F', 'CALCULEL3_90', sk=cham19)
    endif
!
    call dismoi('NOM_MAILLA', cham19, 'CARTE', repk=nomma)
!
    call utchca(cham19, nomma, nomail, nocmp, typres,&
                valr, vali, valc, ier)
    if (ier .ne. 0) then
        write (ific,*) 'NOOK '
    else
        call tresu_print_all(tbtxt(1), tbtxt(2), llab, typres, 1,&
                             crit, epsi, 'NON', [refr], valr,&
                             [refi], vali, [refc], valc, ignore=skip,&
                             compare=ordgrd)
    endif
!
end subroutine
