subroutine liscn1(lisold, ichar, nomfct, typfct, phase, &
                  npuis)
!
    implicit     none
!
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
!
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
!
    character(len=19), intent(in) :: lisold
    integer, intent(in)  :: ichar
    character(len=16) , intent(out) :: typfct
    character(len=8), intent(out)  :: nomfct
    real(kind=8), intent(out)  :: phase
    integer, intent(out)  :: npuis
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! CONVERTISSEUR ANCIENNE LISTE_CHARGES -> NOUVELLE LISTE_CHARGES
!
! TYPE DE FONC_MULT
!
! ----------------------------------------------------------------------
!
!
!
!
! ----------------------------------------------------------------------
!
    character(len=24) :: fomult
    integer :: jalifc
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! - ACCES ANCIENNE SD
!
    fomult = lisold(1:19)//'.FCHA'
    call jeveuo(fomult, 'L', jalifc)
!
! - INITIALISATIONS
!
    phase = 0.d0
    npuis = 0
    typfct = 'CONST_REEL'
    nomfct = zk24(jalifc+ichar-1)(1:8)
    if (nomfct(1:8).ne.'&&NMDOME'.and.nomfct.ne.' ') then
        typfct = 'FONCT_REEL'
    endif    
!
    call jedema()
end subroutine
