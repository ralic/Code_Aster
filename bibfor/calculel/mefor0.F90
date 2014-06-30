subroutine mefor0(nomo, chfor0, fonc)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!
#include "jeveux.h"
#include "asterfort/fozerv.h"
#include "asterfort/mecact.h"
    character(len=8) :: nomo
    character(len=*) :: chfor0
!
    logical(kind=1) :: fonc
!
! - CETTE ROUTINE GENERE UN CHAMP DE FORCE NUL (CARTE CONSTANTE)
!       FONC = .TRUE.  FORCE FONCTION
!       FONC = .FALSE. FORCE REELLE
!
!
    real(kind=8) :: rcmp(3)
!
    character(len=8) :: licmp(3), nomf(3), zero
    character(len=19) :: ligrmo
!-----------------------------------------------------------------------
    chfor0 = '&&MEFOR0.FORCE_NULLE'
    zero = '&&MEFOR0'
    ligrmo = nomo//'.MODELE    '
    licmp(1) = 'FX'
    licmp(2) = 'FY'
    licmp(3) = 'FZ'
    rcmp(1) = 0.d0
    rcmp(2) = 0.d0
    rcmp(3) = 0.d0
    if (fonc) then
        call fozerv(zero)
        nomf(1) = zero
        nomf(2) = zero
        nomf(3) = zero
        call mecact('V', chfor0, 'MODELE', ligrmo, 'FORC_F',&
                    ncmp=3, lnomcmp=licmp, vk=nomf)
    else
        call mecact('V', chfor0, 'MODELE', ligrmo, 'FORC_R',&
                    ncmp=3, lnomcmp=licmp, vr=rcmp)
    endif
!
end subroutine
