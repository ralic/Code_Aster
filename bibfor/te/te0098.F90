subroutine te0098(option, nomte)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
    character(len=16) :: nomte, option
!
! --------------------------------------------------------------------------------------------------
!
!    ELEMENT MECABL2
!       OPTION : 'RIGI_MECA'
!
!    ELEMENT MECA_BARRE
!       OPTION : 'RIGI_MECA_GE'
!
!    Dans tous les cas MAT = 0
!
! --------------------------------------------------------------------------------------------------
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
!
    integer :: nno, nbval, imatuu, iadzi, iazk24
    aster_logical :: okte
    character(len=8) :: nomu
    character(len=16) :: concep,cmd
!
    character(len=16) :: vmessk(6)
!
! --------------------------------------------------------------------------------------------------
!   Récupération des arguments de l'opérateur
    call getres(nomu,concep,cmd)
!   Seulement valide pour
!           CALC_MATR_ELEM
!       et
!           MECABL2 et RIGI_MECA
!           ou
!           MECA_BARRE et RIGI_MECA_GE
!
    okte =  ((nomte.eq.'MECABL2')   .and.(option.eq.'RIGI_MECA'))  .or. &
            ((nomte.eq.'MECA_BARRE').and.(option.eq.'RIGI_MECA_GE'))
    okte = okte .and. cmd.eq.'CALC_MATR_ELEM'
    if ( .not. okte ) then
        vmessk(1) = cmd
        vmessk(2) = concep
        vmessk(3) = nomu
        vmessk(4) = nomte
        vmessk(5) = option
        call tecael(iadzi, iazk24)
        vmessk(6) = zk24(iazk24-1+3)(1:8)
        call utmess('F', 'CALCULEL_32', nk=6, valk=vmessk )
    endif
!
! --------------------------------------------------------------------------------------------------
    call elrefe_info(fami='RIGI',nno=nno)
!
!   Parametres en sortie
    call jevech('PMATUUR', 'E', imatuu)
    nbval = 3*nno*(3*nno+1)/2
    zr(imatuu:imatuu-1+nbval)= 0.0d0
end subroutine
