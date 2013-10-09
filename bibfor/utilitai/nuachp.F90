subroutine nuachp(nuage, lno, chpt)
    implicit none
#include "asterfort/dismoi.h"
#include "asterfort/nuacno.h"
#include "asterfort/utmess.h"
    character(len=*) :: nuage, lno, chpt
!     ------------------------------------------------------------------
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
!     PASSAGE D'UNE SD NUAGE A UNE SD CHAM_GD
!
! IN  NUAGE  : NOM DE LA SD NUAGE ALLOUEE
! IN  LNO    : LISTE DES NOEUDS A PRENDRE EN COMPTE
! VAR CHPT   : NOM DE LA SD CHAM_GD (CHPT A ETE CREE)
!     ------------------------------------------------------------------
    character(len=4) :: type
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call dismoi('TYPE_CHAMP', chpt, 'CHAMP', repk=type)
!
    if (type .eq. 'NOEU') then
        call nuacno(nuage, lno, chpt)
    else
        call utmess('F', 'CALCULEL_17')
    endif
!
end subroutine
