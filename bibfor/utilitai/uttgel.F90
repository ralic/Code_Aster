subroutine uttgel(nomte, typgeo)
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
! person_in_charge: jacques.pellet at edf.fr
!  UTILITAIRE - TYPE GEOMETRIQUE D'UN ELEMENT FINI
!  **           *    *                **
! =====================================================================
! IN  NOMTE  : NOM DU TYPE D'ELEMENT FINI
! OUT TYPGEO : TYPE GEOMETRIQUE CORRESPONDANT
!              EN 2D : 'TR', 'QU'
!              EN 3D : 'HE', 'TE', 'PE', 'PY'
! ----------------------------------------------------------------------
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "asterfort/codent.h"
#include "asterfort/utmess.h"
#include "asterfort/teattr.h"
#include "asterfort/assert.h"
    character(len=2) :: typgeo
    character(len=16) :: nomte
!
! 0.2. ==> VARIABLES LOCALES
!
    integer :: ibid
    character(len=8) :: alias8
    character(len=3) :: codtma


    call teattr(nomte, 'S', 'ALIAS8', alias8, ibid)
    codtma=alias8(6:8)

    if (codtma(1:2).eq.'TR') then
        typgeo='TR'
    elseif (codtma(1:2).eq.'QU') then
        typgeo='QU'
    elseif (codtma.eq.'HE8' .or. codtma.eq.'H20' .or. codtma.eq.'H27') then
        typgeo='HE'
    elseif (codtma.eq.'PE6' .or. codtma.eq.'P15' .or. codtma.eq.'P18') then
        typgeo='PE'
    elseif (codtma.eq.'TE4' .or. codtma.eq.'T10') then
        typgeo='TE'
    elseif (codtma.eq.'PY5' .or. codtma.eq.'P13') then
        typgeo='PY'
    else
        ASSERT(.false.)
    endif
!
end subroutine
