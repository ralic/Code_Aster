!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
#include "asterf_types.h"
!
interface
    subroutine xprtor(method, model, noma, cnxinv, fispre,&
                      fiss, vcn, grlr, cnsln, grln,&
                      cnslt, grlt, tore, radtor, radimp,&
                      cnsdis, disfr, cnsbl, nodcal, elecal,&
                      liggrd, vcnt, grlrt)
        character(len=8) :: method
        character(len=8) :: model
        character(len=8) :: noma
        character(len=19) :: cnxinv
        character(len=8) :: fispre
        character(len=8) :: fiss
        character(len=24) :: vcn
        character(len=24) :: grlr
        character(len=19) :: cnsln
        character(len=19) :: grln
        character(len=19) :: cnslt
        character(len=19) :: grlt
        aster_logical :: tore
        real(kind=8) :: radtor
        real(kind=8) :: radimp
        character(len=19) :: cnsdis
        character(len=19) :: disfr
        character(len=19) :: cnsbl
        character(len=19) :: nodcal
        character(len=19) :: elecal
        character(len=19) :: liggrd
        character(len=24) :: vcnt
        character(len=24) :: grlrt
    end subroutine xprtor
end interface
