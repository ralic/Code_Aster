subroutine nlvint(sd_nl_)
    implicit none
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! nlvint : create the internal variables real vector and its corresponding
!          index
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nlget.h"
#include "asterfort/nlinivec.h"
!
!   -0.1- Input/output arguments
    character(len=*) , intent(in) :: sd_nl_
!
!   -0.2- Local variables
    integer           :: i, mxlevel, nltype_i, add, vint_reclen
    character(len=8)  :: sd_nl
    integer, pointer  :: vindx(:) => null()   
!
    call jemarq()
!
    sd_nl  = sd_nl_
!
    call nlget   (sd_nl, _MAX_LEVEL, iscal=mxlevel)
    call nlinivec(sd_nl, _INTERNAL_VARS_INDEX, mxlevel+1, vi=vindx)
    vindx(1) = 1
    do i =1, mxlevel
        call nlget(sd_nl, _NL_TYPE, iocc=i, iscal=nltype_i)
        select case (nltype_i)
            case(NL_CHOC)
                add = NBVARINT_CHOC
            case(NL_BUCKLING)
                add = NBVARINT_FLAM
            case(NL_ANTI_SISMIC)
                add = NBVARINT_ANTS
            case(NL_DIS_VISC)
                add = NBVARINT_DVIS
            case(NL_DIS_ECRO_TRAC)
                add = NBVARINT_DECR
            case(NL_CRACKED_ROTOR)
                add = NBVARINT_ROTF
            case(NL_LUBRICATION)
                add = NBVARINT_YACS
             case(NL_FX_RELATIONSHIP)
                add = NBVARINT_FXRL
             case(NL_FV_RELATIONSHIP)
                add = NBVARINT_FVRL
            case default
                ASSERT(.false.)
        end select
        vindx(i+1) = vindx(i)+add
    end do

    vint_reclen = vindx(mxlevel+1)-1
    call nlinivec(sd_nl, _INTERNAL_VARS, vint_reclen)

    call jedema()
end subroutine
