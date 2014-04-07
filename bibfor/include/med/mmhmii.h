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
interface
#include "asterf_types.h"
    subroutine mmhmii(fid, it, name, sdim, mdim,&
                      mtype, desc, dtunit, stype, nstep,&
                      atype, aname, aunit, cret)
        med_int :: fid
        med_int :: it
        character(len=*) :: name
        med_int :: sdim
        med_int :: mdim
        med_int :: mtype
        character(len=*) :: desc
        character(len=*) :: dtunit
        med_int :: stype
        med_int :: nstep
        med_int :: atype
        character(len=*) :: aname(3)
        character(len=*) :: aunit(3)
        med_int :: cret
    end subroutine mmhmii
end interface
