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
    subroutine rcevo2(nbinti, kinti, csigm, cinst, csiex,&
                      kemixt, cstex, csmex, lfatig, flexio,&
                      lrocht, cnoc, cresu, cpres)
        integer :: nbinti
        character(len=16) :: kinti
        character(len=24) :: csigm
        character(len=24) :: cinst
        character(len=24) :: csiex
        aster_logical :: kemixt
        character(len=24) :: cstex
        character(len=24) :: csmex
        aster_logical :: lfatig
        aster_logical :: flexio
        aster_logical :: lrocht
        character(len=24) :: cnoc
        character(len=24) :: cresu
        character(len=24) :: cpres
    end subroutine rcevo2
end interface
