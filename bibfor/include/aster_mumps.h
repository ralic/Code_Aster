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
!----------------------------------------------------------------
!    include necessaire a la gestion des instances MUMPS
!----------------------------------------------------------------
#   include "smumps_struc.h"
#   include "cmumps_struc.h"
#   include "dmumps_struc.h"
#   include "zmumps_struc.h"

    integer :: nmxins
    parameter (nmxins=5)

    character(len=1) :: roucs(nmxins), precs(nmxins)
    character(len=4) :: etams(nmxins)
    character(len=14) :: nonus(nmxins)
    character(len=19) :: nomats(nmxins), nosols(nmxins)

    common /mumpsh/ nonus,nomats,nosols,etams,roucs,precs

    type (smumps_struc) , target :: smps(nmxins)
    type (cmumps_struc) , target :: cmps(nmxins)
    type (dmumps_struc) , target :: dmps(nmxins)
    type (zmumps_struc) , target :: zmps(nmxins)

    common /mumpss/ smps
    common /mumpsc/ cmps
    common /mumpsd/ dmps
    common /mumpsz/ zmps
!----------------------------------------------------------------
