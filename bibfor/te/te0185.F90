subroutine te0185(option, nomte)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
! aslint: disable=W0104
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/ef0031.h"
#include "asterfort/ef0033.h"
#include "asterfort/ef0039.h"
#include "asterfort/ef0042.h"
#include "asterfort/ef0142.h"
#include "asterfort/ef0154.h"
#include "asterfort/ef0156.h"
#include "asterfort/ef0231.h"
#include "asterfort/ef0344.h"
#include "asterfort/ef0347.h"
#include "asterfort/ef0409.h"
#include "asterfort/ef0410.h"
#include "asterfort/ef0415.h"
#include "asterfort/ef0436.h"
#include "asterfort/ef0517.h"
#include "asterfort/ef0585.h"
#include "asterfort/ef0587.h"
#include "asterfort/jevech.h"
#include "asterfort/teattr.h"
    character(len=16) :: option, nomte
!     ----------------------------------------------------------------
!     CALCUL DE L'OPTION EFGE_ELNO
!     CETTE ROUTINE NE SERT QUE D'INDIRECTION VERS LE BON TE00IJ
!     EN FONCTION DE LA MODELISATION ET DE LA NATURE DU CALCUL :
!     LINEAIRE OU NON-LINEAIRE
    integer :: j1, inlin, ibid
    aster_logical :: line
    character(len=3) :: cmod
    character(len=8) :: alias8
!-----------------------------------------------------------------------
!
!     -- CALCUL DE LINE :
    call jevech('PNONLIN', 'L', j1)
    inlin=zi(j1-1+1)
    line=(inlin.eq.0)
!
!     -- CALCUL DE CMOD (CODE DE LA MODELISATION) :
    call teattr('S', 'ALIAS8', alias8, ibid)
    cmod=alias8(3:5)
!
    if (line) then
!       -- QUAND LE CALCUL EST LINEAIRE :
        if (cmod .eq. '2DB') then
!         -- 2D_BARRE
            call ef0154(nomte)
        else if (cmod.eq.'2DT'.or.cmod.eq.'2TR') then
!         -- 2D_DIS_T , 2D_DIS_TR
            call ef0042(nomte)
        else if (cmod.eq.'BAR') then
!         -- BARRE
            call ef0154(nomte)
        else if (cmod.eq.'CAB'.or.cmod.eq.'CAP') then
!         -- CABLE, CABLE_POULIE
            ASSERT(.false.)
        else if (cmod.eq.'CQ3') then
!         -- COQUE_3D
            call ef0410(nomte)
        else if (cmod.eq.'CQA'.or.cmod.eq.'CQC'.or.cmod.eq.'CQD') then
!         -- COQUE_AXIS, COQUE_C_PLAN, COQUE_D_PLAN
            call ef0231(nomte)
        else if (cmod.eq.'DIT') then
!         -- DIS_T
            call ef0042(nomte)
        else if (cmod.eq.'DTG'.or.cmod.eq.'Q4S') then
!         -- DKTG, Q4GG
            call ef0033(nomte)
        else if (cmod.eq.'DKT'.or.cmod.eq.'DST'.or.cmod.eq.'Q4G') then
!         -- DKT, DST, Q4G
            call ef0033(nomte)
        else if (cmod.eq.'DTR') then
!         -- DIS_TR
            call ef0042(nomte)
        else if (cmod.eq.'PCT') then
!         -- POU_C_T
            call ef0142(nomte)
        else if (cmod.eq.'PDE') then
!         -- POU_D_E
            call ef0142(nomte)
        else if (cmod.eq.'PDG') then
!         -- POU_D_TG
            call ef0344(nomte)
        else if (cmod.eq.'PDT') then
!         -- POU_D_T
            call ef0142(nomte)
        else if (cmod.eq.'PGD') then
!         -- POU_D_T_GD
            ASSERT(.false.)
        else if (cmod.eq.'PFM') then
!         -- POU_D_EM
            call ef0142(nomte)
        else if (cmod.eq.'PGM') then
!         -- POU_D_TGM
            ASSERT(.false.)
        else if (cmod.eq.'TU3'.or.cmod.eq.'TU6') then
!         -- TUYAU_3M, TUYAU_6M
            call ef0585(nomte)
        else if (cmod.eq.'MMB') then
!         -- MEMBRANE
            call ef0436(nomte)
        else
            ASSERT(.false.)
        endif
!
!
!
    else
!     -- QUAND LE CALCUL EST NON-LINEAIRE :
        if (cmod .eq. '2DB') then
!         -- 2D_BARRE
            call ef0156(nomte)
        else if (cmod.eq.'2DT'.or.cmod.eq.'2TR') then
!         -- 2D_DIS_T , 2D_DIS_TR
            call ef0039(nomte)
        else if (cmod.eq.'BAR') then
!         -- BARRE
            call ef0156(nomte)
        else if (cmod.eq.'CAB'.or.cmod.eq.'CAP') then
!         -- CABLE, CABLE_POULIE
            ASSERT(.false.)
        else if (cmod.eq.'CQ3') then
!         -- COQUE_3D
            call ef0415(nomte)
        else if (cmod.eq.'CQA'.or.cmod.eq.'CQC'.or.cmod.eq.'CQD') then
!         -- COQUE_AXIS, COQUE_C_PLAN, COQUE_D_PLAN
            ASSERT(.false.)
        else if (cmod.eq.'DIT') then
!         -- DIS_T
            call ef0039(nomte)
        else if (cmod.eq.'DTG'.or.cmod.eq.'Q4S') then
!         -- DKTG, Q4GG
            call ef0409(nomte)
        else if (cmod.eq.'DKT') then
!         -- DKT
            call ef0031(nomte)
        else if (cmod.eq.'DTR') then
!         -- DIS_TR
            call ef0039(nomte)
        else if (cmod.eq.'PCT') then
!         -- POU_C_T
            call ef0039(nomte)
        else if (cmod.eq.'PDE') then
!         -- POU_D_E
            call ef0347(nomte)
        else if (cmod.eq.'PDG') then
!         -- POU_D_TG
            call ef0347(nomte)
        else if (cmod.eq.'PDT') then
!         -- POU_D_T
            call ef0347(nomte)
        else if (cmod.eq.'PGD') then
!         -- POU_D_T_GD
            ASSERT(.false.)
        else if (cmod.eq.'PFM') then
!         -- POU_D_EM
            call ef0517(nomte)
        else if (cmod.eq.'PGM') then
!         -- POU_D_TGM
            call ef0517(nomte)
        else if (cmod.eq.'TU3'.or.cmod.eq.'TU6') then
!         -- TUYAU_3M, TUYAU_6M
            call ef0587(nomte)
        else if (cmod.eq.'MMB') then
!         -- MEMBRANE
            call ef0156(nomte)
        else
            ASSERT(.false.)
        endif
    endif
!
end subroutine
