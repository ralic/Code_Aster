subroutine ntcrch(model, nume_dof, vhydr_, hydr_init_)
!
implicit none
!
#include "asterfort/carces.h"
#include "asterfort/cescel.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/mecact.h"
#include "asterfort/vtcreb.h"
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: nume_dof
    character(len=24), optional, intent(in) :: vhydr_
    character(len=24), optional, intent(out) :: hydr_init_
!
! --------------------------------------------------------------------------------------------------
!
! THER_LINEAIRE - Init
!
! Create unknowns
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of model
! In  nume_dof         : name of numbering object (NUME_DDL)
! In  vhydr            : field for hydration
! Out hydr_init        : field for initial hydration
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: hydric, hydris, ligrmo
    integer :: ibid, nncp, iret
    character(len=24) :: vtemp
!
! --------------------------------------------------------------------------------------------------
!
    call dismoi('NOM_LIGREL', model, 'MODELE', repk=ligrmo)
!
! - Create hydration
!
    hydric     = '&&NTCRCH.HYDR_C'
    hydris     = '&&NTCRCH.HYDR_S'
    if (present(vhydr_)) then
        hydr_init_ = '&&NTCRCH.HYDR0'
        call mecact('V', hydric, 'MODELE', ligrmo, 'HYDR_R',&
                    ncmp=1, nomcmp='HYDR', sr=0.d0)
        call carces(hydric, 'ELNO', ' ', 'V', hydris,&
                    'A', iret)
        call cescel(hydris, ligrmo, 'RESI_RIGI_MASS', 'PHYDRPP', 'NON',&
                    nncp, 'V', hydr_init_, 'F', ibid)
        call copisd('CHAMP_GD', 'V', hydr_init_, vhydr_)
    endif
!
! - Create temperature
!
    vtemp='&&NXLECTVAR_____'
    call vtcreb(vtemp, 'V', 'R', nume_ddlz = nume_dof)
!
    call detrsd('CHAMP', hydric)
    call detrsd('CHAMP', hydris)
!
end subroutine
