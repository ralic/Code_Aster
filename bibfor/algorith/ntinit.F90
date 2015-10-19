subroutine ntinit(modele, mate    , carele, lischa,&
                  para  , numedd  , lostat, l_evol, lnonl ,&
                  sddisc, ds_inout, mailla, sdcrit, time)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/dismoi.h"
#include "asterfort/gnomsd.h"
#include "asterfort/ntcrch.h"
#include "asterfort/ntcrcv.h"
#include "asterfort/ntetcr.h"
#include "asterfort/numero.h"
#include "asterfort/nxdoet.h"
#include "asterfort/nxnoli.h"
#include "asterfort/rsnume.h"
#include "asterfort/tiinit.h"
#include "asterfort/utmess.h"
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    aster_logical :: lostat, l_evol, lnonl
    character(len=19) :: lischa
    character(len=19) :: sddisc, sdcrit
    character(len=24) :: modele, mate, carele
    character(len=24) :: numedd, time
    type(NL_DS_InOut), intent(inout) :: ds_inout
    character(len=8) :: mailla
    real(kind=8) :: para(*)
!
! --------------------------------------------------------------------------------------------------
!
!     THERMIQUE : INITIALISATIONS.
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: result
    character(len=14) :: nuposs
    character(len=24) :: noojb, vhydr, hydr0
    real(kind=8) :: init_time
!
! --------------------------------------------------------------------------------------------------
!
    vhydr  = ' '
    lostat = .false.
    result = ds_inout%result
    time   = result(1:8)//'.CHTPS'
    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=mailla)
!
! --- NUMEROTATION ET CREATION DU PROFIL DE LA MATRICE
!
    numedd = '12345678.NUMED'
    noojb = '12345678.00000.NUME.PRNO'
    call gnomsd(' ', noojb, 10, 14)
    numedd = noojb(1:14)
    call rsnume(result, 'TEMP', nuposs)
    call numero(numedd, 'VG',&
                old_nume_ddlz = nuposs,&
                modelz = modele , list_loadz = lischa)
!
! --- CREATION DES CHAMPS
!
    call ntcrch(modele, numedd, hydr0, vhydr)
!
! - Create input/output datastructure
!
    call ntetcr(numedd, lnonl, ds_inout,&
                list_load_ = lischa)
!
! - Read initial state
!
    call nxdoet(modele, numedd, lostat, ds_inout)
    init_time = ds_inout%init_time
!
! - Time discretization and storing datastructures
!
    call tiinit(ds_inout, sddisc, lostat, l_evol)
!
! - Prepare storing
!
    call nxnoli(modele, mate, carele, lostat, lnonl   ,&
                l_evol, para, sddisc, sdcrit, ds_inout)
!
! --- CREATION DE LA SD POUR ARCHIVAGE DES INFORMATIONS DE CONVERGENCE
!
    call ntcrcv(sdcrit)
!
end subroutine
