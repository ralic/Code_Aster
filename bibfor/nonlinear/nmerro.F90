subroutine nmerro(sderro, ds_measure, nume_inst)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/etausr.h"
#include "asterfort/nmecev.h"
#include "asterfort/nmerge.h"
#include "asterfort/sigusr.h"
#include "asterfort/utmess.h"
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
    character(len=24), intent(in) :: sderro
    type(NL_DS_Measure), intent(in) :: ds_measure
    integer, intent(in) :: nume_inst
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Error management
!
! Write messages for errors
!
! --------------------------------------------------------------------------------------------------
!
! In  sderro           : datastructure for errors during algorithm
! In  ds_measure       : datastructure for measure and statistics management
! In  nume_inst        : index of current time step
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: rtab(2)
    integer :: itab(2)
    aster_logical :: echldc, echeq1, echeq2, echco1, echco2, echpil
    aster_logical :: mtcpui, mtcpup, itemax
    aster_logical :: echpfg, echpff, echpfc
    aster_logical :: errres
    real(kind=8) :: remain_time, iter_mean_time, step_mean_time
    character(len=16) :: nomevd, action, valk(2)
!
! --------------------------------------------------------------------------------------------------
!
    if (etausr() .eq. 1) then
        call sigusr()
    endif
!
! - Get times
!
    remain_time    = ds_measure%step_remain_time
    iter_mean_time = ds_measure%iter_mean_time
    step_mean_time = ds_measure%step_mean_time
!
! --- RECUPERE LES CODES ERREURS ACTIFS
!
    call nmerge(sderro, 'ERRE_INTE', echldc)
    call nmerge(sderro, 'ERRE_PILO', echpil)
    call nmerge(sderro, 'ERRE_FACS', echeq1)
    call nmerge(sderro, 'ERRE_FACT', echeq2)
    call nmerge(sderro, 'ERRE_CTD1', echco1)
    call nmerge(sderro, 'ERRE_CTD2', echco2)
    call nmerge(sderro, 'ERRE_TIMN', mtcpui)
    call nmerge(sderro, 'ERRE_TIMP', mtcpup)
    call nmerge(sderro, 'ITER_MAXI', itemax)
    call nmerge(sderro, 'ERRE_CTCG', echpfg)
    call nmerge(sderro, 'ERRE_CTCF', echpff)
    call nmerge(sderro, 'ERRE_CTCC', echpfc)
    call nmerge(sderro, 'SOLV_ITMX', errres)
!
! --- LANCEE EXCEPTIONS
!
    if (mtcpui) then
        itab(1) = nume_inst
        rtab(1) = iter_mean_time
        rtab(2) = remain_time
        call utmess('Z', 'MECANONLINE9_1', si=itab(1), nr=2, valr=rtab,&
                    num_except=28)
    else if (mtcpup) then
        itab(1) = nume_inst
        rtab(1) = step_mean_time
        rtab(2) = remain_time
        call utmess('Z', 'MECANONLINE9_2', si=itab(1), nr=2, valr=rtab,&
                    num_except=28)
    else if (echldc) then
        call utmess('Z', 'MECANONLINE9_3', num_except=23)
    else if (echeq1.or.echeq2) then
        call utmess('Z', 'MECANONLINE9_4', num_except=25)
    else if (echco1) then
        call utmess('Z', 'MECANONLINE9_5', num_except=26)
    else if (echco2) then
        call utmess('Z', 'MECANONLINE9_6', num_except=27)
    else if (itemax) then
        call utmess('Z', 'MECANONLINE9_7', num_except=22)
    else if (echpil) then
        call utmess('Z', 'MECANONLINE9_8', num_except=29)
    else if (echpfg) then
        call utmess('Z', 'MECANONLINE9_9', num_except=30)
    else if (echpff) then
        call utmess('Z', 'MECANONLINE9_10', num_except=31)
    else if (echpfc) then
        call utmess('Z', 'MECANONLINE9_11', num_except=32)
    else if (errres) then
        call utmess('Z', 'MECANONLINE9_12', num_except=35)
    else
        call nmecev(sderro, 'L', nomevd, action)
        valk(1) = action
        valk(2) = nomevd
        if (action .eq. 'ARRET') then
            call utmess('Z', 'MECANONLINE9_51', sk=nomevd, num_except=33)
        else
            call utmess('Z', 'MECANONLINE9_50', nk=2, valk=valk, num_except=34)
        endif
    endif
!
end subroutine
