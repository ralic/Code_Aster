subroutine dflldb(sdlist, ifm)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/dflld2.h"
#include "asterfort/dfllvd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
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
!
    character(len=8), intent(in) :: sdlist
    integer, intent(in) :: ifm
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_LIST_INST
!
! Debug
!
! --------------------------------------------------------------------------------------------------
!
! In  sdlist           : name of DEFI_LIST_INST datastructure
! In  ifm              : unit for message
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_fail, nb_fail, nb_inst, nb_adap
    integer :: nbpamx
    real(kind=8) :: dtmin, pasmin, pasmax
    integer :: leevr, leevk, lesur
    character(len=24) :: sdlist_evenr
    real(kind=8), pointer :: v_sdlist_evenr(:) => null()
    character(len=24) :: sdlist_evenk
    character(len=16), pointer :: v_sdlist_evenk(:) => null()
    character(len=24) :: sdlist_subdr
    real(kind=8), pointer :: v_sdlist_subdr(:) => null()
    character(len=24) :: sdlist_infor
    real(kind=8), pointer :: v_sdlist_infor(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Get sizes of objects
!
    leevr = dfllvd('LEEVR')
    leevk = dfllvd('LEEVK')
    lesur = dfllvd('LESUR')
!
! - Access to datastructures
!
    sdlist_infor = sdlist(1:8)//'.LIST.INFOR'
    call jeveuo(sdlist_infor, 'L', vr = v_sdlist_infor)
    sdlist_evenr = sdlist(1:8)//'.ECHE.EVENR'
    sdlist_evenk = sdlist(1:8)//'.ECHE.EVENK'
    sdlist_subdr = sdlist(1:8)//'.ECHE.SUBDR'
    call jeveuo(sdlist_evenr, 'L', vr   = v_sdlist_evenr)
    call jeveuo(sdlist_evenk, 'L', vk16 = v_sdlist_evenk)
    call jeveuo(sdlist_subdr, 'L', vr   = v_sdlist_subdr)
    sdlist_infor = sdlist(1:8)//'.LIST.INFOR'
!
! - Numbers
!
    nb_fail = nint(v_sdlist_infor(9))
    nb_inst = nint(v_sdlist_infor(8))
    nb_adap = nint(v_sdlist_infor(10))
!
! - Time list management
!
    if (nint(v_sdlist_infor(1)) .eq. 1) then
        write(ifm,*) '<DEFILISTINST> GESTION MANUELLE DE LA LISTE D''INSTANTS'
    else if (nint(v_sdlist_infor(1)) .eq. 2) then
        write(ifm,*) '<DEFILISTINST> GESTION AUTOMATIQUE DE LA LISTE D''INSTANTS'
    else
        ASSERT(.false.)
    endif
    dtmin = v_sdlist_infor(5)
    write(ifm,*) '<DEFILISTINST> ... LA LISTE CONTIENT ',nb_inst,&
                 ' INSTANTS ET LE PAS MINIMUM VAUT ',dtmin
!
! - Time list management: automatic
!
    if (nint(v_sdlist_infor(1)) .eq. 2) then
        pasmin = v_sdlist_infor(2)
        pasmax = v_sdlist_infor(3)
        nbpamx = nint(v_sdlist_infor(4))
        write(ifm,*) '<DEFILISTINST> PARAMETRES DE LA GESTION AUTOMATIQUE DE LA LISTE D''INSTANTS'
        write(ifm,*) '<DEFILISTINST> ... PAS MINI   : ',pasmin
        write(ifm,*) '<DEFILISTINST> ... PAS MAXI   : ',pasmax
        write(ifm,*) '<DEFILISTINST> ... NB_PAS_MAXI: ',nbpamx
    endif
!
! - Failures
!
    if (nb_fail .gt. 0) then
        write(ifm,*) '<DEFILISTINST> GESTION DES EVENEMENTS (',nb_fail,' EVENEMENTS)'
        do i_fail = 1, nb_fail
            write(ifm,*) '<DEFILISTINST> ... EVENEMENT : ', i_fail
            if (nint(v_sdlist_evenr(leevr*(i_fail-1)+1)) .eq. 0) then
                write(ifm,*) '<DEFILISTINST> ...... ERRE'
            else if (nint(v_sdlist_evenr(leevr*(i_fail-1)+1)) .eq. 1) then
                write(ifm,*) '<DEFILISTINST> ...... DELTA_GRANDEUR'
                write(ifm,*) '<DEFILISTINST> ......... CHAMP      :',&
                v_sdlist_evenk(leevk*(i_fail-1)+1)
                write(ifm,*) '<DEFILISTINST> ......... COMPOSANTE :',&
                v_sdlist_evenk(leevk*(i_fail-1)+2)
                write(ifm,*) '<DEFILISTINST> ......... COMPARATEUR:',&
                v_sdlist_evenk(leevk*(i_fail-1)+3)
            else if (nint(v_sdlist_evenr(leevr*(i_fail-1)+1)) .eq. 2) then
                write(ifm,*) '<DEFILISTINST> ...... COLLISION'
            else if (nint(v_sdlist_evenr(leevr*(i_fail-1)+1)) .eq. 3) then
                write(ifm,*) '<DEFILISTINST> ...... INTERPENETRATION'
                write(ifm,*) '<DEFILISTINST> ......... PENE_MAXI  :',&
                v_sdlist_evenr(leevr*(i_fail-1)+6)
            else if (nint(v_sdlist_evenr(leevr*(i_fail-1)+1)) .eq. 4) then
                write(ifm,*) '<DEFILISTINST> ...... DIVE_RESI'
            else if (nint(v_sdlist_evenr(leevr*(i_fail-1)+1)) .eq. 5) then
                write(ifm,*) '<DEFILISTINST> ...... INSTABILITE'
            else if (nint(v_sdlist_evenr(leevr*(i_fail-1)+1)) .eq. 6) then
                write(ifm,*) '<DEFILISTINST> ...... RESI_MAXI'
                write(ifm,*) '<DEFILISTINST> ......... VALE_RESI  :',&
                v_sdlist_evenr(leevr*(i_fail-1)+7)
            else
                ASSERT(.false.)
            endif
!
! --------- Action
!
            if (nint(v_sdlist_evenr(leevr*(i_fail-1)+2)) .eq. 0) then
                write(ifm,*) '<DEFILISTINST> ...... ARRET DU CALCUL'
            else if (nint(v_sdlist_evenr(leevr*(i_fail-1)+2)) .eq. 1) then
                write(ifm,*) '<DEFILISTINST> ...... DECOUPE DU PAS DE TEMPS'
                call dflld2(sdlist, ifm, i_fail)
            else if (nint(v_sdlist_evenr(leevr*(i_fail-1)+2)) .eq. 2) then
                write(ifm,*) '<DEFILISTINST> ...... AUGMENTATION DU NOMBRE D''ITERATIONS DE NEWTON'
                write(ifm,*) '<DEFILISTINST> ......... EN PERMETTANT',&
                               nint(v_sdlist_subdr(lesur*(i_fail-1)+7)),&
                               ' % D''ITERATIONS EN PLUS'
                if (nint(v_sdlist_subdr(lesur*(i_fail-1)+1)) .eq. 0) then
                    write(ifm,*) '<DEFILISTINST> ....... SANS '//&
                    'PERMETTRE UN DECOUPAGE EN CAS D''ECHEC'
                else if (nint(v_sdlist_subdr(lesur*(i_fail-1)+1)) .eq. 1) then
                    write(ifm,*) '<DEFILISTINST> ....... EN '//&
                    'PERMETTANT UN DECOUPAGE EN CAS D''ECHEC'
                    call dflld2(sdlist, ifm, i_fail)
                else if (nint(v_sdlist_subdr(lesur*(i_fail-1)+1)) .eq. 2) then
                    write(ifm,*) '<DEFILISTINST> ....... EN '//&
                    'PERMETTANT UN DECOUPAGE EN CAS D''ECHEC'
                    call dflld2(sdlist, ifm, i_fail)
                else
                    ASSERT(.false.)
                endif
            else if (nint(v_sdlist_evenr(leevr*(i_fail-1)+2)) .eq. 3) then
                write(ifm,*) '<DEFILISTINST> ...... CHANGEMENT DE LA SOLUTION DE PILOTAGE'
                if (nint(v_sdlist_subdr(lesur*(i_fail-1)+1)) .eq. 0) then
                    write(ifm,*) '<DEFILISTINST> ....... SANS '//&
                    'PERMETTRE UN DECOUPAGE EN CAS D''ECHEC'
                else if (nint(v_sdlist_subdr(lesur*(i_fail-1)+1)) .eq. 1) then
                    write(ifm,*) '<DEFILISTINST> ....... EN '//&
                    ' PERMETTANT UN DECOUPAGE EN CAS D''ECHEC'
                    call dflld2(sdlist, ifm, i_fail)
                endif
            else if (nint(v_sdlist_evenr(leevr*(i_fail-1)+2)) .eq. 4) then
                write(ifm,*) '<DEFILISTINST> ...... ADAPTATION DU COEFFICIENT DE PENALISATION'
                write(ifm,*) '<DEFILISTINST> ......... EN PERMETTANT UN COEF. MAXI DE: ',&
                             v_sdlist_subdr(lesur*(i_fail-1)+8)
            else if (nint(v_sdlist_evenr(leevr*(i_fail-1)+2)) .eq. 5) then
                write(ifm,*) '<DEFILISTINST> ...... ON CONTINUE LE CALCUL'
            else
                ASSERT(.false.)
            endif
        end do
    endif
!
! - Adaptation
!
    if (nb_adap .gt. 0) then
        write(ifm,*) '<DEFILISTINST> SCHEMAS D''ADAPTATION DU PAS DE TEMPS (',&
                      nb_adap,' ADAPTATIONS)'
    endif
!
    call jedema()
end subroutine
