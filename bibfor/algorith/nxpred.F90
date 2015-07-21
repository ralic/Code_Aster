subroutine nxpred(model , mate     , cara_elem, list_load, nume_dof ,&
                  solver, lostat   , tpsthe   , time     , matass   ,&
                  lonch , maprec   , varc_curr, temp_prev, temp_iter,&
                  cn2mbr, hydr_prev, hydr_curr, dry_prev , dry_curr ,&
                  compor, cndirp   , cnchci   , vec2nd   , vec2ni   )
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/asasve.h"
#include "asterfort/ascova.h"
#include "asterfort/copisd.h"
#include "asterfort/jedema.h"
#include "asterfort/nxreso.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/resoud.h"
#include "asterfort/verstp.h"
#include "asterfort/vethbt.h"
#include "asterfort/vethbu.h"
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
! aslint: disable=W1504
!
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: mate
    character(len=24), intent(in) :: cara_elem
    character(len=19), intent(in) :: list_load
    character(len=24), intent(in) :: nume_dof
    character(len=19), intent(in) :: solver
    real(kind=8) :: tpsthe(6)
    character(len=24), intent(in) :: time
    character(len=19), intent(in) :: varc_curr
    integer :: lonch
    character(len=19) :: maprec
    character(len=24) :: matass, cndirp, cnchci, cnresi
    character(len=24) :: temp_iter, temp_prev, vec2nd, vec2ni
    character(len=24) :: hydr_prev, hydr_curr, compor, dry_prev, dry_curr
    aster_logical :: lostat
    character(len=24), intent(in) :: cn2mbr
!
! --------------------------------------------------------------------------------------------------
!
! COMMANDE THER_NON_LINE : PHASE DE PREDICTION
!
! --------------------------------------------------------------------------------------------------
!
!     VAR temp_iter : ITERE PRECEDENT DU CHAMP DE TEMPERATURE
!
! In  cn2mbr : name of vector for second member
!
! --------------------------------------------------------------------------------------------------
!
    integer :: k, j2nd, j2ni
    integer :: jtempm
    integer :: jbuem
    real(kind=8) :: rbid
    real(kind=8) :: time_curr
    character(len=1) :: typres
    character(len=19) :: chsol
    character(len=24) :: bidon, veresi, varesi, vabtla, vebtla
    character(len=24) :: vebuem, vabuem, cnvabt, cnvabu
    character(len=24) :: lload_name, lload_info
    real(kind=8), pointer :: v_cn2mbr(:) => null()
    real(kind=8), pointer :: btla(:) => null()
    real(kind=8), pointer :: dirp(:) => null()
    real(kind=8), pointer :: temp(:) => null()
    real(kind=8), pointer :: vare(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    varesi = '&&VARESI'
    cnvabt = ' '
    cnvabu = ' '
    typres = 'R'
    chsol  = '&&NXPRED.SOLUTION'
    bidon  = '&&FOMULT.BIDON'
    veresi = '&&VERESI           .RELR'
    vebtla = '&&VETBTL           .RELR'
    vabtla = ' '
    vebuem = '&&VEBUEM           .RELR'
    vabuem = ' '
    cnresi = ' '
    time_curr = tpsthe(1)
    lload_name = list_load(1:19)//'.LCHA'
    lload_info = list_load(1:19)//'.INFC'
!
! --- RECUPERATION D'ADRESSES
!
    call jeveuo(vec2nd(1:19)//'.VALE', 'L', j2nd)
    call jeveuo(cn2mbr(1:19)//'.VALE', 'E', vr = v_cn2mbr)
    call jeveuo(vec2ni(1:19)//'.VALE', 'L', j2ni)
    call jeveuo(cndirp(1:19)//'.VALE', 'L', vr=dirp)
    call jeveuo(temp_iter(1:19)//'.VALE', 'E', jtempm)
    call jeveuo(temp_prev(1:19)//'.VALE', 'L', vr=temp)
!
    if (lostat) then
!
!=======================================================================
!  INITIALISATION POUR LE PREMIER PAS DE CALCUL
!=======================================================================
!
! ----- Neumann loads elementary vectors (residuals)
!
        call verstp(model    , lload_name, lload_info, mate     , time_curr,&
                    time     , compor    , temp_prev , temp_prev, hydr_prev,&
                    hydr_curr, dry_prev  , dry_curr  , varc_curr, veresi)
!
! ----- Neumann loads vector (residuals)
!
        call asasve(veresi, nume_dof, typres, varesi)
        call ascova('D', varesi, bidon, 'INST', rbid,&
                    typres, cnresi)
        call jeveuo(cnresi(1:19)//'.VALE', 'L', vr=vare)
!
! --- BT LAMBDA - CALCUL ET ASSEMBLAGE
!
        call vethbt(model, lload_name, lload_info, cara_elem, mate,&
                    temp_prev, vebtla)
        call asasve(vebtla, nume_dof, typres, vabtla)
        call ascova('D', vabtla, bidon, 'INST', rbid,&
                    typres, cnvabt)
        call jeveuo(cnvabt(1:19)//'.VALE', 'L', vr=btla)
!
! --- B . TEMPERATURE - CALCUL ET ASSEMBLAGE
!
        call vethbu(model, matass, lload_name, lload_info, cara_elem,&
                    mate, temp_prev, vebuem)
        call asasve(vebuem, nume_dof, typres, vabuem)
        call ascova('D', vabuem, bidon, 'INST', rbid,&
                    typres, cnvabu)
        call jeveuo(cnvabu(1:19)//'.VALE', 'L', jbuem)
!
        do k = 1, lonch
            v_cn2mbr(k) = zr(j2nd+k-1) - vare(k) + dirp(k) - btla(k)- zr(jbuem+k-1)
        end do
!
! ----- Solve linear system
!
        call nxreso(matass, maprec, solver, cnchci, cn2mbr,&
                    chsol)
!
! --- RECOPIE DANS temp_iter DU CHAMP SOLUTION CHSOL
!
        call copisd('CHAMP_GD', 'V', chsol, temp_iter(1:19))
        call jeveuo(temp_iter(1:19)//'.VALE', 'E', jtempm)
        do k = 1, lonch
            zr(jtempm+k-1) = zr(jtempm+k-1) + temp(k)
        end do
!
    else
!
!=======================================================================
!  INITIALISATION POUR LE PREMIER PAS, CALCUL TRANSITOIRE, PAS COURANT
!=======================================================================
!
        do k = 1, lonch
            v_cn2mbr(k) = zr(j2ni+k-1) + dirp(k)
        end do

!
! ----- Solve linear system
!
        call nxreso(matass, maprec, solver, cnchci, cn2mbr,&
                    chsol)
!
! --- RECOPIE DANS temp_iter DU CHAMP SOLUTION CHSOL
!
        call copisd('CHAMP_GD', 'V', chsol, temp_iter(1:19))
!
    endif
!
    call jedema()
end subroutine
