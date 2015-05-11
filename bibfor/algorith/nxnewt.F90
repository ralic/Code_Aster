subroutine nxnewt(model    , mate       , cara_elem  , list_load, nume_dof ,&
                  solver   , tpsthe     , time       , lonch    , matass   ,&
                  maprec   , cnchci     , varc_curr  , temp_prev, temp_iter,&
                  vtempp   , vec2nd     , mediri     , conver   , hydr_prev,&
                  hydr_curr, dry_prev   , dry_curr   , compor   , cnvabt   ,&
                  cnresi   , ther_crit_i, ther_crit_r, reasma   , testr    ,&
                  testm)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/asasve.h"
#include "asterfort/ascova.h"
#include "asterfort/asmatr.h"
#include "asterfort/copisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/merxth.h"
#include "asterfort/preres.h"
#include "asterfort/resoud.h"
#include "asterfort/verstp.h"
#include "asterfort/vethbt.h"
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
    aster_logical :: conver, reasma
    character(len=19) :: maprec
    character(len=24) :: matass, cnchci, cnresi, temp_prev, temp_iter, vtempp, vec2nd
    character(len=24) :: hydr_prev, hydr_curr, compor, dry_prev, dry_curr
    integer :: ther_crit_i(*)
    real(kind=8) :: ther_crit_r(*)
!
! --------------------------------------------------------------------------------------------------
!
! COMMANDE THER_NON_LINE : ITERATION DE NEWTON
!
! --------------------------------------------------------------------------------------------------
!
!     VAR temp_iter : ITERE PRECEDENT DU CHAMP DE TEMPERATURE
!     OUT VTEMPP : ITERE COURANT   DU CHAMP DE TEMPERATURE
!     OUT VHYDRP : ITERE COURANT   DU CHAMP D HYDRATATION
!
! --------------------------------------------------------------------------------------------------
!
    complex(kind=8) :: cbid
    integer :: k, j2nd, ibid
    integer :: jmed, jmer, nbmat, ierr
    real(kind=8) :: r8bid
    character(len=1) :: typres
    character(len=19) :: chsol
    character(len=24) :: bidon, veresi, varesi, vabtla, vebtla, criter
    character(len=24) :: tlimat(2), mediri, merigi, cnvabt
    real(kind=8) :: testr, testm, vnorm
    real(kind=8) :: time_curr
    character(len=24) :: lload_name, lload_info
    integer :: iret
    real(kind=8), pointer :: btla(:) => null()
    real(kind=8), pointer :: tempp(:) => null()
    real(kind=8), pointer :: vare(:) => null()
    cbid = dcmplx(0.d0, 0.d0)
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    varesi = '&&VARESI'
    vabtla = '&&VATBTL'
    cnresi = ' '
    cnvabt = ' '
    criter = '&&RESGRA_GCPC'
    typres = 'R'
    chsol  = '&&NXNEWT.SOLUTION'
    bidon  = '&&FOMULT.BIDON'
    veresi = '&&VERESI'
    vebtla = '&&VETBTL           .RELR'
    merigi = '&&METRIG           .RELR'
    time_curr = tpsthe(1)
    lload_name = list_load(1:19)//'.LCHA'
    lload_info = list_load(1:19)//'.INFC'
!
! --- RECUPERATION D'ADRESSES
!
    call jeveuo(vec2nd(1:19)//'.VALE', 'L', j2nd)
!
! - Neumann loads elementary vectors (residuals)
!
    call verstp(model    , lload_name, lload_info, mate     , time_curr,&
                time     , compor    , temp_prev , temp_iter, hydr_prev,&
                hydr_curr, dry_prev  , dry_curr  , varc_curr, veresi)
!
! - Neumann loads vector (residuals)
!
    call asasve(veresi, nume_dof, typres, varesi)
    call ascova('D', varesi, bidon, 'INST', r8bid,&
                typres, cnresi)
    call jeveuo(cnresi(1:19)//'.VALE', 'L', vr=vare)
!
! --- BT LAMBDA - CALCUL ET ASSEMBLAGE
!
    call vethbt(model, lload_name, lload_info, cara_elem, mate,&
                temp_iter, vebtla)
    call asasve(vebtla, nume_dof, typres, vabtla)
    call ascova('D', vabtla, bidon, 'INST', r8bid,&
                typres, cnvabt)
    call jeveuo(cnvabt(1:19)//'.VALE', 'L', vr=btla)
!
!==========================================================
! --- CALCUL DU RESIDU ET
!     DU CRITERE DE CONVERGENCE DES ITERATIONS (NORME SUP)
!==========================================================
!
    call jeveuo(vtempp(1:19)//'.VALE', 'E', vr=tempp)
    testr = 0.d0
    testm = 0.d0
    vnorm = 0.d0
    do k = 1, lonch
        tempp(k) = zr(j2nd+k-1) - vare(k) - btla(k)
        testr = testr + ( tempp(k) )**2
        vnorm = vnorm + ( zr(j2nd+k-1) - btla(k) )**2
        testm = max( testm,abs( tempp(k) ) )
    end do
    if (vnorm .gt. 0d0) then
        testr = sqrt( testr / vnorm )
    endif
!
    if (ther_crit_i(1) .ne. 0) then
        if (testm .lt. ther_crit_r(1)) then
            conver=.true.
            call copisd('CHAMP_GD', 'V', temp_iter(1:19), vtempp(1:19))
            goto 999
        else
            conver=.false.
        endif
    else
        if (testr .lt. ther_crit_r(2)) then
            conver=.true.
            call copisd('CHAMP_GD', 'V', temp_iter(1:19), vtempp(1:19))
            goto 999
        else
            conver=.false.
        endif
    endif
!
    if (reasma) then
!
! ---- Tangent matrix (non-linear) - Volumic and surfacic terms
!
        call merxth(model    , lload_name, lload_info, cara_elem, mate     ,&
                    time_curr, time      , temp_iter , compor   , varc_curr,&
                    dry_prev , dry_curr  , merigi)
        call jeveuo(merigi, 'L', jmer)
        call jeveuo(mediri, 'L', jmed)
!
        nbmat = 0
        if (zk24(jmer)(1:8) .ne. '        ') then
            nbmat = nbmat + 1
            tlimat(nbmat) =merigi(1:19)
        endif
        if (zk24(jmed)(1:8) .ne. '        ') then
            nbmat = nbmat + 1
            tlimat(nbmat) =mediri(1:19)
        endif
!
! --- ASSEMBLAGE DE LA MATRICE
!
        call asmatr(nbmat, tlimat, ' ', nume_dof, solver,&
                    list_load, 'ZERO', 'V', 1, matass)
!
! --- DECOMPOSITION OU CALCUL DE LA MATRICE DE PRECONDITIONNEMENT
!
        call preres(solver, 'V', ierr, maprec, matass,&
                    ibid, -9999)
!
    endif
!
!==========================================================
! --- RESOLUTION (VTEMPP CONTIENT LE SECOND MEMBRE, CHSOL LA SOLUTION)
!
    call resoud(matass, maprec, solver, cnchci, 0,&
                vtempp, chsol, 'V', [0.d0], [cbid],&
                criter, .true._1, 0, iret)
!
! --- RECOPIE DANS VTEMPP DU CHAMP SOLUTION CHSOL,
!     INCREMENT DE TEMPERATURE
!
    call copisd('CHAMP_GD', 'V', chsol, vtempp(1:19))
!
999 continue
    call jedema()
end subroutine
