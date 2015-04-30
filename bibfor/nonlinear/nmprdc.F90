subroutine nmprdc(algo_meth, nume_dof , disp_prev, sddisc, nume_inst,&
                  incr_esti, disp_esti)
!
implicit none
!
#include "asterfort/copisd.h"
#include "asterfort/diinst.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsinch.h"
#include "asterfort/utmess.h"
#include "asterfort/vrrefe.h"
#include "asterfort/vtcopy.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
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
    character(len=16), intent(in) :: algo_meth(*)
    character(len=24), intent(in) :: nume_dof
    character(len=19), intent(in) :: disp_prev
    character(len=19), intent(in) :: sddisc
    integer, intent(in)  :: nume_inst
    character(len=19), intent(in) :: incr_esti
    character(len=19), intent(in) :: disp_esti
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Algorithm - Euler prediction
!
! DEPL_CALCULE option
!
! --------------------------------------------------------------------------------------------------
!
! In  algo_meth        : parameters for algorithm methods
! In  nume_dof         : name of numbering (NUME_DDL)
! In  disp_prev        : previous displacement (T-)
! In  sddisc           : datastructure for time discretization
! In  nume_inst        : index of current time step
! In  incr_esti        : name of increment estimation field
! In  disp_esti        : name of displacement estimation field
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nb_equa, iret
    real(kind=8) :: time
    character(len=19) :: disp_extr
    character(len=8) :: result_extr
    real(kind=8), pointer :: v_disp_esti(:) => null()
    real(kind=8), pointer :: v_disp_prev(:) => null()
    real(kind=8), pointer :: v_incr_esti(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... PAR DEPL. CALCULE'
    endif
!
! - Initializations
!
    call dismoi('NB_EQUA', nume_dof, 'NUME_DDL', repi=nb_equa)
    time        = diinst(sddisc,nume_inst)
!
! - Get results datastructure for PREDICTION='DEPL_CALCULE
!
    result_extr = algo_meth(6)(1:8)
!
! - Get displacement in results datastructure
!
    disp_extr = '&&NMPRDC.DEPEST'
    call rsinch(result_extr, 'DEPL', 'INST', time, disp_extr,&
                'EXCLU', 'EXCLU', 0, 'V', iret)
    if (iret .gt. 0) then
        call utmess('F', 'MECANONLINE2_27', sk=result_extr, sr=time)
    endif
!
! - Copy displacement
!
    if (nume_inst .eq. 1) then
        call vtcopy(disp_extr, disp_esti, 'F', iret)
    else
        call vrrefe(disp_extr, disp_esti, iret)
        if (iret.gt.0) then
            call utmess('F', 'MECANONLINE2_28', sk=result_extr, sr=time)
        else
            call copisd('CHAMP_GD', 'V', disp_extr, disp_esti)
        endif
    endif
!
! - Compute increment: incr_esti = disp_esti - disp_prev
!
    call jeveuo(disp_esti(1:19)//'.VALE', 'L', vr=v_disp_esti)
    call jeveuo(disp_prev(1:19)//'.VALE', 'L', vr=v_disp_prev)
    call jeveuo(incr_esti(1:19)//'.VALE', 'E', vr=v_incr_esti)
    call dcopy(nb_equa, v_disp_esti, 1, v_incr_esti, 1)
    call daxpy(nb_equa, -1.d0, v_disp_prev, 1, v_incr_esti,1)
!
end subroutine
