subroutine ddr_comp(ds_empi, v_list_equa)
!
use Rom_Datastructure_type
!
implicit none
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterc/r8gaem.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
#include "blas/dgesv.h"
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
! aslint: disable=W1304
!
    type(ROM_DS_Empi), intent(in) :: ds_empi
    integer, pointer, intent(in)  :: v_list_equa(:)
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_DOMAINE_REDUIT - Main process
!
! Application of DEIM method
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_empi          : datastructure for empiric modes
! In  v_list_equa      : list of dof chosen by DEIM method
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv, iret
    integer :: i_equa, i_mode, i_vect, i_matr, k_mode    
    integer :: nb_equa, nb_mode, nb_vect, jv_para, nb_motr
    integer :: equa_maxi, lval, ntp, ntm
    integer(kind=4) :: info
    integer(kind=4), pointer :: IPIV(:) => null()
    character(len=8)  :: base
    character(len=24) :: field_type
    character(len=24) :: mode = '&&CEIM_MODE'
    real(kind=8) :: vale_maxi, term
    real(kind=8), pointer :: v_mode(:) => null()
    real(kind=8), pointer :: v_resi(:) => null()
    real(kind=8), pointer :: v_base(:) => null()
    real(kind=8), pointer :: v_matr(:) => null()
    real(kind=8), pointer :: v_vect(:) => null()
    integer, pointer :: v_npl(:) => null()
    integer, pointer :: v_tuan(:) => null()
    integer, pointer :: v_list_loca(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM4_20', sk = ds_empi%base)
    endif
!
! - Get parameters
!        
    nb_equa     = ds_empi%nb_equa
    nb_mode     = ds_empi%nb_mode
    base        = ds_empi%base
    field_type  = ds_empi%field_type
!
! - Prepare working objects
!
    AS_ALLOCATE(vr = v_resi, size = nb_equa)
    AS_ALLOCATE(vi = v_npl , size = nb_mode)
    AS_ALLOCATE(vi = v_tuan, size = nb_mode)
!
! - Get informations from the reduced base
!
    do i_mode = 1, nb_mode
        call rsadpa(base, 'L', 1, 'NUME_PLAN', i_mode, 0, sjv=jv_para)
        v_npl(i_mode) = zi(jv_para)
    enddo
    ntp = 1
    do i_mode = 2, nb_mode
        if(v_npl(i_mode).ne.v_npl(i_mode-1)) then
            ntm = ntp
            ntp = 1
            v_tuan(i_mode - ntm) = ntm
        else
            v_tuan(i_mode) = 0
            ntp = ntp +1
        endif
    enddo
    v_tuan(nb_mode+1-ntp) = ntp
!
! - Loop on modes
!
    do i_mode = 1, nb_mode
! - Check the mode (linear or 3D, how many slices?)
        if (v_tuan(i_mode).ne. 0) then
            nb_motr = v_tuan(i_mode)
            AS_ALLOCATE(vr=v_base, size=nb_equa*nb_motr)
            AS_ALLOCATE(vi=v_list_loca, size=nb_motr)
! - First mode of slice
            k_mode = 1
            call rsexch(' ', base, field_type, i_mode+k_mode-1, mode, iret)
            ASSERT(iret .eq. 0)
            call jeveuo(mode(1:19)//'.VALE', 'E', vr = v_mode)
            vale_maxi   = -r8gaem()
            equa_maxi   = 0
            do i_equa = 1, nb_equa
                v_base(i_equa) = v_mode(i_equa)
                if (abs(v_mode(i_equa)) .ge. vale_maxi) then
                    vale_maxi = abs(v_mode(i_equa))
                    equa_maxi = i_equa
                endif
            enddo
            v_list_equa(i_mode) = equa_maxi
            v_list_loca(k_mode) = equa_maxi
! - Loop on mode of slice
            do k_mode = 2, nb_motr
                call rsexch(' ', base, field_type, i_mode+k_mode-1, mode, iret)
                ASSERT(iret .eq. 0)
                call jeveuo(mode(1:19)//'.VALE', 'E', vr = v_mode)
                nb_vect = k_mode-1
                AS_ALLOCATE(vr=v_vect, size=nb_vect)
                AS_ALLOCATE(vr=v_matr, size=nb_vect*nb_vect)
                AS_ALLOCATE(vi4=IPIV, size=nb_vect)
                do i_vect = 1, nb_vect
                    v_vect(i_vect) = v_mode(v_list_loca(i_vect))
                    do i_matr = 1, nb_vect
                        v_matr(i_vect+nb_vect*(i_matr-1)) =&
                            v_base(v_list_loca(i_vect)+nb_equa*(i_matr-1))
                    enddo
                enddo
                lval = MAX(1,nb_vect)
                call dgesv(nb_vect,1,v_matr,lval,IPIV,v_vect,lval,info)
                if (info .ne. 0) then
                    call utmess('F', 'ROM4_7')
                endif
                do i_equa = 1, nb_equa
                    term = 0
                    do i_vect = 1, nb_vect
                        term = term+v_base(i_equa+nb_equa*(i_vect-1))*v_vect(i_vect)    
                    enddo
                    v_resi(i_equa)=v_mode(i_equa)-term        
                enddo
                vale_maxi   = -r8gaem()
                equa_maxi   = 0
                do i_equa = 1, nb_equa
                    v_base(i_equa+nb_equa*(k_mode-1)) = v_mode(i_equa)
                    if (abs(v_resi(i_equa)) .ge. vale_maxi) then
                        vale_maxi = abs(v_resi(i_equa))
                        equa_maxi = i_equa
                    endif
                enddo
                v_list_equa(i_mode+k_mode-1) = equa_maxi
                v_list_loca(k_mode) = equa_maxi
                AS_DEALLOCATE(vi4=IPIV)
                AS_DEALLOCATE(vr=v_vect)
                AS_DEALLOCATE(vr=v_matr)   
            enddo
            AS_DEALLOCATE(vr=v_base)
            AS_DEALLOCATE(vi=v_list_loca)
        endif        
    enddo
    AS_DEALLOCATE(vr=v_resi)
    AS_DEALLOCATE(vi=v_npl)
    AS_DEALLOCATE(vi=v_tuan)
!
end subroutine
