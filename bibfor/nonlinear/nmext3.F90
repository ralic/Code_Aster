subroutine nmext3(mesh         , field    , field_type, field_s       , nb_cmp   ,&
                  nb_elem      , nb_poin  , nb_spoi   , type_extr_elem, type_extr,&
                  type_extr_cmp, list_elem, list_poin , list_spoi     , list_cmp ,&
                  work_poin    , work_elem)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/nmextj.h"
#include "asterfort/sdmpic.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=8), intent(in) :: mesh
    integer, intent(in) :: nb_elem
    integer, intent(in) :: nb_poin
    integer, intent(in) :: nb_spoi
    integer, intent(in) :: nb_cmp
    character(len=19), intent(in) :: field
    character(len=24), intent(in) :: field_type
    character(len=24), intent(in) :: field_s
    character(len=24), intent(in) :: list_elem
    character(len=24), intent(in) :: list_poin
    character(len=24), intent(in) :: list_spoi
    character(len=24), intent(in) :: list_cmp
    character(len=8), intent(in) :: type_extr
    character(len=8), intent(in) :: type_extr_elem
    character(len=8), intent(in) :: type_extr_cmp
    character(len=19), intent(in) :: work_poin
    character(len=19), intent(in) :: work_elem
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Field extraction datastructure
!
! Extract value(s) at point and store them in working vectors
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  nb_elem          : number of elements
! In  nb_poin          : number of points (Gauss)
! In  nb_spoi          : number of subpoints
! In  nb_cmp           : number of components
! In  field            : name of field
! In  field_type       : type of field (name in results datastructure)
! In  field_s          : name of reduced field (CHAM_ELEM_S)
! In  list_elem        : name of object contains list of elements
! In  list_poin        : name of object contains list of points (Gauss)
! In  list_spoi        : name of object contains list of subpoints
! In  list_cmp         : name of object contains list of components
! In  type_extr        : type of extraction
! In  type_extr_elem   : type of extraction by element
! In  type_extr_cmp    : type of extraction for components
! In  work_elem        : working vector to save element values
! In  work_poin        : working vector to save point (Gauss) values
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_para_maxi
    parameter    (nb_para_maxi=20)
    real(kind=8) :: vale_resu(nb_para_maxi)
!
    integer :: i_elem, i_poin, i_spoi, i_elem_r, i_poin_r, i_spoi_r, elem_nume
    integer :: poin_nume, spoi_nume, iret
    integer :: nb_elem_poin, nb_elem_spoi, nb_poin_r, nb_spoi_r
    character(len=8) :: elem_name
    integer :: i_vale, nb_vale
    real(kind=8) :: valr, val2r
    integer :: jcesd, jcesl, jcesv, jcesc
    integer, pointer :: v_list_elem(:) => null()
    integer, pointer :: v_list_poin(:) => null()
    integer, pointer :: v_list_spoi(:) => null()
    real(kind=8), pointer :: v_work_poin(:) => null()
    real(kind=8), pointer :: v_work_elem(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    ASSERT(nb_cmp.le.nb_para_maxi)
!
! - Conversion to reduced field (CHAM_ELEM_S)
!
    call jeexin(field_s, iret)
    if (iret .eq. 0) then
        call sdmpic('CHAM_ELEM', field)
        call celces(field, 'V', field_s)
    endif
!
! - Access to reduced field (CHAM_ELEM_S)
!
    call jeveuo(field_s(1:19)//'.CESD', 'L', jcesd)
    call jeveuo(field_s(1:19)//'.CESL', 'L', jcesl)
    call jeveuo(field_s(1:19)//'.CESV', 'L', jcesv)
    call jeveuo(field_s(1:19)//'.CESC', 'L', jcesc)
!
! - Get access to working vectors
!
    call jeveuo(work_elem, 'E', vr = v_work_elem)
    call jeveuo(work_poin, 'E', vr = v_work_poin)
!
! - Get access to lists
!
    call jeveuo(list_elem, 'L', vi = v_list_elem)
    call jeveuo(list_poin, 'L', vi = v_list_poin)
    call jeveuo(list_spoi, 'L', vi = v_list_spoi)
!
! - Loop on elements
!
    do i_elem = 1, nb_elem
!
! ----- Current element
!
        elem_nume = v_list_elem(i_elem)
        call jenuno(jexnum(mesh(1:8)//'.NOMMAI', elem_nume), elem_name)
!
! --------- Number of points/subpoints on current element
!
            nb_elem_poin = zi(jcesd+5+4*(elem_nume-1))
            nb_elem_spoi = zi(jcesd+5+4*(elem_nume-1)+1)
!
! --------- Check
!
            nb_poin_r = nb_poin
            nb_spoi_r = nb_spoi
            if (nb_poin_r .gt. nb_elem_poin) nb_poin_r = nb_elem_poin
            if (nb_spoi_r .gt. nb_elem_spoi) nb_spoi_r = nb_elem_spoi
!
! --------- Extract and set point/subpoint value(s) by element
!
            do i_poin = 1, nb_poin_r
                do i_spoi = 1, nb_spoi_r
!
! ----------------- Index on point/subpoint
!
                    poin_nume = v_list_poin(i_poin)
                    spoi_nume = v_list_spoi(i_spoi)
!
! ----------------- Extract value at Gauss point
!
                    call nmextj(field_type, nb_cmp , list_cmp , type_extr_cmp, poin_nume,&
                                spoi_nume , nb_vale, elem_nume, jcesd        , jcesv    ,&
                                jcesl     , jcesc  , vale_resu)
!
! ----------------- Select index in working vectors (point/subpoint)
!
                    if (type_extr_elem .eq. 'VALE') then
                        i_poin_r = i_poin
                        i_spoi_r = i_spoi
                    else
                        i_poin_r = 1
                        i_spoi_r = 1
                    endif
!
! ----------------- Save values in working vector (element)
!
                    do i_vale = 1, nb_vale
                        valr  = vale_resu(i_vale)
                        val2r = v_work_poin(1+nb_cmp*(i_vale-1)+&
                                              nb_poin*(i_poin_r-1)+&
                                              nb_spoi*(i_spoi_r-1))
                        if (type_extr_elem .eq. 'VALE') then
                            v_work_poin(1+nb_poin*nb_spoi*(i_vale-1) +&
                                          nb_spoi*(i_poin_r-1)+&
                                          (i_spoi_r-1)) = valr
                        else if (type_extr_elem.eq.'MAX') then
                            v_work_poin(1+nb_poin*nb_spoi*(i_vale-1) +&
                                          nb_spoi*(i_poin_r-1)+&
                                          (i_spoi_r-1)) = max(valr,val2r)
                        else if (type_extr_elem.eq.'MIN') then
                            v_work_poin(1+nb_poin*nb_spoi*(i_vale-1) +&
                                          nb_spoi*(i_poin_r-1)+&
                                          (i_spoi_r-1)) = min(valr,val2r)
                        else if (type_extr_elem.eq.'MOY') then
                            v_work_poin(1+nb_poin*nb_spoi*(i_vale-1) +&
                                          nb_spoi*(i_poin_r-1)+&
                                          (i_spoi_r-1)) = valr+val2r
                        else
                            ASSERT(.false.)
                        endif
                    end do
                end do
            end do
!
! --------- Extract and set point/subpoint value(s) by point/subpoint
!
            do i_poin = 1, nb_poin_r
                do i_spoi = 1, nb_spoi_r
!
! ----------------- Select index in working vectors (point/subpoint)
!
                    if (type_extr_elem .eq. 'VALE') then
                        i_poin_r = i_poin
                        i_spoi_r = i_spoi
                    else
                        i_poin_r = 1
                        i_spoi_r = 1
                    endif
!
! ----------------- Save values in working vector (point)
!
                    do i_vale = 1, nb_vale
!
! ----------------- Select index in working vector (element)
!
                        if (type_extr .eq. 'VALE') then
                            i_elem_r = i_elem
                        else
                            i_elem_r = 1
                        endif
                        valr  = v_work_poin(1+nb_poin*nb_spoi*(i_vale-1)+&
                                              nb_spoi*(i_poin_r-1)+&
                                              (i_spoi_r-1))
                        val2r = v_work_elem(1+nb_cmp*nb_poin*nb_spoi*(i_elem_r-1)+&
                                              nb_poin*nb_spoi*(i_vale-1)+&
                                              nb_spoi*(i_poin_r-1)+&
                                              (i_spoi_r-1))
                        if (type_extr .eq. 'VALE') then
                            v_work_elem(1+nb_cmp*nb_poin*nb_spoi*(i_elem_r-1)+&
                                          nb_poin*nb_spoi*(i_vale-1)+&
                                          nb_spoi*(i_poin_r-1)+&
                                          (i_spoi_r-1)) = valr
                        else if (type_extr.eq.'MAX') then
                            v_work_elem(1+nb_cmp*nb_poin*nb_spoi*(i_elem_r-1)+&
                                          nb_poin*nb_spoi*(i_vale-1)+&
                                          nb_spoi*(i_poin_r-1)+&
                                          (i_spoi_r-1)) = max(valr,val2r)
                        else if (type_extr.eq.'MIN') then
                            v_work_elem(1+nb_cmp*nb_poin*nb_spoi*(i_elem_r-1)+&
                                          nb_poin*nb_spoi*(i_vale-1)+&
                                          nb_spoi*(i_poin_r-1)+&
                                          (i_spoi_r-1)) =  min(valr,val2r)
                        else if (type_extr.eq.'MAXI_ABS') then
                            v_work_elem(1+nb_cmp*nb_poin*nb_spoi*(i_elem_r-1)+&
                                          nb_poin*nb_spoi*(i_vale-1)+&
                                          nb_spoi*(i_poin_r-1)+&
                                          (i_spoi_r-1)) = max(abs(val2r),abs(valr))
                        else if (type_extr.eq.'MINI_ABS') then
                            v_work_elem(1+nb_cmp*nb_poin*nb_spoi*(i_elem_r-1)+&
                                          nb_poin*nb_spoi*(i_vale-1)+&
                                          nb_spoi*(i_poin_r-1)+&
                                          (i_spoi_r-1)) = min(abs(val2r),abs(valr))
                        else if (type_extr.eq.'MOY') then
                            v_work_elem(1+nb_cmp*nb_poin*nb_spoi*(i_elem_r-1)+&
                                          nb_poin*nb_spoi*(i_vale-1)+&
                                          nb_spoi*(i_poin_r-1)+&
                                          (i_spoi_r-1)) = valr+val2r
                        else
                            ASSERT(.false.)
                        endif
                    end do
                end do
            end do
    end do
!
! - For mean value
!
    if (type_extr .eq. 'MOY') then
        i_elem_r = 1
        do i_poin = 1, nb_poin_r
            do i_spoi = 1, nb_spoi_r
                do i_vale = 1, nb_vale
                    val2r = v_work_elem(1+nb_cmp*nb_poin*nb_spoi*(i_elem_r-1)+&
                                          nb_poin*nb_spoi*(i_vale-1)+&
                                          nb_spoi*(i_poin_r-1)+&
                                          (i_spoi_r-1))
                    v_work_elem(1+nb_cmp*nb_poin*nb_spoi*(i_elem_r-1)+&
                                  nb_poin*nb_spoi*(i_vale-1)+&
                                  nb_spoi*(i_poin_r-1) +&
                                  (i_spoi_r-1)) = val2r/nb_elem
                end do
            end do
        end do
    endif
!
end subroutine
