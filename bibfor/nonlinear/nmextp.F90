subroutine nmextp(keyw_fact, i_keyw_fact, field_type, field  , field_s       ,&
                  list_poin, list_spoi  , nb_poin   , nb_spoi, type_extr_elem)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/exisd.h"
#include "asterfort/jeveuo.h"
#include "asterfort/sdmpic.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
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
    character(len=16), intent(in) :: keyw_fact
    integer, intent(in) :: i_keyw_fact
    character(len=19), intent(in) :: field
    character(len=24), intent(in) :: field_type
    character(len=24), intent(in) :: field_s
    character(len=24), intent(in) :: list_poin
    character(len=24), intent(in) :: list_spoi
    integer, intent(out) :: nb_poin
    integer, intent(out) :: nb_spoi
    character(len=8), intent(out) :: type_extr_elem
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Extraction (OBSERVATION/SUIVI_DDL) utilities 
!
! Get topology (point and subpoints) and type of extraction for element
!
! --------------------------------------------------------------------------------------------------
!
! In  keyw_fact        : factor keyword to read extraction parameters
! In  i_keyw_fact      : index of keyword to read extraction parameters
! In  field            : name of field
! In  field_s          : name of reduced field (CHAM_ELEM_S)
! In  field_type       : type of field (name in results datastructure)
! In  list_poin        : name of object contains list of points (Gauss)
! Out nb_poin          : number of points (Gauss)
! In  list_spoi        : name of object contains list of subpoints
! Out nb_spoi          : number of subpoints
! Out type_extr_elem   : type of extraction by element
!                'MIN'  VALEUR MINI SUR TOUS LES POINTS DE GAUSS
!                'MAX'  VALEUR MAXI SUR TOUS LES POINTS DE GAUSS
!                'MOY'  VALEUR MOYENNE SUR TOUS LES POINTS DE GAUSS
!                'VALE' VALEUR SUR POINT/SOUS_POINTS DONNES
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_poin, i_spoi
    integer :: n1, n2, n3, iret
    integer :: nb_poin_maxi, nb_spoi_maxi
    integer, pointer :: cesd(:) => null()
    integer, pointer :: v_list_poin(:) => null()
    integer, pointer :: v_list_spoi(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nb_poin = 0
    nb_spoi = 0
!
! - Conversion to reduced field (CHAM_ELEM_S)
! 
    call exisd('CHAM_ELEM', field, iret)
    ASSERT(iret.eq.1)
    call exisd('CHAM_ELEM_S', field_s, iret)
    if (iret .eq. 0) then
        call sdmpic('CHAM_ELEM', field)
        call celces(field, 'V', field_s)
    endif
    call jeveuo(field_s(1:19)//'.CESD', 'L', vi=cesd)
!
! - Type of extraction on element
!
    call getvtx(keyw_fact, 'EVAL_ELGA', iocc=i_keyw_fact, scal=type_extr_elem, nbret=n1)
    if (n1 .eq. 0) then
        type_extr_elem = 'VALE'
        call utmess('A', 'EXTRACTION_6', sk=field_type)
    endif
!
! - Max number of points/subpoint for this field
!
    nb_poin_maxi = cesd(3)
    nb_spoi_maxi = cesd(4)
!
! - Number of points/subpoint
!
    if (type_extr_elem .eq. 'VALE') then
        call getvis(keyw_fact, 'POINT', iocc=i_keyw_fact, nbval=0, nbret=n2)
        call getvis(keyw_fact, 'SOUS_POINT', iocc=i_keyw_fact, nbval=0, nbret=n3)
        if (n2 .eq. 0) then
            call utmess('F', 'EXTRACTION_7', sk = field_type)
        endif
        nb_poin = -n2
        if ((n2.ne.0) .and. (n3.eq.0)) then
            nb_spoi = nb_spoi_maxi
        else
            nb_spoi = -n3
        endif
    else
        nb_poin = nb_poin_maxi
        nb_spoi = nb_spoi_maxi
    endif
!
! - Protection
!
    if (nb_poin .gt. nb_poin_maxi) nb_poin = nb_poin_maxi
    if (nb_spoi .gt. nb_spoi_maxi) nb_spoi = nb_spoi_maxi
!
! - Create lists
!
    call wkvect(list_poin, 'V V I', nb_poin, vi = v_list_poin)
    if (nb_spoi .ne. 0) then
        call wkvect(list_spoi, 'V V I', nb_spoi, vi = v_list_spoi)
    endif
!
! - Set lists
!
    if (type_extr_elem .eq. 'VALE') then
        call getvis(keyw_fact, 'POINT', iocc=i_keyw_fact, nbval=nb_poin, vect= v_list_poin)
        if (nb_spoi .ne. 0) then
            call getvis(keyw_fact, 'SOUS_POINT', iocc=i_keyw_fact, nbval=nb_spoi,&
                        vect = v_list_spoi, nbret=n3)
            if (n3 .eq. 0) then
                do i_spoi = 1, nb_spoi
                    v_list_spoi(i_spoi) = i_spoi
                end do
            endif
        endif
    else
        do i_poin = 1, nb_poin
            v_list_poin(i_poin) = i_poin
        end do
        do i_spoi = 1, nb_spoi
            v_list_spoi(i_spoi) = i_spoi
        end do
    endif
!
end subroutine
