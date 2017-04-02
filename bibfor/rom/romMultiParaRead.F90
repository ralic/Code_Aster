subroutine romMultiParaRead(ds_multipara)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getfac.h"
#include "asterc/getexm.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/jelira.h"
#include "asterfort/utmess.h"
#include "asterfort/romMultiCoefRead.h"
#include "asterfort/romVariParaRead.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
    type(ROM_DS_MultiPara), intent(inout) :: ds_multipara
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Read data for multiparametric problems
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_multipara     : datastructure for multiparametric problems
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nb_matr = 0
    integer :: i_matr, i_vect, i_vari_para, nbret, nb_vari_coef, nb_vari_para
    character(len=1)  :: matr_type, vect_type, ktyp, matr_elem_type
    character(len=16) :: keywfact, type_vari_coef
    character(len=8) :: matr_asse, vect_asse, gran_name
    type(ROM_DS_MultiCoef) :: ds_multicoef
    type(ROM_DS_VariPara) :: ds_varipara
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM2_20')
    endif
!
! - List of matrix
!
    if (niv .ge. 2) then
        call utmess('I', 'ROM2_25')
    endif
    keywfact  = 'MATR_ASSE'
    matr_type = 'R'
    call getfac(keywfact, nb_matr)
    ASSERT(nb_matr .gt. 0)
    do i_matr = 1, nb_matr
        matr_asse      = ' '
        matr_elem_type = ' '
        if (getexm(keywfact, 'MATRICE') .eq. 1) then
            call getvid(keywfact, 'MATRICE', iocc=i_matr, scal=matr_asse, nbret=nbret)
            ASSERT(nbret .gt. 0)
            call jelira(matr_asse//'           .VALM', 'TYPE', cval=ktyp)
            if (ktyp .eq. 'C') then
                matr_elem_type = 'C'
                matr_type      = 'C'
            elseif (ktyp .eq. 'R') then
                matr_elem_type = 'R'
            else
                ASSERT(.false.)
            endif   
        endif
        call romMultiCoefRead(ds_multicoef, keywfact, i_matr)
        ds_multipara%matr_name(i_matr) = matr_asse
        ds_multipara%matr_type(i_matr) = matr_elem_type
        ds_multipara%matr_coef(i_matr) = ds_multicoef
    end do
    ds_multipara%nb_matr      = nb_matr
!
! - Get informations about second member
!
    if (niv .ge. 2) then
        call utmess('I', 'ROM2_23')
    endif
    keywfact  = ' '
    i_vect    = 1
    call getvid(keywfact, 'VECTEUR', iocc=0, scal=vect_asse, nbret=nbret)
    call dismoi('NOM_GD', vect_asse, 'CHAM_NO', repk=gran_name)
    if (gran_name .eq. 'DEPL_R') then
        vect_type = 'R'
    elseif (gran_name .eq. 'DEPL_C') then
        vect_type = 'C'
    else
        ASSERT(.false.)
    endif
    call romMultiCoefRead(ds_multicoef, keywfact, i_vect)
    ds_multipara%vect_name = vect_asse
    ds_multipara%vect_type = vect_type
    ds_multipara%vect_coef = ds_multicoef
!
! - Set system type
!
    ds_multipara%syst_type    = 'R'
    if (vect_type .eq. 'C' .or. matr_type .eq. 'C') then
        ds_multipara%syst_type    = 'C'
    endif
!
! - Read data for variations of multiparametric problems
!
    call getvis(' ', 'NB_VARI_COEF' , scal = nb_vari_coef, nbret = nbret)
    ASSERT(nbret .eq. 1)
    ds_multipara%nb_vari_coef   = nb_vari_coef
    call getvtx(' ', 'TYPE_VARI_COEF', scal = type_vari_coef, nbret = nbret)
    ds_multipara%type_vari_coef = type_vari_coef
    keywfact  = 'VARI_PARA'
    call getfac(keywfact, nb_vari_para)
    if (niv .ge. 2) then
        if (nb_vari_para .eq. 0) then
            call utmess('I', 'ROM2_38')
        else
            call utmess('I', 'ROM2_24')
        endif
    endif
    ds_multipara%nb_vari_para = nb_vari_para
    do i_vari_para = 1, nb_vari_para
        call romVariParaRead(ds_varipara, keywfact, i_vari_para)
        ds_multipara%vari_para(i_vari_para) = ds_varipara
    end do
!
end subroutine
