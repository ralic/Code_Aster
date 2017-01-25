subroutine afvarc_read_cata(varc_cata)
!
use Material_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
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
!
    type(Mat_DS_VarcListCata), intent(out) :: varc_cata
!
! --------------------------------------------------------------------------------------------------
!
! Material - External state variables (VARC)
!
! Read list of variables from AFFE_MATERIAU catalog
!
! --------------------------------------------------------------------------------------------------
!
! Out varc_cata        : datastructure for catalog of external state variables
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nmxfac=20
    integer :: i_varc_cata, i_cmp
    integer :: nb_varc_cata, nb_cmp, nocc
    character(len=8) :: list_cata_varc(nmxfac), list_cmpgd(nmxfac), list_cmpvarc(nmxfac)
    character(len=8) :: varc_name, phys_para
    character(len=16) :: keywfact, field_type
!
! --------------------------------------------------------------------------------------------------
!
    call getvtx(' ', 'LIST_NOM_VARC', nbval=nmxfac, vect=list_cata_varc, nbret=nb_varc_cata)
    ASSERT(nb_varc_cata .gt. 0)
    ASSERT(nb_varc_cata .le. nmxfac)
!
! - Allocate
!
    varc_cata%nb_varc = nb_varc_cata
    allocate(varc_cata%list_cata_varc(nb_varc_cata))
!
! - Read keywords for definition
!
    do i_varc_cata = 1, nb_varc_cata
        keywfact = 'VARC_'//list_cata_varc(i_varc_cata)
        call getvtx(keywfact, 'NOM_VARC', iocc=1, scal=varc_name, nbret=nocc)
        ASSERT(nocc .eq. 1)
        call getvtx(keywfact, 'GRANDEUR', iocc=1, scal=phys_para, nbret=nocc)
        ASSERT(nocc .eq. 1)
        call getvtx(keywfact, 'CMP_GD', iocc=1, nbval=0, nbret=nb_cmp)
        nb_cmp = -nb_cmp
        call getvtx(keywfact, 'CMP_GD', iocc=1, nbval=nb_cmp, vect=list_cmpgd)
        call getvtx(keywfact, 'CMP_VARC', iocc=1, nbval=0, nbret=nocc)
        nocc = -nocc
        ASSERT(nocc .eq. nb_cmp)
        call getvtx(keywfact, 'CMP_VARC', iocc=1, nbval=nb_cmp, vect=list_cmpvarc)
        varc_cata%list_cata_varc(i_varc_cata)%name           = varc_name
        varc_cata%list_cata_varc(i_varc_cata)%type_phys_para = phys_para
        varc_cata%list_cata_varc(i_varc_cata)%nb_cmp         = nb_cmp
        allocate(varc_cata%list_cata_varc(i_varc_cata)%list_cmp(nb_cmp))
        do i_cmp = 1, nb_cmp
            varc_cata%list_cata_varc(i_varc_cata)%list_cmp(i_cmp)%phys_para_cmp = &
                list_cmpgd(i_cmp)
            varc_cata%list_cata_varc(i_varc_cata)%list_cmp(i_cmp)%varc_cmp      = &
                list_cmpvarc(i_cmp)
        end do   
        if (varc_name .eq. 'SECH') then
            field_type = 'TEMP'
        else if (varc_name .eq. 'HYDR') then
            field_type = 'HYDR_ELNO'
        else if (varc_name .eq. 'HYDR') then
            field_type = 'EPSA'
        else if (varc_name .eq. 'EPSA_ELNO') then
            field_type = 'NEUT'
        else if (varc_name .eq. 'M_ACIER') then
            field_type = 'META_ELNO'
        else if (varc_name .eq. 'M_ZIRC') then
            field_type = 'META_ELNO'
        else if (varc_name(1:4) .eq. 'NEUT') then
            field_type = 'NEUT'
        else
            field_type = varc_name
        endif
        varc_cata%list_cata_varc(i_varc_cata)%field_type_def = field_type
    end do
!
end subroutine
