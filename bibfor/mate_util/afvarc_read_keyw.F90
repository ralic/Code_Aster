subroutine afvarc_read_keyw(varc_cata, varc_affe)
!
use Material_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getfac.h"
#include "asterc/r8nnem.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/dismoi.h"
#include "asterfort/utmess.h"
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
    type(Mat_DS_VarcListCata), intent(in)  :: varc_cata
    type(Mat_DS_VarcListAffe), intent(out) :: varc_affe
!
! --------------------------------------------------------------------------------------------------
!
! Material - External state variables (VARC)
!
! Get external state variables affected from command file
!
! --------------------------------------------------------------------------------------------------
!
! In  varc_cata        : datastructure for catalog of external state variables
! Out varc_affe        : datastructure for external state variables affected
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_affe_varc, nb_varc_cata, nb_cmp, nb_varc_cmp
    integer :: nocc, n1, n2
    integer :: i_affe_varc, i_varc_cata
    integer :: indx_cata
    character(len=24) :: valk(3)
    character(len=8) :: varc_name, type_affe
    real(kind=8) :: vale_refe
    character(len=8) :: evol_func, evol
    character(len=16) :: evol_prol_l, evol_prol_r, vale_phys_para
    character(len=8) :: type_phys_para, type_phys_par2
    aster_logical :: l_found
!
! --------------------------------------------------------------------------------------------------
!
    nb_varc_cmp  = 0
    nb_varc_cata = varc_cata%nb_varc
!
    call getfac('AFFE_VARC', nb_affe_varc)
    varc_affe%nb_affe_varc = nb_affe_varc
!
! - Count
!
    do i_varc_cata = 1, nb_varc_cata
        nb_cmp = varc_cata%list_cata_varc(i_varc_cata)%nb_cmp
        l_found = .false.
        do i_affe_varc = 1, nb_affe_varc
            call getvtx('AFFE_VARC', 'NOM_VARC', iocc=i_affe_varc, scal=varc_name, nbret=nocc)
            ASSERT(nocc .eq. 1)
            if (varc_name .eq. varc_cata%list_cata_varc(i_varc_cata)%name) then
                l_found = .true.
                exit
            endif
        end do
        if (l_found) then
            nb_varc_cmp = nb_varc_cmp + nb_cmp
        endif
    end do
!
    varc_affe%nb_varc_cmp = nb_varc_cmp
!
! - Get properties
!
    if (nb_affe_varc .ne. 0) then
! ----- Allocate
        allocate(varc_affe%list_affe_varc(nb_affe_varc))
        do i_affe_varc = 1, nb_affe_varc
! --------- Get index to catalog
            call getvtx('AFFE_VARC', 'NOM_VARC', iocc=i_affe_varc, scal=varc_name, nbret=nocc)
            ASSERT(nocc .eq. 1)
            indx_cata = 0
            do i_varc_cata = 1, nb_varc_cata
                nb_cmp = varc_cata%list_cata_varc(i_varc_cata)%nb_cmp
                if (varc_name .eq. varc_cata%list_cata_varc(i_varc_cata)%name) then
                    indx_cata   = i_varc_cata
                    exit
                endif
            end do
            ASSERT(indx_cata .gt. 0)
            varc_affe%list_affe_varc(i_affe_varc)%indx_cata = indx_cata
! --------- Reference value
            call getvr8('AFFE_VARC', 'VALE_REF', iocc=i_affe_varc, scal=vale_refe, nbret=nocc)
            if (nocc .eq. 0) then
                vale_refe = r8vide()
            endif
            varc_affe%list_affe_varc(i_affe_varc)%vale_refe = vale_refe
! --------- To affect
            type_affe      = 'VIDE'
            vale_phys_para = ' '
            evol           = ' '
            call getvid('AFFE_VARC', 'CHAM_GD', iocc=i_affe_varc, scal=vale_phys_para, nbret=n1)
            call getvid('AFFE_VARC', 'EVOL'   , iocc=i_affe_varc, scal=evol          , nbret=n2)
            ASSERT(n1+n2 .le. 1)
            if (n1 .eq. 1) then
                type_affe = 'CHAMP'
                evol      = ' '
            else if (n2 .eq. 1) then
                type_affe      = 'EVOL'
                vale_phys_para = ' '
            else
                type_affe      = 'VIDE'
                vale_phys_para = ' '
                evol           = ' '
                if (varc_name .ne. 'TEMP') then
                    call utmess('F', 'MATERIAL2_11', sk=varc_name)
                endif
!           -- POUR LA THM, ON PEUT UTILISER VALE_REF SANS DONNER CHAM_GD NI EVOL :
                ASSERT(vale_refe .ne. r8vide())
            endif
            evol_prol_l = ' '
            evol_prol_r = ' '
            evol_func   = ' '
            if (type_affe .eq. 'EVOL') then
                call getvtx('AFFE_VARC', 'PROL_GAUCHE', iocc=i_affe_varc, scal=evol_prol_l,&
                            nbret=n1)
                call getvtx('AFFE_VARC', 'PROL_DROITE', iocc=i_affe_varc, scal=evol_prol_r,&
                            nbret=n1)
                call getvid('AFFE_VARC', 'FONC_INST'  , iocc=i_affe_varc, scal=evol_func  ,&
                            nbret=n1)
                if (n1 .eq. 0) then
                    evol_func   = ' '
                endif
                call getvtx('AFFE_VARC', 'NOM_CHAM'   , iocc=i_affe_varc, scal=vale_phys_para,&
                            nbret=n1)
                if (n1 .eq. 0) then
                    vale_phys_para = varc_cata%list_cata_varc(indx_cata)%field_type_def
                endif
            endif
            varc_affe%list_affe_varc(i_affe_varc)%type_affe      = type_affe
            varc_affe%list_affe_varc(i_affe_varc)%vale_phys_para = vale_phys_para
            varc_affe%list_affe_varc(i_affe_varc)%evol           = evol
            varc_affe%list_affe_varc(i_affe_varc)%evol_func      = evol_func
            varc_affe%list_affe_varc(i_affe_varc)%evol_prol_l    = evol_prol_l
            varc_affe%list_affe_varc(i_affe_varc)%evol_prol_r    = evol_prol_r
! --------- Check
            if (type_affe .eq. 'CHAMP') then
                type_phys_para = varc_cata%list_cata_varc(i_varc_cata)%type_phys_para
                call dismoi('NOM_GD', vale_phys_para, 'CHAMP', repk=type_phys_par2)
                if (type_phys_para .ne. type_phys_par2) then
                    valk(1) = varc_name
                    valk(2) = type_phys_para
                    valk(3) = type_phys_par2
                    call utmess('F', 'MATERIAL2_50', nk=3, valk=valk)
                endif
            endif
! --------- Read topology ?
        end do
    endif
!
    if (.false.) then
        write(6,*) 'Nombre d affectactions :', varc_affe%nb_affe_varc
        do i_affe_varc = 1, varc_affe%nb_affe_varc
            write(6,*) 'Variable de commande :', i_affe_varc
            write(6,*) 'Lié au catalogue :', varc_affe%list_affe_varc(i_affe_varc)%indx_cata
            write(6,*) ' > VALE_REFE :', &
                varc_affe%list_affe_varc(i_affe_varc)%vale_refe
            write(6,*) ' > Valeur du champ  :', &
                varc_affe%list_affe_varc(i_affe_varc)%vale_phys_para
            write(6,*) ' > Type affectation :', &
                varc_affe%list_affe_varc(i_affe_varc)%type_affe
            if (varc_affe%list_affe_varc(i_affe_varc)%type_affe .eq. 'EVOL') then
                write(6,*) ' > EVOL :', &
                    varc_affe%list_affe_varc(i_affe_varc)%evol
                write(6,*) ' > FONC_INST :', &
                    varc_affe%list_affe_varc(i_affe_varc)%evol_func
                write(6,*) ' > PROL_GAUCHE :', &
                    varc_affe%list_affe_varc(i_affe_varc)%evol_prol_l
                write(6,*) ' > PROL_DROITE :', &
                    varc_affe%list_affe_varc(i_affe_varc)%evol_prol_r
            endif
        end do
        write(6,*) 'Nombre total de composantes :', varc_affe%nb_varc_cmp
    endif
!
end subroutine
