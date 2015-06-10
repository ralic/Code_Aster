subroutine surfc3(sdcont, mesh, unit_msg)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: sdcont
    character(len=8), intent(in) :: mesh
    integer, intent(in) :: unit_msg
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Print debug for XFEM formulation
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont           : name of contact concept (DEFI_CONTACT)
! In  mesh             : name of mesh
! In  unit_msg         : logical unit for messages (print)
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_cont_xfem_gg
    integer :: nb_cont_zone
    integer :: i_zone, statut
    character(len=24) :: sdcont_defi
    character(len=8) :: elem_slav_name, zone_name
    integer :: i_elem_slav, nt_elem_slav, elem_slav_nume
    character(len=24) :: sdcont_caraxf
    real(kind=8), pointer :: v_sdcont_caraxf(:) => null()
    character(len=24) :: sdcont_xfimai
    character(len=8), pointer :: v_sdcont_xfimai(:) => null()
    character(len=24) :: sdcont_maescx
    integer, pointer :: v_sdcont_maescx(:) => null()
    integer :: zcmxf, zmesx
!
! --------------------------------------------------------------------------------------------------
!
    sdcont_defi = sdcont(1:8)//'.CONTACT'
!
! - Datastructure for contact definition
!
    sdcont_caraxf = sdcont_defi(1:16)//'.CARAXF'
    sdcont_xfimai = sdcont_defi(1:16)//'.XFIMAI'
    sdcont_maescx = sdcont_defi(1:16)//'.MAESCX'
    call jeveuo(sdcont_caraxf, 'L', vr = v_sdcont_caraxf)
    call jeveuo(sdcont_xfimai, 'L', vk8 = v_sdcont_xfimai)
    zcmxf = cfmmvd('ZCMXF')
    zmesx = cfmmvd('ZMESX')
!
! - Parameters
!
    nb_cont_zone   = cfdisi(sdcont_defi,'NZOCO')
    l_cont_xfem_gg = cfdisl(sdcont_defi,'CONT_XFEM_GG')
    nt_elem_slav   = cfdisi(sdcont_defi,'NTMAE')
!
! - User print
!
    write (unit_msg,*)
    write (unit_msg,*) '<CONTACT> INFOS SPECIFIQUES SUR LA FORMULATION XFEM'
    write (unit_msg,*)
!
    write (unit_msg,*) '<CONTACT> ... ZONES XFEM.'
    do  i_zone = 1, nb_cont_zone
        write (unit_msg,*) '<CONTACT> ...... ZONE : ', i_zone
        write (unit_msg,110) v_sdcont_xfimai(i_zone)
    end do
!
110 format (' <CONTACT> ...... FISS. MAITRE : ',a18)
!
! - Print variables parameters
!
    write (unit_msg,*) '<CONTACT> ... PARAMETRES VARIABLES SUIVANT LES ZONES'
    do i_zone = 1, nb_cont_zone
        write (unit_msg,*) '<CONTACT> ...... ZONE : ',i_zone
        write (unit_msg,171) 'INTEGRATION     ',v_sdcont_caraxf(zcmxf*(i_zone-1)+1)
        write (unit_msg,171) 'COEF_REGU_CONT  ',v_sdcont_caraxf(zcmxf*(i_zone-1)+2)
        write (unit_msg,171) 'COEF_REGU_FROT  ',v_sdcont_caraxf(zcmxf*(i_zone-1)+3)
        write (unit_msg,171) 'COEF_STAB_CONT  ',v_sdcont_caraxf(zcmxf*(i_zone-1)+11)
        write (unit_msg,171) 'COEF_PENA_CONT  ',v_sdcont_caraxf(zcmxf*(i_zone-1)+12)
        write (unit_msg,171) 'COEF_STAB_FROT  ',v_sdcont_caraxf(zcmxf*(i_zone-1)+13)
        write (unit_msg,171) 'COEF_PENA_CONT  ',v_sdcont_caraxf(zcmxf*(i_zone-1)+14)
        write (unit_msg,171) 'FROTTEMENT      ',v_sdcont_caraxf(zcmxf*(i_zone-1)+5)
        write (unit_msg,171) 'COULOMB         ',v_sdcont_caraxf(zcmxf*(i_zone-1)+4)
        write (unit_msg,171) 'SEUIL_INIT      ',v_sdcont_caraxf(zcmxf*(i_zone-1)+6)
        write (unit_msg,171) 'CONTACT_INIT    ',v_sdcont_caraxf(zcmxf*(i_zone-1)+7)
        write (unit_msg,171) 'COEF_ECHELLE    ',v_sdcont_caraxf(zcmxf*(i_zone-1)+8)
        write (unit_msg,171) 'ALGO_LAGR       ',v_sdcont_caraxf(zcmxf*(i_zone-1)+9)
        write (unit_msg,171) 'GLISSIERE       ',v_sdcont_caraxf(zcmxf*(i_zone-1)+10)
    end do
!
! - Slave elements
!
    if (l_cont_xfem_gg) then
        call jeveuo(sdcont_maescx, 'L', vi = v_sdcont_maescx)
        write (unit_msg,*) '<CONTACT> ... INFORMATIONS SUR MAILLES ESCLAVES'
        do i_zone = 1, nb_cont_zone
            zone_name = v_sdcont_xfimai(i_zone)
            write (unit_msg,*) '<CONTACT> ...... ZONE : ',i_zone
            write (unit_msg,110) zone_name
            do i_elem_slav = 1, nt_elem_slav
                elem_slav_nume = v_sdcont_maescx(zmesx*(i_elem_slav-1)+1)
                call jenuno(jexnum(mesh(1:8)//'.NOMMAI', elem_slav_nume), elem_slav_name)
                write (unit_msg,180) elem_slav_name
                write (unit_msg,170) 'ZONE            ',v_sdcont_maescx(zmesx*(i_elem_slav-1)+2)
                write (unit_msg,170) 'NB. PTS. INT.   ',v_sdcont_maescx(zmesx*(i_elem_slav-1)+3)
                statut = v_sdcont_maescx(zmesx*(i_elem_slav-1)+4)
                if (statut .eq. 0) then
                    write (unit_msg,104) 'PAS DE FOND. FISS.'
                else if (statut.eq.1) then
                    write (unit_msg,104) 'HEAVISIDE'
                else if (statut.eq.-2) then
                    write (unit_msg,104) 'CRACK-TIP'
                else if (statut.eq.3) then
                    write (unit_msg,104) 'HEAVISIDE + CRACK-TIP'
                else
                    write (unit_msg,170) 'STATUT          ',statut
                endif
            end do
        end do
    endif
!
104 format (' <CONTACT> ...... ',a25)
170 format (' <CONTACT> ...... PARAM. : ',a16,' - VAL. : ',i5)
171 format (' <CONTACT> ...... PARAM. : ',a16,' - VAL. : ',e12.5)
180 format (' <CONTACT> ... MAILLE : ',a8)
!
end subroutine
