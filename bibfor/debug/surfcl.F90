subroutine surfcl(sdcont, mesh, unit_msg)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/mminfr.h"
#include "asterfort/surfll.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
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
! Print debug for meshed formulations (discrete/continue)
!
! --------------------------------------------------------------------------------------------------
!
! In  unit_msg         : logical unit for messages (print)
! In  mesh             : name of mesh
! In  sdcont           : name of contact concept (DEFI_CONTACT)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: zdirn
    integer :: model_ndim, nb_cont_zone, nb_cont_surf, nb_cont_elem
    integer :: nb_cont_node, nt_elem_node, nt_node_elem
    integer :: nt_node_slav, nt_elem_slav, nt_node_slavc, nt_elem_slavc
    integer :: nt_node_mast, nt_elem_mast, nt_node_mastc, nt_elem_mastc
    integer :: nb_node_excl, nb_cont_poinc, nb_cont_poin
    integer :: i_zone, i_surf, i_elem, i_node, node_nume
    real(kind=8) :: tole_interp, tole_appa, tole_proj_ext
    integer :: type_norm, type_appa_search, type_norm_mast, type_norm_slav, type_appa
    aster_logical :: l_dist_shell, l_dist_beam, l_veri
    aster_logical :: l_liss, l_exis_verif, lstop
    character(len=8) :: jeuf1, jeuf2
    character(len=24) :: sdcont_defi
    character(len=24) :: sdcont_pzoneco
    integer, pointer :: v_sdcont_pzoneco(:) => null()
    character(len=24) :: sdcont_mailco
    integer, pointer :: v_sdcont_mailco(:) => null()
    character(len=24) :: sdcont_noeuco
    integer, pointer :: v_sdcont_noeuco(:) => null()
    character(len=24) :: sdcont_psumaco
    integer, pointer :: v_sdcont_psumaco(:) => null()
    character(len=24) :: sdcont_psunoco
    integer, pointer :: v_sdcont_psunoco(:) => null()
    character(len=24) :: sdcont_ssnoco
    integer, pointer :: v_sdcont_ssnoco(:) => null()
    character(len=24) :: sdcont_pssnoco
    integer, pointer :: v_sdcont_pssnoco(:) => null()
    character(len=24) :: sdcont_dirapp
    real(kind=8), pointer :: v_sdcont_dirapp(:) => null()
    character(len=24) :: sdcont_dirnor
    real(kind=8), pointer :: v_sdcont_dirnor(:) => null()
    character(len=24) :: sdcont_jeufo1
    character(len=8), pointer :: v_sdcont_jeufo1(:) => null()
    character(len=24) :: sdcont_jeufo2
    character(len=8), pointer :: v_sdcont_jeufo2(:) => null()
    character(len=24) :: sdcont_manoco
    integer, pointer :: v_sdcont_manoco(:) => null()
    character(len=24) :: sdcont_pmanoco
    integer, pointer :: v_sdcont_pmanoco(:) => null()
    character(len=24) :: sdcont_nomaco
    integer, pointer :: v_sdcont_nomaco(:) => null()
    character(len=24) :: sdcont_pnomaco
    integer, pointer :: v_sdcont_pnomaco(:) => null()
    character(len=8), pointer :: v_work_node(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    sdcont_defi = sdcont(1:8)//'.CONTACT'
!
! - Datastructure for contact definition
!
    sdcont_pzoneco = sdcont_defi(1:16)//'.PZONECO'
    sdcont_mailco  = sdcont_defi(1:16)//'.MAILCO'
    sdcont_noeuco  = sdcont_defi(1:16)//'.NOEUCO'
    sdcont_psumaco = sdcont_defi(1:16)//'.PSUMACO'
    sdcont_psunoco = sdcont_defi(1:16)//'.PSUNOCO'
    sdcont_ssnoco  = sdcont_defi(1:16)//'.SSNOCO'
    sdcont_pssnoco = sdcont_defi(1:16)//'.PSSNOCO'
    sdcont_dirapp  = sdcont_defi(1:16)//'.DIRAPP'
    sdcont_dirnor  = sdcont_defi(1:16)//'.DIRNOR'
    sdcont_manoco  = sdcont_defi(1:16)//'.MANOCO'
    sdcont_pmanoco = sdcont_defi(1:16)//'.PMANOCO'
    sdcont_nomaco  = sdcont_defi(1:16)//'.NOMACO'
    sdcont_pnomaco = sdcont_defi(1:16)//'.PNOMACO'
    sdcont_jeufo1  = sdcont_defi(1:16)//'.JFO1CO'
    sdcont_jeufo2  = sdcont_defi(1:16)//'.JFO2CO'
    call jeveuo(sdcont_pzoneco, 'L', vi = v_sdcont_pzoneco)
    call jeveuo(sdcont_mailco , 'L', vi = v_sdcont_mailco)
    call jeveuo(sdcont_noeuco , 'L', vi = v_sdcont_noeuco)
    call jeveuo(sdcont_psumaco, 'L', vi = v_sdcont_psumaco)
    call jeveuo(sdcont_psunoco, 'L', vi = v_sdcont_psunoco)
    call jeveuo(sdcont_ssnoco , 'L', vi = v_sdcont_ssnoco)
    call jeveuo(sdcont_pssnoco, 'L', vi = v_sdcont_pssnoco)
    call jeveuo(sdcont_dirapp , 'L', vr = v_sdcont_dirapp)
    call jeveuo(sdcont_dirnor , 'L', vr = v_sdcont_dirnor)
    call jeveuo(sdcont_manoco , 'L', vi = v_sdcont_manoco)
    call jeveuo(sdcont_pmanoco, 'L', vi = v_sdcont_pmanoco)
    call jeveuo(sdcont_nomaco , 'L', vi = v_sdcont_nomaco)
    call jeveuo(sdcont_pnomaco, 'L', vi = v_sdcont_pnomaco)
    call jeveuo(sdcont_jeufo1 , 'L', vk8 = v_sdcont_jeufo1)
    call jeveuo(sdcont_jeufo2 , 'L', vk8 = v_sdcont_jeufo2)
    zdirn = cfmmvd('ZDIRN')
!
! - Parameters
!
    model_ndim    = cfdisi(sdcont_defi,'NDIM' )
    nb_cont_zone  = cfdisi(sdcont_defi,'NZOCO' )
    nb_cont_surf  = cfdisi(sdcont_defi,'NSUCO' )
    nb_cont_elem  = cfdisi(sdcont_defi,'NMACO' )
    nb_cont_node  = cfdisi(sdcont_defi,'NNOCO' )
    nt_elem_slav  = cfdisi(sdcont_defi,'NTMAE' )
    nt_node_slav  = cfdisi(sdcont_defi,'NTNOE' )
    nt_elem_slavc = cfdisi(sdcont_defi,'NTMAEC')
    nt_node_slavc = cfdisi(sdcont_defi,'NTNOEC')
    nt_elem_mast  = cfdisi(sdcont_defi,'NTMAM' )
    nt_node_mast  = cfdisi(sdcont_defi,'NTNOM' )
    nt_elem_mastc = cfdisi(sdcont_defi,'NTMAMC')
    nt_node_mastc = cfdisi(sdcont_defi,'NTNOMC')
    nb_cont_poinc = cfdisi(sdcont_defi,'NTPC' )
    nb_cont_poin  = cfdisi(sdcont_defi,'NTPT' )
!
! - Temporary vector
!
    AS_ALLOCATE(vk8=v_work_node, size=nb_cont_zone*nb_cont_node)
!
! - User print
!
    write (unit_msg,*)
    write (unit_msg,*) '<CONTACT> INFOS GENERALES SUR LES FORMULATIONS MAILLEES'
    write (unit_msg,*)
!
! - General parameters for pairing
!
    write (unit_msg,*) '<CONTACT> ... PARAMETRES GENERAUX APPARIEMENT'
!
! - Normals smoothing
!
    l_liss = cfdisl(sdcont_defi,'LISSAGE')
    if (l_liss) then
        write (unit_msg,*) '<CONTACT> ...... NORMALES LISSEES'
    endif
!
    do i_zone = 1, nb_cont_zone
        type_norm        = mminfi(sdcont_defi, 'NORMALE'      , i_zone)
        type_appa        = mminfi(sdcont_defi, 'APPARIEMENT'  , i_zone)
        type_appa_search = mminfi(sdcont_defi, 'TYPE_APPA'    , i_zone)
        type_norm_mast   = mminfi(sdcont_defi, 'VECT_MAIT'    , i_zone)
        type_norm_slav   = mminfi(sdcont_defi, 'VECT_ESCL'    , i_zone)
        tole_proj_ext    = mminfr(sdcont_defi, 'TOLE_PROJ_EXT', i_zone)
        tole_appa        = mminfr(sdcont_defi, 'TOLE_APPA'    , i_zone)
!
        write (unit_msg,*) '<CONTACT> ...... OPTIONS SUR LA ZONE : ',i_zone
!
        write (unit_msg,170) 'APPARIEMENT     ',type_appa
        write (unit_msg,170) 'NORMALE         ',type_norm
        write (unit_msg,170) 'TYPE_APPA       ',type_appa_search
!
        if (type_appa_search .eq. 1) then
            write (unit_msg,172) 'DIRE_APPA       ',&
                v_sdcont_dirapp(3*(i_zone-1)+1),&
                v_sdcont_dirapp(3*(i_zone-1)+2),&
                v_sdcont_dirapp(3*(i_zone-1)+3)
        endif
!
! ----- Normals options
!
        write (unit_msg,170) 'VECT_MAIT       ',type_norm_mast
        if (type_norm_mast .eq. 1) then
            write (unit_msg,172) 'MAIT_FIXE       ',&
                v_sdcont_dirnor(zdirn*(i_zone-1)+1),&
                v_sdcont_dirnor(zdirn*(i_zone-1)+2),&
                v_sdcont_dirnor(zdirn*(i_zone-1)+3)
        endif
        if (type_norm_mast .eq. 2) then
            write (unit_msg,172) 'MAIT_VECT_Y     ',&
                v_sdcont_dirnor(zdirn*(i_zone-1)+1),&
                v_sdcont_dirnor(zdirn*(i_zone-1)+2),&
                v_sdcont_dirnor(zdirn*(i_zone-1)+3)
        endif
        write (unit_msg,170) 'VECT_ESCL       ',type_norm_slav
        if (type_norm_slav .eq. 1) then
            write (unit_msg,172) 'ESCL_FIXE       ',&
                v_sdcont_dirnor(zdirn*(i_zone-1)+4),&
                v_sdcont_dirnor(zdirn*(i_zone-1)+5),&
                v_sdcont_dirnor(zdirn*(i_zone-1)+6)
        endif
        if (type_norm_slav .eq. 2) then
            write (unit_msg,172) 'ESCL_VECT_Y     ',&
                v_sdcont_dirnor(zdirn*(i_zone-1)+4),&
                v_sdcont_dirnor(zdirn*(i_zone-1)+5),&
                v_sdcont_dirnor(zdirn*(i_zone-1)+6)
        endif
!
! ----- Tolerances
!
        write (unit_msg,171) 'TOLE_PROJ_EXT   ',tole_proj_ext
        write (unit_msg,171) 'TOLE_APPA       ',tole_appa
!
! ----- SANS_GROUP_NO
!
        write (unit_msg,*) '<CONTACT> ...... NOEUDS EXCLUS '
        nb_node_excl = v_sdcont_pssnoco(i_zone+1) - v_sdcont_pssnoco(i_zone)
        if (nb_node_excl .eq. 0) then
            write (unit_msg,*) '<CONTACT> ...... PAS DE NOEUDS DE TYPE SANS_GROUP_NO A EXCLURE'
        else
            write (unit_msg,*) '<CONTACT> ...... NOMBRE DE NOEUDS DE TYPE '//&
                                'SANS_GROUP_NO A EXCLURE: ',nb_node_excl
            do i_node = 1, nb_node_excl
                node_nume = v_sdcont_ssnoco(v_sdcont_pssnoco(i_zone)+i_node)
                call jenuno(jexnum(mesh(1:8)//'.NOMNOE', node_nume), v_work_node(i_node))
            end do
            write (unit_msg,104) '     LISTE DES NOEUDS  : '
            write (unit_msg,105) (v_work_node(i_node), i_node = 1,nb_node_excl)
        endif
    end do
!
104 format (' <CONTACT> ...... ',a25)
105 format ((' <CONTACT> ...... ',17x,4(a8,1x)))
170 format (' <CONTACT> ...... PARAM. : ',a16,' - VAL. : ',i5)
171 format (' <CONTACT> ...... PARAM. : ',a16,' - VAL. : ',e12.5)
172 format (' <CONTACT> ...... PARMA. : ',a16,' - VAL. : ',e12.5,e12.5,e12.5)
!
! - Supplementary gaps
!
    write (unit_msg,*) '<CONTACT> ... JEUX SUPPLEMENTAIRES'
    do i_zone = 1, nb_cont_zone
        l_dist_shell = mminfl(sdcont_defi, 'DIST_COQUE' , i_zone)
        l_dist_beam  = mminfl(sdcont_defi, 'DIST_POUTRE', i_zone)
        jeuf1        = v_sdcont_jeufo1(i_zone)
        jeuf2        = v_sdcont_jeufo2(i_zone)
!
        if ((jeuf1.ne.' ') .or. (jeuf2.ne.' ')) then
            write (unit_msg,*) '<CONTACT> ... JEU SUPP. SUR ZONE ',i_zone
        endif
        if (jeuf1 .ne. ' ') then
            write (unit_msg,131) jeuf1
        endif
        if (jeuf2 .ne. ' ') then
            write (unit_msg,132) jeuf2
        endif
        if (l_dist_beam) then
            write (unit_msg,*) '<CONTACT> ...... JEU SUPP. PAR DIST_POUT'
        endif
        if (l_dist_shell) then
            write (unit_msg,*) '<CONTACT> ...... JEU SUPP. PAR DIST_COQUE'
        endif
    end do
!
131 format (' <CONTACT> ...... JEU SUPP. TYPE FONC. SUR ESCLAVE : ',a8)
132 format (' <CONTACT> ...... JEU SUPP. TYPE FONC. SUR MAITRE  : ',a8)
!
! - Contact computation
!
    l_exis_verif = cfdisl(sdcont_defi,'EXIS_VERIF')
    if (l_exis_verif) then
        lstop = cfdisl(sdcont_defi,'STOP_INTERP')
        write (unit_msg,*) '<CONTACT> ... ZONES SANS RESOLUTION DU CONTACT'
        if (lstop) then
            write (unit_msg,*) '<CONTACT> ...... STOP SI INTERPENETRATION'
        else
            write (unit_msg,*) '<CONTACT> ...... ALARME SI INTERPENETRATION'
        endif
        do i_zone = 1, nb_cont_zone
            l_veri      = mminfl(sdcont_defi,'VERIF'      , i_zone)
            tole_interp = mminfr(sdcont_defi,'TOLE_INTERP', i_zone)
            write (unit_msg,*) '<CONTACT> ...... OPTIONS SUR LA ZONE : ',&
            i_zone
            if (l_veri) then
                write (unit_msg,*) '<CONTACT> ...... RESOLUTION DU CONTACT'
            else
                write (unit_msg,*) '<CONTACT> ...... PAS DE RESOLUTION DU CONTACT'
                write (unit_msg,171) 'TOLE_INTERP   ', tole_interp
            endif
        end do
    endif
!
! - Global parameters (user)
!
    write (unit_msg,*)
    write (unit_msg,*) '<CONTACT> INFOS SPECIFIQUES SUR LES FORMULATIONS MAILLEES'
    write (unit_msg,*)
    write (unit_msg,*) '<CONTACT> DIMENSION DE L''ESPACE   : ',model_ndim
    write (unit_msg,*) '<CONTACT> NBRE ZONES DE CONTACT   : ',nb_cont_zone
    write (unit_msg,*) '<CONTACT> NBRE SURF. DE CONTACT   : ',nb_cont_surf
    write (unit_msg,*) '<CONTACT> NBRE MAIL. DE CONTACT   : ',nb_cont_elem
    write (unit_msg,*) '<CONTACT> NBRE NOEU. DE CONTACT   : ',nb_cont_node
    write (unit_msg,*) '<CONTACT> NBRE NOEU. ESCL. TOTAL  : ',nt_node_slav,&
                        ' DONT CALCUL ',nt_node_slavc
    write (unit_msg,*) '<CONTACT> NBRE MAIL. ESCL. TOTAL  : ',nt_elem_slav,&
                        ' DONT CALCUL ',nt_elem_slavc
    write (unit_msg,*) '<CONTACT> NBRE NOEU. MAIT. TOTAL  : ',nt_node_mast,&
                        ' DONT CALCUL ',nt_node_mastc
    write (unit_msg,*) '<CONTACT> NBRE MAIL. MAIT. TOTAL  : ',nt_elem_mast,&
                        ' DONT CALCUL ',nt_elem_mastc
    write (unit_msg,*) '<CONTACT> NBRE PT. CONTACT TOTAL  : ',nb_cont_poin,&
                        ' DONT CALCUL ',nb_cont_poinc
!
! - Nodes and elements
!
    call surfll(sdcont_defi , mesh, unit_msg, nb_cont_zone, nb_cont_elem,&
                nb_cont_node)
!
! - Global parameters (development)
!
    write (unit_msg,*)
    write (unit_msg,*) '<CONTACT> INFOS SPECIFIQUES SUR LES FORMULATIONS MAILLEES ',&
                        '- NIVEAU DEVELOPPEUR'
    write (unit_msg,*)
!
    write (unit_msg,108) 'PZONE  : '
    write (unit_msg,106) (v_sdcont_pzoneco(i_zone+1), i_zone = 0,nb_cont_zone)
    write (unit_msg,108) 'PSURMA : '
    write (unit_msg,106) (v_sdcont_psumaco(i_surf+1), i_surf = 0,nb_cont_surf)
    write (unit_msg,108) 'PSURNO : '
    write (unit_msg,106) (v_sdcont_psunoco(i_surf+1), i_surf = 0,nb_cont_surf)
    write (unit_msg,108) 'CONTMA : '
    write (unit_msg,106) (v_sdcont_mailco(i_elem), i_elem = 1,nb_cont_elem)
    write (unit_msg,108) 'CONTNO : '
    write (unit_msg,106) (v_sdcont_noeuco(i_node), i_node = 1,nb_cont_node)
!
106 format ((' <CONTACT> ',9x,6(i7,1x)))
108 format (' <CONTACT> ',a9)
!
    write (unit_msg,*)
    write (unit_msg,*) '<CONTACT> INFOS SPECIFIQUES SUR LES FORMULATIONS MAILLEES ',&
                        '- CONNECTIVITES INVERSES'
    write (unit_msg,*)
    call jelira(sdcont_manoco, 'LONUTI', nt_elem_node)
    call jelira(sdcont_nomaco, 'LONUTI', nt_node_elem)
!
    write (unit_msg,*) '<CONTACT> NBRE CONNEC. INV        : ',nt_elem_node
    write (unit_msg,*) '<CONTACT> NNOMA                   : ',nt_node_elem
    write (unit_msg,208) 'MANOCO : '
    write (unit_msg,206) (v_sdcont_manoco(i_elem), i_elem = 1,nt_elem_node)
    write (unit_msg,208) 'PMANO  : '
    write (unit_msg,206) (v_sdcont_pmanoco(i_node+1), i_node = 0,nb_cont_node)
    write (unit_msg,208) 'NOMACO : '
    write (unit_msg,206) (v_sdcont_nomaco(i_node), i_node = 1,nt_node_elem)
    write (unit_msg,208) 'PNOMA  : '
    write (unit_msg,206) (v_sdcont_pnomaco(i_elem+1), i_elem = 0,nb_cont_elem)
    write (unit_msg,*)
!
206 format ((' <CONTACT> ',9x,6(i7,1x)))
208 format (' <CONTACT> ',a9)
!
! - Clean
!
    AS_DEALLOCATE(vk8=v_work_node)
!
end subroutine
