subroutine irvari(ifi        , field_med    , vari_elga, field_loca, model    ,&
                  nb_cmp_sele, cmp_name_sele, partie   , numpt     , instan   ,&
                  nume_store , nbmaec       , limaec   , result    , cara_elem,&
                  codret)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/comp_meca_pvar.h"
#include "asterfort/comp_meca_uvar.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/cescrm.h"
#include "asterfort/cescel.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
#include "asterfort/detrsd.h"
#include "asterfort/codent.h"
#include "asterfort/jedema.h"
#include "asterfort/irceme.h"
#include "asterfort/rsexch.h"
#include "asterfort/jexnum.h"
#include "asterfort/jexatr.h"
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
!
    integer, intent(in) :: ifi
    character(len=64), intent(in) :: field_med
    character(len=19), intent(in) :: vari_elga
    character(len=8), intent(in) :: field_loca
    character(len=8), intent(in) :: model
    integer, intent(in) :: nb_cmp_sele
    character(len=*), intent(in) :: cmp_name_sele(*)
    character(len=*), intent(in) :: partie
    integer, intent(in) :: numpt
    real(kind=8), intent(in) :: instan
    integer, intent(in) :: nume_store
    integer, intent(in) :: nbmaec
    integer, intent(in) :: limaec(*)
    character(len=8), intent(in) :: result
    character(len=8), intent(in) :: cara_elem
    integer, intent(out) :: codret
!
! --------------------------------------------------------------------------------------------------
!
! Post-treatment (IMPR_RESU)
!
! Create VARI_ELGA_NOMME for name of internal variables
!
! --------------------------------------------------------------------------------------------------
!
! In  nume_store       : index to store in results
! In  field_med        : name of MED field
! In  field_loca       : localization of field
!                        /'ELNO'/'ELGA'/'ELEM'
! In  result           : name of results datastructure
! In  model            : name of model                    
!       IFI    : UNITE LOGIQUE D'IMPRESSION DU CHAMP
!       PARTIE : IMPRESSION DE LA PARTIE IMAGINAIRE OU REELLE POUR
!                UN CHAMP COMPLEXE
!       nb_cmp_sele  : NOMBRE DE COMPOSANTES A ECRIRE
!       cmp_name_sele : NOMS DES COMPOSANTES A ECRIRE
!       NUMPT  : NUMERO DE PAS DE TEMPS
!       INSTAN : VALEUR DE L'INSTANT A ARCHIVER
!       NUMORD : NUMERO D'ORDRE DU CHAMP
!       NBMAEC : NOMBRE DE MAILLES A ECRIRE (0, SI TOUTES LES MAILLES)
!       LIMAEC : LISTE DES MAILLES A ECRIRE SI EXTRAIT
!       CODRET : CODE DE RETOUR (0 : PAS DE PB, NON NUL SI PB)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_zone, i_elem, i_pt, i_vari, i_vari_redu, i_spt
    integer :: nb_vari, nb_pt, nb_spt, nb_vari_zone
    integer :: nb_vari_redu, nb_zone, nb_elem, nb_vari_maxi, nb_elem_mesh, nb_elem_zone
    integer :: nt_vari
    integer :: posit, iret, affe_type, affe_indx, nume_elem
    integer :: jv_elga_cesd, jv_elga_cesl, jv_elgr_cesd, jv_elgr_cesl, jv_elga, jv_elgr
    character(len=7) :: saux07
    character(len=8) :: saux08
    character(len=8), parameter :: base_name = '&&IRVARI'
    character(len=19) :: compor, ligrel
    character(len=19), parameter :: vari_elga_s = '&&IRVARI.VARIELGA_S'
    character(len=19), parameter :: vari_elgr   = '&&IRVARI.VARIELGR'
    character(len=19), parameter :: vari_elgr_s = '&&IRVARI.VARIELGR_S'
    character(len=19) :: vari_link
    character(len=19), parameter :: vari_redu = '&&IRVARI.VARIREDU'
    integer, pointer :: v_vari_link(:) => null()
    character(len=16), pointer :: v_vari_redu(:) => null() 
    character(len=19), parameter :: label_med = '&&IRVARI.LABELMED'
    character(len=19), parameter :: label_vxx = '&&IRVARI.LABELVXX'
    character(len=8), pointer :: v_label_vxx(:) => null()
    character(len=16), pointer :: v_label_med(:) => null() 
    character(len=64) :: nomres
    real(kind=8), pointer :: v_elgr_cesv(:) => null()
    real(kind=8), pointer :: v_elga_cesv(:) => null()
    character(len=16), pointer :: v_compor_vale(:) => null()
    integer, pointer :: v_compor_desc(:) => null()
    integer, pointer :: v_compor_lima(:) => null()
    integer, pointer :: v_compor_lima_lc(:) => null()
    character(len=19), parameter :: compor_info = '&&IRVARI.INFO'
    integer, pointer :: v_info(:) => null()
    integer, pointer :: v_zone(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    ASSERT(field_loca .eq. 'ELGA')
    codret = 0
    ligrel = model//'.MODELE'
!
! - Get name of <CARTE> COMPOR
!
    call rsexch('F', result, 'COMPORTEMENT', nume_store, compor,&
                iret)
!
! - Prepare informations about internal variables
!
    call comp_meca_pvar(model_ = model, compor_cart_ = compor, compor_info = compor_info,&
                        l_list_elem_ = .true._1, l_info_full_ = .false._1)
!
! - Access to informations
!
    call jeveuo(compor_info(1:19)//'.INFO', 'L', vi = v_info)
    nb_elem_mesh = v_info(1)
    nb_zone      = v_info(2)
    nb_vari_maxi = v_info(3)
    nt_vari      = v_info(4)
!
    if ( nt_vari .eq. 0 ) then
        codret = 200
        goto 999
    endif
    call jeveuo(compor_info(1:19)//'.ZONE', 'L', vi = v_zone)
! 
! - Create list of internal variables and link to zone in <CARTE> COMPOR
!
    call comp_meca_uvar(compor_info, base_name, vari_redu, nb_vari_redu, codret)
    call jeveuo(vari_redu, 'L', vk16 = v_vari_redu)
    if ( nb_vari_redu .eq. 0 .or. codret .eq. 200) then
        codret = 200
        goto 999
    endif
!
! - Access to <CARTE> COMPOR
!
    call jeveuo(compor//'.DESC', 'L', vi   = v_compor_desc)
    call jeveuo(compor//'.VALE', 'L', vk16 = v_compor_vale)
    call jeveuo(jexnum(compor//'.LIMA', 1), 'L', vi = v_compor_lima)
    call jeveuo(jexatr(compor//'.LIMA', 'LONCUM'), 'L', vi = v_compor_lima_lc)
!
! - Transform VARI_ELGA in VARI_ELGA_S
!
    call celces(vari_elga, 'V', vari_elga_s)
    call jeveuo(vari_elga_s//'.CESD', 'L', jv_elga_cesd)
    call jeveuo(vari_elga_s//'.CESL', 'L', jv_elga_cesl)
    call jeveuo(vari_elga_s//'.CESV', 'L', vr=v_elga_cesv)
!
! - Prepare objects to reduced list of internal variables
!
    call wkvect(label_vxx, 'V V K8', nb_vari_redu, vk8 = v_label_vxx)
    call wkvect(label_med, 'V V K16', 2*nb_vari_redu, vk16 = v_label_med)
    do i_vari_redu = 1, nb_vari_redu
        call codent(i_vari_redu, 'G', saux07)
        v_label_vxx(i_vari_redu)         = 'V'//saux07
        v_label_med(2*(i_vari_redu-1)+1) = 'V'//saux07
        v_label_med(2*(i_vari_redu-1)+2) = v_vari_redu(i_vari_redu)
    end do
!
! - Create VARI_ELGR_S on reduced list of internal variables
!
    call cescrm('V', vari_elgr_s, field_loca, 'VARI_R', nb_vari_redu,&
                v_label_vxx, vari_elga_s)
    call jeveuo(vari_elgr_s//'.CESD', 'L', jv_elgr_cesd)
    call jeveuo(vari_elgr_s//'.CESL', 'L', jv_elgr_cesl)
    call jeveuo(vari_elgr_s//'.CESV', 'L', vr=v_elgr_cesv)
!
! - Fill VARI_ELGR_S on reduced list of internal variables
!
    do i_zone = 1, nb_zone
        nb_elem_zone = v_zone(i_zone)
        if (nb_elem_zone .ne. 0) then
!
! --------- Get object to link zone to internal variables
!
            call codent(i_zone, 'G', saux08)
            vari_link = base_name//saux08
            call jeveuo(vari_link, 'L', vi = v_vari_link)
!
! --------- Access to current zone in CARTE
!
            affe_type = v_compor_desc(1+3+(i_zone-1)*2)
            affe_indx = v_compor_desc(1+4+(i_zone-1)*2)
            if (affe_type .eq. 3) then
                nb_elem = v_compor_lima_lc(1+affe_indx)-v_compor_lima_lc(affe_indx)
                posit   = v_compor_lima_lc(affe_indx)
            elseif (affe_type .eq. 1) then
                nb_elem = nb_elem_mesh
                posit   = 0
            else
                ASSERT(.false.)
            endif
            call jelira(jexnum(compor_info(1:19)//'.VARI', i_zone), 'LONMAX', nb_vari_zone)
!
! --------- Loop on elements in zone of CARTE
!
            do i_elem = 1, nb_elem
                if (affe_type .eq. 3) then
                    nume_elem = v_compor_lima(posit+i_elem-1)
                elseif (affe_type .eq. 1) then
                    nume_elem = i_elem
                else
                    ASSERT(.false.)
                endif
                nb_pt   = zi(jv_elga_cesd-1+5+4*(nume_elem-1)+1)
                nb_spt  = zi(jv_elga_cesd-1+5+4*(nume_elem-1)+2)
                nb_vari = zi(jv_elga_cesd-1+5+4*(nume_elem-1)+3)
                do i_pt = 1, nb_pt
                    do i_spt = 1, nb_spt
                        do i_vari = 1, nb_vari
                            call cesexi('C'  , jv_elga_cesd, jv_elga_cesl, nume_elem, i_pt,&
                                        i_spt, i_vari      , jv_elga)
                            if (jv_elga .gt. 0 .and. i_vari .le. nb_vari_zone) then
                                i_vari_redu = v_vari_link(i_vari)
                                if (i_vari_redu .ne. 0) then
                                    call cesexi('C'  , jv_elgr_cesd, jv_elgr_cesl, nume_elem, i_pt,&
                                                i_spt, i_vari_redu , jv_elgr)
                                    ASSERT(jv_elgr .ne. 0)
                                    jv_elgr = abs(jv_elgr)
                                    v_elgr_cesv(jv_elgr)      = v_elga_cesv(jv_elga)
                                    zl(jv_elgr_cesl-1+jv_elgr) = .true.
                                endif
                            endif
                        end do
                    end do
                end do
            end do
        endif
    end do
!
! - Transform VARI_ELGR_S in VARI_ELGR
!
    nomres = field_med(1:8)//'VARI_ELGA_NOMME'
    call cescel(vari_elgr_s, ligrel, ' ', ' ', 'OUI',&
                nume_elem, 'V', vari_elgr, 'F', codret)
!
! - Write in MED file
!
    call irceme(ifi, nomres, vari_elgr, field_loca, model,&
                nb_cmp_sele, cmp_name_sele, label_med, partie, numpt,&
                instan, nume_store, nbmaec, limaec, cara_elem,&
                codret)
!
999 continue
!
! - Cleaning
!
    call detrsd('CHAM_ELEM_S', vari_elga_s)
    call detrsd('CHAM_ELEM_S', vari_elgr_s)
    call detrsd('CHAM_ELEM_S', vari_elgr)
    call jedetr(compor_info(1:19)//'.ZONE')
    call jedetr(compor_info(1:19)//'.INFO')
    call jedetr(compor_info(1:19)//'.ELEM')
    call jedetr(compor_info(1:19)//'.RELA')
    call jedetc('V', compor_info(1:19)//'.VARI', 1)
    call jedetr(vari_redu)
    call jedetr(label_vxx)
    call jedetr(label_med)
    do i_zone = 1,nb_zone
        call codent(i_zone, 'G', saux08)
         vari_link = base_name//saux08
        call jedetr(vari_link)
    end do
!
    call jedema()
!
end subroutine
