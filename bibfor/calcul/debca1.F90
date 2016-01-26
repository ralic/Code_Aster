subroutine debca1(nin)

use calcul_module, only : ca_caindz_, ca_calvoi_, ca_iadsgd_,&
     ca_iainel_, ca_ialiel_, ca_iamaco_, ca_iamloc_,&
     ca_iamsco_, ca_ianoop_, ca_ianote_, ca_iaobtr_, ca_iaopds_, ca_iaopmo_,&
     ca_iaopno_, ca_iaoppa_, ca_iaoptt_,&
     ca_icaeli_, ca_icaelk_, ca_illiel_, ca_ilmaco_, ca_ilmloc_, ca_ilmsco_,&
     ca_ilopmo_, ca_ilopno_, ca_ininel_,&
     ca_jelvoi_, ca_jnbelr_, ca_jnoelr_, ca_jnolfp_, ca_jpnlfp_, ca_jptvoi_,&
     ca_jrepe_, ca_lgco_, ca_nblfpg_,&
     ca_nbobj_, ca_nbobmx_, ca_nbobtr_, ca_nbsav_, ca_nparin_, ca_npario_,&
     ca_td1_, ca_tf1_, ca_timed1_, ca_timef1_,&
     ca_ldist_, ca_ldgrel_, ca_rang_, ca_nbproc_, ca_numsd_, ca_nbelmx_,&
     ca_option_, ca_ligrel_, ca_lparal_, ca_paral_, ca_nbelgr_, ca_nbgr_


implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr

#include "jeveux.h"
#include "asterc/r8nnem.h"
#include "asterfort/cormgi.h"
#include "asterfort/dismoi.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbgrel.h"
#include "asterfort/nbelem.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/asmpi_info.h"

    integer :: nin
!----------------------------------------------------------------------
!     but : initialiser certaines variables de calcul_module
!
!     entrees:
!        nin    :  majorant du nombre de champs "in"
!     sorties:
!        allocation de certains objets de travail
!
!----------------------------------------------------------------------
    mpi_int :: mrank, msize
    character(len=8) :: ma
    integer :: iret, ier, opt,  nbscmx, nbpara
    integer :: nnomx, nbopt, nbte, i,j, vali(2)
    character(len=16) :: nomop2, nomte
    character(len=3) :: bevois, exiele
    character(len=12) :: vge
    character(len=19) :: partit
    real(kind=8) :: rundef
    integer, pointer :: nbligcol(:) => null()
    character(len=16), pointer :: nvge(:) => null()
    integer, pointer :: prti(:) => null()
    character(len=24), pointer :: prtk(:) => null()

! -------------------------------------------------------------------

!   -- Remarque : cette routine ne fait pas appel a jemarq / jedema
!      car elle stocke des adresses jeveux.

    call dismoi('EXI_ELEM', ca_ligrel_, 'LIGREL', repk=exiele)
    if (exiele .ne. 'OUI') then
        call utmess('F', 'CALCUL_1', sk=ca_ligrel_)
    endif
    ca_nbgr_=nbgrel(ca_ligrel_)
    ca_nbelmx_=0
    do j = 1, ca_nbgr_
        ca_nbelmx_=max(nbelem(ca_ligrel_,j,1),ca_nbelmx_)
    enddo

    call jenonu(jexnom('&CATA.OP.NOMOPT', ca_option_), opt)
    if (opt .eq. 0) then
        call utmess('F', 'CALCUL_2', sk=ca_option_)
    endif


    call jeveuo('&CATA.TE.OPTTE', 'L', ca_iaoptt_)
    call jeveuo('&CATA.TE.NBLIGCOL', 'L', vi=nbligcol)
    ca_lgco_=nbligcol(1)
    call jeveuo('&CATA.TE.OPTMOD', 'L', ca_iaopmo_)
    call jeveuo(jexatr('&CATA.TE.OPTMOD', 'LONCUM'), 'L', ca_ilopmo_)
    call jeveuo('&CATA.TE.OPTNOM', 'L', ca_iaopno_)
    call jeveuo(jexatr('&CATA.TE.OPTNOM', 'LONCUM'), 'L', ca_ilopno_)
    call jeveuo(jexnum('&CATA.OP.DESCOPT', opt), 'L', ca_iaopds_)
    call jeveuo(jexnum('&CATA.OP.OPTPARA', opt), 'L', ca_iaoppa_)

    ca_nparin_=zi(ca_iaopds_-1+2)
    ca_npario_=zi(ca_iaopds_-1+2)+zi(ca_iaopds_-1+3)
    call jeveuo('&CATA.TE.MODELOC', 'L', ca_iamloc_)
    call jeveuo(jexatr('&CATA.TE.MODELOC', 'LONCUM'), 'L', ca_ilmloc_)
    call jeveuo('&CATA.GD.DESCRIGD', 'L', ca_iadsgd_)


!   Creation de l'objet '&&CALCUL.OBJETS_TRAV' qui contiendra le nom de
!   tous les objets de travail necessaires a calcul :
!   ------------------------------------------------------------------
    nbscmx=9
!     nbscmx = nb de types scalaires max : I,R,C,L,K8,K16,K24,K32,K80
    ca_nbobtr_=0
    nbpara=zi(ca_iaopds_-1+2)+zi(ca_iaopds_-1+3)
    ca_nbobmx_=2*nbscmx+nin*3+nbpara+36
    call wkvect('&&CALCUL.OBJETS_TRAV', 'V V K24', ca_nbobmx_, ca_iaobtr_)


    call dismoi('NOM_MAILLA', ca_ligrel_, 'LIGREL', repk=ma)
    call jeexin(ma//'.CONNEX', iret)
    if (iret .gt. 0) then
        call jeveuo(ma//'.CONNEX', 'L', ca_iamaco_)
        call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', ca_ilmaco_)
    endif
    call jeexin(ca_ligrel_//'.NEMA', iret)
    if (iret .gt. 0) then
        call jeveuo(ca_ligrel_//'.NEMA', 'L', ca_iamsco_)
        call jeveuo(jexatr(ca_ligrel_//'.NEMA', 'LONCUM'), 'L', ca_ilmsco_)
    endif
    call jeveuo(ca_ligrel_//'.LIEL', 'L', ca_ialiel_)
    call jeveuo(jexatr(ca_ligrel_//'.LIEL', 'LONCUM'), 'L', ca_illiel_)


    call jeveuo('&CATA.TE.NBELREFE', 'L', ca_jnbelr_)
    call jeveuo('&CATA.TE.NOELREFE', 'L', ca_jnoelr_)
    call jeveuo('&CATA.TE.PNLOCFPG', 'L', ca_jpnlfp_)
    call jeveuo('&CATA.TE.NOLOCFPG', 'L', ca_jnolfp_)
    call jelira('&CATA.TE.NOLOCFPG', 'LONMAX', ca_nblfpg_)


    call dismoi('BESOIN_VOISIN', ca_ligrel_, 'LIGREL', repk=bevois)
    if (bevois .eq. 'OUI') then
        ca_calvoi_=1
    else
        ca_calvoi_=0
    endif
    if (ca_calvoi_ .eq. 1) then
        call jeexin(ca_ligrel_//'.REPE', ier)
        if (ier .eq. 0) call cormgi('V', ca_ligrel_)
        call jeveuo(ca_ligrel_//'.REPE', 'L', ca_jrepe_)
        call jeexin(ca_ligrel_//'.NVGE', ier)
        if (ier .ne. 0) then
            call jeveuo(ca_ligrel_//'.NVGE', 'L', vk16=nvge)
            vge=nvge(1)(1:12)
            call jeveuo(vge//'.PTVOIS', 'L', ca_jptvoi_)
            call jeveuo(vge//'.ELVOIS', 'L', ca_jelvoi_)
        else
            ca_jrepe_=0
        endif
    else
        ca_jrepe_=0
    endif


    call dismoi('NB_NO_MAX', '&', 'CATALOGUE', repi=nnomx)
    call wkvect('&&CALCUL.TECAEL_K24', 'V V K24', 6+nnomx, ca_icaelk_)
    ca_nbobtr_=ca_nbobtr_+1
    zk24(ca_iaobtr_-1+ca_nbobtr_)='&&CALCUL.TECAEL_K24'
    zk24(ca_icaelk_-1+1)=ma
    zk24(ca_icaelk_-1+2)=ca_ligrel_
    call wkvect('&&CALCUL.TECAEL_I', 'V V I', 4+nnomx, ca_icaeli_)
    ca_nbobtr_=ca_nbobtr_+1
    zk24(ca_iaobtr_-1+ca_nbobtr_)='&&CALCUL.TECAEL_I'


!   ca_nbobj_ est le nombre maxi d'objets '&INEL.XXX' crees par un ini00k
!   ------------------------------------------------------------------
    ca_nbobj_=30
    call wkvect('&&CALCUL.NOM_&INEL', 'V V K24', ca_nbobj_, ca_ininel_)
    ca_nbobtr_=ca_nbobtr_+1
    zk24(ca_iaobtr_-1+ca_nbobtr_)='&&CALCUL.NOM_&INEL'
    call wkvect('&&CALCUL.IAD_&INEL', 'V V I', ca_nbobj_, ca_iainel_)
    ca_nbobtr_=ca_nbobtr_+1
    zk24(ca_iaobtr_-1+ca_nbobtr_)='&&CALCUL.IAD_&INEL'


    ca_caindz_(1:512)=1
    ca_nbsav_=0
    rundef=r8nnem()
    ca_timed1_=rundef
    ca_timef1_=rundef
    ca_td1_=rundef
    ca_tf1_=rundef


!   Les 2 tableaux suivants peuvent n'etre construits qu'une
!   fois pour toutes lors du premier appel a debcal.
!   '&&CALCUL.NOMOP' et '&&CALCUL.NOMTE' ne seront supprimes
!   qu'en sortie de commande.
!   --------------------------------------------------------
    call jeexin('&&CALCUL.NOMOP', iret)
    if (iret .eq. 0) then
        call jelira('&CATA.OP.NOMOPT', 'NOMMAX', nbopt)
        call wkvect('&&CALCUL.NOMOP', 'V V K16', nbopt, ca_ianoop_)
        do i = 1, nbopt
            call jenuno(jexnum('&CATA.OP.NOMOPT', i), nomop2)
            zk16(ca_ianoop_-1+i)=nomop2
        end do
        call jelira('&CATA.TE.NOMTE', 'NOMMAX', nbte)
        call wkvect('&&CALCUL.NOMTE', 'V V K16', nbte, ca_ianote_)
        do i = 1, nbte
            call jenuno(jexnum('&CATA.TE.NOMTE', i), nomte)
            zk16(ca_ianote_-1+i)=nomte
        end do
    else
        call jeveuo('&&CALCUL.NOMOP', 'L', ca_ianoop_)
        call jeveuo('&&CALCUL.NOMTE', 'L', ca_ianote_)
    endif


!   -- Paralelisme :
!   =================
    ca_ldist_=.false.
    ca_ldgrel_=.false.
    ca_lparal_=.false.
    call dismoi('PARTITION', ca_ligrel_, 'LIGREL', repk=partit)
    call jeexin(partit//'.PRTK', iret)
    if (iret .ne. 0) then
        ca_ldist_=.true.
        call asmpi_info(rank=mrank, size=msize)
        ca_rang_ = to_aster_int(mrank)
        ca_nbproc_ = to_aster_int(msize)

        call jeveuo(partit//'.PRTK', 'L', vk24=prtk)
        ca_ldgrel_=prtk(1).eq.'GROUP_ELEM'
        if (.not.ca_ldgrel_) then
        call jeveuo(partit//'.PRTI', 'L', vi=prti)
            if (prti(1) .ne. ca_nbproc_) then
                vali(1)=prti(1)
                vali(2)=ca_nbproc_
                call utmess('F', 'CALCUL_35', ni=2, vali=vali)
            endif
            ca_lparal_=.true.
            call jeveuo(partit//'.NUPROC.MAILLE', 'L', vi=ca_numsd_)
            call wkvect('&&CALCUL.PARALLELE', 'V V L', ca_nbelmx_, vl=ca_paral_)
            ca_nbobtr_=ca_nbobtr_+1
            zk24(ca_iaobtr_-1+ca_nbobtr_)='&&CALCUL.PARALLELE'
        endif
    endif



end subroutine
