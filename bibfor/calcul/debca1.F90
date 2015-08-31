subroutine debca1(nomop, ligrel, nin)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
use module_calcul, only : ca_caindz_, ca_calvoi_, ca_evfini_, ca_iadsgd_,&
     ca_iainel_, ca_ialiel_, ca_iamaco_, ca_iamloc_,&
     ca_iamsco_, ca_ianoop_, ca_ianote_, ca_iaobtr_, ca_iaopds_, ca_iaopmo_,&
     ca_iaopno_, ca_iaoppa_, ca_iaoptt_,&
     ca_icaeli_, ca_icaelk_, ca_illiel_, ca_ilmaco_, ca_ilmloc_, ca_ilmsco_,&
     ca_ilopmo_, ca_ilopno_, ca_ininel_,&
     ca_jelvoi_, ca_jnbelr_, ca_jnoelr_, ca_jnolfp_, ca_jpnlfp_, ca_jptvoi_,&
     ca_jrepe_, ca_lgco_, ca_nblfpg_,&
     ca_nbobj_, ca_nbobmx_, ca_nbobtr_, ca_nbsav_, ca_nparin_, ca_npario_,&
     ca_td1_, ca_tf1_, ca_timed1_, ca_timef1_
implicit none
!     ARGUMENTS:
!     ----------
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
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=16) :: nomop
    character(len=19) :: ligrel
    integer :: nin
! ----------------------------------------------------------------------
!     but : initialiser certaines variables de module_calcul
!
!     ENTREES:
!        NOMOP  :  NOM D'1 OPTION
!        LIGREL :  NOM DU LIGREL SUR LEQUEL ON DOIT FAIRE LE CALCUL
!        NIN    :  MAJORANT DU NOMBRE DE CHAMPS "IN"
!     SORTIES:
!       ALLOCATION D'OBJETS DE TRAVAIL
!
! ----------------------------------------------------------------------
    character(len=8) :: ma
! -------------------------------------------------------------------
! VARIABLES LOCALES :
    integer :: iret, ier, opt,  nbscmx, nbpara
    integer :: nnomx, nbopt, nbte, i
    character(len=16) :: nomop2, nomte
    character(len=3) :: bevois, exivf, exiele
    character(len=12) :: vge
    real(kind=8) :: rundef
    integer, pointer :: nbligcol(:) => null()
    character(len=16), pointer :: nvge(:) => null()
! -------------------------------------------------------------------
!
    call dismoi('EXI_ELEM', ligrel, 'LIGREL', repk=exiele)
    if (exiele .ne. 'OUI') then
        call utmess('F', 'CALCUL_1', sk=ligrel)
    endif
!
    call jenonu(jexnom('&CATA.OP.NOMOPT', nomop), opt)
    if (opt .eq. 0) then
        call utmess('F', 'CALCUL_2', sk=nomop)
    endif
!
!
!     ---------------------------------
    call jeveuo('&CATA.TE.OPTTE', 'L', ca_iaoptt_)
    call jeveuo('&CATA.TE.NBLIGCOL', 'L', vi=nbligcol)
    ca_lgco_=nbligcol(1)
    call jeveuo('&CATA.TE.OPTMOD', 'L', ca_iaopmo_)
    call jeveuo(jexatr('&CATA.TE.OPTMOD', 'LONCUM'), 'L', ca_ilopmo_)
    call jeveuo('&CATA.TE.OPTNOM', 'L', ca_iaopno_)
    call jeveuo(jexatr('&CATA.TE.OPTNOM', 'LONCUM'), 'L', ca_ilopno_)
    call jeveuo(jexnum('&CATA.OP.DESCOPT', opt), 'L', ca_iaopds_)
    call jeveuo(jexnum('&CATA.OP.OPTPARA', opt), 'L', ca_iaoppa_)
!
    ca_nparin_=zi(ca_iaopds_-1+2)
    ca_npario_=zi(ca_iaopds_-1+2)+zi(ca_iaopds_-1+3)
    call jeveuo('&CATA.TE.MODELOC', 'L', ca_iamloc_)
    call jeveuo(jexatr('&CATA.TE.MODELOC', 'LONCUM'), 'L', ca_ilmloc_)
    call jeveuo('&CATA.GD.DESCRIGD', 'L', ca_iadsgd_)
!
!
!     ET CREATION DE L'OBJET '&&CALCUL.OBJETS_TRAV' QUI CONTIENDRA TOUS
!     LES NOMS DES OBJETS DE TRAVAIL NECESSAIRES A CALCUL :
!     ------------------------------------------------------------------
    nbscmx=9
!     NBSCMX = NB DE TYPES SCALAIRES MAX : I,R,C,L,K8,K16,K24,K32,K80
    ca_nbobtr_=0
    nbpara=zi(ca_iaopds_-1+2)+zi(ca_iaopds_-1+3)
    ca_nbobmx_=2*nbscmx+nin*3+nbpara+35
    call wkvect('&&CALCUL.OBJETS_TRAV', 'V V K24', ca_nbobmx_, ca_iaobtr_)
!
!
!     ---------------------------------
    call dismoi('NOM_MAILLA', ligrel, 'LIGREL', repk=ma)
    call jeexin(ma//'.CONNEX', iret)
    if (iret .gt. 0) then
        call jeveuo(ma//'.CONNEX', 'L', ca_iamaco_)
        call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', ca_ilmaco_)
    endif
    call jeexin(ligrel//'.NEMA', iret)
    if (iret .gt. 0) then
        call jeveuo(ligrel//'.NEMA', 'L', ca_iamsco_)
        call jeveuo(jexatr(ligrel//'.NEMA', 'LONCUM'), 'L', ca_ilmsco_)
    endif
    call jeveuo(ligrel//'.LIEL', 'L', ca_ialiel_)
    call jeveuo(jexatr(ligrel//'.LIEL', 'LONCUM'), 'L', ca_illiel_)
!
!
!     ---------------------------------
    call jeveuo('&CATA.TE.NBELREFE', 'L', ca_jnbelr_)
    call jeveuo('&CATA.TE.NOELREFE', 'L', ca_jnoelr_)
    call jeveuo('&CATA.TE.PNLOCFPG', 'L', ca_jpnlfp_)
    call jeveuo('&CATA.TE.NOLOCFPG', 'L', ca_jnolfp_)
    call jelira('&CATA.TE.NOLOCFPG', 'LONMAX', ca_nblfpg_)
!
!
!     ---------------------------------
    call dismoi('EXI_VF', ligrel, 'LIGREL', repk=exivf)
    call dismoi('BESOIN_VOISIN', ligrel, 'LIGREL', repk=bevois)
    if (exivf .eq. 'OUI') then
        ca_evfini_=1
        ca_calvoi_=1
    else
        ca_evfini_=0
        if (bevois .eq. 'OUI') then
            ca_calvoi_=1
        else
            ca_calvoi_=0
        endif
    endif
    if (ca_calvoi_ .eq. 1) then
        call jeexin(ligrel//'.REPE', ier)
        if (ier .eq. 0) call cormgi('V', ligrel)
        call jeveuo(ligrel//'.REPE', 'L', ca_jrepe_)
        call jeexin(ligrel//'.NVGE', ier)
        if (ier .ne. 0) then
            call jeveuo(ligrel//'.NVGE', 'L', vk16=nvge)
            vge=nvge(1)(1:12)
            call jeveuo(vge//'.PTVOIS', 'L', ca_jptvoi_)
            call jeveuo(vge//'.ELVOIS', 'L', ca_jelvoi_)
        else
            ca_jrepe_=0
        endif
    else
        ca_jrepe_=0
    endif
!
!
!     ---------------------------------
    call dismoi('NB_NO_MAX', '&', 'CATALOGUE', repi=nnomx)
    call wkvect('&&CALCUL.TECAEL_K24', 'V V K24', 6+nnomx, ca_icaelk_)
    ca_nbobtr_=ca_nbobtr_+1
    zk24(ca_iaobtr_-1+ca_nbobtr_)='&&CALCUL.TECAEL_K24'
    zk24(ca_icaelk_-1+1)=ma
    zk24(ca_icaelk_-1+2)=ligrel
    call wkvect('&&CALCUL.TECAEL_I', 'V V I', 4+nnomx, ca_icaeli_)
    ca_nbobtr_=ca_nbobtr_+1
    zk24(ca_iaobtr_-1+ca_nbobtr_)='&&CALCUL.TECAEL_I'
!
!
!     NBOBJ EST LE NOMBRE MAXI D'OBJETS '&INEL.XXX' CREES PAR UN INI00K
!     ------------------------------------------------------------------
    ca_nbobj_=30
    call wkvect('&&CALCUL.NOM_&INEL', 'V V K24', ca_nbobj_, ca_ininel_)
    ca_nbobtr_=ca_nbobtr_+1
    zk24(ca_iaobtr_-1+ca_nbobtr_)='&&CALCUL.NOM_&INEL'
    call wkvect('&&CALCUL.IAD_&INEL', 'V V I', ca_nbobj_, ca_iainel_)
    ca_nbobtr_=ca_nbobtr_+1
    zk24(ca_iaobtr_-1+ca_nbobtr_)='&&CALCUL.IAD_&INEL'
!
!
!     -------------------------------------
    do i = 1, 512
        ca_caindz_(i)=1
    end do
!
!     -------------------------------------
    ca_nbsav_=0
!
!     -------------------------------------
    rundef=r8nnem()
    ca_timed1_=rundef
    ca_timef1_=rundef
    ca_td1_=rundef
    ca_tf1_=rundef
!
!
!
!
!     LES TABLEAUX SUIVANTS PEUVENT N'ETRE CONSTRUITS QU'UNE
!     FOIS POUR TOUTES LORS DU PREMIER APPEL A DEBCAL
!     &&CALCUL.NOMOP ET &&CALCUL.NOMTE NE SERONT SUPPRIMES
!     QU'EN SORTIE DE COMMANDE
!     --------------------------------------------------------
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
!
!
end subroutine
