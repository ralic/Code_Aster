subroutine debca1(nomop, ligrel, nin)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
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
#include "asterfort/mecoel.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=16) :: nomop
    character(len=19) :: ligrel
    integer :: nin
! ----------------------------------------------------------------------
!     BUT : INITIALISER CERTAINS COMMONS DE CALCUL : CAII02, ...
!
!     ENTREES:
!        NOMOP  :  NOM D'1 OPTION
!        LIGREL :  NOM DU LIGREL SUR LEQUEL ON DOIT FAIRE LE CALCUL
!        NIN    :  MAJORANT DU NOMBRE DE CHAMPS "IN"
!     SORTIES:
!       ALLOCATION D'OBJETS DE TRAVAIL ET MISE A JOUR DE COMMONS
!
! ----------------------------------------------------------------------
    character(len=8) :: ma
!---------------- COMMUNS POUR CALCUL ----------------------------------
    integer :: iaoptt, lgco, iaopmo, ilopmo, iaopno, ilopno, iaopds
    integer :: iaoppa, npario, nparin, iamloc, ilmloc, iadsgd
    common /caii02/iaoptt,lgco,iaopmo,ilopmo,iaopno,ilopno,iaopds,&
     &       iaoppa,npario,nparin,iamloc,ilmloc,iadsgd
!
    integer :: iamaco, ilmaco, iamsco, ilmsco, ialiel, illiel
    common /caii03/iamaco,ilmaco,iamsco,ilmsco,ialiel,illiel
!
    integer :: ianoop, ianote, nbobtr, iaobtr, nbobmx
    common /caii05/ianoop,ianote,nbobtr,iaobtr,nbobmx
!
    integer :: nbobj, iainel, ininel
    common /caii09/nbobj,iainel,ininel
!
    integer :: icaeli, icaelk
    common /caii10/icaeli,icaelk
!
    integer :: nute, jnbelr, jnoelr, iactif, jpnlfp, jnolfp, nblfpg
    common /caii11/nute,jnbelr,jnoelr,iactif,jpnlfp,jnolfp,nblfpg
!
    integer :: caindz(512), capoiz
    common /caii12/caindz,capoiz
!
    integer :: nbsav
    common /caii13/nbsav
!
    integer :: evfini, calvoi, jrepe, jptvoi, jelvoi
    common /caii19/evfini,calvoi,jrepe,jptvoi,jelvoi
!
    real(kind=8) :: timed1, timef1, td1, tf1
    common /carr01/timed1,timef1,td1,tf1
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
        call utmess('F', 'CALCULEL2_25', sk=ligrel)
    endif
!
    call jenonu(jexnom('&CATA.OP.NOMOPT', nomop), opt)
    if (opt .eq. 0) then
        call utmess('F', 'CALCULEL2_26', sk=nomop)
    endif
!
!
!     INITIALISATION DU COMMON CAII02 :
!     ---------------------------------
    call jeveuo('&CATA.TE.OPTTE', 'L', iaoptt)
    call jeveuo('&CATA.TE.NBLIGCOL', 'L', vi=nbligcol)
    lgco=nbligcol(1)
    call jeveuo('&CATA.TE.OPTMOD', 'L', iaopmo)
    call jeveuo(jexatr('&CATA.TE.OPTMOD', 'LONCUM'), 'L', ilopmo)
    call jeveuo('&CATA.TE.OPTNOM', 'L', iaopno)
    call jeveuo(jexatr('&CATA.TE.OPTNOM', 'LONCUM'), 'L', ilopno)
    call jeveuo(jexnum('&CATA.OP.DESCOPT', opt), 'L', iaopds)
    call jeveuo(jexnum('&CATA.OP.OPTPARA', opt), 'L', iaoppa)
!
    nparin=zi(iaopds-1+2)
    npario=zi(iaopds-1+2)+zi(iaopds-1+3)
    call jeveuo('&CATA.TE.MODELOC', 'L', iamloc)
    call jeveuo(jexatr('&CATA.TE.MODELOC', 'LONCUM'), 'L', ilmloc)
    call jeveuo('&CATA.GD.DESCRIGD', 'L', iadsgd)
!
!
!     INITIALISATION DU COMMON CAII05
!     ET CREATION DE L'OBJET '&&CALCUL.OBJETS_TRAV' QUI CONTIENDRA TOUS
!     LES NOMS DES OBJETS DE TRAVAIL NECESSAIRES A CALCUL :
!     ------------------------------------------------------------------
    nbscmx=9
!     NBSCMX = NB DE TYPES SCALAIRES MAX : I,R,C,L,K8,K16,K24,K32,K80
    nbobtr=0
    nbpara=zi(iaopds-1+2)+zi(iaopds-1+3)
    nbobmx=2*nbscmx+nin*3+nbpara+35
    call wkvect('&&CALCUL.OBJETS_TRAV', 'V V K24', nbobmx, iaobtr)
!
!
!     INITIALISATION DU COMMON CAII03 :
!     ---------------------------------
    call dismoi('NOM_MAILLA', ligrel, 'LIGREL', repk=ma)
    call jeexin(ma//'.CONNEX', iret)
    if (iret .gt. 0) then
        call jeveuo(ma//'.CONNEX', 'L', iamaco)
        call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', ilmaco)
    endif
    call jeexin(ligrel//'.NEMA', iret)
    if (iret .gt. 0) then
        call jeveuo(ligrel//'.NEMA', 'L', iamsco)
        call jeveuo(jexatr(ligrel//'.NEMA', 'LONCUM'), 'L', ilmsco)
    endif
    call jeveuo(ligrel//'.LIEL', 'L', ialiel)
    call jeveuo(jexatr(ligrel//'.LIEL', 'LONCUM'), 'L', illiel)
!
!
!     INITIALISATION DU COMMON CAII11 :
!     ---------------------------------
    call jeveuo('&CATA.TE.NBELREFE', 'L', jnbelr)
    call jeveuo('&CATA.TE.NOELREFE', 'L', jnoelr)
    call jeveuo('&CATA.TE.PNLOCFPG', 'L', jpnlfp)
    call jeveuo('&CATA.TE.NOLOCFPG', 'L', jnolfp)
    call jelira('&CATA.TE.NOLOCFPG', 'LONMAX', nblfpg)
!
!
!     INITIALISATION DU COMMON CAII19 :
!     ---------------------------------
    call dismoi('EXI_VF', ligrel, 'LIGREL', repk=exivf)
    call dismoi('BESOIN_VOISIN', ligrel, 'LIGREL', repk=bevois)
    if (exivf .eq. 'OUI') then
        evfini=1
        calvoi=1
    else
        evfini=0
        if (bevois .eq. 'OUI') then
            calvoi=1
        else
            calvoi=0
        endif
    endif
    if (calvoi .eq. 1) then
        call jeexin(ligrel//'.REPE', ier)
        if (ier .eq. 0) call cormgi('V', ligrel)
        call jeveuo(ligrel//'.REPE', 'L', jrepe)
        call dismoi('NOM_MAILLA', ligrel, 'LIGREL', repk=ma)
        call jeexin(ligrel//'.NVGE', ier)
        if (ier .ne. 0) then
            call jeveuo(ligrel//'.NVGE', 'L', vk16=nvge)
            vge=nvge(1)
            call jeveuo(vge//'.PTVOIS', 'L', jptvoi)
            call jeveuo(vge//'.ELVOIS', 'L', jelvoi)
        else
            jrepe=0
        endif
    else
        jrepe=0
    endif
!
!
!     INITIALISATION DU COMMON CAII10 :
!     ---------------------------------
    call dismoi('NB_NO_MAX', '&', 'CATALOGUE', repi=nnomx)
    call wkvect('&&CALCUL.TECAEL_K24', 'V V K24', 8+nnomx, icaelk)
    nbobtr=nbobtr+1
    zk24(iaobtr-1+nbobtr)='&&CALCUL.TECAEL_K24'
    zk24(icaelk-1+1)=ma
    zk24(icaelk-1+2)=ligrel
    call wkvect('&&CALCUL.TECAEL_I', 'V V I', 4+nnomx, icaeli)
    nbobtr=nbobtr+1
    zk24(iaobtr-1+nbobtr)='&&CALCUL.TECAEL_I'
!
!
!     -- INITIALISATION DU COMMON CAII09 :
!     NBOBJ EST LE NOMBRE MAXI D'OBJETS '&INEL.XXX' CREES PAR UN INI00K
!     ------------------------------------------------------------------
    nbobj=30
    call wkvect('&&CALCUL.NOM_&INEL', 'V V K24', nbobj, ininel)
    nbobtr=nbobtr+1
    zk24(iaobtr-1+nbobtr)='&&CALCUL.NOM_&INEL'
    call wkvect('&&CALCUL.IAD_&INEL', 'V V I', nbobj, iainel)
    nbobtr=nbobtr+1
    zk24(iaobtr-1+nbobtr)='&&CALCUL.IAD_&INEL'
!
!
!     -- INITIALISATION DU COMMON CAII12 :
!     -------------------------------------
    do i = 1, 512
        caindz(i)=1
    end do
!
!     -- INITIALISATION DU COMMON CAII13 :
!     -------------------------------------
    nbsav=0
!
!     -- INITIALISATION DU COMMON CARR01 :
!     -------------------------------------
    rundef=r8nnem()
    timed1=rundef
    timef1=rundef
    td1=rundef
    tf1=rundef
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
        call wkvect('&&CALCUL.NOMOP', 'V V K16', nbopt, ianoop)
        do i = 1, nbopt
            call jenuno(jexnum('&CATA.OP.NOMOPT', i), nomop2)
            zk16(ianoop-1+i)=nomop2
        end do
        call jelira('&CATA.TE.NOMTE', 'NOMMAX', nbte)
        call wkvect('&&CALCUL.NOMTE', 'V V K16', nbte, ianote)
        do i = 1, nbte
            call jenuno(jexnum('&CATA.TE.NOMTE', i), nomte)
            zk16(ianote-1+i)=nomte
        end do
    else
        call jeveuo('&&CALCUL.NOMOP', 'L', ianoop)
        call jeveuo('&&CALCUL.NOMTE', 'L', ianote)
    endif
!
!     -- APPEL A MECOEL (INUTILE) POUR CONSERVER LA DOC :
    call mecoel(1)
!
end subroutine
