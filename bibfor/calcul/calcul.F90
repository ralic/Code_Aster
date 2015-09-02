subroutine calcul(stop, optio, ligrlz, nin, lchin,&
                  lpain, nou, lchou, lpaou, base,&
                  mpic)
! aslint: disable=W1306
use calcul_module, only : ca_iainel_, ca_ialiel_, ca_iaobtr_, ca_iaopds_,&
     ca_iaoppa_, ca_igr_, ca_illiel_, ca_ininel_,&
     ca_jcteat_, ca_lcteat_, ca_nbelgr_, ca_nbgr_, &
     ca_nbobj_, ca_nbobtr_, ca_nomte_, ca_nomtm_, ca_nute_,&
     ca_option_, ca_iactif_
implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!     ARGUMENTS:
!     ----------
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/indik8.h"
#include "asterfort/alchlo.h"
#include "asterfort/alrslt.h"
#include "asterfort/asmpi_barrier.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/caldbg.h"
#include "asterfort/caundf.h"
#include "asterfort/debca1.h"
#include "asterfort/debcal.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/extrai.h"
#include "asterfort/infniv.h"
#include "asterfort/inigrl.h"
#include "asterfort/inpara.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/kndoub.h"
#include "asterfort/montee.h"
#include "asterfort/nbelem.h"
#include "asterfort/nbgrel.h"
#include "asterfort/nucalc.h"
#include "asterfort/sdmpic.h"
#include "asterfort/te0000.h"
#include "asterfort/typele.h"
#include "asterfort/utimsd.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/vrcdec.h"
#include "asterfort/wkvect.h"
#include "asterfort/zechlo.h"
!
    integer, intent(in) :: nou
    integer, intent(in) :: nin
    character(len=1), intent(in) :: stop
    character(len=*), intent(in) :: optio
    character(len=*), intent(in) :: ligrlz
    character(len=*), intent(in) :: lchin(*)
    character(len=*), intent(in) :: lpain(*)
    character(len=*), intent(in) :: lchou(*)
    character(len=*), intent(in) :: lpaou(*)
    character(len=*), intent(in) :: base
    character(len=*), intent(in) :: mpic
!
! ----------------------------------------------------------------------
!     ENTREES:
!        STOP   :  /'S' : ON S'ARRETE SI AUCUN ELEMENT FINI DU LIGREL
!                         NE SAIT CALCULER L'OPTION.
!                  /'C' : ON CONTINUE SI AUCUN ELEMENT FINI DU LIGREL
!                         NE SAIT CALCULER L'OPTION. IL N'EXISTE PAS DE
!                         CHAMP "OUT" DANS CE CAS.
!        OPTIO  :  NOM D'1 OPTION
!        LIGRLZ :  NOM DU LIGREL SUR LEQUEL ON DOIT FAIRE LE CALCUL
!        NIN    :  NOMBRE DE CHAMPS PARAMETRES "IN"
!        NOU    :  NOMBRE DE CHAMPS PARAMETRES "OUT"
!        LCHIN  :  LISTE DES NOMS DES CHAMPS "IN"
!        LCHOU  :  LISTE DES NOMS DES CHAMPS "OUT"
!        LPAIN  :  LISTE DES NOMS DES PARAMETRES "IN"
!        LPAOU  :  LISTE DES NOMS DES PARAMETRES "OUT"
!        BASE   :  'G' , 'V' OU 'L'
!        MPIC   :  'OUI' / 'NON' :
!                  'OUI' : SI LE CALCUL EST DISTRIBUE, ON "COMPLETE" LES
!                          CHAM_ELEM "OUT" POUR QUE LE CHAMP SOIT LE
!                          MEME SUR TOUS LES PROCESSEURS.
!
!     SORTIES:
!       ALLOCATION ET CALCUL DES OBJETS CORRESPONDANT AUX CHAMPS "OUT"
!
! ----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
    aster_logical :: ldist, dbg, ldgrel
    character(len=8) :: lpain2(nin), lpaou2(nou)
    character(len=19) :: lchin2(nin), lchou2(nou)
    character(len=19) :: ligrel
    character(len=24) :: valk(2)
    integer ::  nbproc
    integer ::   ima, rang, ifm
    integer :: niv
    integer :: ier
    integer ::  iret, iuncod, j
    integer ::  n1
    integer :: vali(4)
    integer ::  nval
    integer :: opt, afaire
    integer :: iel, numc
    integer :: i, ipar, nin2, nin3, nou2, nou3
    character(len=1) :: base2
    character(len=8) :: nompar, exiele, k8bid, partit, tych
    character(len=10) :: k10b
    character(len=16) :: k16bid, cmde
    character(len=20) :: k20b1, k20b2, k20b3, k20b4
    mpi_int :: mrank, msize
    integer, pointer :: prti(:) => null()
    character(len=8), pointer :: typma(:) => null()
    integer, pointer :: numsd(:) => null()
    character(len=24), pointer :: prtk(:) => null()
    aster_logical, pointer :: paral(:) => null()
!----------------------------------------------------------------------
!
!   -- FONCTIONS FORMULES :
!   NUMAIL(IGR,IEL)=NUMERO DE LA MAILLE ASSOCIEE A L'ELEMENT IEL
# define numail(ca_igr_,iel) zi(ca_ialiel_-1+zi(ca_illiel_-1+ca_igr_)-1+iel)
!
! DEB-------------------------------------------------------------------
!
    call jemarq()
!
!     -- POUR QUE LES MESURES DE TEMPS EN // SOIENT COMPREHENSIBLES
!        PAR LES UTILISATEURS, IL FAUT FORCER UNE SYNCHRO AVANT LES
!        MESURES :
    call asmpi_barrier()
    call uttcpu('CPU.CALC.1', 'DEBUT', ' ')
    call uttcpu('CPU.CALC.2', 'DEBUT', ' ')
!
    call infniv(ifm, niv)
    ligrel=ligrlz
    ca_nbobtr_=0
    base2=base
    ca_option_=optio
    ASSERT(mpic.eq.'OUI'.or.mpic.eq.'NON')
    ASSERT(stop.eq.'S'.or.stop.eq.'C')
!
    dbg=.false.
!
!
!
!     -- S'IL N'Y A PAS D'ELEMENTS FINIS, ON SORT :
!     ----------------------------------------------
    call dismoi('EXI_ELEM', ligrel, 'LIGREL', repk=exiele)
    if (exiele .ne. 'OUI') then
        if (stop .eq. 'S') then
            call utmess('F', 'CALCUL_1', sk=ligrel)
        else
            goto 120
        endif
    endif
!
!
!     -- DEBCA1 MET CERTAINS OBJETS EN MEMOIRE
!     -----------------------------------------------------------------
    call debca1(ca_option_, ligrel, nin)
!
    call jeveuo('&CATA.TE.TYPEMA', 'L', vk8=typma)
    call jenonu(jexnom('&CATA.OP.NOMOPT', ca_option_), opt)
!
!     -- POUR SAVOIR L'UNITE LOGIQUE OU ECRIRE LE FICHIER ".CODE" :
    iuncod = iunifi('CODE')
    if (iuncod .gt. 0) call getres(k8bid, k16bid, cmde)
!
!
!
!     -- CAS D'UN CALCUL "DISTRIBUE" :
!     -- CALCUL DE LDIST :
!          .TRUE.  : LES CALCULS ELEMENTAIRES SONT DISTRIBUES
!          .FALSE. : SINON
!     -- CALCUL DE LDGREL :
!          .TRUE.  : LES CALCULS ELEMENTAIRES SONT DISTRIBUES PAR GREL
!                    IGREL -> RANG=MOD(LIGREL,NBPROC)
!     -- SI LDIST  == .TRUE. : CALCUL DE  RANG, NBPROC, [JNUMSD]
!     -------------------------------------------------------------
    ldist=.false.
    ldgrel=.false.
    call dismoi('PARTITION', ligrel, 'LIGREL', repk=partit)
    call jeexin(partit//'.PRTK', iret)
    if (iret .ne. 0) then
        ldist=.true.
        call asmpi_info(rank=mrank, size=msize)
        rang = to_aster_int(mrank)
        nbproc = to_aster_int(msize)
        call jeveuo(partit//'.PRTI', 'L', vi=prti)
        if (prti(1) .ne. nbproc) then
            vali(1)=prti(1)
            vali(2)=nbproc
            call utmess('F', 'CALCUL_35', ni=2, vali=vali)
        endif
!
        call jeveuo(partit//'.PRTK', 'L', vk24=prtk)
        ldgrel=prtk(1).eq.'GROUP_ELEM'
        if (.not.ldgrel) then
            call jeveuo(partit//'.NUPROC.MAILLE', 'L', vi=numsd)
            call jelira(partit//'.NUPROC.MAILLE', 'LONMAX', n1)
        endif
    endif
!
!
!
!     1- SI AUCUN TYPE_ELEMENT DU LIGREL NE SAIT CALCULER L'OPTION,
!     -- ON VA DIRECTEMENT A LA SORTIE :
!     -------------------------------------------------------------
    afaire=0
    ier=0
    ca_nbgr_=nbgrel(ligrel)
    do j = 1, ca_nbgr_
        ca_nute_=typele(ligrel,j,1)
        call jenuno(jexnum('&CATA.TE.NOMTE', ca_nute_), ca_nomte_)
        ca_nomtm_=typma(ca_nute_)
        numc=nucalc(opt,ca_nute_,0)
!
!        -- SI LE NUMERO DU TEOOIJ EST NEGATIF :
        if (numc .lt. 0) then
            if (numc .eq. -1 .or. numc .eq. -2 ) then
                valk(1)=ca_nomte_
                valk(2)=ca_option_
                if (numc .eq. -1) then
                    call utmess('F', 'CALCUL_37', nk=2, valk=valk)
                else
                    call utmess('A', 'CALCUL_41', nk=2, valk=valk)
                endif
            else
                ASSERT(.false.)
            endif
        endif
!
        afaire=max(afaire,numc)
    end do
    ASSERT(ier.le.0)
    if (afaire .eq. 0) then
        if (stop .eq. 'S') then
            call utmess('F', 'CALCUL_38', sk=ca_option_)
        else
            goto 120
!
        endif
    endif
!
!     2- ON REND PROPRES LES LISTES : LPAIN,LCHIN,LPAOU,LCHOU :
!        EN NE GARDANT QUE LES PARAMETRES DU CATALOGUE DE L'OPTION
!        QUI SERVENT A AU MOINS UN TYPE_ELEMENT
!     ---------------------------------------------------------
!     TEST SUR ERREUR PROGRAMMEUR : TROP DE CHAMPS "IN"
    ASSERT(nin.le.80)
    nin3=zi(ca_iaopds_-1+2)
    nou3=zi(ca_iaopds_-1+3)
!
    nin2=0
    do i = 1, nin
        nompar=lpain(i)
        ipar=indik8(zk8(ca_iaoppa_),nompar,1,nin3)
        if (ipar .gt. 0) then
            do j = 1, ca_nbgr_
                ca_nute_=typele(ligrel,j,1)
                ipar=inpara(opt,ca_nute_,'IN ',nompar)
!
                if (ipar .eq. 0) goto 40
                call exisd('CHAMP_GD', lchin(i), iret)
                if (iret .eq. 0) goto 40
                nin2=nin2+1
                lpain2(nin2)=lpain(i)
                lchin2(nin2)=lchin(i)
                goto 50
!
 40             continue
            end do
        endif
 50     continue
    end do
!
!     -- VERIF PAS DE DOUBLONS DANS LPAIN2 :
    call kndoub(8, lpain2, nin2, iret)
    ASSERT(iret.eq.0)
!
    nou2=0
    do i = 1, nou
        nompar=lpaou(i)
        ipar=indik8(zk8(ca_iaoppa_+nin3),nompar,1,nou3)
        if (ipar .gt. 0) then
            do j = 1, ca_nbgr_
                ca_nute_=typele(ligrel,j,1)
                ipar=inpara(opt,ca_nute_,'OUT',nompar)
!
                if (ipar .eq. 0) goto 60
                nou2=nou2+1
                lpaou2(nou2)=lpaou(i)
                lchou2(nou2)=lchou(i)
!           -- ON INTERDIT LA CREATION DU CHAMP ' ' :
                ASSERT(lchou2(nou2).ne.' ')
                goto 70
!
 60             continue
            end do
        endif
 70     continue
    end do
!     -- VERIF PAS DE DOUBLONS DANS LPAOU2 :
    call kndoub(8, lpaou2, nou2, iret)
    ASSERT(iret.eq.0)
!
!     3- DEBCAL FAIT DES INITIALISATIONS ET MET LES OBJETS EN MEMOIRE :
!     -----------------------------------------------------------------
    call debcal(ca_option_, ligrel, nin2, lchin2, lpain2,&
                nou2, lchou2)
    if (dbg) call caldbg('IN', nin2, lchin2, lpain2)
!
!     4- ALLOCATION DES RESULTATS ET DES CHAMPS LOCAUX:
!     -------------------------------------------------
    call alrslt(opt, ligrel, nou2, lchou2, lpaou2,&
                base2, ldist)
    call alchlo(opt, ligrel, nin2, lpain2, nou2,&
                lpaou2)
!
!     5- AVANT BOUCLE SUR LES GREL :
!     QUELQUES ACTIONS HORS BOUCLE GREL DUES A CALVOI==1 :
!     -----------------------------------------------------
    call extrai(nin2, lchin2, lpain2, opt, ca_nute_,&
                ligrel, 'INIT')
!
!     6- BOUCLE SUR LES GREL :
!     -------------------------------------------------
    ca_iactif_=1
    do ca_igr_ = 1, ca_nbgr_
!
!       -- SI PARALLELISME='GROUP_ELEM' : ON PEUT PARFOIS TOUT "SAUTER"
        if (ldgrel .and. mod(ca_igr_,nbproc) .ne. rang) goto 100
!
!       -- SI LE GREL EST VIDE, IL FAUT "SAUTER" :
        ca_nbelgr_=nbelem(ligrel,ca_igr_,1)
        if (ca_nbelgr_ .eq. 0) goto 100
!
        ca_nute_=typele(ligrel,ca_igr_,1)
        call jenuno(jexnum('&CATA.TE.NOMTE', ca_nute_), ca_nomte_)
        ca_nomtm_=typma(ca_nute_)
        call jelira(jexnum('&CATA.TE.CTE_ATTR', ca_nute_), 'LONMAX', ca_lcteat_)
        if (ca_lcteat_ .gt. 0) then
            call jeveuo(jexnum('&CATA.TE.CTE_ATTR', ca_nute_), 'L', ca_jcteat_)
        else
            ca_jcteat_=0
        endif
!
        numc=nucalc(opt,ca_nute_,0)
        ASSERT(numc.ge.-10)
        ASSERT(numc.le.9999)
!
        if (numc .gt. 0) then
!
!         -- EN MODE PARALLELE
!         -- SI CALCUL DISTRIBUE , ON VA REMPLIR
!         -- LE VECTEUR AUXILIAIRE '&CALCUL.PARALLELE'
            if (ldist) then
                call wkvect('&CALCUL.PARALLELE', 'V V L', ca_nbelgr_, vl=paral)
                do iel = 1, ca_nbelgr_
                    ima=numail(ca_igr_,iel)
                    if (ldist) then
                        if (.not.ldgrel) then
                            if (ima .lt. 0) then
                                if (rang .eq. 0) then
                                    paral(iel)=.true.
                                endif
                            else if (ima.gt.0) then
                                if (numsd(ima) .eq. rang) then
                                    paral(iel)=.true.
                                endif
                            endif
                        else
!                           -- SI LDGREL, ON EST SUR LE BON PROC :
                            paral(iel)=.true.
                        endif
                    endif
                end do
            endif
!
!         6.1 INITIALISATION DES TYPE_ELEM :
            call inigrl(ligrel, ca_igr_, ca_nbobj_, zi(ca_iainel_), zk24(ca_ininel_),&
                        nval)
!
!         6.2 ECRITURE AU FORMAT ".CODE" DU COUPLE (OPTION,TYPE_ELEM)
            if (iuncod .gt. 0) then
                k10b = 'TEST'
                k20b1 = '&&CALCUL'
                k20b2 = ca_option_
                k20b3 = ca_nomte_
                k20b4 = cmde
                write (iuncod, 101) k10b, k20b1, k20b2, k20b3, k20b4
            endif
!
!         6.3 PREPARATION DES CHAMPS "IN"
            call extrai(nin2, lchin2, lpain2, opt, ca_nute_,&
                        ligrel, ' ')
!
!         6.4 MISE A ZERO DES CHAMPS "OUT"
            call zechlo(opt, ca_nute_)
!
!         6.5 ON ECRIT UNE VALEUR "UNDEF" AU BOUT DE
!             TOUS LES CHAMPS LOCAUX "IN" ET "OUT":
            call caundf('ECRIT', opt, ca_nute_)
!
!         6.6 ON FAIT LES CALCULS ELEMENTAIRES:
            if (dbg) write (6,*)'&&CALCUL OPTION= ',ca_option_,' ',ca_nomte_, ' ',numc
            call vrcdec()
            call te0000(numc, opt, ca_nute_)
!
!         6.7 ON VERIFIE LA VALEUR "UNDEF" DES CHAMPS LOCAUX "OUT" :
            call caundf('VERIF', opt, ca_nute_)
!
!         6.8 ON RECOPIE DES CHAMPS LOCAUX DANS LES CHAMPS GLOBAUX:
            call montee(opt, ligrel, nou2, lchou2, lpaou2,&
                        ' ')
!         IMPRESSIONS DE DEBUG POUR DETERMINER LES TEXXXX COUPABLES :
            if (dbg) call caldbg('OUTG', nou2, lchou2, lpaou2)
!
            call jedetr('&CALCUL.PARALLELE')
        endif
100     continue
    end do
!     ---FIN BOUCLE IGR
!
!     7- APRES BOUCLE SUR LES GREL :
!     QUELQUES ACTIONS HORS BOUCLE GREL DUES A CALVOI==1 :
!     -----------------------------------------------------
    call montee(opt, ligrel, nou2, lchou2, lpaou2,&
                'FIN')
!
!     8- ON "COMPLETE" LES CHAM_ELEM "OUT" SI NECESSAIRE :
!     ----------------------------------------------------
    if (mpic .eq. 'OUI' .and. ldist) then
        do i = 1, nou2
            call dismoi('TYPE_CHAMP', lchou2(i), 'CHAMP', repk=tych)
            if (tych(1:2) .eq. 'EL') call sdmpic('CHAM_ELEM', lchou2(i))
        end do
    endif
!
    if (dbg) call caldbg('OUTF', nou2, lchou2, lpaou2)
!
120 continue
!
!
!     9- ON DETRUIT LES OBJETS VOLATILES CREES PAR CALCUL:
!     ----------------------------------------------------
    do i = 1, ca_nbobtr_
        call jedetr(zk24(ca_iaobtr_-1+i))
    end do
    call jedetr('&&CALCUL.OBJETS_TRAV')
    ca_iactif_=0
!
!
!     9- MESURE DU TEMPS CONSOMME :
!     ----------------------------------
    call asmpi_barrier()
    call uttcpu('CPU.CALC.2', 'FIN', ' ')
    call uttcpu('CPU.CALC.1', 'FIN', ' ')
!
!
    call jedema()
!
    101 format(1x,a10,a20,1x,a20,a20,a20)
!
end subroutine
