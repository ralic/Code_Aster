subroutine calcul(stop, option_, ligrel_, nin, lchin,&
                  lpain, nou, lchou, lpaou, base,&
                  mpic)
! aslint: disable=W1306
use calcul_module, only : ca_iainel_, ca_ialiel_, ca_iaobtr_, ca_iaopds_,&
     ca_iaoppa_, ca_igr_, ca_illiel_, ca_ininel_,&
     ca_jcteat_, ca_lcteat_, ca_nbelgr_, ca_nbgr_, &
     ca_nbobj_, ca_nbobtr_, ca_nomte_, ca_nomtm_, ca_nute_,&
     ca_nuop_, ca_ligrel_, ca_option_, ca_iactif_,&
     ca_ldist_, ca_ldgrel_, ca_rang_, ca_nbproc_, ca_numsd_,&
     ca_lparal_, ca_paral_, ca_iel_
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
#include "asterfort/uttcpu.h"
#include "asterfort/vrcdec.h"
#include "asterfort/wkvect.h"
#include "asterfort/zechlo.h"

    integer, intent(in) :: nou
    integer, intent(in) :: nin
    character(len=1), intent(in) :: stop
    character(len=*), intent(in) :: option_
    character(len=*), intent(in) :: ligrel_
    character(len=*), intent(in) :: lchin(*)
    character(len=*), intent(in) :: lpain(*)
    character(len=*), intent(in) :: lchou(*)
    character(len=*), intent(in) :: lpaou(*)
    character(len=*), intent(in) :: base
    character(len=*), intent(in) :: mpic

! ----------------------------------------------------------------------
! But : Effectuer les calculs elementaires d'une option (option_)
!       par les elements finis de ligrel (ligrel_)

!     entrees:
!        stop   :  /'S' : on s'arrete si aucun element fini du ligrel
!                         ne sait calculer l'option.
!                  /'C' : On continue meme si aucun element fini du ligrel
!                         ne sait calculer l'option.
!                         Il n'existe pas de champ "out" dans ce cas.
!        option_:  nom d'1 option
!        ligrel_ :  nom du ligrel sur lequel on doit faire le calcul
!        nin    :  nombre de champs parametres "in"
!        nou    :  nombre de champs parametres "out"
!        lchin  :  liste des noms des champs "in"
!        lchou  :  liste des noms des champs "out"
!        lpain  :  liste des noms des parametres "in"
!        lpaou  :  liste des noms des parametres "out"
!        base   :  'G' , 'V'
!        mpic   :  'OUI' / 'NON' :
!                  'OUI' : si le calcul est distribue, on "complete" les
!                          cham_elem "out" pour que le champ soit le
!                          meme sur tous les processeurs.

!     sorties:
!       allocation et calcul des objets correspondant aux champs "out"
!-----------------------------------------------------------------------
    aster_logical :: dbg
    character(len=8) :: lpain2(nin), lpaou2(nou)
    character(len=19) :: lchin2(nin), lchou2(nou)
    character(len=24) :: valk(2)
    integer ::   ima, ifm
    integer :: niv
    integer :: ier
    integer ::  iret, iuncod, j
    integer ::  nval
    integer :: afaire
    integer :: numc
    integer :: i, ipar, jpar, nin2, nin3, nou2, nou3
    character(len=8) :: nompar, exiele, k8bid, tych
    character(len=10) :: k10b
    character(len=16) :: k16bid, cmde
    character(len=20) :: k20b1, k20b2, k20b3, k20b4
    character(len=8), pointer :: typma(:) => null()
!----------------------------------------------------------------------
!   -- fonctions formules :
!   numail(igr,iel) = numero de la maille associee a l'element (igr,iel)
# define numail(ca_igr_,ca_iel_) zi(ca_ialiel_-1+zi(ca_illiel_-1+ca_igr_)-1+ca_iel_)

!-------------------------------------------------------------------

    call jemarq()

!   dbg : une variable pour provoquer des ecritures de debug :
    dbg=.false.

    ca_ligrel_=ligrel_
    ca_option_=option_

!   -- Pour que les mesures de temps en // soient comprehensibles
!      par les utilisateurs, il faut forcer une synchro avant les mesures :
!   -----------------------------------------------------------------------
    call asmpi_barrier()
    call uttcpu('CPU.CALC.1', 'DEBUT', ' ')
    call uttcpu('CPU.CALC.2', 'DEBUT', ' ')

    call infniv(ifm, niv)
    ca_nbobtr_=0
    ASSERT(mpic.eq.'OUI'.or.mpic.eq.'NON')
    ASSERT(stop.eq.'S'.or.stop.eq.'C')


!   -- S'il n'y a pas d'elements finis, on sort :
!   ----------------------------------------------
    call dismoi('EXI_ELEM', ca_ligrel_, 'LIGREL', repk=exiele)
    if (exiele .ne. 'OUI') then
        if (stop .eq. 'S') then
            call utmess('F', 'CALCUL_1', sk=ca_ligrel_)
        else
            goto 999
        endif
    endif


!   -- debca1 : preparartion du calcul (hors listes de champs)
!   -----------------------------------------------------------------
    call debca1(nin)

    call jeveuo('&CATA.TE.TYPEMA', 'L', vk8=typma)
    call jenonu(jexnom('&CATA.OP.NOMOPT', ca_option_), ca_nuop_)

!   -- pour savoir l'unite logique ou ecrire le fichier ".code" :
    iuncod = iunifi('CODE')
    if (iuncod .gt. 0) call getres(k8bid, k16bid, cmde)



!   1. Si aucun type_element du ligrel ne sait calculer l'option,
!      on va directement a la sortie :
!   -------------------------------------------------------------
    afaire=0
    ier=0
    ca_nbgr_=nbgrel(ca_ligrel_)
    do j = 1, ca_nbgr_
        ca_nute_=typele(ca_ligrel_,j,1)
        call jenuno(jexnum('&CATA.TE.NOMTE', ca_nute_), ca_nomte_)
        ca_nomtm_=typma(ca_nute_)
        numc=nucalc(ca_nuop_,ca_nute_,0)

!        -- si le numero du teooij est negatif :
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

        afaire=max(afaire,numc)
    enddo
    ASSERT(ier.le.0)
    if (afaire .eq. 0) then
        if (stop .eq. 'S') then
            call utmess('F', 'CALCUL_38', sk=ca_option_)
        else
            goto 999
        endif
    endif


!   2. On rend propres les listes : lpain,lchin,lpaou,lchou :
!      en ne gardant que les parametres du catalogue de l'option
!      qui servent a au moins un type_element.
!      On supprime egalement les champs "in" qui n'existent pas.
!   ---------------------------------------------------------
    ASSERT(nin.le.80)
    nin3=zi(ca_iaopds_-1+2)
    nou3=zi(ca_iaopds_-1+3)

    nin2=0
    loop_nin : &
    do i = 1, nin
        nompar=lpain(i)
        ipar=indik8(zk8(ca_iaoppa_),nompar,1,nin3)
        if (ipar .gt. 0) then
            do j = 1, ca_nbgr_
                ca_nute_=typele(ca_ligrel_,j,1)
                jpar=inpara(ca_nuop_,ca_nute_,'IN ',nompar)
                if (jpar .eq. 0) cycle
                call exisd('CHAMP_GD', lchin(i), iret)
                if (iret .eq. 0) cycle loop_nin
                nin2=nin2+1
                lpain2(nin2)=lpain(i)
                lchin2(nin2)=lchin(i)
                cycle loop_nin
            enddo
        endif
    enddo loop_nin

!   -- verif pas de doublons dans lpain2 :
    call kndoub(8, lpain2, nin2, iret)
    ASSERT(iret.eq.0)

    nou2=0
    loop_nou : &
    do i = 1, nou
        nompar=lpaou(i)
        ipar=indik8(zk8(ca_iaoppa_+nin3),nompar,1,nou3)
        if (ipar .gt. 0) then
            do j = 1, ca_nbgr_
                ca_nute_=typele(ca_ligrel_,j,1)
                ipar=inpara(ca_nuop_,ca_nute_,'OUT',nompar)
                if (ipar .gt. 0) then
                    nou2=nou2+1
                    lpaou2(nou2)=lpaou(i)
                    lchou2(nou2)=lchou(i)
                    ASSERT(lchou2(nou2).ne.' ')
                    cycle loop_nou
                endif
            enddo
        endif
    enddo  loop_nou

!   -- verif pas de doublons dans lpaou2 :
    call kndoub(8, lpaou2, nou2, iret)
    ASSERT(iret.eq.0)


!   3. debcal fait des initialisations et met les objets en memoire :
!      (s'occupe des champs des listes lchin2 et lchou2)
!   -----------------------------------------------------------------
    call debcal(nin2, lchin2, lpain2, nou2, lchou2)
    if (dbg) call caldbg('IN', nin2, lchin2, lpain2)


!   4. Allocation des resultats et des champs locaux:
!   -------------------------------------------------
    call alrslt(nou2, lchou2, lpaou2, base)
    call alchlo(nin2, lpain2, nou2, lpaou2)


!   5. Avant boucle sur les grel :
!      Quelques actions hors boucle grel dues a calvoi==1 :
!   -----------------------------------------------------
    call extrai(nin2, lchin2, lpain2, 'INIT')


!   6. boucle sur les grel :
!   ------------------------
    ca_iactif_=1
    loop_grel : &
    do ca_igr_ = 1, ca_nbgr_

!       -- si parallelisme='group_elem' : on peut parfois tout "sauter"
        if (ca_ldgrel_ .and. mod(ca_igr_,ca_nbproc_) .ne. ca_rang_) cycle loop_grel

!       -- si le grel est vide, il faut "sauter" :
        ca_nbelgr_=nbelem(ca_ligrel_,ca_igr_,1)
        if (ca_nbelgr_ .eq. 0) cycle loop_grel

        ca_nute_=typele(ca_ligrel_,ca_igr_,1)
        call jenuno(jexnum('&CATA.TE.NOMTE', ca_nute_), ca_nomte_)
        ca_nomtm_=typma(ca_nute_)
        call jelira(jexnum('&CATA.TE.CTE_ATTR', ca_nute_), 'LONMAX', ca_lcteat_)
        if (ca_lcteat_ .gt. 0) then
            call jeveuo(jexnum('&CATA.TE.CTE_ATTR', ca_nute_), 'L', ca_jcteat_)
        else
            ca_jcteat_=0
        endif

        numc=nucalc(ca_nuop_,ca_nute_,0)
        ASSERT(numc.ge.-10)
        ASSERT(numc.le.9999)

        if (numc .gt. 0) then

!           -- Si calcul distribue par element on renseigne ca_paral_ :
            if (ca_lparal_) then
                do ca_iel_ = 1, ca_nbelgr_
                    ca_paral_(ca_iel_)=.false.
                    ima=numail(ca_igr_,ca_iel_)
                    if (ima .lt. 0) then
                        if (ca_rang_ .eq. 0) then
                            ca_paral_(ca_iel_)=.true.
                        endif
                    else if (ima.gt.0) then
                        if (ca_numsd_(ima) .eq. ca_rang_) then
                            ca_paral_(ca_iel_)=.true.
                        endif
                    endif
                enddo
            endif

!           6.1 Initialisation des type_elem :
            call inigrl(ca_ligrel_, ca_igr_, ca_nbobj_, zi(ca_iainel_), zk24(ca_ininel_),&
                        nval)

!           6.2 Ecriture au format ".code" du couple (option,type_elem)
            if (iuncod .gt. 0) then
                k10b = 'TEST'
                k20b1 = '&&CALCUL'
                k20b2 = ca_option_
                k20b3 = ca_nomte_
                k20b4 = cmde
                write (iuncod, 101) k10b, k20b1, k20b2, k20b3, k20b4
            endif

!           6.3 Preparation des champs "in"
            call extrai(nin2, lchin2, lpain2, ' ')

!           6.4 Mise a zero des champs "out"
            call zechlo(ca_nuop_, ca_nute_)

!           6.5 On ecrit une valeur "undef" au bout de
!               tous les champs locaux "in" et "out":
            call caundf('ECRIT', ca_nuop_, ca_nute_)

!           6.6 On realise les calculs elementaires:
            if (dbg) write (6,*)'&&CALCUL OPTION= ',ca_option_,' ',ca_nomte_, ' ',numc
            call vrcdec()
            ca_iactif_=3
            call te0000(numc, ca_nuop_, ca_nute_)
            ca_iactif_=1

!           6.7 On verifie la valeur "undef" des champs locaux "out" :
            call caundf('VERIF', ca_nuop_, ca_nute_)

!           6.8 On recopie des champs locaux dans les champs globaux:
            call montee(nou2, lchou2, lpaou2, ' ')

            if (dbg) call caldbg('OUTG', nou2, lchou2, lpaou2)

        endif
    enddo loop_grel


!   7- Apres boucle sur les grel :
!      Quelques actions hors boucle grel dues a calvoi==1 :
!   -------------------------------------------------------
    call montee(nou2, lchou2, lpaou2, 'FIN')


!   8- On "complete" les cham_elem "out" si necessaire :
!   ----------------------------------------------------
    if (mpic .eq. 'OUI' .and. ca_ldist_) then
        do i = 1, nou2
            call dismoi('TYPE_CHAMP', lchou2(i), 'CHAMP', repk=tych)
            if (tych(1:2) .eq. 'EL') call sdmpic('CHAM_ELEM', lchou2(i))
        enddo
    endif

    if (dbg) call caldbg('OUTF', nou2, lchou2, lpaou2)

999 continue


!   9. On detruit les objets volatiles crees par calcul:
!   ----------------------------------------------------
    do i = 1, ca_nbobtr_
        call jedetr(zk24(ca_iaobtr_-1+i))
    enddo
    call jedetr('&&CALCUL.OBJETS_TRAV')
    ca_iactif_=0


!   9- Mesure du temps consomme :
!   ----------------------------------
    call asmpi_barrier()
    call uttcpu('CPU.CALC.2', 'FIN', ' ')
    call uttcpu('CPU.CALC.1', 'FIN', ' ')


    call jedema()

101 format(1x,a10,a20,1x,a20,a20,a20)

end subroutine
