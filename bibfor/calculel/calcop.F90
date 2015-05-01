subroutine calcop(option, lisopt, resuin, resuou, lisord,&
                  nbordr, lischa, ncharg, chtype, typesd,&
                  codret)
    implicit none
!     --- ARGUMENTS ---
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterfort/ccchel.h"
#include "asterfort/ccchno.h"
#include "asterfort/ccliop.h"
#include "asterfort/cclodr.h"
#include "asterfort/cclord.h"
#include "asterfort/ccnett.h"
#include "asterfort/ccvepo.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/getvtx.h"
#include "asterfort/indk16.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/medom2.h"
#include "asterfort/reliem.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rslesd.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rsorac.h"
#include "asterfort/srmedo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nbordr, ncharg, codret, tbid(1)
    character(len=4) :: chtype
    character(len=8) :: resuin, resuou
    character(len=16) :: option, typesd
    character(len=19) :: lischa, lisord
    character(len=*) :: lisopt
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
! ----------------------------------------------------------------------
!  CALC_CHAMP - CALCUL D'UNE OPTION
!               ----         --
! ----------------------------------------------------------------------
!
!  ROUTINE DE BASE DE CALC_CHAMP
!
! IN  :
!   OPTION  K16  NOM DE L'OPTION A CALCULER
!   RESUIN  K8   NOM DE LA STRUCTURE DE DONNEES RESULTAT IN
!   RESUOU  K8   NOM DE LA STRUCTURE DE DONNEES RESULTAT OUT
!   NBORDR  I    NOMBRE DE NUMEROS D'ORDRE
!   LISORD  K19  LISTE DE NUMEROS D'ORDRE
!   LISCHA  K19  NOM DE L'OBJET JEVEUX CONTENANT LES CHARGES
!   NCHARG  I    NOMBRE DE CHARGES
!   CHTYPE  K4   TYPE DES CHARGES
!   TYPESD  K16  TYPE DE LA STRUCTURE DE DONNEES RESULTAT
!
! IN/OUT :
!   LISOPT  K19  LISTE D'OPTIONS A METTRE SUR LA BASE GLOBALE
!                ATTENTION CETTE LISTE PEUT ETRE MODIFIEE PAR CALCOP
!                LES OPTIONS DECLENCHEES SONT SUPPRIMEES DE LA LISTE
!
! OUT :
!   CODRET  I    CODE RETOUR (0 SI OK, 1 SINON)
! ----------------------------------------------------------------------
! person_in_charge: nicolas.sellenet at edf.fr
    aster_logical :: exitim, exipou, optdem
!
    integer :: nopout, jlisop, iop, ibid, nbord2, lres, n0, n1, n2, n3, posopt
    integer :: nbtrou, minord, maxord, jlinst, iordr, nbordl, lcompo
    integer :: numord, iexcit, iret, npass, nbma, codre2, jliopg, nbopt
    integer :: jacalc, nordm1, jpara, nbchre, ioccur, icompo
!
    real(kind=8) :: r8b
!
    complex(kind=8) :: c16b
!
    character(len=1) :: basopt
    character(len=5) :: numopt
    character(len=8) :: modele, carael, k8b
    character(len=8) :: nomail, nobase, modeli
    character(len=11) :: nobaop
    character(len=16) :: optio2, typmcl(4), motcle(4),valk(2)
    character(len=19) :: excit, nonbor, compor
    character(len=24) :: chaout, ligrel, mateco, ligres
    character(len=24) :: noliop, lisins, mesmai, lacalc, suropt
!
    aster_logical :: ligmod
!
    call jemarq()
    codret = 1
    npass = 0
    nobase = '&&CALCOP'
!
!     ON CONSERVE CES OPTIONS POUR PERMETTRE LE CALCUL DANS STANLEY
    if ((option.eq.'ERTH_ELEM') .or. (option.eq.'ERTH_ELNO')) goto 999
!
    if ((option.eq.'ERME_ELEM') .or. (option.eq.'ERME_ELNO') .or. (option.eq.'QIRE_ELEM') .or.&
        (option.eq.'QIRE_ELNO')) goto 999
!
    if ((option.eq.'SIZ1_NOEU') .or. (option.eq.'SIZ2_NOEU') .or. (option.eq.'ERZ1_ELEM') .or.&
        (option.eq.'ERZ2_ELEM') .or. (option.eq.'QIZ1_ELEM') .or. (option.eq.'QIZ2_ELEM')) &
    goto 999
!
    if ((option.eq.'SING_ELEM') .or. (option.eq.'SING_ELNO')) goto 999
!
    call ccliop('OPTION', option, nobase, noliop, nopout)
    if (nopout .eq. 0) goto 999
!
!
    if (option(1:4).eq.'EPSI')then
! ------get COMPORTEMENT from RESULT
        call rsexch(' ', resuin, 'COMPORTEMENT', 1, compor, lcompo)
        if (lcompo .eq.0) then
! ------get DEFORMATION value from RESULT
            call jeveuo(compor//'.VALE','L',icompo)
! ------Coherence verification for large deformation
            if ((zk16(icompo+43-1)(1:8) .eq. 'GDEF_LOG').or. &
                (zk16(icompo+43-1)(1:10) .eq. 'SIMO_MIEHE').or. &
                (zk16(icompo+43-1)(1:14) .eq. 'GDEF_HYPO_ELAS'))then
                valk(1) = zk16(icompo+43-1)
                valk(2) = option(6:10)
                call utmess('A','CALCCHAMP_3',nk=2,valk=valk)
            endif
        endif
    endif
!
    nonbor = nobase//'.NB_ORDRE'
    lacalc = nobase//'.ACALCULER'
!
    call jeveuo(noliop, 'L', jlisop)
!
    jliopg = 0
    nbopt = 0
    if (lisopt .ne. ' ') then
        call jeveuo(lisopt, 'E', jliopg)
        call jelira(lisopt, 'LONMAX', nbopt)
    endif
!
    exitim = .false.
    call jenonu(jexnom(resuin//'           .NOVA', 'INST'), iret)
    if (iret .ne. 0) exitim = .true.
!
    call rsorac(resuin, 'TOUT_ORDRE', ibid, r8b, k8b,&
                c16b, r8b, k8b, tbid, 1,&
                nbtrou)
    if (nbtrou .lt. 0) nbtrou = -nbtrou
    call wkvect(nonbor, 'V V I', nbtrou, lres)
    call rsorac(resuin, 'TOUT_ORDRE', ibid, r8b, k8b,&
                c16b, r8b, k8b, zi(lres), nbtrou,&
                nbord2)
!     ON EN EXTRAIT LE MIN ET MAX DES NUMEROS D'ORDRE DE LA SD_RESUTLAT
    minord = zi(lres)
    maxord = zi(lres+nbord2-1)
!
    call rslesd(resuin, minord, modele, mateco(1:8), carael,&
                excit, iexcit)
    call rsadpa(resuin, 'L', 1, 'MODELE', minord,&
                0, sjv=jpara, styp=k8b)
    if (zk8(jpara) .ne. modele) then
        call utmess('A', 'CALCULEL_24')
    endif
!
    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=nomail)
!
    call dismoi('MODELISATION', modele, 'MODELE', repk=modeli)
!
    if ((modeli(1:6).eq.'C_PLAN').and.(option(1:4).eq.'EPSI')&
        .and. (lcompo .eq. 0)) then
        if (zk16(icompo+41-1)(1:4) .ne. 'ELAS' .and. &
            zk16(icompo+41-1)(1:16) .ne. '                ') then
            call utmess('A', 'ELEMENTS3_11')
        endif
    endif
!
    call ccvepo(modele, resuin, lischa, ncharg, typesd,&
                nbchre, ioccur, suropt, ligrel, exipou)
!
    if (option(6:9) .eq. 'NOEU') then
        nbma = 0
        n0 = getexm(' ','GROUP_MA')
        n1 = getexm(' ','MAILLE')
        mesmai = '&&OP0106.MES_MAILLES'
        if (n0+n1 .ne. 0) then
            call getvtx(' ', 'MAILLE', nbval=0, nbret=n2)
            call getvtx(' ', 'GROUP_MA', nbval=0, nbret=n3)
            if (n2+n3 .ne. 0) then
                motcle(1) = 'GROUP_MA'
                motcle(2) = 'MAILLE'
                typmcl(1) = 'GROUP_MA'
                typmcl(2) = 'MAILLE'
                call reliem(' ', nomail, 'NU_MAILLE', ' ', 1,&
                            2, motcle, typmcl, mesmai, nbma)
            endif
        endif
    endif
!
!     PREMIER PASSAGE POUR DETERMINER LES OPTIONS REELLEMENT A CALCULER
!     EN PRENANT EN COMPTE LA DEPENDANCE
!     PAR EXEMPLE SI SIGM_NOEU A BESOIN DE SIGM_ELNO QUI A BESOIN DE
!     SIGM_ELGA ET QUE SIGM_ELNO EST PRESENTE ALORS ON N'A PAS BESOIN
!     DE CALCULER SIGM_ELGA
    call wkvect(lacalc, 'V V I', nopout, jacalc)
!
!     PAR DEFAUT, ON DOIT TOUT CALCULER
!     ON COMMENCE PAR CALCULER LA LISTE DE NUMEROS D'ORDRE
    do iop = 1, nopout
        optio2 = zk24(jlisop+iop-1)(1:16)
!
        optdem = .false.
        if (option .eq. optio2) optdem = .true.
!
        call cclord(iop, nbordr, lisord, nobase, optdem,&
                    minord, maxord, resuin, resuou, lisins)
        zi(jacalc-1+iop) = 1
    end do
!
!     PUIS ON RETIRE LES OPTIONS DONT LE CALCUL N'EST PAS UTILE
    do iop = nopout-1, 1, -1
        optio2 = zk24(jlisop+iop-1)(1:16)
!
        call cclodr(iop, nbordr, lisord, nobase, minord,&
                    maxord, resuin, resuou, lacalc)
    end do
!
!
!     COMME ON PARCOURT LES OPTIONS DANS L'ORDRE INVERSE DES DEPENDANCES
!     ON SAIT QUE LES LISTES D'INSTANT SERONT CORRECTEMENT CREES
    nobaop = nobase//'.OP'
    do iop = 1, nopout
        if (zi(jacalc-1+iop) .eq. 0) goto 20
        optio2 = zk24(jlisop+iop-1)(1:16)
!
        optdem = .false.
        if (option .eq. optio2) optdem = .true.
!
!       RECUPERATION DE LA LISTE DE NUMERO D'ORDRE
        call codent(iop, 'D0', numopt)
        lisins = nobaop//numopt
        call jeveuo(lisins, 'L', jlinst)
        nbordl = zi(jlinst)
!
!       SI L'OPTION CALCULEE ICI EST DEMANDEE PAR
!       L'UTILISATUER, ON LA MET SUR LA BASE GLOBALE
        basopt = 'G'
        if (optio2 .ne. option) basopt = 'V'
!
        if (nbopt .ne. 0) then
            posopt = indk16(zk16(jliopg),optio2,1,nbopt)
            if (posopt .ne. 0) basopt = 'G'
!         CE BLOC A ETE AJOUTE POUR LE CAS OU UNE OPTION1 A DECLENCHE
!         LE CALCUL D'UNE OPTION2 MAIS QUE CETTE OPTION2 EST ENSUITE
!         REDEMANDEE DANS LE MEME CALC_CHAMP PAR L'UTILISATEUR
            if (.not.optdem .and. posopt .ne. 0) zk16(jliopg+posopt-1) = ' '
        endif
!
        if (optdem .and. (nbordl.eq.0)) then
            call utmess('A', 'CALCCHAMP_1', sk=optio2)
        endif
!
        codre2 = 0
        ligrel = ' '
        ligres = ' '
        do iordr = 1, nbordl
            ligmod = .false.
            numord = zi(jlinst+iordr+2)
!
!         NORDM1 NE SERT QUE POUR ENDO_ELGA
            nordm1 = numord-1
!
            if (optio2(6:9) .eq. 'NOEU') then
!
                call medom2(modele, mateco, carael, lischa, ncharg,&
                            chtype, resuin, numord, nbordr, 'V',&
                            npass, ligrel)
!
                if (ligres .ne. ligrel) ligmod = .true.
!
                call ccchno(optio2, numord, resuin, resuou, chaout(1:19),&
                            mesmai, nomail, modele, carael, basopt,&
                            ligrel, ligmod, codre2)
!
            else if (optio2(6:7).eq.'EL') then
!
                if (option .eq. 'SIRO_ELEM') then
                    call srmedo(modele, mateco, carael, lischa, ncharg,&
                                chtype, resuin, numord, nbordr, basopt,&
                                npass, ligrel)
                else
                    call medom2(modele, mateco, carael, lischa, ncharg,&
                                chtype, resuin, numord, nbordr, basopt,&
                                npass, ligrel)
                endif
!
                call ccchel(optio2, modele, resuin, resuou, numord,&
                            nordm1, mateco, carael, typesd, ligrel,&
                            exipou, exitim, lischa, nbchre, ioccur,&
                            suropt, basopt, chaout)
                if (chaout .eq. ' ') goto 20
!
            endif
!
            call exisd('CHAMP_GD', chaout, iret)
            if (basopt .eq. 'G') then
                if (iret .eq. 0) then
                    codret = 1
                    call utmess('A', 'CALCULEL2_89', sk=optio2)
                else
                    call rsnoch(resuou, optio2, numord)
                endif
            endif
!
            if (exipou) call jedetc('V', '&&MECHPO', 1)
            call detrsd('CHAM_ELEM_S', chaout)
!
            ligres = ligrel
        end do
!
 20     continue
    end do
!
    codret = 0
!
!     NETTOYAGE
    call jedetr(nonbor)
    call jedetr(lacalc)
    call ccnett(nobase, nopout)
    if (option(6:9) .eq. 'NOEU' .and. nbma .ne. 0) call jedetr(mesmai)
!
999 continue
!
    call jedema()
!
end subroutine
