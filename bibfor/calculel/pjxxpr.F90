subroutine pjxxpr(resu1, resu2, moa1, moa2, corres,&
                  base, noca, method)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
    implicit   none
! ---------------------------------------------------------------------
! BUT :
!  PROJETER LES CHAMPS CONTENUS DANS LA SD RESU1
!  SUR LE MODELE (OU MAILLAGE) MOA2
!  ET CREER UNE NOUVELLE SD RESU2 DE MEME TYPE QUE RESU1
!
!  IN/JXIN  RESU1   K8  : NOM DE LA SD_RESULTAT A PROJETER
!  IN/JXOUT RESU2   K8  : NOM DE LA SD_RESULTAT RESULTAT
!  IN/JXIN  MOA1  K8  : NOM DU MODELE (OU MAILLAGE) ASSOCIE A RESU1
!  IN/JXIN  MOA2  K8  : NOM DU MODELE (OU MAILLAGE) ASSOCIE A RESU2
!  IN/JXIN  CORRES  K16 : NOM DE LA SD CORRESP_2_MAILLA
!
!  RESTRICTIONS :
!   1- ON TRAITE SYSTEMATIQUEMENT TOUS LES NUMEROS D'ORDRE
!   2- ON NE TRAITE CORRECTEMENT QUE LES EVOL_XXX (INST)
!  -----------------------------------------------------------------
!
! 0.1. ==> ARGUMENTS
!
#include "jeveux.h"
!
#include "asterfort/ajrefd.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/gnomsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/pjspma.h"
#include "asterfort/pjxxch.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rsorac.h"
#include "asterfort/rsutc4.h"
#include "asterfort/rsutnu.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/vpcrea.h"
#include "asterfort/wkvect.h"
    character(len=1) :: base
    character(len=8) :: resu1, resu2, moa1, moa2, noca
    character(len=16) :: corres, typres
    character(len=19) :: method
!
!
! 0.2. ==> COMMUNS
!
    character(len=24) :: noojb
!
!
! 0.3. ==> VARIABLES LOCALES
!
!
!
    integer :: ibid, ie, jxxk1, iret, jordr, nbordr, i, iordr
    integer :: iains1, iains2, nbsym, isym, ico, ind, nbmax
    parameter (nbmax=50)
    integer :: ipar, ipar1, ipar2
    logical :: acceno
    character(len=4) :: tychv
    character(len=8) :: kb, ma1, ma2, nume, prol0, k8b, typ1, typ2, crit, mo2
    character(len=16) :: nomsym(200), k16b, nomcmd
    character(len=19) :: ch1, ch2, prfchn, ligrel, prfch2
    character(len=19) :: noms2, kpar(nbmax)
    character(len=24) :: valk(3)
    character(len=1) :: typerr
    integer :: iexi, jpara, ier, inume
    real(kind=8) :: r8b, prec
    complex(kind=8) :: c16b
    integer :: iarg
!
!
! DEB -----------------------------------------------------------------
    call jemarq()
    k8b = ' '
    tychv = ' '
    call getres(k8b, k16b, nomcmd)
!
!
!     -- CALCUL DE MA1, MA2, LIGREL :
    call jeexin(moa1//'.MODELE    .REPE', iexi)
    if (iexi .gt. 0) then
        call dismoi('F', 'NOM_MAILLA', moa1, 'MODELE', ibid,&
                    ma1, ie)
    else
        ma1=moa1
    endif
!
    call jeexin(moa2//'.MODELE    .REPE', iexi)
    if (iexi .gt. 0) then
        call dismoi('F', 'NOM_MAILLA', moa2, 'MODELE', ibid,&
                    ma2, ie)
        mo2=moa2
        ligrel = mo2//'.MODELE'
!
    else
        ma2=moa2
        mo2=' '
        ligrel=' '
    endif
!
!
!
    if (nomcmd .eq. 'DEPL_INTERNE') then
!       ON NE TRAITE QUE LE CHAMP DEPL
        nbsym = 1
        nomsym(1) = 'DEPL'
        call rsorac(resu1, 'LONUTI', ibid, r8b, kb,&
                    c16b, r8b, kb, nbordr, 1,&
                    ibid)
        if (nbordr .eq. 0) then
            call u2mesk('F', 'CALCULEL4_62', 1, resu1)
        endif
!
        call wkvect('&&PJXXPR.NUME_ORDRE', 'V V I', nbordr, jordr)
!
        call rsorac(resu1, 'TOUT_ORDRE', ibid, r8b, kb,&
                    c16b, r8b, kb, zi(jordr), nbordr,&
                    ibid)
!
        prol0 = 'OUI'
!
    else
        call jeveuo(corres//'.PJXX_K1', 'L', jxxk1)
        if (method .ne. 'SOUS_POINT') then
            if (zk24(jxxk1-1+2) .ne. ma2) call u2mess('F', 'CALCULEL4_60')
        endif
!
!
        call rsutc4(resu1, ' ', 1, 200, nomsym,&
                    nbsym, acceno)
        ASSERT(nbsym.gt.0)
!
        call getvtx(' ', 'PROL_ZERO', 1, iarg, 1,&
                    prol0, ier)
        call getvtx(' ', 'TYPE_CHAM', 1, iarg, 1,&
                    tychv, ibid)
!
!
!     1- CREATION DE LA SD RESULTAT : RESU2
!     ------------------------------------
        call getvr8(' ', 'PRECISION', 0, iarg, 1,&
                    prec, ie)
        call getvtx(' ', 'CRITERE', 0, iarg, 1,&
                    crit, ie)
        call rsutnu(resu1, ' ', 0, '&&PJXXPR.NUME_ORDRE', nbordr,&
                    prec, crit, iret)
        if (iret .ne. 0) then
            call u2mesk('F', 'CALCULEL4_61', 1, resu1)
        endif
        if (nbordr .eq. 0) then
            call u2mesk('F', 'CALCULEL4_62', 1, resu1)
        endif
        call jeveuo('&&PJXXPR.NUME_ORDRE', 'L', jordr)
    endif
!
    noms2 = resu2
    call jeexin(noms2//'.DESC', iret)
    if (iret .eq. 0) then
        call gettco(resu1, typres)
        call rscrsd(base, resu2, typres, nbordr)
    endif
!
!     DANS LE CAS DES CONCEPTS TYPE MODE_MECA ON TESTE LA PRESENCE
!     DES MATRICES AFIN DE RECUPERER LA NUMEROTATION SOUS-JACENTE
    prfch2 = '12345678.00000.NUME'
    if (nomcmd .eq. 'DEPL_INTERNE') then
!
    else
!       ON ESSAYE DE RECUPERER LA NUMEROTATION IMPOSEE
        call getvid(' ', 'NUME_DDL', 1, iarg, 1,&
                    nume, ier)
        if (ier .ne. 0) then
            prfch2 = nume(1:8)//'      .NUME'
        endif
    endif
!
!
!
!     2- ON CALCULE LES CHAMPS RESULTATS :
!     ------------------------------------
    ico = 0
    do 30,isym = 1,nbsym
!
    if (prfch2 .ne. '12345678.00000.NUME') then
!         ON PREND LA NUMEROTATION IMPOSEE
        prfchn = prfch2
!
    else
!         ON DEFINIT UNE NUMEROTATION 'BIDON"
        noojb = '12345678.00000.NUME.PRNO'
        call gnomsd(' ', noojb, 10, 14)
        prfchn = noojb(1:19)
    endif
!
    do 20,i = 1,nbordr
    iordr = zi(jordr+i-1)
    call rsexch(' ', resu1, nomsym(isym), iordr, ch1,&
                iret)
    if (iret .gt. 0) goto 20
!
!       -- PROJECTION DU CHAMP SI POSSIBLE :
    call rsexch(' ', resu2, nomsym(isym), iordr, ch2,&
                iret)
    if (method .eq. 'SOUS_POINT') then
        call pjspma(corres, ch1, ch2, prol0, ligrel,&
                    noca, base, iret)
    else
        call pjxxch(corres, ch1, ch2, tychv, prfchn,&
                    prol0, ligrel, base, iret)
    endif
    ASSERT(iret.eq.0.or.iret.eq.1.or.iret.eq.10)
!         -- ELGA ET CART : ON NE FAIT RIEN :
    if (iret .eq. 10) goto 20
!
    if (iret .gt. 0) then
        if (acceno) then
!             -- L'UTILISATEUR A DEMANDE EXPLICITEMENT LA PROJECTION :
            typerr = 'F'
!
        else
!             -- L'UTILISATEUR N'A PAS DEMANDE EXPLICITEMENT
!                LA PROJECTION, ON SE CONTENTE D'UNE ALARME  :
            typerr = 'A'
        endif
        valk(1) = nomsym(isym)
        valk(2) = resu1
        valk(3) = resu2
        call u2mesg(typerr, 'CALCULEL4_63', 3, valk, 1,&
                    iordr, 0, 0.d0)
        goto 20
!
    endif
    call rsnoch(resu2, nomsym(isym), iordr)
!
!       -- ATTRIBUTION DES ATTRIBUTS DU CONCEPT RESULTAT
!         EXTRACTION DES PARAMETRES MODAUX
    if ((typres(1:9).eq.'MODE_MECA') .or. (typres(1:4) .eq.'BASE')) then
        call vpcrea(0, resu2, ' ', ' ', ' ',&
                    prfch2(1:8), ier)
        call rsadpa(resu1, 'L', 1, 'FREQ', iordr,&
                    0, iains1, kb)
        call rsadpa(resu2, 'E', 1, 'FREQ', iordr,&
                    0, iains2, kb)
        zr(iains2) = zr(iains1)
!
!           RECOPIE DE NUME_MODE S'IL EXISTE:
        call jenonu(jexnom(resu1//'           .NOVA', 'NUME_MODE'), inume)
        if (inume .ne. 0) then
            call rsadpa(resu1, 'L', 1, 'NUME_MODE', iordr,&
                        0, iains1, kb)
            call rsadpa(resu2, 'E', 1, 'NUME_MODE', iordr,&
                        0, iains2, kb)
            zi(iains2) = zi(iains1)
        endif
!
    else if (typres(1:9).eq.'MODE_STAT') then
        call vpcrea(0, resu2, ' ', ' ', ' ',&
                    prfch2(1:8), ier)
        call rsadpa(resu1, 'L', 1, 'NOEUD_CMP', iordr,&
                    0, iains1, kb)
        call rsadpa(resu2, 'E', 1, 'NOEUD_CMP', iordr,&
                    0, iains2, kb)
        zk16(iains2) = zk16(iains1)
!
    else if (typres.eq.'DYNA_HARMO') then
        call rsadpa(resu1, 'L', 1, 'FREQ', iordr,&
                    0, iains1, kb)
        call rsadpa(resu2, 'E', 1, 'FREQ', iordr,&
                    0, iains2, kb)
        zr(iains2) = zr(iains1)
!
        elseif ((typres(1:4).eq.'EVOL') .or. (typres(1:4)&
            .eq.'DYNA')) then
        call rsadpa(resu1, 'L', 1, 'INST', iordr,&
                    0, iains1, kb)
        call rsadpa(resu2, 'E', 1, 'INST', iordr,&
                    0, iains2, kb)
        zr(iains2) = zr(iains1)
!
    else
!            ON NE FAIT RIEN
    endif
!
    if (nomcmd .eq. 'DEPL_INTERNE') then
        ipar = 0
!
    else
!           REMPLIT D AUTRES PARAMETRES SI DEMANDE PAR UTILISATEUR
        call getvtx(' ', 'NOM_PARA', 1, iarg, nbmax,&
                    kpar, ipar)
    endif
!
!
    do 10 ind = 1, ipar
        call rsadpa(resu1, 'L', 1, kpar(ind), iordr,&
                    1, ipar1, typ1)
        call rsadpa(resu2, 'E', 1, kpar(ind), iordr,&
                    0, ipar2, typ2)
        if (typ1(1:1) .eq. 'I') then
            zi(ipar2) = zi(ipar1)
!
        else if (typ1(1:1).eq.'R') then
            zr(ipar2) = zr(ipar1)
!
        else if (typ1(1:2).eq.'K8') then
            zk8(ipar2) = zk8(ipar1)
!
        else if (typ1(1:3).eq.'K16') then
            zk16(ipar2) = zk16(ipar1)
!
        else if (typ1(1:3).eq.'K32') then
            zk32(ipar2) = zk32(ipar1)
!
        else
!               ON NE FAIT RIEN
        endif
10  continue
    ico = ico + 1
!
20  continue
    30 end do
!
    if (ico .eq. 0) call u2mess('F', 'CALCULEL4_64')
    call jedetr('&&PJXXPR.NUME_ORDRE')
!
!
    if (mo2 .ne. ' ') then
        call jeveuo(resu2//'           .ORDR', 'L', jordr)
        call jelira(resu2//'           .ORDR', 'LONUTI', nbordr, k8b)
        do 11 i = 1, nbordr
            call rsadpa(resu2, 'E', 1, 'MODELE', zi(jordr-1+i),&
                        0, jpara, k8b)
            zk8(jpara)=mo2
11      continue
    endif
!
!     -- CREATION DE L'OBJET .REFD SI NECESSAIRE:
    call ajrefd(resu1, resu2, 'ZERO')
!
!
    call jedema()
end subroutine
