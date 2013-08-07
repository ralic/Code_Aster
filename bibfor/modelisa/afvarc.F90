subroutine afvarc(chmat, nomail, nomode)
    implicit   none
#include "asterc/getfac.h"
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/r8nnem.h"
#include "asterc/r8vide.h"
#include "asterfort/afva01.h"
#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcncon.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/juveca.h"
#include "asterfort/mecact.h"
#include "asterfort/nocart.h"
#include "asterfort/reliem.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
    character(len=8) :: chmat, nomail, nomode
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
!     TRAITEMENT DE AFFE_MATERIAU / AFFE_VARC
! ----------------------------------------------------------------------
!
#include "jeveux.h"
    complex(kind=8) :: cbid
!
    integer :: n1, n2, nboccv, nbma, k, ncmp
    integer :: ifac, nbfac, nmxfac, nmxcmp, ibid, nbvarc, nbtou, jma
    integer :: iocc, jncmp1, jncmp2, jvalv1, jvalv2, kvarc, nbcvrc
    integer :: jcvnom, jcvvar, jcvcmp, jcvgd, itrou, nbm1, nbgm1, ier
    integer :: iarg, nref, nbdetr, nbgdut, nbgdmx, ico, jadetr
    integer :: jvale, jdesc
!
    character(len=8) :: k8b, typmcl(2), nomgd, kbid
    character(len=8) :: nomgd2, chamgd, evol, nocmp1, nocmp2, finst, evouch
    character(len=16) :: motcle(2), nomcha, prolga, proldr, k16a, k16b, k16c
    character(len=24) :: mesmai, cvnom, cvvar, cvgd, cvcmp, valk(3)
    parameter (nmxfac=20,nmxcmp=20)
    character(len=16) :: motfac(nmxfac), limfac(nmxfac), mofac
    character(len=19) :: cart1, cart2, carvid
    character(len=8) :: novarc, novar1, novar2, livarc(nmxfac), knumer
    character(len=8) :: nocvrc
    real(kind=8) :: vrcref(nmxcmp), rcmp(10), vref
    logical :: errgd, ldetr, lautr
! ----------------------------------------------------------------------
!
    call jemarq()
!
    mesmai = '&&AFVARC.MES_MAILLES'
    motcle(1) = 'GROUP_MA'
    motcle(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
    call getfac('AFFE_VARC', nboccv)
!
!
!     1. CALCUL DE NBVARC, NBCVRC, LIVARC ET LIMFAC
!        ALLOCATION DES 5 OBJETS : .CVRCNOM .CVRCVARC ...
!     -------------------------------------------------
    call getvtx(' ', 'LIST_NOM_VARC', 1, iarg, nmxfac,&
                livarc, n1)
    ASSERT(n1.gt.0)
    nbfac=n1
    do 1,ifac=1,nbfac
    motfac(ifac)='VARC_'//livarc(ifac)
    1 end do
!
    nbvarc = 0
    nbcvrc=0
    do 20,ifac = 1,nbfac
    mofac=motfac(ifac)
    ASSERT(mofac(1:5).eq.'VARC_')
    call getvtx(mofac, 'NOM_VARC', 1, iarg, 1,&
                novarc, n1)
    ASSERT(n1.eq.1)
    itrou=0
    do 21,iocc = 1,nboccv
    call getvtx('AFFE_VARC', 'NOM_VARC', iocc, iarg, 1,&
                novar1, n1)
    ASSERT(n1.eq.1)
    if (novar1 .eq. novarc) itrou=1
21  continue
    if (itrou .eq. 0) goto 20
    nbvarc = nbvarc + 1
    livarc(nbvarc) = novarc
    limfac(nbvarc) = mofac
    call getvtx(mofac, 'CMP_GD', 1, iarg, 0,&
                k8b, ncmp)
    ASSERT(ncmp.lt.0)
    ncmp=-ncmp
    nbcvrc=nbcvrc+ncmp
    20 end do
    if (nbvarc .eq. 0) goto 9999
!
!
    cvnom = chmat//'.CVRCNOM'
    cvvar = chmat//'.CVRCVARC'
    cvgd = chmat//'.CVRCGD'
    cvcmp = chmat//'.CVRCCMP'
    call wkvect(cvnom, 'G V K8', nbcvrc, jcvnom)
    call wkvect(cvvar, 'G V K8', nbcvrc, jcvvar)
    call wkvect(cvgd, 'G V K8', nbcvrc, jcvgd)
    call wkvect(cvcmp, 'G V K8', nbcvrc, jcvcmp)
!
!
!
!     2. ALLOCATION ET REMPLISSAGE DES CARTES
!        REMPLISSAGE DE .CVRCNOM .CVRCVARC ...
!     --------------------------------------------
    nbcvrc=0
    do 90,kvarc = 1,nbvarc
    mofac = limfac(kvarc)
!
    cart1 = chmat//'.'//livarc(kvarc)//'.1'
    cart2 = chmat//'.'//livarc(kvarc)//'.2'
    call alcart('G', cart1, nomail, 'NEUT_R')
    call alcart('G', cart2, nomail, 'NEUT_K16')
    call jeveuo(cart1//'.NCMP', 'E', jncmp1)
    call jeveuo(cart1//'.VALV', 'E', jvalv1)
    call jeveuo(cart2//'.NCMP', 'E', jncmp2)
    call jeveuo(cart2//'.VALV', 'E', jvalv2)
    nocmp1 = 'X'
    do 30,k = 1,nmxcmp
    call codent(k, 'G', nocmp1(2:8))
    zk8(jncmp1-1+k) = nocmp1
30  continue
    nocmp2 = 'Z'
    do 40,k = 1,7
    call codent(k, 'G', nocmp2(2:8))
    zk8(jncmp2-1+k) = nocmp2
40  continue
!
!
!       2.1 REPLISSAGE DE .CVRCNOM, .CVRCVARC, ...
!       ------------------------------------------------------------
    call getvtx(mofac, 'NOM_VARC', 1, iarg, 1,&
                novarc, n1)
    call getvtx(mofac, 'GRANDEUR', 1, iarg, 1,&
                nomgd, n1)
    call getvtx(mofac, 'CMP_GD', 1, iarg, nmxcmp,&
                zk8(jcvcmp+nbcvrc), ncmp)
    ASSERT(ncmp.ge.1)
    call getvtx(mofac, 'CMP_VARC', 1, iarg, nmxcmp,&
                zk8(jcvnom+nbcvrc), n1)
    ASSERT(n1.eq.ncmp)
    do 49,k = 1,ncmp
    zk8(jcvvar+nbcvrc-1+k) = novarc
    zk8(jcvgd +nbcvrc-1+k) = nomgd
49  continue
!
    do 80,iocc = 1,nboccv
    call getvtx('AFFE_VARC', 'NOM_VARC', iocc, iarg, 1,&
                novar2, n1)
    ASSERT(n1.eq.1)
    if (novar2 .ne. novarc) goto 80
!
!         2.2 CALCUL DE  VRCREF(:) :
!         ---------------------------
    call getvr8('AFFE_VARC', 'VALE_REF', iocc, iarg, nmxcmp,&
                vrcref, n1)
!         -- ON NE PEUT DONNER QU'UNE SEULE VALEUR (TEMP OU SECH) :
    nref=n1
    ASSERT(n1.eq.0 .or. n1.eq.1)
    if (n1 .eq. 1) then
        vref=vrcref(1)
    else
        vref=r8vide()
    endif
!         -- IL FAUT RECOPIER VREF POUR TEMP QUI A PLUSIEURS CMPS :
    do 60,k = 1,ncmp
    vrcref(k) = vref
60  continue
!
!
!         2.3 CALCUL DE EVOL,CHAMGD,NOMCHA ET VERIFICATIONS :
!         ------------------------------------------------------------
    evol = ' '
    chamgd = ' '
    nomcha = ' '
    errgd = .false.
!
    call getvid('AFFE_VARC', 'CHAM_GD', iocc, iarg, 1,&
                chamgd, n1)
    call getvid('AFFE_VARC', 'EVOL', iocc, iarg, 1,&
                evol, n2)
    ASSERT(n1+n2.le.1)
    if (n1 .eq. 1) then
        evouch='CHAMP'
    else if (n2.eq.1) then
        evouch='EVOL'
    else
        evouch='VIDE'
        if (novarc .ne. 'TEMP') call u2mesk('F', 'CALCULEL4_11', 1, novarc)
!           -- POUR LA THM, ON PEUT UTILISER VALE_REF SANS DONNER
!              CHAM_GD NI EVOL :
        ASSERT(nref.eq.1)
    endif
!
!
    if (evouch .eq. 'CHAMP') then
        call dismoi('F', 'NOM_GD', chamgd, 'CHAMP', ibid,&
                    nomgd2, ier)
        if (nomgd2 .ne. nomgd) errgd = .true.
!
    else if (evouch.eq.'EVOL') then
        call getvtx('AFFE_VARC', 'NOM_CHAM', iocc, iarg, 1,&
                    nomcha, n1)
!           -- NOM_CHAMP (VALEUR PAR DEFAUT) :
        if (n1 .eq. 0) then
            if (novarc .eq. 'SECH') then
                nomcha='TEMP'
            else if (novarc.eq.'HYDR') then
                nomcha='HYDR_ELNO'
            else if (novarc.eq.'HYDR') then
                nomcha='EPSA'
            else if (novarc.eq.'EPSA_ELNO') then
                nomcha='NEUT'
            else if (novarc.eq.'M_ACIER') then
                nomcha='META_ELNO'
            else if (novarc.eq.'M_ZIRC') then
                nomcha='META_ELNO'
            else if (novarc(1:4).eq.'NEUT') then
                nomcha='NEUT'
            else
                nomcha=novarc
            endif
        endif
        call getvtx('AFFE_VARC', 'PROL_GAUCHE', iocc, iarg, 1,&
                    prolga, n1)
        call getvtx('AFFE_VARC', 'PROL_DROITE', iocc, iarg, 1,&
                    proldr, n1)
        call getvid('AFFE_VARC', 'FONC_INST', iocc, iarg, 1,&
                    finst, n1)
        if (n1 .eq. 0) finst=' '
!           A FAIRE ??? VERIFIER QUE EVOL+NOMCHA => LA BONNE GRANDEUR
    endif
!
    if (errgd) then
        valk(1) = mofac
        valk(2) = nomgd
        valk(3) = nomgd2
        call u2mesk('A', 'MODELISA5_50', 3, valk)
    endif
!
!         2.4 ECRITURE DANS LES CARTES :
!         ------------------------------------------------------------
    zk16(jvalv2-1+1) = livarc(kvarc)
    if (evouch .eq. 'CHAMP') then
        zk16(jvalv2-1+2) = 'CHAMP'
        zk16(jvalv2-1+3) = chamgd
        zk16(jvalv2-1+4) = ' '
        zk16(jvalv2-1+5) = ' '
        zk16(jvalv2-1+6) = ' '
        zk16(jvalv2-1+7) = ' '
    else if (evouch.eq.'EVOL') then
        zk16(jvalv2-1+2) = 'EVOL'
        zk16(jvalv2-1+3) = evol
        zk16(jvalv2-1+4) = nomcha
        zk16(jvalv2-1+5) = prolga
        zk16(jvalv2-1+6) = proldr
        zk16(jvalv2-1+7) = finst
    else if (evouch.eq.'VIDE') then
!           -- ON AFFECTE UNE CARTE CONTENANT DES R8NNEM :
        call gcncon('_', knumer)
        carvid = knumer
        ASSERT(ncmp.le.10)
        do 31,k = 1,ncmp
        rcmp(k) = r8nnem()
31      continue
        call mecact('G', carvid, 'MAILLA', nomail, nomgd,&
                    ncmp, zk8(jcvcmp+nbcvrc), ibid, rcmp, cbid,&
                    kbid)
!
        zk16(jvalv2-1+2) = 'CHAMP'
        zk16(jvalv2-1+3) = carvid(1:16)
        zk16(jvalv2-1+4) = ' '
        zk16(jvalv2-1+5) = ' '
        zk16(jvalv2-1+6) = ' '
        zk16(jvalv2-1+7) = ' '
    endif
    do 70,k = 1,ncmp
    zr(jvalv1-1+k) = vrcref(k)
70  continue
!
!         TOUT='OUI' PAR DEFAUT :
    call getvtx('AFFE_VARC', 'TOUT', iocc, iarg, 1,&
                k8b, nbtou)
    call getvtx('AFFE_VARC', 'GROUP_MA', iocc, iarg, 0,&
                k8b, nbgm1)
    call getvtx('AFFE_VARC', 'MAILLE', iocc, iarg, 0,&
                k8b, nbm1)
    if (nbgm1+nbm1 .eq. 0) nbtou=1
!
    if (nbtou .ne. 0) then
        call nocart(cart1, 1, ' ', 'NOM', 0,&
                    ' ', 0, ' ', ncmp)
        call nocart(cart2, 1, ' ', 'NOM', 0,&
                    ' ', 0, ' ', 7)
    else
        call reliem(nomode, nomail, 'NU_MAILLE', 'AFFE_VARC', iocc,&
                    2, motcle, typmcl, mesmai, nbma)
        if (nbma .eq. 0) goto 80
        call jeveuo(mesmai, 'L', jma)
        call nocart(cart1, 3, k8b, 'NUM', nbma,&
                    ' ', zi(jma), ' ', ncmp)
        call nocart(cart2, 3, k8b, 'NUM', nbma,&
                    ' ', zi(jma), ' ', 7)
        call jedetr(mesmai)
    endif
!
!
80  continue
!
    nbcvrc=nbcvrc+ncmp
    90 end do
!
!
!     3. POUR NE PAS PENALISER LES CALCULS N'AYANT QUE LA COMPOSANTE
!        TEMP (POUR NOM_VARC='TEMP'), ON VA TENTER DE REDUIRE NBCVRC :
!     -----------------------------------------------------------------
    call wkvect('&&AFVARC.ADETR', 'V V I', nbcvrc, jadetr)
    nbdetr=0
    do 91, k=1,nbcvrc
    novarc=zk8(jcvvar-1+k)
    nocvrc=zk8(jcvnom-1+k)
    if (novarc .eq. 'TEMP') then
        if (nocvrc .ne. 'TEMP') then
            nbdetr=nbdetr+1
            zi(jadetr-1+k)=1
        endif
    endif
    91 end do
    if (nbdetr .eq. 0) goto 9999
!
!     3.1 PEUT-ON REELLEMENT SUPPRIMER CES CVRC ?
!     -------------------------------------------
    ldetr=.true.
    cart2 = chmat//'.TEMP    .2'
    call jeveuo(cart2//'.DESC', 'L', jdesc)
    call jeveuo(cart2//'.VALE', 'L', jvale)
    call jelira(jexnom('&CATA.GD.NOMCMP', 'NEUT_K16'), 'LONMAX', ncmp)
    nbgdmx=zi(jdesc-1+2)
    nbgdut=zi(jdesc-1+3)
    call jelira(chmat//'.TEMP    .2.VALE', 'LONMAX', n1)
    ASSERT(n1.eq.nbgdmx*ncmp)
!     3.2 ON PARCOURT LES SD STOCKEES DANS LA CARTE ET ON REGARDE S'IL
!         EXISTE D'AUTRES CMPS QUE TEMP ET LAGR :  LAUTR=.TRUE.
!     --------------------------------------------------------------
    do 93, k=1,nbgdut
    k16a=zk16(jvale-1+ncmp*(k-1)+1)
    ASSERT(k16a.eq.'TEMP')
    k16a=zk16(jvale-1+ncmp*(k-1)+2)
    k16b=zk16(jvale-1+ncmp*(k-1)+3)
    k16c=zk16(jvale-1+ncmp*(k-1)+4)
    call afva01(k16a, k16b, k16c, lautr)
    if (lautr) then
        ldetr=.false.
        goto 94
    endif
!
    93 end do
94  continue
!
!     3.3 ON SUPPRIME CE QUI NE SERT A RIEN :
!     ----------------------------------------
    if (ldetr) then
        ico=0
        do 92, k=1,nbcvrc
        if (zi(jadetr-1+k) .eq. 0) then
            ico=ico+1
            zk8(jcvnom-1+ico)=zk8(jcvnom-1+k)
            zk8(jcvvar-1+ico)=zk8(jcvvar-1+k)
            zk8(jcvgd-1+ico)=zk8(jcvgd-1+k)
            zk8(jcvcmp-1+ico)=zk8(jcvcmp-1+k)
        endif
92      continue
        ASSERT(ico.eq.nbcvrc-nbdetr)
        call juveca(cvnom, ico)
        call juveca(cvvar, ico)
        call juveca(cvgd, ico)
        call juveca(cvcmp, ico)
    endif
!
!
9999  continue
!
    call jedema()
end subroutine
