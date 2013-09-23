subroutine remngl(nomres, typsd, modcyc, profno, indirf,&
                  mailsk)
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
    implicit none
!-----------------------------------------------------------------------
!
!  BUT:  < RESTITUTION MAC-NEAL GLOBALE >
!
!  RESTITUER LES RESULTATS ISSUS D'UN CALCUL CYCLIQUE AVEC DES
!  INTERFACES DE TYPE MAC-NEAL
!     => RESULTAT COMPOSE DE TYPE MODE_MECA DEJA ALLOUE PAR LA
!        ROUTINE APPELLANTE
!
!  DONNEES DU PROFCHNO DEJA CONSTITUE ET DE LA TABLE INDIRECTION
!  DES NUMEROS EQUATIONS CORRESPONDANTES (COLLECTION NUMEROTEE
!  POINTEE PAR LES NUMEROS DE SECTEUR)
!-----------------------------------------------------------------------
!
! NOMRES  /I/: NOM UT DU CONCEPT RESULTAT A REMPLIR
! MODCYC  /I/: NOM UT DU RESULTAT ISSU DU CALCUL CYCLIQUE
! PROFNO  /I/: NOM K19 DU PROFIL CHAMNO DEJA CONSTITUE
! INDIRF  /I/: NOM K24 DE LA FAMILLE DES INDIRECTIONS
! MAILSK  /I/: NOM K8 DU MAILLAGE SKELETTE
! TYPSD   /I/: NOM DU TYPE DE STRUCTURE DE DONNEES RESULTAT
!
!
!
!
!
#include "jeveux.h"
#include "asterc/r8depi.h"
#include "asterfort/bmnodi.h"
#include "asterfort/ctetgd.h"
#include "asterfort/dismoi.h"
#include "asterfort/flexib.h"
#include "asterfort/genecy.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mtdscr.h"
#include "asterfort/mtexis.h"
#include "asterfort/ordr8.h"
#include "asterfort/remnbn.h"
#include "asterfort/rotchm.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcrea.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: nomres, basmod, modcyc, intf, kbid, mailsk, k8b
    character(len=16) :: depl, typsd, typsup(1)
    character(len=19) :: chamva, numddl, profno, mass
    character(len=24) :: flexdr, flexga, flexax, tetgd, tetax
    character(len=24) :: valk(2)
    character(len=24) :: indirf, crefe(2)
    complex(kind=8) :: dephc, dephco
    real(kind=8) :: para(2), depi, fact, genek, beta
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iad, ibid(1), icomp, iddi, idi
    integer :: idiam, idicou, ieqf, ieqi, ier, ii, inum
    integer :: iorc, iormo, j, jj, k, ldfreq, ldkge
    integer :: ldmge, ldom2, ldomo, ldotm, ldtyd, llcham, lldesc
    integer :: lldiam, llfreq, llinsk, llmoc, llnsec, llnumi, llref
    integer :: lmass, ltetax, ltetgd, ltflax, ltfldr, ltflga, ltinds
    integer :: ltorf, ltorto, lttsc, ltveco, ltvere, ltvezt, mdiapa
    integer :: nbcmp, nbdax, nbddg, nbddr, nbdia, nbmoc, nbmod
    integer :: nbnot, nborc, nbsec, nddcou, neq, neqsec, numa
    integer :: numd, numg
!-----------------------------------------------------------------------
    data depl   /'DEPL            '/
    data typsup /'MODE_MECA       '/
!
!-----------------------------------------------------------------------
!
    call jemarq()
!
    depi = r8depi()
!
!-----VERIFICATION DU TYPE DE STRUCTURE RESULTAT------------------------
!
    if (typsd .ne. typsup(1)) then
        valk (1) = typsd
        valk (2) = typsup(1)
        call utmess('F', 'ALGORITH14_4', nk=2, valk=valk)
    endif
!
!-----REMPLISSAGE DU CREFE POUR CREATION CHAMNO-------------------------
!
    crefe(1) = mailsk
    crefe(2) = profno
!
!-----RECUPERATION DE LA BASE MODALE AMONT------------------------------
!
    call jeveuo(modcyc//'.CYCL_REFE', 'L', llref)
    basmod = zk24(llref+2)
!
!-----RECUPERATION DU .DESC---------------------------------------------
!
    call jeveuo(modcyc//'.CYCL_DESC', 'L', lldesc)
    nbmod = zi(lldesc)
    nbddr = zi(lldesc+1)
    nbdax = zi(lldesc+2)
!
!-----RECUPERATION DU NOMBRE DE SECTEURS--------------------------------
!
    call jeveuo(modcyc//'.CYCL_NBSC', 'L', llnsec)
    nbsec = zi(llnsec)
    mdiapa = int(nbsec/2)*int(1-nbsec+(2*int(nbsec/2)))
!
!-----RECUPERATION DES NOMBRES DE DIAMETRES NODAUX----------------------
!
    call jeveuo(modcyc//'.CYCL_DIAM', 'L', lldiam)
    call jelira(modcyc//'.CYCL_DIAM', 'LONMAX', nbdia)
    nbdia = nbdia / 2
!
!-----RECUPERATION DU NOMBRE DE DDL PHYSIQUES DU SECTEUR----------------
!
    call dismoi('F', 'REF_INTD_PREM', basmod, 'RESU_DYNA', ibid(1),&
                intf, ier)
    call dismoi('F', 'NUME_DDL', basmod, 'RESU_DYNA', ibid(1),&
                numddl, ier)
    call dismoi('F', 'NB_EQUA', numddl, 'NUME_DDL', neqsec,&
                k8b, ier)
    call dismoi('F', 'NB_CMP_MAX', intf, 'INTERF_DYNA', nbcmp,&
                k8b, ier)
!
!-----RECUPERATION DU NOMBRE DE DDL PHYSIQUES GLOBAUX-------------------
!
    call jelira(profno//'.DEEQ', 'LONMAX', neq)
    neq = neq / 2
!
!-----RECUPERATION DES FREQUENCES---------------------------------------
!
    call jeveuo(modcyc//'.CYCL_FREQ', 'L', llfreq)
!
!-----RECUPERATION MATRICE DE MASSE-------------------------------------
!
    call dismoi('F', 'REF_MASS_PREM', basmod, 'RESU_DYNA', ibid(1),&
                mass, ier)
    call mtexis(mass, ier)
    if (ier .eq. 0) then
        valk (1) = mass(1:8)
        call utmess('F', 'ALGORITH12_39', sk=valk(1))
    endif
    call mtdscr(mass)
    call jeveuo(mass(1:19)//'.&INT', 'E', lmass)
!
!-----ALLOCATION DES VECTEURS DE TRAVAIL--------------------------------
!
    call wkvect('&&REMNGL.VEC.TRAVC', 'V V C', neqsec, ltvezt)
    call wkvect('&&REMNGL.VEC.COMP', 'V V C', neqsec, ltveco)
    call wkvect('&&REMNGL.VEC.REEL', 'V V R', neqsec, ltvere)
!
!-----RECUPERATION DES NUMEROS D'INTERFACE------------------------------
!
    call jeveuo(modcyc//'.CYCL_NUIN', 'L', llnumi)
    numd = zi(llnumi)
    numg = zi(llnumi+1)
    numa = zi(llnumi+2)
!
!-----CALCUL DES MATRICES DE FLEXIBILITE RESIDUELLE---------------------
!
    kbid=' '
    call bmnodi(basmod, kbid, '         ', numd, 0,&
                ibid(1), nbddr)
    flexdr='&&REMNGL.FLEX.DROITE'
    call wkvect(flexdr, 'V V R', nbddr*neqsec, ltfldr)
    ibid(1) = 0
    call flexib(basmod, nbmod, zr(ltfldr), neqsec, nbddr,&
                ibid(1), numd)
!
    flexga='&&REMNGL.FLEX.GAUCHE'
    call wkvect(flexga, 'V V R', nbddr*neqsec, ltflga)
    call flexib(basmod, nbmod, zr(ltflga), neqsec, nbddr,&
                ibid(1), numg)
!
    if (numa .gt. 0) then
        flexax='&&REMNGL.FLEX.AXE'
        kbid=' '
        call bmnodi(basmod, kbid, '         ', numa, 0,&
                    ibid(1), nbdax)
        call wkvect(flexax, 'V V R', nbdax*neqsec, ltflax)
        call flexib(basmod, nbmod, zr(ltflax), neqsec, nbddr,&
                    ibid(1), numa)
    endif
!
!-----CALCUL DES MATRICES DE CHANGEMENT DE BASE TETA--------------------
!
    tetgd='&&REMNGL.TETGD'
    call wkvect(tetgd, 'V V R', nbddr*nbddr, ltetgd)
    call ctetgd(basmod, numd, numg, nbsec, zr(ltetgd),&
                nbddr)
!
    if (numa .gt. 0) then
        tetax='&&REMNGL.TETAX'
        call wkvect(tetax, 'V V R', nbdax*nbdax, ltetax)
        call ctetgd(basmod, numa, numg, nbsec, zr(ltetax),&
                    nbdax)
    endif
!
!-----CLASSEMENT DES MODES PROPRES--------------------------------------
!
    nbmoc = 0
    do 5 iddi = 1, nbdia
        nbmoc = nbmoc + zi(lldiam+nbdia+iddi-1)
 5  continue
    call wkvect('&&REMNGL.ORDRE.FREQ', 'V V I', nbmoc, ltorf)
    call wkvect('&&REMNGL.ORDRE.TMPO', 'V V I', nbmoc, ltorto)
    call ordr8(zr(llfreq), nbmoc, zi(ltorto))
    nborc = 0
    do 6 ii = 1, nbmoc
        iormo = zi(ltorto+ii-1)
        icomp = 0
        idicou = 0
        do 7 jj = 1, nbdia
            icomp = icomp + zi(lldiam+nbdia+jj-1)
            if (icomp .ge. iormo .and. idicou .eq. 0) idicou = jj
 7      continue
        nborc = nborc + 1
        zi(ltorf+iormo-1) = nborc
        idiam = zi(lldiam+idicou-1)
        if (idiam .ne. 0 .and. idiam .ne. mdiapa) nborc = nborc + 1
 6  continue
    call jedetr('&&REMNGL.ORDRE.TMPO')
!
!-----RECUPERATION DES MODES COMPLEXES----------------------------------
!
    call jeveuo(modcyc//'.CYCL_CMODE', 'L', llmoc)
!
!-----CALCUL DU TETA DE CHAQUE SECTEUR----------------------------------
!
    call wkvect('&&REMNGL.TETA_SECTEUR', 'V V R', nbsec, lttsc)
    do 8 i = 1, nbsec
        zr(lttsc+i-1) = depi*(i-1) / nbsec
 8  continue
!
!-----RECUPERATION DE L'INDIRECTION SQUELETTE---------------------------
!
    call jeveuo(mailsk//'.INV.SKELETON', 'L', llinsk)
    call dismoi('F', 'NB_NO_MAILLA', mailsk, 'MAILLAGE', nbnot,&
                k8b, ier)
!
!***********************************************************************
!     RESTITUTION
!***********************************************************************
!
    nbddg = nbmod + nbddr + nbdax
    icomp = 0
    inum = 0
!
!  BOUCLE SUR LES DIAMETRES NODAUX
!
    do 10 idi = 1, nbdia
!
!  CALCUL DU DEPHASAGE INTER-SECTEUR
!
        idiam = zi(lldiam+idi-1)
        beta = (depi/nbsec)*idiam
        dephc = dcmplx(cos(beta),sin(beta))
!
!  BOUCLE SUR LES MODES PROPRES DU DIAMETRE COURANT
!
        do 15 i = 1, zi(lldiam+nbdia+idi-1)
            icomp = icomp + 1
            inum = inum +1
            iorc = zi(ltorf+icomp-1)
            iad = llmoc + ((icomp-1)*nbddg)
!
!***********************************************************************
!      RESTITUTION DU MODE PROPRE REEL (PARTIE RELLE)
!***********************************************************************
!
            call rsexch(' ', nomres, depl, inum, chamva,&
                        ier)
            call vtcrea(chamva, crefe, 'G', 'R', neq)
            call rsnoch(nomres, depl, inum)
            call jeveuo(chamva//'.VALE', 'E', llcham)
!
!  CALCUL MODE COMPLEXE SECTEUR DE BASE
!
            call remnbn(basmod, nbmod, nbddr, nbdax, flexdr,&
                        flexga, flexax, tetgd, tetax, zc(iad),&
                        zc(ltveco), neqsec, beta)
!
!  CALCUL MASSE GENERALISEE
!
            call genecy(zc(ltveco), zc(ltveco), neqsec, lmass, para,&
                        nbsec, beta, beta, zc(ltvezt))
!
!  COMMUN POUR MODE_MECA ET BASE_MODALE
!
            call rsadpa(nomres, 'E', 1, 'FREQ', inum,&
                        0, sjv=ldfreq, styp=k8b)
            call rsadpa(nomres, 'E', 1, 'RIGI_GENE', inum,&
                        0, sjv=ldkge, styp=k8b)
            call rsadpa(nomres, 'E', 1, 'MASS_GENE', inum,&
                        0, sjv=ldmge, styp=k8b)
            call rsadpa(nomres, 'E', 1, 'OMEGA2', inum,&
                        0, sjv=ldom2, styp=k8b)
            call rsadpa(nomres, 'E', 1, 'NUME_MODE', inum,&
                        0, sjv=ldomo, styp=k8b)
            call rsadpa(nomres, 'E', 1, 'TYPE_MODE', inum,&
                        0, sjv=ldotm, styp=k8b)
            fact = 1.d0 / (para(1)**0.5d0)
            genek = (zr(llfreq+icomp-1)*depi)**2
            zr(ldfreq) = zr(llfreq+icomp-1)
            zr(ldkge) = genek
            zr(ldmge) = 1.d0
            zr(ldom2) = genek
            zi(ldomo) = iorc
            zk16(ldotm) = 'MODE_DYN'
!
!  SPECIFIQUE A BASE_MODALE
!
            call rsadpa(nomres, 'E', 1, 'TYPE_DEFO', inum,&
                        0, sjv=ldtyd, styp=k8b)
            zk16(ldtyd) = 'PROPRE          '
!
!  BOUCLE SUR LES SECTEURS
!
            do 20 k = 1, nbsec
                if (k .gt. 1) then
                    dephco = dephc
                else
                    dephco = dcmplx(1.d0,0.d0)
                endif
                do 30 j = 1, neqsec
                    zc(ltveco+j-1) = zc(ltveco+j-1)*dephco
                    zr(ltvere+j-1) = dble(zc(ltveco+j-1))
30              continue
                call jeveuo(jexnum(indirf, k), 'L', ltinds)
                call jelira(jexnum(indirf, k), 'LONMAX', nddcou)
                nddcou = nddcou/2
                do 40 j = 1, nddcou
                    ieqi = zi(ltinds+(j-1)*2)
                    ieqf = zi(ltinds+(j-1)*2+1)
                    zr(llcham+ieqf-1) = zr(ltvere+ieqi-1)*fact
40              continue
20          continue
!
!  PRISE EN COMPTE ROTATION SUR CHAQUE SECTEUR
!
            call rotchm(profno, zr(llcham), zr(lttsc), nbsec, zi(llinsk),&
                        nbnot, nbcmp, 3)
!
!***********************************************************************
!      EVENTUELLE RESTITUTION DE LA PARTIE IMAGINAIRE
!***********************************************************************
!
            if (idiam .ne. 0 .and. idiam .ne. mdiapa) then
                iorc = iorc + 1
                inum = inum + 1
!
!  CALCUL MODE COMPLEXE SECTEUR DE BASE
!
                call remnbn(basmod, nbmod, nbddr, nbdax, flexdr,&
                            flexga, flexax, tetgd, tetax, zc(iad),&
                            zc(ltveco), neqsec, beta)
!
!  CALCUL MASSE GENERALISEE
!
                call genecy(zc(ltveco), zc(ltveco), neqsec, lmass, para,&
                            nbsec, beta, beta, zc(ltvezt))
!
                call rsexch(' ', nomres, depl, inum, chamva,&
                            ier)
                call vtcrea(chamva, crefe, 'G', 'R', neq)
                call rsnoch(nomres, depl, inum)
                call jeveuo(chamva//'.VALE', 'E', llcham)
!
!  COMMUN POUR MODE_MECA ET BASE_MODALE
!
                call rsadpa(nomres, 'E', 1, 'FREQ', inum,&
                            0, sjv=ldfreq, styp=k8b)
                call rsadpa(nomres, 'E', 1, 'RIGI_GENE', inum,&
                            0, sjv=ldkge, styp=k8b)
                call rsadpa(nomres, 'E', 1, 'MASS_GENE', inum,&
                            0, sjv=ldmge, styp=k8b)
                call rsadpa(nomres, 'E', 1, 'OMEGA2', inum,&
                            0, sjv=ldom2, styp=k8b)
                call rsadpa(nomres, 'E', 1, 'NUME_MODE', inum,&
                            0, sjv=ldomo, styp=k8b)
                call rsadpa(nomres, 'E', 1, 'TYPE_MODE', inum,&
                            0, sjv=ldotm, styp=k8b)
                fact = 1.d0 / (para(2)**0.5d0)
                genek = (zr(llfreq+icomp-1)*depi)**2
                zr(ldfreq) = zr(llfreq+icomp-1)
                zr(ldkge) = genek
                zr(ldmge) = 1.d0
                zr(ldom2) = genek
                zi(ldomo) = iorc
                zk16(ldotm) = 'MODE_DYN'
!
!  SPECIFIQUE A BASE_MODALE
!
                call rsadpa(nomres, 'E', 1, 'TYPE_DEFO', inum,&
                            0, sjv=ldtyd, styp=k8b)
                zk16(ldtyd) = 'PROPRE          '
!
!  BOUCLE SUR LES SECTEURS
!
                do 50 k = 1, nbsec
                    if (k .gt. 1) then
                        dephco = dephc
                    else
                        dephco = dcmplx(1.d0,0.d0)
                    endif
                    do 60 j = 1, neqsec
                        zc(ltveco+j-1) = zc(ltveco+j-1)*dephco
                        zr(ltvere+j-1) = dimag(zc(ltveco+j-1))
60                  continue
                    call jeveuo(jexnum(indirf, k), 'L', ltinds)
                    call jelira(jexnum(indirf, k), 'LONMAX', nddcou)
                    nddcou = nddcou / 2
                    do 70 j = 1, nddcou
                        ieqi = zi(ltinds+(j-1)*2)
                        ieqf = zi(ltinds+(j-1)*2+1)
                        zr(llcham+ieqf-1) = zr(ltvere+ieqi-1)*fact
70                  continue
50              continue
!
!  PRISE EN COMPTE ROTATION SUR CHAQUE SECTEUR
!
                call rotchm(profno, zr(llcham), zr(lttsc), nbsec, zi( llinsk),&
                            nbnot, nbcmp, 3)
!
            endif
!
15      continue
10  continue
!
    call jedetr('&&REMNGL.VEC.TRAVC')
    call jedetr('&&REMNGL.VEC.COMP')
    call jedetr('&&REMNGL.VEC.REEL')
    call jedetr('&&REMNGL.FLEX.DROITE')
    call jedetr('&&REMNGL.FLEX.GAUCHE')
    call jedetr('&&REMNGL.TETGD')
    call jedetr('&&REMNGL.TETA_SECTEUR')
    call jedetr('&&REMNGL.ORDRE.FREQ')
    if (numa .gt. 0) then
        call jedetr('&&REMNGL.FLEX.AXE')
        call jedetr('&&REMNGL.TETAX')
    endif
!
    call jedema()
end subroutine
