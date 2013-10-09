subroutine remnec(nomres, typesd, basmod, modcyc, numsec)
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
!  BUT:      < RESTITUTI0N MAC-NEAL ECLATEE >
!
!   RESTITUER LES RESULTATS ISSUS D'UN CALCUL CYCLIQUE
!          AVEC DES INTERFACES DE TYPE MAC-NEAL
!     => RESULTAT COMPOSE DE TYPE MODE_MECA ALLOUE PAR LA
!   ROUTINE
!-----------------------------------------------------------------------
!
! NOMRES  /I/: NOM UT DU CONCEPT RESULTAT A REMPLIR
! TYPESD  /I/: NOM K16 DU TYPE DE LA STRUCTURE DE DONNEE
! BASMOD  /I/: NOM UT DE LA BASE MODALE EN AMONT DU CALCUL CYCLIQUE
! MODCYC  /I/: NOM UT DU RESULTAT ISSU DU CALCUL CYCLIQUE
! NUMSEC  /I/: NUMERO DU SECTEUR  SUR LEQUEL RESTITUER
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
#include "asterfort/mtdscr.h"
#include "asterfort/mtexis.h"
#include "asterfort/ordr8.h"
#include "asterfort/remnbn.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcrem.h"
#include "asterfort/wkvect.h"
#include "blas/daxpy.h"
!
    character(len=8) :: nomres, basmod, modcyc, kbid, k8b
    character(len=16) :: depl, typesd, typsup(1)
    character(len=19) :: chamva, numddl, matrix, mass
    character(len=24) :: flexdr, flexga, flexax, tetgd, tetax
    character(len=24) :: valk(2)
    complex(kind=8) :: dephc
    real(kind=8) :: para(2), depi, fact, genek, beta
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iad, ibid(1), icomp, iddi, idi
    integer :: idia, idiam, idicou, ier, ii, inum, iorc
    integer :: iormo, j, jj, ldfre, ldkge, ldmge, ldom2
    integer :: ldomo, ldotm, ldtyd, llcham, lldesc, lldiam, llfreq
    integer :: llmoc, llnsec, llnumi, lmass, ltetax, ltetgd
    integer :: ltflax, ltfldr, ltflga, ltorf, ltorto, ltveco, ltvere
    integer :: ltvezt, mdiapa, nbdax, nbddg, nbddr, nbdia, nbmoc
    integer :: nbmod, nbmor, nborc, nbsec, nbtmp, neq, numa
    integer :: numd, numg, numsec
    real(kind=8) :: aaa, bbb, betsec
!-----------------------------------------------------------------------
    data depl   /'DEPL            '/
    data typsup /'MODE_MECA       '/
!-----------------------------------------------------------------------
!
    call jemarq()
!
    depi = r8depi()
!
!----------------VERIFICATION DU TYPE DE STRUCTURE RESULTAT-------------
!
    if (typesd .ne. typsup(1)) then
        valk (1) = typesd
        valk (2) = typsup(1)
        call utmess('F', 'ALGORITH14_4', nk=2, valk=valk)
    endif
!
!--------------------------RECUPERATION DU .DESC------------------------
!
    call jeveuo(modcyc//'.CYCL_DESC', 'L', lldesc)
    nbmod = zi(lldesc)
    nbddr = zi(lldesc+1)
    nbdax = zi(lldesc+2)
!
!-------------------RECUPERATION DU NOMBRE DE SECTEUR-------------------
!
    call jeveuo(modcyc//'.CYCL_NBSC', 'L', llnsec)
    nbsec = zi(llnsec)
    mdiapa = int(nbsec/2)*(1-nbsec+(2*int(nbsec/2)))
!
!------------------RECUPERATION DES NOMBRES DE DIAMETRES MODAUX---------
!
    call jeveuo(modcyc//'.CYCL_DIAM', 'L', lldiam)
    call jelira(modcyc//'.CYCL_DIAM', 'LONMAX', nbdia)
    nbdia = nbdia / 2
!
!-----------------RECUPERATION DU NOMBRE DE DDL PHYSIQUES---------------
!
    call dismoi('NUME_DDL', basmod, 'RESU_DYNA', repk=numddl)
    call dismoi('REF_RIGI_PREM', basmod, 'RESU_DYNA', repk=matrix)
    call dismoi('NB_EQUA', numddl, 'NUME_DDL', repi=neq)
!
!-------------RECUPERATION DES FREQUENCES ------------------------------
!
    call jeveuo(modcyc//'.CYCL_FREQ', 'L', llfreq)
!
!----------------RECUPERATION MATRICE DE MASSE--------------------------
!
    call dismoi('REF_MASS_PREM', basmod, 'RESU_DYNA', repk=mass)
    call mtexis(mass, ier)
    if (ier .eq. 0) then
        valk (1) = mass(1:8)
        call utmess('F', 'ALGORITH12_39', sk=valk(1))
    endif
    call mtdscr(mass)
    call jeveuo(mass(1:19)//'.&INT', 'E', lmass)
!
!------------------ALLOCATION DES VECTEURS DE TRAVAIL-------------------
!
    call wkvect('&&REMNEC.VEC.TRAVC', 'V V C', neq, ltvezt)
    call wkvect('&&REMNEC.VEC.COMP', 'V V C', neq, ltveco)
    call wkvect('&&REMNEC.VEC.REEL', 'V V R', neq, ltvere)
!
!-----------------RECUPERATION DES NUMERO D'INTERFACE-------------------
!
    call jeveuo(modcyc//'.CYCL_NUIN', 'L', llnumi)
    numd = zi(llnumi)
    numg = zi(llnumi+1)
    numa = zi(llnumi+2)
!
!-------------CALCUL DES MATRICES DE FLEXIBILITE RESIDUELLE-------------
!
    kbid = ' '
    call bmnodi(basmod, kbid, '         ', numd, 0,&
                ibid(1), nbddr)
    flexdr = '&&REMNEC.FLEX.DROITE'
    call wkvect(flexdr, 'V V R', nbddr*neq, ltfldr)
    ibid(1) = 0
    call flexib(basmod, nbmod, zr(ltfldr), neq, nbddr,&
                ibid(1), numd)
!
    flexga = '&&REMNEC.FLEX.GAUCHE'
    call wkvect(flexga, 'V V R', nbddr*neq, ltflga)
    call flexib(basmod, nbmod, zr(ltflga), neq, nbddr,&
                ibid(1), numg)
!
    if (numa .gt. 0) then
        flexax = '&&REMNEC.FLEX.AXE'
        kbid = ' '
        call bmnodi(basmod, kbid, '         ', numa, 0,&
                    ibid(1), nbdax)
        call wkvect(flexax, 'V V R', nbdax*neq, ltflax)
        call flexib(basmod, nbmod, zr(ltflax), neq, nbddr,&
                    ibid(1), numa)
    endif
!
!--------------CALCUL DES MATRICES DE CHANGEMENT DE BASE TETA-----------
!
    tetgd = '&&REMNEC.TETGD'
    call wkvect(tetgd, 'V V R', nbddr*nbddr, ltetgd)
    call ctetgd(basmod, numd, numg, nbsec, zr(ltetgd),&
                nbddr)
!
    if (numa .gt. 0) then
        tetax = '&&REMNEC.TETAX'
        call wkvect(tetax, 'V V R', nbdax*nbdax, ltetax)
        call ctetgd(basmod, numa, numg, nbsec, zr(ltetax),&
                    nbdax)
    endif
!
!--------------------CLASSEMENT DES MODES PROPRES-----------------------
!               COMPTAGE DU NOMBRE DE MODES PHYSIQUES
!
    nbmoc = 0
    nbmor = 0
    do 5 iddi = 1, nbdia
        nbtmp = zi(lldiam+nbdia+iddi-1)
        nbmoc = nbmoc + nbtmp
        idia = zi(lldiam+iddi-1)
        if (idia .eq. 0 .or. idia .eq. mdiapa) then
            nbmor = nbmor + nbtmp
        else
            nbmor = nbmor + 2*nbtmp
        endif
  5 continue
    call wkvect('&&REMNEC.ORDRE.FREQ', 'V V I', nbmoc, ltorf)
    call wkvect('&&REMNEC.ORDRE.TMPO', 'V V I', nbmoc, ltorto)
    call ordr8(zr(llfreq), nbmoc, zi(ltorto))
!
!
!-----------------ALLOCATION STRUCTURE DE DONNEES-----------------------
!
    call rscrsd('G', nomres, typesd, nbmor)
!
!-------DETERMINATION DES FUTUR NUMERO ORDRES DE MODES REELS------------
!
    nborc = 0
    do 6 ii = 1, nbmoc
        iormo = zi(ltorto+ii-1)
        icomp = 0
        idicou = 0
        do 7 jj = 1, nbdia
            icomp = icomp + zi(lldiam+nbdia+jj-1)
            if (icomp .ge. iormo .and. idicou .eq. 0) idicou = jj
  7     continue
        nborc = nborc + 1
        zi(ltorf+iormo-1) = nborc
        idiam = zi(lldiam+idicou-1)
        if (idiam .ne. 0 .and. idiam .ne. mdiapa) nborc = nborc + 1
  6 continue
    call jedetr('&&REMNEC.ORDRE.TMPO')
!
!---------------------RECUPERATION DES MODES COMPLEXES------------------
!
    call jeveuo(modcyc//'.CYCL_CMODE', 'L', llmoc)
!
!--------------------------RESTITUTION----------------------------------
!
    nbddg = nbmod + nbddr + nbdax
    icomp = 0
    inum = 0
!
! --- BOUCLE SUR LES NOMBRE DE DIAMETRES
!
    do 10 idi = 1, nbdia
!
! ----- CALCUL DEPHASAGE DU SECTEUR DEMANDE
!
        idiam = zi(lldiam+idi-1)
        beta = (depi/nbsec)*idiam
        betsec = (numsec-1)*beta
        aaa = cos(betsec)
        bbb = sin(betsec)
        dephc = dcmplx(aaa,bbb)
!
! ----- BOUCLE SUR MODES DU NOMBRE DE DIAMETRE COURANT
!
        do 15 i = 1, zi(lldiam+nbdia+idi-1)
!
            icomp = icomp + 1
            inum = inum + 1
            iorc = zi(ltorf+icomp-1)
            iad = llmoc + ((icomp-1)*nbddg)
!
! ------- CALCUL MODE COMPLEXE SECTEUR DE BASE
!
            call remnbn(basmod, nbmod, nbddr, nbdax, flexdr,&
                        flexga, flexax, tetgd, tetax, zc(iad),&
                        zc(ltveco), neq, beta)
!
! ------- CALCUL MASSE GENERALISEE
!
            call genecy(zc(ltveco), zc(ltveco), neq, lmass, para,&
                        nbsec, beta, beta, zc(ltvezt))
!
            do 20 j = 1, neq
                zc(ltveco+j-1) = zc(ltveco+j-1)*dephc
                zr(ltvere+j-1) = dble(zc(ltveco+j-1))
 20         continue
!
! ------- RESTITUTION DU MODE PROPRES REEL (PARTIE RELLE)
!
            call rsexch(' ', nomres, depl, inum, chamva,&
                        ier)
            call vtcrem(chamva, matrix, 'G', 'R')
            call jeveuo(chamva//'.VALE', 'E', llcham)
            call rsadpa(nomres, 'E', 1, 'FREQ', inum,&
                        0, sjv=ldfre, styp=k8b)
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
!
            fact = 1.d0 / (para(1)**0.5d0)
            genek = (zr(llfreq+icomp-1)*depi)**2
            call daxpy(neq, fact, zr(ltvere), 1, zr(llcham),&
                       1)
            zr(ldfre) = zr(llfreq+icomp-1)
            zr(ldkge) = genek
            zr(ldmge) = 1.d0
            zr(ldom2) = genek
            zi(ldomo) = iorc
            zk16(ldotm) = 'MODE_DYN'
!
! ------- SPECIFIQUE A BASE_MODALE
!
            call rsadpa(nomres, 'E', 1, 'TYPE_DEFO', inum,&
                        0, sjv=ldtyd, styp=k8b)
            zk16(ldtyd) = 'PROPRE          '
!
            call rsnoch(nomres, depl, inum)
!
! ------- EVENTUELLE RESTITUTION DE LA PARTIE IMAGINAIRE
!
            if (idiam .ne. 0 .and. idiam .ne. mdiapa) then
!
                do 30 j = 1, neq
                    zr(ltvere+j-1) = dimag(zc(ltveco+j-1))
 30             continue
                iorc = iorc + 1
                inum = inum + 1
!
                call rsexch(' ', nomres, depl, inum, chamva,&
                            ier)
                call vtcrem(chamva, matrix, 'G', 'R')
                call jeveuo(chamva//'.VALE', 'E', llcham)
!
                call rsadpa(nomres, 'E', 1, 'FREQ', inum,&
                            0, sjv=ldfre, styp=k8b)
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
!
                fact = 1.d0 / (para(2)**0.5d0)
                genek = (zr(llfreq+icomp-1)*depi)**2
                call daxpy(neq, fact, zr(ltvere), 1, zr(llcham),&
                           1)
                zr(ldfre) = zr(llfreq+icomp-1)
                zr(ldkge) = genek
                zr(ldmge) = 1.d0
                zr(ldom2) = genek
                zi(ldomo) = iorc
                zk16(ldotm) = 'MODE_DYN'
!
                call rsadpa(nomres, 'E', 1, 'TYPE_DEFO', inum,&
                            0, sjv=ldtyd, styp=k8b)
                zk16(ldtyd) = 'PROPRE          '
!
                call rsnoch(nomres, depl, inum)
!
            endif
!
 15     continue
!
 10 continue
!
    call jedetr('&&REMNEC.VEC.TRAVC')
    call jedetr('&&REMNEC.VEC.COMP')
    call jedetr('&&REMNEC.VEC.REEL')
    call jedetr('&&REMNEC.FLEX.DROITE')
    call jedetr('&&REMNEC.FLEX.GAUCHE')
    call jedetr('&&REMNEC.TETGD')
    call jedetr('&&REMNEC.ORDRE.FREQ')
    if (numa .gt. 0) then
        call jedetr('&&REMNEC.FLEX.AXE')
        call jedetr('&&REMNEC.TETAX')
    endif
!
    call jedema()
end subroutine
