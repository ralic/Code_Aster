subroutine recbgl(nomres, typsd, modcyc, profno, indirf,&
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
!  BUT:  < RESTITUTION CRAIG-BAMPTON GLOBALE >
!
!  RESTITUER LES RESULTATS ISSUS D'UN CALCUL CYCLIQUE AVEC DES
!  INTERFACES DE TYPE CRAIG-BAMPTON
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
#include "asterfort/recbbn.h"
#include "asterfort/rotchm.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcrea.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=8) :: nomres, basmod, modcyc, intf, kbid, mailsk, k8b
    character(len=16) :: depl, typsd, typsup(1)
    character(len=19) :: chamva, numddl, profno, mass
    character(len=24) :: tetgd, nomvec
    character(len=24) :: valk(2)
    character(len=24) :: indirf, crefe(2)
    complex(kind=8) :: dephc, dephco
    real(kind=8) :: para(2), depi, fact, genek, beta
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iad, ibid, icomp, iddi, idi, idiam
    integer :: idicou, ieqf, ieqi, ier, ii, inum, iorc
    integer :: iormo, j, jj, k, ldfreq, ldkge, ldmge
    integer :: ldom2, ldomo, ldotm, ldtyd, llcham, lldesc, lldiam
    integer :: llfreq, llinsk, llmoc, llnsec, llnumi, llref, lmass
    integer :: ltetgd, ltinds, ltora, ltord, ltorf, ltorg, ltorto
    integer ::  ltveco, ltvere, ltvezt, mdiapa, nbcmp, nbdax
    integer :: nbddg, nbddr, nbdia, nbmoc, nbmod, nbnot, nborc
    integer :: nbsec, nddcou, neq, neqsec, numa, numd, numg
    real(kind=8), pointer :: teta_secteur(:) => null()
!
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
        call utmess('F', 'ALGORITH14_4', nk=3, valk=valk)
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
    call dismoi('REF_INTD_PREM', basmod, 'RESU_DYNA', repk=intf)
    call dismoi('NUME_DDL', basmod, 'RESU_DYNA', repk=numddl)
    call dismoi('NB_EQUA', numddl, 'NUME_DDL', repi=neqsec)
    call dismoi('NB_CMP_MAX', intf, 'INTERF_DYNA', repi=nbcmp)
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
    call dismoi('REF_MASS_PREM', basmod, 'RESU_DYNA', repk=mass)
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
    call wkvect('&&RECBGL.VEC.TRAVC', 'V V C', neqsec, ltvezt)
    call wkvect('&&RECBGL.VEC.COMP', 'V V C', neqsec, ltveco)
    call wkvect('&&RECBGL.VEC.REEL', 'V V R', neqsec, ltvere)
!
!-----RECUPERATION DES NUMEROS D'INTERFACE------------------------------
!
    call jeveuo(modcyc//'.CYCL_NUIN', 'L', llnumi)
    numd = zi(llnumi)
    numg = zi(llnumi+1)
    numa = zi(llnumi+2)
!
!-----RECUPERATION DU NUMERO D'ORDRE DES DEFORMEES----------------------
!
    nomvec = '&&RECBGL.ORD.DEF.DR'
    call wkvect(nomvec, 'V V I', nbddr, ltord)
    kbid = ' '
    call bmnodi(basmod, kbid, '       ', numd, nbddr,&
                zi(ltord), ibid)
    nomvec = '&&RECBGL.ORD.DEF.GA'
    call wkvect(nomvec, 'V V I', nbddr, ltorg)
    kbid = ' '
    call bmnodi(basmod, kbid, '       ', numg, nbddr,&
                zi(ltorg), ibid)
!
    ltora = 1
    if (nbdax .gt. 0) then
        nomvec = '&&RECBGL.ORD.DEF.AX'
        call wkvect(nomvec, 'V V I', nbdax, ltora)
        kbid = ' '
        call bmnodi(basmod, kbid, '       ', numa, nbdax,&
                    zi(ltora), ibid)
    endif
!
!-----CLASSEMENT DES MODES PROPRES--------------------------------------
!
    nbmoc = 0
    do iddi = 1, nbdia
        nbmoc = nbmoc + zi(lldiam+nbdia+iddi-1)
    end do
    call wkvect('&&RECBGL.ORDRE.FREQ', 'V V I', nbmoc, ltorf)
    call wkvect('&&RECBGL.ORDRE.TMPO', 'V V I', nbmoc, ltorto)
    call ordr8(zr(llfreq), nbmoc, zi(ltorto))
    nborc = 0
    do ii = 1, nbmoc
        iormo = zi(ltorto+ii-1)
        icomp = 0
        idicou = 0
        do jj = 1, nbdia
            icomp = icomp + zi(lldiam+nbdia+jj-1)
            if (icomp .ge. iormo .and. idicou .eq. 0) idicou = jj
        end do
        nborc = nborc + 1
        zi(ltorf+iormo-1) = nborc
        idiam = zi(lldiam+idicou-1)
        if (idiam .ne. 0 .and. idiam .ne. mdiapa) nborc = nborc + 1
    end do
    call jedetr('&&RECBGL.ORDRE.TMPO')
!
!-----RECUPERATION DES MODES COMPLEXES----------------------------------
!
    call jeveuo(modcyc//'.CYCL_CMODE', 'L', llmoc)
    nbddg = nbmod + nbddr + nbdax
!
!-----CALCUL DU TETA DE CHANGEMENT DE BASE GAUCHE-DROITE----------------
!
    tetgd = '&&RECBGL.TETGD'
    call wkvect(tetgd, 'V V R', nbddr*nbddr, ltetgd)
    call ctetgd(basmod, numd, numg, nbsec, zr(ltetgd),&
                nbddr)
!
!-----CALCUL DU TETA DE CHAQUE SECTEUR----------------------------------
!
    AS_ALLOCATE(vr=teta_secteur, size=nbsec)
    do i = 1, nbsec
        teta_secteur(i) = depi*(i-1) / nbsec
    end do
!
!-----RECUPERATION DE L'INDIRECTION SQUELETTE---------------------------
!
    call jeveuo(mailsk//'.INV.SKELETON', 'L', llinsk)
    call dismoi('NB_NO_MAILLA', mailsk, 'MAILLAGE', repi=nbnot)
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
    do idi = 1, nbdia
!
!  CALCUL DU DEPHASAGE INTER-SECTEUR
!
        idiam = zi(lldiam+idi-1)
        beta = (depi/nbsec)*idiam
        dephc = dcmplx(cos(beta),sin(beta))
!
!  BOUCLE SUR LES MODES PROPRES DU DIAMETRE COURANT
!
        do i = 1, zi(lldiam+nbdia+idi-1)
            icomp = icomp + 1
            inum = inum + 1
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
            call recbbn(basmod, nbmod, nbddr, nbdax, tetgd,&
                        zi(ltord), zi(ltorg), zi(ltora), zc(iad), zc(ltveco),&
                        neqsec, beta)
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
            do k = 1, nbsec
                if (k .gt. 1) then
                    dephco = dephc
                else
                    dephco = dcmplx(1.d0,0.d0)
                endif
                do j = 1, neqsec
                    zc(ltveco+j-1) = zc(ltveco+j-1)*dephco
                    zr(ltvere+j-1) = dble(zc(ltveco+j-1))
                end do
                call jeveuo(jexnum(indirf, k), 'L', ltinds)
                call jelira(jexnum(indirf, k), 'LONMAX', nddcou)
                nddcou = nddcou/2
                do j = 1, nddcou
                    ieqi = zi(ltinds+(j-1)*2)
                    ieqf = zi(ltinds+(j-1)*2+1)
                    zr(llcham+ieqf-1) = zr(ltvere+ieqi-1)*fact
                end do
            end do
!
!  PRISE EN COMPTE ROTATION SUR CHAQUE SECTEUR
!
            call rotchm(profno, zr(llcham), teta_secteur, nbsec, zi(llinsk),&
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
                call recbbn(basmod, nbmod, nbddr, nbdax, tetgd,&
                            zi(ltord), zi(ltorg), zi(ltora), zc(iad), zc(ltveco),&
                            neqsec, beta)
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
                do k = 1, nbsec
                    if (k .gt. 1) then
                        dephco = dephc
                    else
                        dephco = dcmplx(1.d0,0.d0)
                    endif
                    do j = 1, neqsec
                        zc(ltveco+j-1) = zc(ltveco+j-1)*dephco
                        zr(ltvere+j-1) = dimag(zc(ltveco+j-1))
                    end do
                    call jeveuo(jexnum(indirf, k), 'L', ltinds)
                    call jelira(jexnum(indirf, k), 'LONMAX', nddcou)
                    nddcou = nddcou / 2
                    do j = 1, nddcou
                        ieqi = zi(ltinds+(j-1)*2)
                        ieqf = zi(ltinds+(j-1)*2+1)
                        zr(llcham+ieqf-1) = zr(ltvere+ieqi-1)*fact
                    end do
                end do
!
!  PRISE EN COMPTE ROTATION SUR CHAQUE SECTEUR
!
                call rotchm(profno, zr(llcham), teta_secteur, nbsec, zi( llinsk),&
                            nbnot, nbcmp, 3)
!
            endif
!
        end do
    end do
!
    call jedetr('&&RECBGL.VEC.TRAVC')
    call jedetr('&&RECBGL.VEC.COMP')
    call jedetr('&&RECBGL.VEC.REEL')
    call jedetr('&&RECBGL.ORD.DEF.DR')
    call jedetr('&&RECBGL.ORD.DEF.GA')
    call jedetr('&&RECBGL.ORDRE.FREQ')
    AS_DEALLOCATE(vr=teta_secteur)
    call jedetr('&&RECBGL.TETGD')
    if (nbdax .gt. 0) call jedetr('&&RECBGL.ORD.DEF.AX')
!
    call jedema()
end subroutine
