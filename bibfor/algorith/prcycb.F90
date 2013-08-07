subroutine prcycb(nomres, soumat, repmat)
! aslint: disable=W1501
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!***********************************************************************
!  P. RICHARD     DATE 11/03/91
!-----------------------------------------------------------------------
!  BUT : < PROJECTION CYCLIQUE CRAIG-BAMPTON >
!
!        PROJETER LES MATRICES MASSE ET RAIDEUR ET SORTIR LES SOUS
!        MATRICES POUR TRAITER LE CAS CYCLIQUE AVEC INTERFACES
!        DE CRAIG BAMPTON (CF RAPPORT)
!
!  PARTICULARITES:
!
!  LES MATRICES ISSUES DES PRODUITS MODES-MATRICES-MODES SONT
!  DIAGONALES, ICI ELLES SONT CONSIDEREES PLEINES ET CALCULEES
!  PAR PROJECTION, LA METHODE OBTENUE RESTE AINSI EXACTE SI
!  LES MODES PROPRES DU SECTEUR N'ONT PAS BIEN CONVERGE
!
!  LES MATRICES ISSUES DES PRODUITS DEFORMEES-RAIDEURS-DEFORMEES
!  SONT NULLES POUR LA METHODE DE CRAIG-BAMPTON AVEC MODES
!  CONTRAINTS STATIQUES MAS PAS AVEC DES MODES CONTRAINTS
!  HARMONIQUES, ELLES SONT DONC SYSTEMATIQUEMENT CALCULEES
!  ICI ET ASSEMBLEES APRES
!
!-----------------------------------------------------------------------
!
! NOMRES /I/ : NOM UTILISATEUR DU RESULTAT
! SOUMAT /I/ : NOM K24 DE LA FAMILLE DES SOUS-MATRICES
! REPMAT /I/ : NOM K24 DU REPERTOIRE DES NOMS DES SOUS-MATRICES
!
!
!
#include "jeveux.h"
!
#include "asterfort/amppr.h"
#include "asterfort/bmnodi.h"
#include "asterfort/ctetax.h"
#include "asterfort/ctetgd.h"
#include "asterfort/dcapno.h"
#include "asterfort/dismoi.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtdscr.h"
#include "asterfort/mtexis.h"
#include "asterfort/pmppr.h"
#include "asterfort/u2mesg.h"
#include "asterfort/wkvect.h"
#include "asterfort/zerlag.h"
#include "blas/dcopy.h"
#include "blas/ddot.h"
!
!
    character(len=6) :: pgc
    character(len=8) :: nomres, basmod, intf, kbid, k8bid
    character(len=14) :: num
    character(len=19) :: raid, mass
    character(len=24) :: repmat, soumat, noeint, chamva
    character(len=24) :: valk
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ibid, iddeeq, ier1, ier2, iord, iret
    integer :: j, k, ktrian, ldk0aa, ldk0aj, ldk0ia, ldk0ii
    integer :: ldk0ij, ldk0jj, ldkpaa, ldkpaj, ldkpia, ldkpij, ldkpja
    integer :: ldkpjj, ldm0aa, ldm0aj, ldm0ia, ldm0ii, ldm0ij, ldm0jj
    integer :: ldmpaa, ldmpaj, ldmpia, ldmpij, ldmpja, ldmpjj, llcham
    integer :: lldesc, llnin, llnoa, llnod, llnog, llnoms, llref1
    integer :: llref2, lmatk, lmatm, ltetax, ltetgd, ltkaa, ltkag
    integer :: ltkdg, ltkgg, ltkia, ltkig, ltmaa, ltmag, ltmdg
    integer :: ltmgg, ltmia, ltmig, ltora, ltord, ltorg, ltvec1
    integer :: ltvec3, ltveca, ltvecb, ltvecc, ltvecd, nbdax, nbddr
    integer :: nbdga, nbmod, nbnoa, nbnod, nbnog, nbsec, nbsma
    integer :: neq, ntail, ntrian, numa, numd, numg
    real(kind=8) :: xprod
    complex(kind=8) :: cbid
!-----------------------------------------------------------------------
    data pgc /'PRCYCB'/
!-----------------------------------------------------------------------
!
! --- RECUPERATION DES CONCEPTS AMONT
!
    call jemarq()
    call jeveuo(nomres//'.CYCL_REFE', 'L', llref1)
    intf  =zk24(llref1+1)
    basmod=zk24(llref1+2)
    call jelibe(nomres//'.CYCL_REFE')
    call jeveuo(basmod//'           .REFD', 'L', llref2)
    raid=zk24(llref2)
    mass=zk24(llref2+1)
    call jelibe(basmod//'           .REFD')
!
! --- RECUPERATION DES DIMENSIONS DU PROBLEME GENERALISE
!
    call jeveuo(nomres//'.CYCL_DESC', 'L', lldesc)
    nbmod=zi(lldesc)
    nbddr=zi(lldesc+1)
    nbdga=nbddr
    nbdax=zi(lldesc+2)
    call jelibe(nomres//'.CYCL_DESC')
!
! --- RECUPERATION DES NUMEROS INTERFACE DROITE ET GAUCHE
!
    call jeveuo(nomres//'.CYCL_NUIN', 'L', llnin)
    numd=zi(llnin)
    numg=zi(llnin+1)
    numa=zi(llnin+2)
!
! --- ALLOCATION DU REPERTOIRE DES NOMS DES SOUS-MATRICES
!
    if (nbdax .gt. 0) then
        nbsma=24
    else
        nbsma=10
    endif
!
    call jecreo(repmat, 'V N K8')
    call jeecra(repmat, 'NOMMAX', nbsma)
!
! --- CREATION DE LA FAMILLE DES SOUS-MATRICES
!
    call jecrec(soumat, 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                nbsma)
!
! --- ALLOCATION DES MATRICES
!
! --- STOCKAGE DIAGONAL
!
    ntail=nbmod*(nbmod+1)/2
!
    call jecroc(jexnom(repmat, 'K0II'))
    call jenonu(jexnom(repmat, 'K0II'), ibid)
    call jeecra(jexnum(soumat, ibid), 'LONMAX', ntail)
!
    call jecroc(jexnom(repmat, 'M0II'))
    call jenonu(jexnom(repmat, 'M0II'), ibid)
    call jeecra(jexnum(soumat, ibid), 'LONMAX', ntail)
!
! --- STOCKAGE PLEIN
!
    ntrian=nbddr*(nbddr+1)/2
    ntail=nbddr*nbddr
!
    call jecroc(jexnom(repmat, 'K0JJ'))
    call jenonu(jexnom(repmat, 'K0JJ'), ibid)
    call jeecra(jexnum(soumat, ibid), 'LONMAX', ntrian)
!
    call jecroc(jexnom(repmat, 'KPLUSJJ'))
    call jenonu(jexnom(repmat, 'KPLUSJJ'), ibid)
    call jeecra(jexnum(soumat, ibid), 'LONMAX', ntail)
!
    call jecroc(jexnom(repmat, 'K0IJ'))
    call jenonu(jexnom(repmat, 'K0IJ'), ibid)
    call jeecra(jexnum(soumat, ibid), 'LONMAX', nbmod*nbddr)
!
    call jecroc(jexnom(repmat, 'KPLUSIJ'))
    call jenonu(jexnom(repmat, 'KPLUSIJ'), ibid)
    call jeecra(jexnum(soumat, ibid), 'LONMAX', nbmod*nbddr)
!
    call jecroc(jexnom(repmat, 'M0JJ'))
    call jenonu(jexnom(repmat, 'M0JJ'), ibid)
    call jeecra(jexnum(soumat, ibid), 'LONMAX', ntrian)
!
    call jecroc(jexnom(repmat, 'MPLUSJJ'))
    call jenonu(jexnom(repmat, 'MPLUSJJ'), ibid)
    call jeecra(jexnum(soumat, ibid), 'LONMAX', ntail)
!
    call jecroc(jexnom(repmat, 'M0IJ'))
    call jenonu(jexnom(repmat, 'M0IJ'), ibid)
    call jeecra(jexnum(soumat, ibid), 'LONMAX', nbmod*nbddr)
!
    call jecroc(jexnom(repmat, 'MPLUSIJ'))
    call jenonu(jexnom(repmat, 'MPLUSIJ'), ibid)
    call jeecra(jexnum(soumat, ibid), 'LONMAX', nbmod*nbddr)
!
    if (nbdax .gt. 0) then
!
        ntail=nbddr*nbdax
!
        call jecroc(jexnom(repmat, 'K0AJ'))
        call jenonu(jexnom(repmat, 'K0AJ'), ibid)
        call jeecra(jexnum(soumat, ibid), 'LONMAX', ntail)
!
        call jecroc(jexnom(repmat, 'K0AA'))
        call jenonu(jexnom(repmat, 'K0AA'), ibid)
        call jeecra(jexnum(soumat, ibid), 'LONMAX', nbdax**2)
!
        call jecroc(jexnom(repmat, 'KPLUSAA'))
        call jenonu(jexnom(repmat, 'KPLUSAA'), ibid)
        call jeecra(jexnum(soumat, ibid), 'LONMAX', nbdax**2)
!
        call jecroc(jexnom(repmat, 'KPLUSIA'))
        call jenonu(jexnom(repmat, 'KPLUSIA'), ibid)
        call jeecra(jexnum(soumat, ibid), 'LONMAX', nbmod*nbdax)
!
        call jecroc(jexnom(repmat, 'KPLUSJA'))
        call jenonu(jexnom(repmat, 'KPLUSJA'), ibid)
        call jeecra(jexnum(soumat, ibid), 'LONMAX', ntail)
!
        call jecroc(jexnom(repmat, 'KPLUSAJ'))
        call jenonu(jexnom(repmat, 'KPLUSAJ'), ibid)
        call jeecra(jexnum(soumat, ibid), 'LONMAX', ntail)
!
        call jecroc(jexnom(repmat, 'K0IA'))
        call jenonu(jexnom(repmat, 'K0IA'), ibid)
        call jeecra(jexnum(soumat, ibid), 'LONMAX', nbmod*nbdax)
!
        call jecroc(jexnom(repmat, 'M0IA'))
        call jenonu(jexnom(repmat, 'M0IA'), ibid)
        call jeecra(jexnum(soumat, ibid), 'LONMAX', nbmod*nbdax)
!
        call jecroc(jexnom(repmat, 'MPLUSIA'))
        call jenonu(jexnom(repmat, 'MPLUSIA'), ibid)
        call jeecra(jexnum(soumat, ibid), 'LONMAX', nbmod*nbdax)
!
        call jecroc(jexnom(repmat, 'M0AJ'))
        call jenonu(jexnom(repmat, 'M0AJ'), ibid)
        call jeecra(jexnum(soumat, ibid), 'LONMAX', ntail)
!
        call jecroc(jexnom(repmat, 'MPLUSAJ'))
        call jenonu(jexnom(repmat, 'MPLUSAJ'), ibid)
        call jeecra(jexnum(soumat, ibid), 'LONMAX', ntail)
!
        call jecroc(jexnom(repmat, 'MPLUSJA'))
        call jenonu(jexnom(repmat, 'MPLUSJA'), ibid)
        call jeecra(jexnum(soumat, ibid), 'LONMAX', ntail)
!
        call jecroc(jexnom(repmat, 'M0AA'))
        call jenonu(jexnom(repmat, 'M0AA'), ibid)
        call jeecra(jexnum(soumat, ibid), 'LONMAX', nbdax**2)
!
        call jecroc(jexnom(repmat, 'MPLUSAA'))
        call jenonu(jexnom(repmat, 'MPLUSAA'), ibid)
        call jeecra(jexnum(soumat, ibid), 'LONMAX', nbdax**2)
!
    endif
!
! --- ALLOCATION DES TABLEAUX DE TRAVAIL
!
    call wkvect('&&'//pgc//'.ORD.DROIT', 'V V I', nbddr, ltord)
    call wkvect('&&'//pgc//'.ORD.GAUCH', 'V V I', nbddr, ltorg)
    if (nbdax .gt. 0) then
        call wkvect('&&'//pgc//'.ORD.AXE', 'V V I', nbdax, ltora)
    endif
!
!
! --- RECUPERATION NUMEROTATION ET NB EQUATIONS
!
    call dismoi('F', 'NB_EQUA', raid, 'MATR_ASSE', neq,&
                k8bid, iret)
    call dismoi('F', 'NOM_NUME_DDL', raid, 'MATR_ASSE', ibid,&
                num, iret)
    call jeveuo(num//'.NUME.DEEQ', 'L', iddeeq)
!
! --- RECUPERATION DU NOMBRE DE NOEUDS DES INTERFACES
!
    noeint=intf//'.IDC_LINO'
!
    call jelira(jexnum(noeint, numd), 'LONMAX', nbnod)
    call jeveuo(jexnum(noeint, numd), 'L', llnod)
!
    call jelira(jexnum(noeint, numg), 'LONMAX', nbnog)
    call jeveuo(jexnum(noeint, numg), 'L', llnog)
!
    if (nbdax .gt. 0) then
        call jelira(jexnum(noeint, numa), 'LONMAX', nbnoa)
        call jeveuo(jexnum(noeint, numa), 'L', llnoa)
    endif
!
! --- RECUPERATION DU NOMBRE DE SECTEURS
!
    call jeveuo(nomres//'.CYCL_NBSC', 'L', llnoms)
    nbsec=zi(llnoms)
    call jelibe(nomres//'.CYCL_NBSC')
!
! --- RECUPERATION DES NUMEROS D'ORDRE DES DEFORMEES
!
! --- RECUPERATION DES DEFORMEES DES NOEUDS DROITE
!
    kbid=' '
    call bmnodi(basmod, kbid, '        ', numd, nbddr,&
                zi(ltord), ibid)
!
! --- RECUPERATION DES DEFORMEES DES NOEUDS GAUCHE
!
    kbid=' '
    call bmnodi(basmod, kbid, '        ', numg, nbddr,&
                zi(ltorg), ibid)
!
! --- RECUPERATION DES DEFORMEES EVENTUELLES DES NOEUDS D'AXE
!
    if (nbdax .gt. 0) then
        kbid=' '
        call bmnodi(basmod, kbid, '        ', numa, nbdax,&
                    zi(ltora), ibid)
    endif
!
!
!
! --- CALCUL DES MATRICES TETA DE CHANGEMENT DE REPERE
!
    ntail=nbddr**2
    call wkvect('&&'//pgc//'.TETGD', 'V V R', ntail, ltetgd)
    call ctetgd(basmod, numd, numg, nbsec, zr(ltetgd),&
                nbddr)
!
    if (nbdax .gt. 0) then
        call wkvect('&&'//pgc//'.TETAX', 'V V R', nbdax*nbdax, ltetax)
        call ctetax(basmod, numa, nbsec, zr(ltetax), nbdax)
    endif
!
! --- RECUPERATION DES MODES
!
    call wkvect('&&'//pgc//'.VECTA', 'V V R', nbmod*neq, ltveca)
    call wkvect('&&'//pgc//'.VECTB', 'V V R', nbddr*neq, ltvecb)
    call wkvect('&&'//pgc//'.VECTC', 'V V R', nbddr*neq, ltvecc)
    if (nbdax .gt. 0) then
        call wkvect('&&'//pgc//'.VECTD', 'V V R', nbdax*neq, ltvecd)
    endif
    do 666 i = 1, nbmod
        call dcapno(basmod, 'DEPL    ', i, chamva)
        call jeveuo(chamva, 'L', llcham)
        call dcopy(neq, zr(llcham), 1, zr(ltveca+(i-1)*neq), 1)
        call jelibe(chamva)
        call zerlag('R', zr(ltveca+(i-1)*neq), cbid, neq, zi(iddeeq))
666  end do
    do 667 i = 1, nbddr
        iord=zi(ltord+i-1)
        call dcapno(basmod, 'DEPL    ', iord, chamva)
        call jeveuo(chamva, 'L', llcham)
        call dcopy(neq, zr(llcham), 1, zr(ltvecb+(i-1)*neq), 1)
        call jelibe(chamva)
        call zerlag('R', zr(ltvecb+(i-1)*neq), cbid, neq, zi(iddeeq))
667  end do
    do 668 i = 1, nbddr
        iord=zi(ltorg+i-1)
        call dcapno(basmod, 'DEPL    ', iord, chamva)
        call jeveuo(chamva, 'L', llcham)
        call dcopy(neq, zr(llcham), 1, zr(ltvecc+(i-1)*neq), 1)
        call jelibe(chamva)
        call zerlag('R', zr(ltvecc+(i-1)*neq), cbid, neq, zi(iddeeq))
668  end do
    if (nbdax .gt. 0) then
        do 669 i = 1, nbdax
            iord=zi(ltora+i-1)
            call dcapno(basmod, 'DEPL    ', iord, chamva)
            call jeveuo(chamva, 'L', llcham)
            call dcopy(neq, zr(llcham), 1, zr(ltvecd+(i-1)*neq), 1)
            call jelibe(chamva)
            call zerlag('R', zr(ltvecd+(i-1)*neq), cbid, neq, zi(iddeeq))
669      continue
    endif
!
    call wkvect('&&'//pgc//'.VECT1', 'V V R', neq, ltvec1)
    call wkvect('&&'//pgc//'.VECT3', 'V V R', neq, ltvec3)
!
!***********************************************************************
!***********************************************************************
!                             PROJECTION
!***********************************************************************
!***********************************************************************
!
! --- CONTROLE D'EXISTENCE DE LA MATRICE DE RAIDEUR & DE MASSE
!
    call mtexis(raid, ier1)
    call mtexis(mass, ier2)
    if (ier1 .eq. 0) then
        valk = raid
        call u2mesg('E', 'ALGORITH12_39', 1, valk, 0,&
                    0, 0, 0.d0)
    else if (ier2.eq.0) then
        valk = mass
        call u2mesg('E', 'ALGORITH12_39', 1, valk, 0,&
                    0, 0, 0.d0)
    endif
!
! --- ALLOCATION DESCRIPTEUR DE LA MATRICE
!
    call mtdscr(raid)
    call jeveuo(raid(1:19)//'.&INT', 'L', lmatk)
    call mtdscr(mass)
    call jeveuo(mass(1:19)//'.&INT', 'L', lmatm)
!
! --- PROJECTION MODES-MATRICE-MODES
!     PAS INDISPENSABLE MAIS LA METHODE RESTE EXACTE SI LES
!     MODES DU SECTEUR ONT MAL CONVERGE
!
! --- REQUETTES MATRICES A REMPLIR
!
    call jenonu(jexnom(repmat, 'K0II'), ibid)
    call jeveuo(jexnum(soumat, ibid), 'E', ldk0ii)
    call jenonu(jexnom(repmat, 'M0II'), ibid)
    call jeveuo(jexnum(soumat, ibid), 'E', ldm0ii)
!
    ktrian = 0
    do 10 i = 1, nbmod
!
! ----- CALCUL PRODUIT MATRICE  MODES
!
        call mrmult('ZERO', lmatk, zr(ltveca+(i-1)*neq), zr(ltvec1), 1,&
                    .false.)
        call mrmult('ZERO', lmatm, zr(ltveca+(i-1)*neq), zr(ltvec3), 1,&
                    .false.)
        call zerlag('R', zr(ltvec1), cbid, neq, zi(iddeeq))
        call zerlag('R', zr(ltvec3), cbid, neq, zi(iddeeq))
!
! ----- PRODUIT AVEC MODES
!
        zr(ldk0ii+ktrian) = ddot(neq,zr(ltvec1),1, zr(ltveca+(i-1)* neq),1)
        zr(ldm0ii+ktrian) = ddot(neq,zr(ltvec3),1, zr(ltveca+(i-1)* neq),1)
        ktrian = ktrian + 1
        do 20 j = i-1, 1, -1
            zr(ldk0ii+ktrian) = ddot(neq,zr(ltvec1),1, zr(ltveca+(j-1) *neq),1)
            zr(ldm0ii+ktrian) = ddot(neq,zr(ltvec3),1, zr(ltveca+(j-1) *neq),1)
            ktrian = ktrian + 1
20      continue
!
10  end do
!
! --- PRODUIT MATRICE DEFORMEES DROITES
!
! --- REQUETTES MATRICES A REMPLIR
!
    call jenonu(jexnom(repmat, 'K0JJ'), ibid)
    call jeveuo(jexnum(soumat, ibid), 'E', ldk0jj)
    call jenonu(jexnom(repmat, 'K0IJ'), ibid)
    call jeveuo(jexnum(soumat, ibid), 'E', ldk0ij)
    call jenonu(jexnom(repmat, 'M0JJ'), ibid)
    call jeveuo(jexnum(soumat, ibid), 'E', ldm0jj)
    call jenonu(jexnom(repmat, 'M0IJ'), ibid)
    call jeveuo(jexnum(soumat, ibid), 'E', ldm0ij)
    if (nbdax .gt. 0) then
        call jenonu(jexnom(repmat, 'K0AJ'), ibid)
        call jeveuo(jexnum(soumat, ibid), 'E', ldk0aj)
        call jenonu(jexnom(repmat, 'M0AJ'), ibid)
        call jeveuo(jexnum(soumat, ibid), 'E', ldm0aj)
    endif
!
    ktrian = 0
! --- PRODUIT MATRICE DEFORMEES DROITES
!
! --- REQUETTES MATRICES A REMPLIR
!
    call jenonu(jexnom(repmat, 'K0JJ'), ibid)
    call jeveuo(jexnum(soumat, ibid), 'E', ldk0jj)
    call jenonu(jexnom(repmat, 'K0IJ'), ibid)
    call jeveuo(jexnum(soumat, ibid), 'E', ldk0ij)
    call jenonu(jexnom(repmat, 'M0JJ'), ibid)
    call jeveuo(jexnum(soumat, ibid), 'E', ldm0jj)
    call jenonu(jexnom(repmat, 'M0IJ'), ibid)
    call jeveuo(jexnum(soumat, ibid), 'E', ldm0ij)
    if (nbdax .gt. 0) then
        call jenonu(jexnom(repmat, 'K0AJ'), ibid)
        call jeveuo(jexnum(soumat, ibid), 'E', ldk0aj)
        call jenonu(jexnom(repmat, 'M0AJ'), ibid)
        call jeveuo(jexnum(soumat, ibid), 'E', ldm0aj)
    endif
!
    ktrian = 0
!
    do 60 i = 1, nbddr
!
! ----- CALCUL PRODUIT MATRICE DEFORMEE DROITE
!
        call mrmult('ZERO', lmatk, zr(ltvecb+(i-1)*neq), zr(ltvec1), 1,&
                    .false.)
        call mrmult('ZERO', lmatm, zr(ltvecb+(i-1)*neq), zr(ltvec3), 1,&
                    .false.)
        call zerlag('R', zr(ltvec1), cbid, neq, zi(iddeeq))
        call zerlag('R', zr(ltvec3), cbid, neq, zi(iddeeq))
!
! ----- CALCUL TERME DIAGONAL
!
        zr(ldk0jj+ktrian) = ddot(neq,zr(ltvec1),1, zr(ltvecb+(i-1)* neq),1)
        zr(ldm0jj+ktrian) = ddot(neq,zr(ltvec3),1, zr(ltvecb+(i-1)* neq),1)
!
! ----- MULTIPLICATION PAR MODES PROPRES
! ----- NUL SI MODES CONTRAINTS STATIQUES MAIS NON NUL
! ----- SI MODES CONTRAINTS HARMONIQUES
!
        do 65 j = 1, nbmod
            xprod= ddot(neq,zr(ltvec1),1,zr(ltveca+(j-1)*neq),1)
            call amppr(zr(ldk0ij), nbmod, nbddr, xprod, 1,&
                       1, j, i)
            xprod= ddot(neq,zr(ltvec3),1,zr(ltveca+(j-1)*neq),1)
            call amppr(zr(ldm0ij), nbmod, nbddr, xprod, 1,&
                       1, j, i)
65      continue
!
! ----- PRODUIT AVEC DEFORMEES DROITES (HORS TERMES DIAGONAUX)
!
        ktrian = ktrian + 1
        do 70 j = i-1, 1, -1
            zr(ldk0jj+ktrian) = ddot(neq,zr(ltvec1),1, zr(ltvecb+(j-1) *neq),1)
            zr(ldm0jj+ktrian) = ddot(neq,zr(ltvec3),1, zr(ltvecb+(j-1) *neq),1)
            ktrian = ktrian + 1
70      continue
!
! ----- PRODUIT AVEC DEFORMEES AXE
!
        if (nbdax .gt. 0) then
            do 75 j = 1, nbdax
                xprod= ddot(neq,zr(ltvec1),1,zr(ltvecd+(j-1)*neq),1)
                call amppr(zr(ldk0aj), nbdax, nbddr, xprod, 1,&
                           1, j, i)
                xprod= ddot(neq,zr(ltvec3),1,zr(ltvecd+(j-1)*neq),1)
                call amppr(zr(ldm0aj), nbdax, nbddr, xprod, 1,&
                           1, j, i)
75          continue
        endif
!
60  end do
!
!
! --- TRAITEMENT DES PRODUITS MATRICIELS PAR TETA
!
! --- POUR K0AJ
!
    if (nbdax .gt. 0) then
        call jenonu(jexnom(repmat, 'KPLUSJA'), ibid)
        call jeveuo(jexnum(soumat, ibid), 'E', ldkpja)
        call pmppr(zr(ldk0aj), nbdax, nbddr, -1, zr(ltetax),&
                   nbdax, nbdax, 1, zr(ldkpja), nbddr,&
                   nbdax)
        call jenonu(jexnom(repmat, 'MPLUSJA'), ibid)
        call jeveuo(jexnum(soumat, ibid), 'E', ldmpja)
        call pmppr(zr(ldm0aj), nbdax, nbddr, -1, zr(ltetax),&
                   nbdax, nbdax, 1, zr(ldmpja), nbddr,&
                   nbdax)
    endif
!
! --- PRODUIT MATRICE DEFORMEES GAUCHES
!
! --- REQUETTE DU TABLEAU A REMPLIR (MATRICE STOCKEE PLEINE)
!
    call wkvect('&&'//pgc//'.KGG', 'V V R', nbddr*nbddr, ltkgg)
    call wkvect('&&'//pgc//'.KDG', 'V V R', nbddr*nbddr, ltkdg)
    call wkvect('&&'//pgc//'.KIG', 'V V R', nbmod*nbddr, ltkig)
    call wkvect('&&'//pgc//'.MGG', 'V V R', nbddr*nbddr, ltmgg)
    call wkvect('&&'//pgc//'.MDG', 'V V R', nbddr*nbddr, ltmdg)
    call wkvect('&&'//pgc//'.MIG', 'V V R', nbmod*nbddr, ltmig)
    if (nbdax .gt. 0) then
        call wkvect('&&'//pgc//'.KAG', 'V V R', nbdax*nbddr, ltkag)
        call wkvect('&&'//pgc//'.MAG', 'V V R', nbdax*nbddr, ltmag)
    endif
!
    do 80 i = 1, nbdga
!
! ----- CALCUL PRODUIT MATRICE DEFORMEE GAUCHE
!
        call mrmult('ZERO', lmatk, zr(ltvecc+(i-1)*neq), zr(ltvec1), 1,&
                    .false.)
        call mrmult('ZERO', lmatm, zr(ltvecc+(i-1)*neq), zr(ltvec3), 1,&
                    .false.)
        call zerlag('R', zr(ltvec1), cbid, neq, zi(iddeeq))
        call zerlag('R', zr(ltvec3), cbid, neq, zi(iddeeq))
!
! ----- CALCUL TERME DIAGONAL
!
        xprod= ddot(neq,zr(ltvec1),1,zr(ltvecc+(i-1)*neq),1)
        call amppr(zr(ltkgg), nbdga, nbdga, xprod, 1,&
                   1, i, i)
        xprod= ddot(neq,zr(ltvec3),1,zr(ltvecc+(i-1)*neq),1)
        call amppr(zr(ltmgg), nbdga, nbdga, xprod, 1,&
                   1, i, i)
!
! ----- MULTIPLICATION PAR MODES PROPRES
!
        do 85 j = 1, nbmod
            xprod= ddot(neq,zr(ltvec1),1,zr(ltveca+(j-1)*neq),1)
            call amppr(zr(ltkig), nbmod, nbdga, xprod, 1,&
                       1, j, i)
            xprod= ddot(neq,zr(ltvec3),1,zr(ltveca+(j-1)*neq),1)
            call amppr(zr(ltmig), nbmod, nbdga, xprod, 1,&
                       1, j, i)
85      continue
!
! ----- PRODUIT AVEC DEFORMEE DROITE
!
        do 90 j = 1, nbddr
            xprod= ddot(neq,zr(ltvec1),1,zr(ltvecb+(j-1)*neq),1)
            call amppr(zr(ltkdg), nbddr, nbddr, xprod, 1,&
                       1, j, i)
            xprod= ddot(neq,zr(ltvec3),1,zr(ltvecb+(j-1)*neq),1)
            call amppr(zr(ltmdg), nbddr, nbddr, xprod, 1,&
                       1, j, i)
90      continue
!
! ----- PRODUIT AVEC DEFORMEES GAUCHES (HORS TERMES DIAGONAUX)
!
        do 100 j = 1, i-1
            xprod= ddot(neq,zr(ltvec1),1,zr(ltvecc+(j-1)*neq),1)
            call amppr(zr(ltkgg), nbdga, nbdga, xprod, 1,&
                       1, j, i)
            call amppr(zr(ltkgg), nbdga, nbdga, xprod, 1,&
                       1, i, j)
            xprod= ddot(neq,zr(ltvec3),1,zr(ltvecc+(j-1)*neq),1)
            call amppr(zr(ltmgg), nbdga, nbdga, xprod, 1,&
                       1, j, i)
            call amppr(zr(ltmgg), nbdga, nbdga, xprod, 1,&
                       1, i, j)
100      continue
!
! ----- PRODUIT AVEC DEFORMEES AXE
!
        if (nbdax .gt. 0) then
            do 105 j = 1, nbdax
                xprod= ddot(neq,zr(ltvec1),1,zr(ltvecd+(j-1)*neq),1)
                call amppr(zr(ltkag), nbdax, nbdga, xprod, 1,&
                           1, j, i)
                xprod= ddot(neq,zr(ltvec3),1,zr(ltvecd+(j-1)*neq),1)
                call amppr(zr(ltmag), nbdax, nbdga, xprod, 1,&
                           1, j, i)
105          continue
        endif
!
80  end do
!
!
! --- TRAITEMENT DES PRODUITS MATRICIELS PAR TETA
!
!
! --- POUR KPLUSIJ
!
    call jenonu(jexnom(repmat, 'KPLUSIJ'), ibid)
    call jeveuo(jexnum(soumat, ibid), 'E', ldkpij)
    call pmppr(zr(ltkig), nbmod, nbddr, 1, zr(ltetgd),&
               nbddr, nbddr, 1, zr(ldkpij), nbmod,&
               nbddr)
!
    call jedetr('&&'//pgc//'.KIG')
! --- POUR MPLUSIJ
    call jenonu(jexnom(repmat, 'MPLUSIJ'), ibid)
    call jeveuo(jexnum(soumat, ibid), 'E', ldmpij)
    call pmppr(zr(ltmig), nbmod, nbddr, 1, zr(ltetgd),&
               nbddr, nbddr, 1, zr(ldmpij), nbmod,&
               nbddr)
!
    call jedetr('&&'//pgc//'.MIG')
!
! --- POUR KPLUSJJ
!
    call jenonu(jexnom(repmat, 'KPLUSJJ'), ibid)
    call jeveuo(jexnum(soumat, ibid), 'E', ldkpjj)
    call pmppr(zr(ltkdg), nbddr, nbddr, 1, zr(ltetgd),&
               nbddr, nbddr, 1, zr(ldkpjj), nbddr,&
               nbddr)
!
! --- POUR K0JJ
!
    call pmppr(zr(ltkgg), nbddr, nbddr, 1, zr(ltetgd),&
               nbddr, nbddr, 1, zr(ltkdg), nbddr,&
               nbddr)
!
    call pmppr(zr(ltetgd), nbddr, nbddr, -1, zr(ltkdg),&
               nbddr, nbddr, 1, zr(ltkgg), nbddr,&
               nbddr)
!
!
    call jenonu(jexnom(repmat, 'MPLUSJJ'), ibid)
    call jeveuo(jexnum(soumat, ibid), 'E', ldmpjj)
    call pmppr(zr(ltmdg), nbddr, nbddr, 1, zr(ltetgd),&
               nbddr, nbddr, 1, zr(ldmpjj), nbddr,&
               nbddr)
!
! --- POUR M0JJ
!
    call pmppr(zr(ltmgg), nbddr, nbddr, 1, zr(ltetgd),&
               nbddr, nbddr, 1, zr(ltmdg), nbddr,&
               nbddr)
!
    call pmppr(zr(ltetgd), nbddr, nbddr, -1, zr(ltmdg),&
               nbddr, nbddr, 1, zr(ltmgg), nbddr,&
               nbddr)
!
    k = 0
    do 107 j = 1, nbddr
        do 107 i = j, 1, -1
            zr(ldk0jj+k) = zr(ldk0jj+k)+zr(ltkgg-1+(j-1)*nbddr+i)
            zr(ldm0jj+k) = zr(ldm0jj+k)+zr(ltmgg-1+(j-1)*nbddr+i)
            k = k + 1
107      continue
!
!
! --- SAUVEGARDE ET DESTRUCTION
    call jedetr('&&'//pgc//'.KGG')
    call jedetr('&&'//pgc//'.KDG')
    call jedetr('&&'//pgc//'.MGG')
    call jedetr('&&'//pgc//'.MDG')
!
    if (nbdax .gt. 0) then
!
        call jenonu(jexnom(repmat, 'KPLUSAJ'), ibid)
        call jeveuo(jexnum(soumat, ibid), 'E', ldkpaj)
        call pmppr(zr(ltkag), nbdax, nbddr, 1, zr(ltetgd),&
                   nbddr, nbddr, 1, zr(ldkpaj), nbdax,&
                   nbddr)
!
        call pmppr(zr(ltetax), nbdax, nbdax, -1, zr(ldkpaj),&
                   nbdax, nbddr, 1, zr(ltkag), nbdax,&
                   nbddr)
!
        call amppr(zr(ldk0aj), nbdax, nbddr, zr(ltkag), nbdax,&
                   nbddr, 1, 1)
!
        call jedetr('&&'//pgc//'.KAG')
        call jenonu(jexnom(repmat, 'MPLUSAJ'), ibid)
        call jeveuo(jexnum(soumat, ibid), 'E', ldmpaj)
        call pmppr(zr(ltmag), nbdax, nbddr, 1, zr(ltetgd),&
                   nbddr, nbddr, 1, zr(ldmpaj), nbdax,&
                   nbddr)
!
        call pmppr(zr(ltetax), nbdax, nbdax, -1, zr(ldmpaj),&
                   nbdax, nbddr, 1, zr(ltmag), nbdax,&
                   nbddr)
!
        call amppr(zr(ldm0aj), nbdax, nbddr, zr(ltmag), nbdax,&
                   nbddr, 1, 1)
!
        call jedetr('&&'//pgc//'.MAG')
!
    endif
!
! --- PRODUIT MATRICE - DEFORMEES AXE
!
! --- REQUETTE DU TABLEAU A REMPLIR (MATRICE STOCKEE PLEINE)
!
    if (nbdax .gt. 0) then
        call wkvect('&&'//pgc//'.KAA', 'V V R', nbdax*nbdax, ltkaa)
        call wkvect('&&'//pgc//'.KIA', 'V V R', nbmod*nbdax, ltkia)
        call wkvect('&&'//pgc//'.MAA', 'V V R', nbdax*nbdax, ltmaa)
        call wkvect('&&'//pgc//'.MIA', 'V V R', nbmod*nbdax, ltmia)
!
        do 210 i = 1, nbdax
!
! ------- CALCUL PROJECTION MATRICE DEFORMEES AXE
!
            call mrmult('ZERO', lmatk, zr(ltvecd+(i-1)*neq), zr(ltvec1), 1,&
                        .false.)
            call mrmult('ZERO', lmatm, zr(ltvecd+(i-1)*neq), zr(ltvec3), 1,&
                        .false.)
            call zerlag('R', zr(ltvec1), cbid, neq, zi(iddeeq))
            call zerlag('R', zr(ltvec3), cbid, neq, zi(iddeeq))
!
! ------- MULTIPLICATION PAR MODES PROPRES
!
            do 220 j = 1, nbmod
                xprod= ddot(neq,zr(ltvec1),1,zr(ltveca+(j-1)*neq),1)
                call amppr(zr(ltkia), nbmod, nbdax, xprod, 1,&
                           1, j, i)
                xprod= ddot(neq,zr(ltvec3),1,zr(ltveca+(j-1)*neq),1)
                call amppr(zr(ltmia), nbmod, nbdax, xprod, 1,&
                           1, j, i)
220          continue
!
! ------- PRODUIT AVEC DEFORMEE AXE
!
            do 230 j = 1, nbdax
                xprod= ddot(neq,zr(ltvec1),1,zr(ltvecd+(j-1)*neq),1)
                call amppr(zr(ltkaa), nbdax, nbdax, xprod, 1,&
                           1, j, i)
                xprod= ddot(neq,zr(ltvec3),1,zr(ltvecd+(j-1)*neq),1)
                call amppr(zr(ltmaa), nbdax, nbdax, xprod, 1,&
                           1, j, i)
230          continue
210      continue
!
! ----- TRAITEMENT DES PRODUITS MATRICIEL PAR TETA
! -----(RECUPERATION EN LECTURE DE K0AA & MOAA)
!
! ----- POUR K0IA ET KPLUSIA
!
        call jenonu(jexnom(repmat, 'K0IA'), ibid)
        call jeveuo(jexnum(soumat, ibid), 'E', ldk0ia)
!
        call jenonu(jexnom(repmat, 'KPLUSIA'), ibid)
        call jeveuo(jexnum(soumat, ibid), 'E', ldkpia)
        call amppr(zr(ldk0ia), nbmod, nbdax, zr(ltkia), nbmod,&
                   nbdax, 1, 1)
!
        call pmppr(zr(ltkia), nbmod, nbdax, 1, zr(ltetax),&
                   nbdax, nbdax, 1, zr(ldkpia), nbmod,&
                   nbdax)
!
        call jedetr('&&'//pgc//'.KIA')
!
! ----- POUR KPLUSAA ET K0AA
!
        call jenonu(jexnom(repmat, 'KPLUSAA'), ibid)
        call jeveuo(jexnum(soumat, ibid), 'E', ldkpaa)
        call jenonu(jexnom(repmat, 'K0AA'), ibid)
        call jeveuo(jexnum(soumat, ibid), 'E', ldk0aa)
!
        call pmppr(zr(ltkaa), nbdax, nbdax, 1, zr(ltetax),&
                   nbdax, nbdax, 1, zr(ldkpaa), nbdax,&
                   nbdax)
!
        call amppr(zr(ldk0aa), nbdax, nbdax, zr(ltkaa), nbdax,&
                   nbdax, 1, 1)
!
        call pmppr(zr(ltetax), nbdax, nbdax, -1, zr(ldkpaa),&
                   nbdax, nbdax, 1, zr(ltkaa), nbdax,&
                   nbdax)
!
        call amppr(zr(ldk0aa), nbdax, nbdax, zr(ltkaa), nbdax,&
                   nbdax, 1, 1)
!
        call jedetr('&&'//pgc//'.KAA')
!
! ----- POUR M0IA  ET MPLUSIA
!
        call jenonu(jexnom(repmat, 'M0IA'), ibid)
        call jeveuo(jexnum(soumat, ibid), 'E', ldm0ia)
        call jenonu(jexnom(repmat, 'MPLUSIA'), ibid)
        call jeveuo(jexnum(soumat, ibid), 'E', ldmpia)
        call pmppr(zr(ltmia), nbmod, nbdax, 1, zr(ltetax),&
                   nbdax, nbdax, 1, zr(ldmpia), nbmod,&
                   nbdax)
!
        call amppr(zr(ldm0ia), nbmod, nbdax, zr(ltmia), nbmod,&
                   nbdax, 1, 1)
        call jedetr('&&'//pgc//'.MIA')
!
! ----- POUR  M0AA
!
        call jenonu(jexnom(repmat, 'MPLUSAA'), ibid)
        call jeveuo(jexnum(soumat, ibid), 'E', ldmpaa)
        call jenonu(jexnom(repmat, 'M0AA'), ibid)
        call jeveuo(jexnum(soumat, ibid), 'E', ldm0aa)
!
        call pmppr(zr(ltmaa), nbdax, nbdax, 1, zr(ltetax),&
                   nbdax, nbdax, 1, zr(ldmpaa), nbdax,&
                   nbdax)
!
        call amppr(zr(ldm0aa), nbdax, nbdax, zr(ltmaa), nbdax,&
                   nbdax, 1, 1)
!
        call pmppr(zr(ltetax), nbdax, nbdax, -1, zr(ldmpaa),&
                   nbdax, nbdax, 1, zr(ltmaa), nbdax,&
                   nbdax)
        call amppr(zr(ldm0aa), nbdax, nbdax, zr(ltmaa), nbdax,&
                   nbdax, 1, 1)
!
        call jedetr('&&'//pgc//'.MAA')
!
    endif
!
    call jedetr('&&PRCYCB.MAT.TRAV')
    call jedetr('&&PRCYCB.VECT')
    call jedetr('&&'//pgc//'.TETGD')
    call jedetr('&&'//'PRCYCB'//'.ORD.DROIT')
    call jedetr('&&'//'PRCYCB'//'.ORD.GAUCH')
    if (nbdax .gt. 0) then
        call jedetr('&&'//pgc//'.TETAX')
        call jedetr('&&'//'PRCYCB'//'.ORD.AXE')
    endif
!
    call jedetr('&&'//pgc//'.VECTA')
    call jedetr('&&'//pgc//'.VECTB')
    call jedetr('&&'//pgc//'.VECTC')
    if (nbdax .gt. 0) then
        call jedetr('&&'//pgc//'.VECTD')
    endif
!
    call jedetr('&&'//pgc//'.VECT1')
    call jedetr('&&'//pgc//'.VECT3')
!
    call jedema()
end subroutine
