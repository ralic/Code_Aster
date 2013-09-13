subroutine gma110(nbgr, exclu, nbgrut, mailla, nomsst,&
                  nbtgrm, nomres, nbincr, tabsgr, tabsst,&
                  tabgma, tabnom)
    implicit none
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
!***********************************************************************
!  BUT : < MAILLAGE SQUELETTE SOUS-STRUCTURATION CLASSIQUE >
!
!  TRAITEMENT DES GROUPES DE MAILLES :  ON CREE DES GROUPES DANS LE
!  SQUELETTE A PARTIR DES GOUPES EXISTANTS DANS LA SOUS-STRUCTURE
!
!-----------------------------------------------------------------------
!
! NBGR    /I/ : NOMBRE DE GROUPES DE MAILLES DES SOUS-STRUCTURES
! EXCLU   /I/ : INDIQUE SI ON NE PREND QUE LES GROUPES DE L'UTILISATEUR
! NBGRUT  /I/ : NOMBRE DE GROUPES DONNES PAR L'UTILISATEUR
! MAILLA  /I/ : NOM DU MAILLAGE
! NOMSST  /I/ : NOM DE LA SOUS-STRUCTURE COURANTE
! NBTGRM  /I&O/ : NOMBRE DE GROUPES PRIS EN COMPTE DANS LE SQUELETTE
! NOMRES  /I/ : NOM K8 DU MAILLAGE A CREER
! NBINCR  /I/ : DECALAGE DES NUMERO DE MAILLES DE LA SOUS-STRUCTURE
! TABSGR  /I/ : NOMS DES GROUPES DE LA SOUS-STRUCTURE
! TABSST  /I/ : NOMS DES SOUS-STRUCTURES DONNES PAR L'UTILIATEUR
! TABGMA  /I/ : NOMS DES GROUPES DONNES PAR L'UTILISATEUR
! TABNOM  /I/ : NOMS DES GROUPES DANS LE SQUELETTE
!
! EXEMPLE : LE GROUPE TABGMA(I) DE LA SOUS-STRUCTURE TABSST(I)
!           RECEVRA LE NOM TABNOM(I) DANS LE SQUELETTE
!
!
!
#include "jeveux.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxlgut.h"
#include "asterfort/utmess.h"
!
!
!
    character(len=24) :: valk(4)
    character(len=8) :: nomsst, nomres, mailla
    character(len=24) :: tabsgr(*), tabgma(*), tabnom(*)
    character(len=8) :: k8bid, tabsst(*), exclu
    character(len=24) :: nomgr, nomut
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: igr, igrold, igrut, ilstma, ilstno, ima, leng1
    integer :: leng2, nbgr, nbgrma, nbgrut, nbincr, nbtgrm
    integer :: ngfind, nsfind
!-----------------------------------------------------------------------
    call jemarq()
!
    ngfind = 0
    do 40 igr = 1, nbgr
        nomgr = tabsgr(igr)
        nomut = ' '
!     --- RECHERCHE DES NOMS DANS NOM_GROUP_MA ---
        igrut = 0
10      continue
        igrut = igrut + 1
        if (igrut .le. nbgrut) then
            if (tabsst(igrut) .ne. nomsst) goto 10
            if (tabgma(igrut) .ne. nomgr) goto 10
            ngfind = ngfind + 1
            nomut = tabnom(igrut)
        endif
        if (exclu .eq. 'OUI' .and. nomut .eq. ' ') goto 40
        call jeveuo(jexnom(mailla//'.GROUPEMA', nomgr), 'L', ilstma)
        call jelira(jexnom(mailla//'.GROUPEMA', nomgr), 'LONMAX', nbgrma)
        if (nomut .eq. ' ') then
            leng1 = lxlgut(nomsst)
            leng2 = lxlgut(nomgr)
            if (leng1+leng2 .gt. 8) then
                valk (1) = nomgr
                valk (2) = nomsst
                call utmess('A', 'SOUSTRUC2_10', nk=2, valk=valk)
            endif
            leng2 = min(8-leng1,leng2)
            if (leng2 .gt. 0) then
                nomut=nomsst(1:leng1)//nomgr(1:leng2)
            else
                nomut=nomsst(1:leng1)
            endif
        endif
        do 20 igrold = 1, nbtgrm
            call jenuno(jexnum(nomres//'.GROUPEMA', igrold), k8bid)
            if (nomut .eq. k8bid) then
                valk (1) = nomut
                valk (2) = nomsst
                valk (3) = nomgr
                valk (4) = k8bid
                call utmess('F', 'ALGORITH13_26', nk=4, valk=valk)
            endif
20      continue
        call jecroc(jexnom(nomres//'.GROUPEMA', nomut))
        call jeecra(jexnom(nomres//'.GROUPEMA', nomut), 'LONMAX', max(1, nbgrma))
        call jeecra(jexnom(nomres//'.GROUPEMA', nomut), 'LONUTI', nbgrma)
        call jeveuo(jexnom(nomres//'.GROUPEMA', nomut), 'E', ilstno)
        nbtgrm = nbtgrm+1
        do 30 ima = 1, nbgrma
            zi(ilstno-1+ima) = zi(ilstma-1+ima) + nbincr
30      continue
40  end do
!
! --- ON VERIFIE SI LES GROUPES UTILISATEURS ONT TOUS ETE TROUVE
!
    nsfind = 0
    do 50 igrut = 1, nbgrut
        if (tabsst(igrut) .eq. nomsst) nsfind = nsfind + 1
50  end do
!
    if (nsfind .gt. ngfind) then
! --- CERTAINS GROUPES N'ONT PAS ETE TROUVE
        do 70 igrut = 1, nbgrut
            nomut = tabgma(igrut)
            if (tabsst(igrut) .eq. nomsst) then
                igr = 1
60              continue
                if (igr .le. nbgr) then
                    if (tabsgr(igr) .ne. nomut) then
                        igr = igr + 1
                        goto 60
                    endif
                endif
                if (igr .gt. nbgr) then
                    valk (1) = nomut
                    valk (2) = k8bid
                    valk (3) = nomsst
                    call utmess('F', 'ALGORITH13_27', nk=3, valk=valk)
                endif
            endif
70      continue
    endif
!
    call jedema()
end subroutine
