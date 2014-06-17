subroutine cyc110(nomres, mailla, nbsect)
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
!  BUT:  CREATION D'UN MAILLAGE SQUELETTE POUR LA SOUS-STRUCTURATION
!        CYCLIQUE
!
!-----------------------------------------------------------------------
!
! NOMRES   /I/: NOM UT DU RESULTAT OPERATEUR COURANT
! NOMA    /I/: NOM DU MAILLAGE
!
!
!
#include "jeveux.h"
#include "asterc/r8depi.h"
#include "asterfort/codent.h"
#include "asterfort/compma.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvtx.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/recuma.h"
#include "asterfort/trnuli.h"
#include "asterfort/uttrii.h"
#include "asterfort/wkvect.h"
!
!
!
    integer :: ligne(2)
    real(kind=8) :: depi
    character(len=3) :: knusec
    character(len=6) :: kchiff
    character(len=8) :: nomres, mailla, nomcou
    character(len=16) :: mcgrm, motfac, mcmail
    character(len=24) :: gpptnm, grmcou
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iatyma, ibid, icomp, igd, ioctou
    integer :: itcon, j, k,  ldcoo, lddesc, lddime
    integer :: ldgrma, ldref, ldskin,  llcona,  llcox
    integer :: lltitr, lltyp, ltnmgr, ltnmma, ltnuma, ltnuno, nbcon
    integer :: nbgr, nbid, nbma, nbmato, nbno, nbnoto, nbsect
    integer :: nbskma, nbskno, nbtemp, nbtout, nbuf, ntacon, ntemna
    integer :: ntemno, numa, numma, numno, nunew
    real(kind=8) :: teta, tetsec, xanc, xnew, yanc, ynew, zanc
    real(kind=8) :: znew
    integer, pointer :: nldtyp(:) => null()
    real(kind=8), pointer :: vale(:) => null()
    integer, pointer :: connex(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    depi = r8depi()
!
!
!--------------INITIALISATION DES DIVERS MOTS-CLE FACTEUR---------------
!
    motfac = 'SECTEUR'
    mcmail = 'MAILLE'
    mcgrm = 'GROUP_MA'
!
!-------TRAITEMENT DES MAILLES DONNEES EN ENTREE------------------------
!
    nbma = 0
    ltnmma = 1
    call getvtx(motfac, mcmail, iocc=1, nbval=0, nbret=nbma)
    if (nbma .lt. 0) then
        nbma = -nbma
        call wkvect('&&CYC110.NOM.MA', 'V V K8', nbma, ltnmma)
        call getvtx(motfac, mcmail, iocc=1, nbval=nbma, vect=zk8(ltnmma),&
                    nbret=nbid)
    endif
!
!-------TRAITEMENT DES GROUPES DE MAILLES EN ENTREE---------------------
!
    nbuf = 0
    ltnmgr = 1
    call getvtx(motfac, mcgrm, iocc=1, nbval=0, nbret=nbgr)
    if (nbgr .lt. 0) then
        nbgr = -nbgr
        call wkvect('&&CYC110.NOM.GRMA', 'V V K24', nbgr, ltnmgr)
        call getvtx(motfac, mcgrm, iocc=1, nbval=nbgr, vect=zk24(ltnmgr),&
                    nbret=nbid)
        call compma(mailla, nbgr, zk24(ltnmgr), nbuf)
    endif
!
!-----------CAS DE LA RESTITUTION DU MAILLAGE EN ENTIER-----------------
!
    call getvtx(motfac, 'TOUT', iocc=1, nbval=0, nbret=ioctou)
    if (ioctou .lt. 0) then
        ioctou = 1
        call dismoi('NB_MA_MAILLA', mailla, 'MAILLAGE', repi=nbtout)
    endif
!
!----------NOMBRE DE MAILLES (AVEC REPETITION EVENTUELLE)---------------
!
    if (ioctou .eq. 1) then
        nbskma = nbtout
    else
        nbskma = nbma + nbuf
    endif
!
!--------ALLOCATION DU VECTEUR DES NUMERO DE MAILLES--------------------
!
    call wkvect('&&CYC110.NUM.SK.MAIL', 'V V I', nbskma, ltnuma)
!
!
!-------RECUPERATION NUMERO DES MAILLES DONNEES EN ARGUMENTS------------
!
    if (ioctou .eq. 1) then
        do i = 1, nbtout
            zi(ltnuma+i-1) = i
        end do
    else
        call recuma(mailla, nbma, nbgr, zk8(ltnmma), zk24(ltnmgr),&
                    nbskma, zi(ltnuma))
    endif
!
!----------------DESTRUCTION DES OBJETS CADUQUES------------------------
!
    if (nbgr .gt. 0) call jedetr('&&CYC110.NOM.GRMA')
    if (nbma .gt. 0) call jedetr('&&CYC110.NOM.MA')
!
!-----------------------SUPPRESSION DES DOUBLES-------------------------
!
    if (nbskma .ne. 0) call uttrii(zi(ltnuma), nbskma)
!
!-----------RECUPERATION DU NOMBRE A LA LOUCHE DES NOEUDS---------------
!
    nbtemp = 0
    do i = 1, nbskma
        numa = zi(ltnuma+i-1)
        call jelira(jexnum(mailla//'.CONNEX', numa), 'LONMAX', nbno)
        nbtemp = nbtemp + nbno
    end do
!
    nbskno = nbtemp
    ntacon = nbtemp
!
!
!---------ALLOCATION DU VECTEUR TEMPORAIRE DES NUMEROS DE NOEUDS--------
!
    call wkvect('&&CYC110.NUM.SK.NOE', 'V V I', nbskno, ltnuno)
!
!----------RECUPERATION DES NUMEROS DES NOEUDS--------------------------
!
    icomp = 0
    do i = 1, nbskma
        numa = zi(ltnuma+i-1)
        call jelira(jexnum(mailla//'.CONNEX', numa), 'LONMAX', nbno)
        call jeveuo(jexnum(mailla//'.CONNEX', numa), 'L', llcox)
        do j = 1, nbno
            icomp = icomp + 1
            zi(ltnuno+icomp-1) = zi(llcox+j-1)
        end do
!
    end do
!
!
!------------------------SUPPRESSION DES DOUBLES------------------------
!
    if (nbskno .ne. 0) call uttrii(zi(ltnuno), nbskno)
!
!----------------RECUPERATION DU NOMBRE DE SECTEURS---------------------
!           ET CALCUL TAILLE CONNECTIVITE TOTALE
!
!
    ntacon = ntacon*nbsect
!
!
!---------------DETERMINATION DU NOMBRE DE NOEUDS TOTAL-----------------
!                 ET INCREMENTS DIVERS
!
    nbmato = nbskma*nbsect
    nbnoto = nbskno*nbsect
!
!
!------------ALLOCATION DES DIVERS OBJETS DU CONCEPT MAILLAGE-----------
!
    call wkvect(nomres//'           .TITR', 'G V K80', 1, lltitr)
!
!
!
    call wkvect(nomres//'.DIME', 'G V I', 6, lddime)
!
    call jecreo(nomres//'.NOMMAI', 'G N K8')
    call jeecra(nomres//'.NOMMAI', 'NOMMAX', nbmato)
!
    call jecrec(nomres//'.CONNEX', 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbmato)
    call jeecra(nomres//'.CONNEX', 'LONT', ntacon)
!
    call wkvect(nomres//'.TYPMAIL', 'G V I', nbmato, ibid)
!
!
    call jecreo(nomres//'.NOMNOE', 'G N K8')
    call jeecra(nomres//'.NOMNOE', 'NOMMAX', nbnoto)
!
    gpptnm = nomres//'.PTRNOMMAI'
    call jecreo(gpptnm, 'G N K24')
    call jeecra(gpptnm, 'NOMMAX', nbsect)
    call jecrec(nomres//'.GROUPEMA', 'G V I', 'NO '//gpptnm, 'DISPERSE', 'VARIABLE',&
                nbsect)
!
!
    call wkvect(nomres//'.COORDO    .REFE', 'G V K24', 4, ldref)
    call wkvect(nomres//'.COORDO    .DESC', 'G V I', 3, lddesc)
    call jeecra(nomres//'.COORDO    .DESC', 'DOCU', cval='CHNO')
    call wkvect(nomres//'.COORDO    .VALE', 'G V R', 3*nbnoto, ldcoo)
!
!
!-----------------ALLOCATION OBJET SUPPLEMENTAIRE-----------------------
!
    call wkvect(nomres//'.INV.SKELETON', 'G V I', nbnoto*2, ldskin)
!
!------------------REMPLISSAGE .REFE ET .DESC ET TITRE -----------------
!
    zk80(lltitr) = 'MAILLAGE SQUELETTE SOUS-STRUCTURATION CYCLIQUE'
!
    zk24(ldref) = mailla
    zk24(ldref) = nomres
!
!
    call dismoi('NUM_GD', 'GEOM_R', 'GRANDEUR', repi=igd)
    zi(lddesc) = igd
    zi(lddesc+1) = -3
    zi(lddesc+2) = 14
!
!-----------------------REMPLISSAGE DU .DIME----------------------------
!
    zi(lddime) = nbnoto
    zi(lddime+1) = 0
    zi(lddime+2) = nbmato
    zi(lddime+3) = 0
    zi(lddime+4) = 0
    zi(lddime+5) = 3
!
!
!
!--------------------BOUCLE SUR LES SECTEURS----------------------------
!
    tetsec = depi/nbsect
!
    ntemna = 0
    ntemno = 0
    itcon = 0
!
!
    call jeveuo(nomres//'.TYPMAIL', 'E', vi=nldtyp)
    call jeveuo(nomres//'.CONNEX', 'E', vi=connex)
!
!
!    REQUETTE COORDONNEES ANCIEN MAILLAGE
!
    call jeveuo(mailla//'.COORDO    .VALE', 'L', vr=vale)
!
    do i = 1, nbsect
        teta = tetsec* (i-1)
!
!  CREATION NOM DES GROUPES
!
        call codent(i, 'D0', knusec)
        grmcou = 'MASEC'//knusec
        call jecroc(jexnom(nomres//'.GROUPEMA', grmcou))
        call jeecra(jexnom(nomres//'.GROUPEMA', grmcou), 'LONMAX', max(1, nbskma))
        call jeecra(jexnom(nomres//'.GROUPEMA', grmcou), 'LONUTI', nbskma)
        call jeveuo(jexnom(nomres//'.GROUPEMA', grmcou), 'E', ldgrma)
!
!   BOUCLE SUR NOEUD GENERIQUES SECTEUR
!
        do j = 1, nbskno
            numno = zi(ltnuno+j-1)
            ntemno = ntemno + 1
            call codent(ntemno, 'D0', kchiff)
            nomcou = 'NO'//kchiff
            call jecroc(jexnom(nomres//'.NOMNOE', nomcou))
!
!
            zi(ldskin+ntemno-1) = i
            zi(ldskin+nbnoto+ntemno-1) = numno
!
!
            xanc = vale(1+3* (numno-1))
            yanc = vale(1+3* (numno-1)+1)
            zanc = vale(1+3* (numno-1)+2)
!
            xnew = xanc*cos(teta) - sin(teta)*yanc
            ynew = yanc*cos(teta) + sin(teta)*xanc
            znew = zanc
!
            zr(ldcoo+ (ntemno-1)*3) = xnew
            zr(ldcoo+ (ntemno-1)*3+1) = ynew
            zr(ldcoo+ (ntemno-1)*3+2) = znew
!
        end do
!
!    BOUCLE SUR LES ELEMENTS DU SECTEUR
!
        do j = 1, nbskma
            numma = zi(ltnuma+j-1)
            ntemna = ntemna + 1
            call codent(ntemna, 'D0', kchiff)
            nomcou = 'MA'//kchiff
!
!
!
            zi(ldgrma+j-1) = ntemna
!
            call jelira(jexnum(mailla//'.CONNEX', numma), 'LONMAX', nbcon)
            call jecroc(jexnom(nomres//'.NOMMAI', nomcou))
            call jenonu(jexnom(nomres//'.NOMMAI', nomcou), ibid)
            call jeecra(jexnum(nomres//'.CONNEX', ibid), 'LONMAX', nbcon)
            call jeecra(jexnum(nomres//'.CONNEX', ibid), 'LONUTI', nbcon)
            call jeveuo(jexnum(mailla//'.CONNEX', numma), 'L', llcona)
!
            do k = 1, nbcon
                itcon = itcon + 1
                ligne(1) = i
                ligne(2) = zi(llcona+k-1)
                call trnuli(zi(ldskin), nbnoto, 2, ligne, nunew)
                connex(itcon) = nunew
            end do
!
            call jeveuo(mailla//'.TYPMAIL', 'L', iatyma)
            lltyp=iatyma-1+numma
            nldtyp(ntemna) = zi(lltyp)
!
        end do
!
!
!
    end do
!
!-------------SAUVEGARDE ET DESTRUCTION DES OBJETS EVENTUELS------------
!
!
    call jedetr('&&CYC110.NUM.SK.MAIL')
    call jedetr('&&CYC110.NUM.SK.NOE')
!
!
    call jedema()
end subroutine
