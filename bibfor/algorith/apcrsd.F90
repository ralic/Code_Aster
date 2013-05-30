subroutine apcrsd(sdappa, nbzone, ntpt, ntma, ntno,&
                  ntmano, nbno)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
    include 'jeveux.h'
    include 'asterfort/apmmvd.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/wkvect.h'
    character(len=19) :: sdappa
    integer :: nbzone, ntpt, ntma, ntmano, ntno, nbno
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT
!
! CREATION DE LA SD
!
! ----------------------------------------------------------------------
!
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  NBZONE : NOMBRE DE ZONES
! IN  NTPT   : NOMBRE TOTAL DE POINT A APPARIER
! IN  NTMA   : NOMBRE TOTAL DE MAILLES
! IN  NTNO   : NOMBRE TOTAL DE NOEUDS
! IN  NTMANO : NOMBRE TOTAL DE NOEUD AUX ELEMENTS (ELNO)
! IN  NBNO   : NOMBRE DE NOEUD TOTAL DU MAILLAGE
!
!
!
!
    integer :: ifm, niv
    character(len=24) :: nomsd, appar
    integer :: jnomsd, jappa
    character(len=24) :: apinzi, apinzr
    integer :: jpinzi, jpinzr
    character(len=24) :: apinfi, apinfr
    integer :: jpinfi, jpinfr
    character(len=24) :: appoin, apinfp
    integer :: jpoin, jinfp
    character(len=24) :: apnoms
    integer :: jpnoms
    character(len=24) :: apdist, aptau1, aptau2, approj
    integer :: jdist, jtau1, jtau2, jproj
    character(len=24) :: aptgno, aptgel
    integer :: jptgno
    character(len=24) :: apverk, apvera
    integer :: jlistn, jlista
    character(len=8) :: k8bid
    integer :: zinzr, zinzi, zinfi, zinfr
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('APPARIEMENT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<APPARIEMENT> CREATION DE LA SD APPARIEMENT'
    endif
!
! --- LONGUEURS DES SDAPPA
!
    zinzr = apmmvd('ZINZR')
    zinzi = apmmvd('ZINZI')
    zinfr = apmmvd('ZINFR')
    zinfi = apmmvd('ZINFI')
!
! --- CREATION SD NOMS
!
    nomsd = sdappa(1:19)//'.NOSD'
    call wkvect(nomsd, 'V V K24', 3, jnomsd)
!
! --- CREATION SD APPARIEMENT
!
    appar = sdappa(1:19)//'.APPA'
    call wkvect(appar, 'V V I', 4*ntpt, jappa)
!
! --- CREATION SD POUR DISTANCE ET TANGENTES
!
    apdist = sdappa(1:19)//'.DIST'
    aptau1 = sdappa(1:19)//'.TAU1'
    aptau2 = sdappa(1:19)//'.TAU2'
    call wkvect(apdist, 'V V R', 4*ntpt, jdist)
    call wkvect(aptau1, 'V V R', 3*ntpt, jtau1)
    call wkvect(aptau2, 'V V R', 3*ntpt, jtau2)
!
! --- CREATION SD COORDONNEES DE LA PROJECTION
!
    approj = sdappa(1:19)//'.PROJ'
    call wkvect(approj, 'V V R', 2*ntpt, jproj)
!
! --- CREATION SD COORDONNEES DES POINTS
!
    appoin = sdappa(1:19)//'.POIN'
    call wkvect(appoin, 'V V R', 3*ntpt, jpoin)
!
! --- CREATION SD INFOS DES POINTS
!
    apinfp = sdappa(1:19)//'.INFP'
    call wkvect(apinfp, 'V V I', ntpt, jinfp)
!
! --- CREATION SD INFORMATIONS GLOBALES
!
    apinfi = sdappa(1:19)//'.INFI'
    call wkvect(apinfi, 'V V I', zinfi, jpinfi)
    apinfr = sdappa(1:19)//'.INFR'
    call wkvect(apinfr, 'V V R', zinfr, jpinfr)
!
! --- CREATION SD INFORMATIONS PAR ZONE
!
    apinzi = sdappa(1:19)//'.INZI'
    call wkvect(apinzi, 'V V I', zinzi*nbzone, jpinzi)
    apinzr = sdappa(1:19)//'.INZR'
    call wkvect(apinzr, 'V V R', zinzr*nbzone, jpinzr)
!
! --- CREATION SD NOMS DES POINTS DE CONTACT
!
    apnoms = sdappa(1:19)//'.NOMS'
    call wkvect(apnoms, 'V V K16', ntpt, jpnoms)
!
! --- CREATION SD TANGENTES EN TOUS LES NOEUDS
!
    aptgno = sdappa(1:19)//'.TGNO'
    call wkvect(aptgno, 'V V R', 6*ntno, jptgno)
!
! --- CREATION SD TANGENTES AUX NOEUDS PAR ELEMENT
!
    aptgel = sdappa(1:19)//'.TGEL'
    call jecrec(aptgel, 'V V R', 'NU', 'CONTIG', 'VARIABLE',&
                ntma)
    call jeecra(aptgel, 'LONT', 6*ntmano, k8bid)
!
! --- CREATION SD VERIFICATION FACETTISATION
!
    apverk = sdappa(1:19)//'.VERK'
    apvera = sdappa(1:19)//'.VERA'
    call wkvect(apverk, 'V V K8', nbno, jlistn)
    call wkvect(apvera, 'V V R', nbno, jlista)
    call jeecra(apverk, 'LONUTI', 0, k8bid)
!
    call jedema()
!
end subroutine
