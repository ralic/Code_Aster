subroutine gfmacr(noma, nbmail, nbnoeu, nbnoma, nbgrfi)
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
!
!-----------------------------------------------------------------------
!
    implicit none
!     IN
    include 'jeveux.h'
!
    include 'asterfort/jecrec.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/wkvect.h'
    integer :: nbmail, nbnoeu, nbnoma, nbgrfi
    character(len=8) :: noma
!
!     ------------------------------------------------------------------
!     CREATION DES DIFFERENTS ATTRIBUTS DE MAILLAGE DU MAILLAGE GLOBAL
!     DES SECTIONS DE POUTRES MULTIFIBRES (DEFI_GEOM_FIBRE)
!     ------------------------------------------------------------------
!
!
!
!
! ----- DECLARATIONS
!
    integer :: iadime, i, icoval, ititr, ntgeo, iad
    character(len=8) :: knoeu
    character(len=24) :: nommai, nomnoe, cooval, grpmai, connex, titre, typmai
    character(len=24) :: dime, cooref, coodsc, gpptnm
!
!
    call jemarq()
!
!
!
!
!     CONSTRUCTION DES NOMS JEVEUX POUR L OBJET-MAILLAGE
!     --------------------------------------------------
!               123456789012345678901234
    dime = noma// '.DIME           '
    nommai = noma// '.NOMMAI         '
    nomnoe = noma// '.NOMNOE         '
    cooval = noma// '.COORDO    .VALE'
    coodsc = noma// '.COORDO    .DESC'
    cooref = noma// '.COORDO    .REFE'
    grpmai = noma// '.GROUPEMA       '
    connex = noma// '.CONNEX         '
    titre = noma// '           .TITR'
    typmai = noma// '.TYPMAIL        '
!
!
!
! -     OBJET TITRE             = VECTEUR DE K80
!
!
    call wkvect(titre, 'G V K80', 1, ititr)
    zk80(ititr)='MAILLAGE GLOBAL DES SECTIONS DE POUTRES'
!
!
! --- CREATION DE L'OBJET .DIME :
!     -------------------------
    call wkvect(dime, 'G V I', 6, iadime)
    zi(iadime-1+1)= nbnoeu
    zi(iadime-1+3)= nbmail
    zi(iadime-1+6)= 2
!
! -   OBJET NOMMAI    = REPERTOIRE NOMS DE MAILLES  K8 SUR GLOBALE
!
    call jecreo(nommai, 'G N K8')
    call jeecra(nommai, 'NOMMAX', nbmail, ' ')
!
! -   OBJET NOMNOE    = REPERTOIRE NOMS DE NOEUDS K8 SUR GLOBALE
!
    call jecreo(nomnoe, 'G N K8')
    call jeecra(nomnoe, 'NOMMAX', nbnoeu, ' ')
!       REMPLISSAGE DU REPERTOIRE AVEC LES NOMS DES NOEUDS
    do 10 i = 1, nbnoeu
        knoeu='N0000000'
        write(knoeu(2:8),'(I7.7)')i
        call jecroc(jexnom(nomnoe, knoeu))
10  continue
!
! -   OBJET CONNEX
!
    call jecrec(connex, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbmail)
    call jeecra(connex, 'LONT', nbnoma, ' ')
!
!
! -   OBJET TYPMAI
!
    call jecreo(typmai, 'G V I')
    call jeecra(typmai, 'LONMAX', nbmail, ' ')
!
!
! -   OBJET COOR     .VALE
!
    call wkvect(cooval, 'G V R', 3*nbnoeu, icoval)
!
!
! -     OBJET COORDO.DESC = VECTEUR 3*IS DESCRIPTEUR DU CHAMP
!
! -     RECUPERATION DU NUMERO IDENTIFIANT LE TYPE DE CHAM_NO GEOMETRIE
!
    call jenonu(jexnom('&CATA.GD.NOMGD', 'GEOM_R'), ntgeo)
!
    call jecreo(coodsc, 'G V I')
    call jeecra(coodsc, 'LONMAX', 3, ' ')
    call jeecra(coodsc, 'DOCU', 0, 'CHNO')
    call jeveuo(coodsc, 'E', iad)
    zi(iad) = ntgeo
    zi(iad+1) = -3
    zi(iad+2) = 14
!
! -     OBJET COORDO.REFE = VECTEUR 2*K24 NOM DU MAILLAGE !!!
!
    call wkvect(cooref, 'G V K24', 4, iad)
    zk24(iad) = noma
!
!
! -     OBJET GROUPEMA  = FAMILLE CONTIGUE DE VECTEURS N*IS
!
    gpptnm = noma//'.PTRNOMMAI      '
    call jecreo(gpptnm, 'G N K24')
    call jeecra(gpptnm, 'NOMMAX', nbgrfi, ' ')
    call jecrec(grpmai, 'G V I', 'NO '//gpptnm, 'DISPERSE', 'VARIABLE',&
                nbgrfi)
!
!
    call jedema()
end subroutine
