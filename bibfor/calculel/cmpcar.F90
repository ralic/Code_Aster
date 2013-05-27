subroutine cmpcar(carte)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!-----------------------------------------------------------------------
    implicit none
!
!     COMPRESSION D'1 CARTE :
! ( LORSQUE LES CMPS D'1 GRANDEUR N'ONT PAS ETE DONNEES SIMULTANEMENT)
!
!-----------------------------------------------------------------------
!
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
!
    include 'asterfort/exisdg.h'
    include 'asterfort/jacopo.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/meiden.h'
    include 'asterfort/nbec.h'
    include 'asterfort/scalai.h'
    character(len=19) :: carte
! ----------------------------------------------------------------------
!     ENTREES:
!       CARTE : NOM D'1 CARTE A COMPRIMER
!     SORTIES:
!      ON A RESTAURE LES OBJETS INITIAUX DE LA CARTE : .VALE,.NOLI,.LIMA
! ----------------------------------------------------------------------
!
!     VARIABLES LOCALES:
!     ------------------
    character(len=8) :: scal, ctype
    character(len=1) :: k1bid, bas1
!
!
!     -- RECUPERATION DES OBJETS JEVEUX DE LA CARTE:
!
!-----------------------------------------------------------------------
    integer :: i, i1, i2, i2lima, i3, i3desc, i3lima
    integer :: i3noli, i3vale, i4, iad, iad1, iad2, iadesc
    integer :: iadgp, ialim2, ialipr, ianoli, ianoma, ianumt, iavale
    integer :: iavalp, iavret, iavtra, ibid, ico, icompt, iedit
    integer :: igd, ii, irtnu, isigne, j, k, n
    integer :: n1, nb, nbedi3, nbedit, nbmato, nboc, ncmp
    integer :: nec, num1, num2
!-----------------------------------------------------------------------
    call jemarq()
    call jeveuo(carte//'.DESC', 'L', iadesc)
    call jeveuo(carte//'.VALE', 'L', iavale)
    call jeveuo(carte//'.VALP', 'L', iad1)
    call jelira(carte//'.VALP', 'TYPELONG', ibid, ctype)
    call jeveuo(carte//'.NOMA', 'L', ianoma)
    call jelira(carte//'.DESC', 'CLAS', ibid, bas1)
!
!
    igd = zi(iadesc-1+1)
    nec = nbec(igd)
!     -- SCAL = I,R,C,K8,...
    scal = scalai(igd)
!
!     -- NCMP : NOMBRE MAXIMAL DE CMP POUR LA GRANDEUR.
!     ----------------------------------------------------
    call jelira(jexnum('&CATA.GD.NOMCMP', igd), 'LONMAX', ncmp, k1bid)
!
!     -- RECUPERATION DES OBJETS  .NOLI .NUMT .VALP ET .DGP:
!     ------------------------------------------------------
    call jeveuo(carte//'.NOLI', 'L', ianoli)
    call jeveuo(carte//'.NUMT', 'L', ianumt)
    call jeveuo(carte//'.VALP', 'L', iavalp)
    call jeveuo(carte//'.DGP ', 'L', iadgp)
    call jelira(carte//'.DGP ', 'LONMAX', n1, k1bid)
!     NOMBRE TOTAL DE MAILLES:
!     CELLES DU MAILLAGE TOUTES LES SUPPL. DES LIGREL ATACHES A LA CARTE
    nbmato = n1/nec
!
!     -- ALLOCATION DES OBJETS: .LIPR .VRET .VTRA ET .LIM2 :
!     ------------------------------------------------------
    call jecreo(carte//'.LIPR', 'V V I')
    call jeecra(carte//'.LIPR', 'LONMAX', nbmato, ' ')
    call jeveuo(carte//'.LIPR', 'E', ialipr)
!     --LIPR CONTIENT A CHAQUE ITERARION, LA LISTE  DES
!     --MAILLES AFFECTEES A LA MEME GRANDEUR.
!
    call jecreo(carte//'.VRET', 'V V I')
    call jeecra(carte//'.VRET', 'LONMAX', nbmato, ' ')
    call jeveuo(carte//'.VRET', 'E', iavret)
!     --VRET NOTE POUR CHAQUE MAILLE SI ELLE A ETE RETENUE COMME MODELE
!
    call jecreo(carte//'.VTRA', 'V V L')
    call jeecra(carte//'.VTRA', 'LONMAX', nbmato, ' ')
    call jeveuo(carte//'.VTRA', 'E', iavtra)
    do 1,i = 1,nbmato
    zl(iavtra-1+i) = .false.
    1 end do
!     --VTRA NOTE POUR CHAQUE MAILLE SI ELLE A ETE TRAITEE.
!
    call jecrec(carte//'.LIM2', 'V V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbmato)
!     -- ON ESPERE QU'IL Y AURA MOINS DE GROUPES QUE DE MAILLES !!!
    call jeecra(carte//'.LIM2', 'LONT', nbmato, ' ')
    call jeveuo(carte//'.LIM2', 'E', ialim2)
!     --LIM2 EST LA COLLECTION QUI REMPLACERA .LIMA.
!
!
!     -- TRAITEMENT:
!     --------------
!
    nbedit = zi(iadesc-1+3)
    ii = 0
    do 11,iedit = 1,nbedit
    if (zi(ianumt-1+3* (iedit-1)+3) .eq. 0) goto 11
    num1 = zi(ianumt-1+ (iedit-1)*3+1)
    num2 = zi(ianumt-1+ (iedit-1)*3+2)
    irtnu = 0
    do 12,i = num1,num2
!           -- SI LA MAILLE A DEJA ETE TRAITEE: ON SORT DE LA BOUCLE.
    if (zl(iavtra-1+i)) goto 12
    icompt = 1
    zi(iavret-1+i) = iedit
    irtnu = irtnu + 1
    zi(ialipr-1+icompt) = i
    i1 = iavalp - 1 + (i-1)*ncmp
    i2 = iadgp - 1 + (i-1)*nec
    do 13,j = i + 1,num2
    if (zl(iavtra-1+i)) goto 13
    i3 = iavalp - 1 + (j-1)*ncmp
    i4 = iadgp - 1 + (j-1)*nec
!              -- TESTE SI LES 2 GRANDEURS SONT PARFAITEMENT IDENTIQUES:
    if (meiden(scal(1:4),ncmp,i1,i3,nec,i2,i4)) then
        icompt = icompt + 1
        zi(ialipr-1+icompt) = j
        zl(iavtra-1+j) = .true.
    endif
13  continue
!           -- RECOPIE DE LA LISTE DE MAILLES .LIPR DANS .LIM2 :
!           -- ATTENTION .LIM2 CONTIENT LES NUMEROS TOTAUX DES MAILLES!
    call jecroc(jexnum(carte//'.LIM2', irtnu))
    call jeecra(jexnum(carte//'.LIM2', irtnu), 'LONMAX', icompt, ' ')
    do 14,k = 1,icompt
    zi(ialim2-1+ii+k) = zi(ialipr-1+k)
14  continue
    ii = ii + icompt
    zl(iavtra-1+i) = .true.
12  continue
    11 end do
!
!     -- ON RECOPIE CE QU'IL FAUT DANS LES OBJETS FINAUX:
!     ---------------------------------------------------
!
    call jelira(carte//'.LIM2', 'NUTIOC', nbedi3, k1bid)
    call jecreo(carte//'.DES3', 'V V I')
    call jeecra(carte//'.DES3', 'LONMAX', 3+nbedi3* (2+nec), ' ')
    call jeveuo(carte//'.DES3', 'E', i3desc)
    zi(i3desc-1+1) = zi(iadesc-1+1)
    zi(i3desc-1+2) = nbedi3
    zi(i3desc-1+3) = nbedi3
!
    call jecreo(carte//'.NOL3', 'V V K24')
    call jeecra(carte//'.NOL3', 'LONMAX', nbedi3, ' ')
    call jeveuo(carte//'.NOL3', 'E', i3noli)
!
    call jecreo(carte//'.VAL3', 'V V '//scal(1:4))
    call jeecra(carte//'.VAL3', 'LONMAX', nbedi3*ncmp, ' ')
    call jeveuo(carte//'.VAL3', 'E', i3vale)
!
    call jecrec(carte//'.LIM3', 'V V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbedi3)
    call jeecra(carte//'.LIM3', 'LONT', nbmato, ' ')
!
    icompt = 0
    do 21,i = 1,nbmato
    if (zi(iavret-1+i) .le. 0) goto 21
    iedit = zi(iavret-1+i)
    icompt = icompt + 1
!
!        --DES3 ET NOL3:
    zk24(i3noli-1+icompt) = zk24(i3noli-1+iedit)
    if (zk24(ianoli-1+iedit) (1:8) .eq. '        ') then
        zi(i3desc-1+3+2* (icompt-1)+1) = 3
    else
        zi(i3desc-1+3+2* (icompt-1)+1) = -3
    endif
    zi(i3desc-1+3+2* (icompt-1)+2) = icompt
    iad = i3desc - 1 + 3 + 2*nbedi3 + nec* (icompt-1)
    do 22,k = 1,nec
    zi(iad+k) = zi(iadgp-1+ (i-1)*nec+k)
22  continue
!
!        --VAL3:
    ico = 0
    do 23,k = 1,ncmp
    if (exisdg(zi(iadgp-1+ (i-1)*nec+1),k)) then
        ico = ico + 1
        call jacopo(1, ctype, iad1+ncmp*(i-1)+k-1, i3vale+ncmp*( icompt-1)+ico-1)
    endif
23  continue
!
!        --LIMA:
    if (zk24(ianoli-1+iedit) (1:8) .eq. '        ') then
        isigne = 1
    else
        isigne = -1
    endif
    call jelira(jexnum(carte//'.LIM2', icompt), 'LONMAX', nb, k1bid)
    call jeecra(jexnum(carte//'.LIM3', icompt), 'LONMAX', nb, ' ')
    call jeveuo(jexnum(carte//'.LIM2', icompt), 'L', i2lima)
    call jeveuo(jexnum(carte//'.LIM3', icompt), 'E', i3lima)
    num1 = zi(ianumt-1+ (iedit-1)*3+1)
    do 24,k = 1,nb
    zi(i3lima-1+k) = isigne* (zi(i2lima-1+k)-num1+1)
24  continue
    21 end do
!
!     ON DETRUIT LA CARTE INITIALE EST ON RECOPIE DEFINITIVEMENT:
!     -----------------------------------------------------------
!
    call jedetr(carte//'.DESC')
    call jedetr(carte//'.VALE')
    call jedetr(carte//'.NOLI')
    call jedetr(carte//'.LIMA')
!
!     DESC :
!     ------
    call jeveuo(carte//'.DES3', 'L', iad1)
    call jelira(carte//'.DES3', 'LONMAX', n, k1bid)
    call jelira(carte//'.DES3', 'TYPELONG', ibid, ctype)
    call jecreo(carte//'.DESC', bas1//' V I')
    call jeecra(carte//'.DESC', 'LONMAX', n, ' ')
    call jeecra(carte//'.DESC', 'DOCU', ibid, 'CART')
    call jeveuo(carte//'.DESC', 'E', iad2)
    call jacopo(n, ctype, iad1, iad2)
!     NOLI :
!     ------
    call jelira(carte//'.NOL3', 'LONMAX', n, k1bid)
    call jelira(carte//'.NOL3', 'TYPELONG', ibid, ctype)
    call jeveuo(carte//'.NOL3', 'L', iad1)
    call jecreo(carte//'.NOLI', bas1//' V K24')
    call jeecra(carte//'.NOLI', 'LONMAX', n, ' ')
    call jeveuo(carte//'.NOLI', 'E', iad2)
    call jacopo(n, ctype, iad1, iad2)
!     VALE :
!     ------
    call jelira(carte//'.VAL3', 'LONMAX', n, k1bid)
    call jelira(carte//'.VAL3', 'TYPELONG', ibid, ctype)
    call jeveuo(carte//'.VAL3', 'E', iad1)
    call jecreo(carte//'.VALE', bas1//' V '//scal(1:4))
    call jeecra(carte//'.VALE', 'LONMAX', n, ' ')
    call jeveuo(carte//'.VALE', 'E', iad2)
    call jacopo(n, ctype, iad1, iad2)
!     LIMA :
!     ------
    call jelira(carte//'.LIM3', 'NMAXOC', nboc, k1bid)
    call jelira(carte//'.LIM3', 'LONT', n, k1bid)
    call jelira(carte//'.LIM3', 'TYPELONG', ibid, ctype)
    call jeveuo(carte//'.LIM3', 'L', iad1)
    call jecrec(carte//'.LIMA', bas1//' V I', 'NU', 'CONTIG', 'VARIABLE',&
                nboc)
    call jeecra(carte//'.LIMA', 'LONT', n, ' ')
    call jeveuo(carte//'.LIMA', 'E', iad2)
!
    do 31 i = 1, nboc
        call jelira(jexnum(carte//'.LIM3', i), 'LONMAX', nb, k1bid)
        call jecroc(jexnum(carte//'.LIMA', i))
        call jeecra(jexnum(carte//'.LIMA', i), 'LONMAX', nb, ' ')
31  end do
    call jacopo(n, ctype, iad1, iad2)
!
!        DESCRIPTION DE TOUS LES OBJETS DE TRAVAIL:
!
    call jedetr(carte//'.DES3')
    call jedetr(carte//'.DGP ')
    call jedetr(carte//'.LIM2')
    call jedetr(carte//'.LIM3')
    call jedetr(carte//'.LIPR')
    call jedetr(carte//'.NOL3')
    call jedetr(carte//'.NUMT')
    call jedetr(carte//'.VALP')
    call jedetr(carte//'.VAL3')
    call jedetr(carte//'.VRET')
    call jedetr(carte//'.VTRA')
    call jedema()
end subroutine
