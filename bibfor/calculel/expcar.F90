subroutine expcar(carte)
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
! EXTENSION COMPLETE DE LA CARTE( EN VUE D'1 COMPRESSION ULTERIEURE )
! ( LORSQUE LES CMPS D'1 GRANDEUR N'ONT PAS ETE DONNEES SIMULTANEMENT)
!
!-----------------------------------------------------------------------
!
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/mecumu.h'
    include 'asterfort/melima.h'
    include 'asterfort/nbec.h'
    include 'asterfort/scalai.h'
    character(len=19) :: carte
! ----------------------------------------------------------------------
!     ENTREES:
!       CARTE : NOM D'1 CARTE A ETENDRE PROVISOIREMENT
!               ( AVANT COMPRESSION)
!     SORTIES:
!      ON A CREE QUELQUES OBJETS SUR LA VOLATILE ...
! ----------------------------------------------------------------------
    logical :: dejavu
    character(len=8) :: scal, noma, kbid
    character(len=24) :: noli
    character(len=1) :: k1bid
!
!
!     -- RECUPERATION DES OBJETS JEVEUX DE LA CARTE:
!
!-----------------------------------------------------------------------
    integer :: i1, i2, i3, i4, i5, iadesc, iadgp
    integer :: ialima, ianoli, ianoma, ianumt, iavale, iavalp, icode
    integer :: ied, iedit, ient, igd, ima, iret, j
    integer :: nbedit, nbgdmx, nbma, nbmato, ncmpmx, nec, num1
    integer :: num2, numat
!-----------------------------------------------------------------------
    call jemarq()
    call jeveuo(carte//'.DESC', 'L', iadesc)
    call jeveuo(carte//'.LIMA', 'L', ialima)
    call jeveuo(carte//'.VALE', 'L', iavale)
    call jeveuo(carte//'.NOMA', 'L', ianoma)
!
    noma = zk8(ianoma-1+1)
!
    igd = zi(iadesc-1+1)
    nec = nbec(igd)
!     -- SCAL = I,R,C,K8,...
    scal = scalai(igd)
!
!     -- NCMPMX : NOMBRE MAXIMAL DE CMP POUR LA GRANDEUR.
!     ----------------------------------------------------
    call jelira(jexnum('&CATA.GD.NOMCMP', igd), 'LONMAX', ncmpmx, k1bid)
!
!     -- NBEDIT : NOMBRE DE VALEURS EDITEES DANS LA CARTE:
!     ----------------------------------------------------
    nbedit = zi(iadesc-1+3)
!
!     -- CREATION DE L'OBJET .NUMT:
!     -----------------------------
!     NUMT(IEDIT) := V(I)  (DIM=3)
!     V(1) : NUMERO GLOBAL DANS .VALP ET .DGP
!            DE LA 1ERE MAILLE AFFECTEE PAR LA GRANDEUR IEDIT.
!     V(2) : NUMERO DE LA DER MAILLE AFFECTEE.
!     V(3) : 1 --> CETTE GRANDEUR EST ASSOCIEE A 1 PAQUET DE MAILLES
!                 NON TRAITE POUR LES IEDIT PRECEDENTS.
!            0 --> SINON
!     SI CODE(IEDIT)= 1:  TTES LES MAILLES DU MAILLAGE'
!     SI CODE(IEDIT)=-1:  TTES LES MAILLES SUPPL. DU LIGREL NOLI(IEDIT)
!     SI CODE(IEDIT)= 2:  LES MAILLES D'1 GROUPE NOMME.
!     SI CODE(IEDIT)= 3:  LES MAILLES DU MAILLAGE D'1 LISTE TARDIVE.
!     SI CODE(IEDIT)=-3:  LES MAILLES SUPPL. D'1 LISTE TARDIVE.
!
    call jeexin(carte//'.NOLI', iret)
    call assert(iret.ne.0)
    call jeveuo(carte//'.NOLI', 'L', ianoli)
    call jecreo(carte//'.NUMT', 'V V I')
    call jeecra(carte//'.NUMT', 'LONMAX', 3*nbedit, ' ')
    call jeveuo(carte//'.NUMT', 'E', ianumt)
    nbmato = 0
    do 1,iedit = 1,nbedit
    icode = zi(iadesc-1+3+2* (iedit-1)+1)
    noli = zk24(ianoli-1+iedit)
    dejavu = .false.
    do 2,j = iedit - 1,1,-1
    if (noli .eq. zk24(ianoli-1+j)) then
        dejavu = .true.
        goto 3
    endif
 2  continue
 3  continue
    if (dejavu) then
        zi(ianumt-1+3* (iedit-1)+1) = zi(ianumt-1+3* (j-1)+1)
        zi(ianumt-1+3* (iedit-1)+2) = zi(ianumt-1+3* (j-1)+2)
        zi(ianumt-1+3* (iedit-1)+3) = 0
    else
        zi(ianumt-1+3* (iedit-1)+1) = nbmato + 1
        if (noli(1:8) .eq. '        ') then
!              MAILLES DU MAILLAGE:
            call jelira(noma//'.NOMMAI', 'NOMMAX', nbma, k1bid)
            nbmato = nbmato + nbma
        else
!              MAILLES SUPPLEMENTAIRES D'1 LIGREL:
!              TEST : ON DOIT AVOIR ICODE=3 POUR DES MAILLES TARDIVES
            call assert(icode.eq.-3)
            call dismoi('F', 'NB_MA_SUP', noli, 'LIGREL', nbma,&
                        kbid, ied)
            nbmato = nbmato + nbma
        endif
        zi(ianumt-1+3* (iedit-1)+2) = nbmato
        zi(ianumt-1+3* (iedit-1)+3) = 1
    endif
    1 end do
!
!     -- ALOCATION DES OBJETS DE TRAVAIL : .VALP ET .DGP
!
    call jecreo(carte//'.VALP', 'V V '//scal(1:4))
    call jecreo(carte//'.DGP ', 'V V I')
    call jeecra(carte//'.VALP', 'LONMAX', ncmpmx*nbmato, ' ')
    call jeecra(carte//'.DGP ', 'LONMAX', nec*nbmato, ' ')
    call jeveuo(carte//'.VALP', 'E', iavalp)
    call jeveuo(carte//'.DGP ', 'E', iadgp)
!
!     --REMPLISSAGE DE .VALP ET .DGP:
!     -------------------------------
!
    nbgdmx = zi(iadesc-1+2)
    do 11,iedit = 1,nbedit
    i1 = iavale + (iedit-1)*ncmpmx
    i3 = iadesc - 1 + 3 + 2*nbgdmx + (iedit-1)*nec + 1
    num1 = zi(ianumt-1+ (iedit-1)*3+1)
    num2 = zi(ianumt-1+ (iedit-1)*3+2)
    icode = zi(iadesc-1+3+2* (iedit-1)+1)
    ient = zi(iadesc-1+3+2* (iedit-1)+2)
    noli = zk24(ianoli-1+iedit)
    if (abs(icode) .eq. 1) then
!            -- GROUPE 'TOUT':
        nbma = num2 - num1 + 1
    else
        call melima(carte, noma, icode, ient, i5,&
                    nbma)
!           --I5 : ADRESSE DANS ZI DE LA LISTE DES MAILLES A TRAITER.
!           --NBMA: NOMBRE DE MAILLES A TRAITER.
    endif
    do 12,ima = 1,nbma
    if (abs(icode) .eq. 1) then
        numat = ima
    else
        numat = num1 - 1 + abs(zi(i5-1+ima))
    endif
    i2 = iavalp + (numat-1)*ncmpmx
    i4 = iadgp - 1 + (numat-1)*nec + 1
    call mecumu(scal, ncmpmx, i1, i2, nec,&
                zi(i3), zi(i4))
12  continue
    11 end do
!
!
    call jedema()
end subroutine
