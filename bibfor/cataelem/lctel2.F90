subroutine lctel2()
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
! ----------------------------------------------------------------------
!     BUT : AJOUTER AUX OBJETS RESULTANTS DE LA LECTURE DES CATALOGUES
!      L'OBJET '&CATA.TE.TAILLMAX'   S V I
!           V(I_TE) : NOMBRE MAXI DE TERMES DANS UNE MATRICE ELEMENTAIRE
!                     POUR L'ELEMENT I_TE
!      L'OBJET '&CATA.TE.NBLIGCOL'
!      L'OBJET '&CATA.TE.OPTTE'
!
! ----------------------------------------------------------------------
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/wkvect.h'
    character(len=24) :: nomolo
    character(len=16) :: nomte
    character(len=8) :: kbid
    integer :: nbte, nbop, nbgd, iatamx, nbmolo, imolo, jnblig, iamolo
    integer :: n, ioptte, iop, ite, joptte, joptt2
!
    call jemarq()
!
    call jelira('&CATA.TE.NOMTE', 'NOMMAX', nbte, kbid)
    call jelira('&CATA.OP.NOMOPT', 'NOMMAX', nbop, kbid)
    call jelira('&CATA.GD.NOMGD', 'NOMMAX', nbgd, kbid)
    call jelira('&CATA.TE.NOMMOLOC', 'NOMMAX', nbmolo, kbid)
!
!
!
!
!     OBJET .TAILLMAX :
!     -------------------
    call wkvect('&CATA.TE.TAILLMAX', 'G V I', nbte, iatamx)
    do 1, imolo=1,nbmolo
    call jeveuo(jexnum('&CATA.TE.MODELOC', imolo), 'L', iamolo)
    if (zi(iamolo-1+1) .eq. 5) then
        call jenuno(jexnum('&CATA.TE.NOMMOLOC', imolo), nomolo)
        nomte= nomolo(1:16)
        call jenonu(jexnom('&CATA.TE.NOMTE', nomte), ite)
        zi(iatamx-1+ite)=max(zi(iatamx-1+ite),zi(iamolo-1+3))
    endif
    1 end do
!
!
!     OBJET .NBLIGCOL :
!     -------------------
    call wkvect('&CATA.TE.NBLIGCOL', 'G V I', 6, jnblig)
    zi(jnblig-1+1)=nbop
    zi(jnblig-1+2)=nbte
    zi(jnblig-1+3)=nbte
    zi(jnblig-1+4)=nbgd
    zi(jnblig-1+5)=nbte
    zi(jnblig-1+6)=nbgd
!
!
!     OBJET .OPTTE :
!     -------------------
    call jeveuo('&CATA.TE.OPTT2', 'L', joptt2)
    call jelira('&CATA.TE.OPTT2', 'LONMAX', n, kbid)
    call wkvect('&CATA.TE.OPTTE', 'G V I', nbte*nbop, joptte)
    do 2,ioptte=1,n/2
    iop=zi(joptt2-1+2*(ioptte-1)+1)
    ite=zi(joptt2-1+2*(ioptte-1)+2)
    if (iop .eq. 0 .or. ite .eq. 0) goto 2
    zi(joptte-1+(ite-1)*nbop+iop)=ioptte
    2 end do
    call jedetr('&CATA.TE.OPTT2')
!
!
!
    call jedema()
end subroutine
