subroutine relagm(mo, ma, nm, nl, newn,&
                  oldn)
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
!
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: mo, ma
    integer :: nm, nl, newn(*), oldn(*)
! ----------------------------------------------------------------------
!     BUT:
!           RENUMEROTER  LES NOEUDS TARDIFS DU MAILLAGE
!           (NOEUDS DE LAGRANGE PROVENANT DES SOUS_STRUCTURES)
!           (CES NOEUDS DOIVENT EN EFFET TOUJOURS ENCADRER LES
!            NOEUDS PHYSIQUES CONTRAINTS)
!
!     IN/OUT:     CF. ROUTINE RENUNO
!     ------
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    integer :: nbnoma, nbnore
    character(len=8) :: kbid
    logical :: exilag
!
!
!
!     -- SI LE MODELE N'A PAS DE SOUS-STRUCTURES ON RESSORT :
!     --------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iaavap, iamail, iaoldt, iasssa, iatypl, ico
    integer :: icol, ierd, il, ima, ino, inomax, inomin
    integer :: iold, iprem, iret, itypi, nbnm, nbsma, nbssa
!
!-----------------------------------------------------------------------
    call jemarq()
    call dismoi('F', 'NB_SS_ACTI', mo, 'MODELE', nbssa,&
                kbid, ierd)
    call dismoi('F', 'NB_SM_MAILLA', mo, 'MODELE', nbsma,&
                kbid, ierd)
    if (nbssa .gt. 0) then
        call jeveuo(mo//'.MODELE    .SSSA', 'L', iasssa)
        call jeexin(ma//'.TYPL', iret)
        if (iret .gt. 0) call jeveuo(ma//'.TYPL', 'L', iatypl)
    else
        goto 9999
    endif
!
!     -- L'OBJET SUIVANT CONTIENDRA EN REGARD DES NUMEROS DE NOEUDS
!        PHYSIQUES DU MAILLAGE UN ENTIER (+1, OU 0) POUR DIRE
!        SI CE NOEUD EST PRECEDE OU SUIVI (+1) DE NOEUDS DE LAGRANGE :
    call wkvect('&&RELAGM.AVAP', 'V V I', nm, iaavap)
!
!
!     -- .OLDT EST UN .OLDN TEMPORAIRE QUE L'ON RECOPIERA A LA FIN
    nbnoma= nm+nl
    call wkvect('&&RELAGM.OLDT', 'V V I', nbnoma, iaoldt)
!
!
!     -- BOUCLE SUR LES (SUPER)MAILLES
!     --------------------------------
    icol= 0
    do 21, ima = 1, nbsma
    exilag=.false.
    if (zi(iasssa-1+ima) .eq. 1) then
        call jeveuo(jexnum(ma//'.SUPMAIL', ima), 'L', iamail)
        call jelira(jexnum(ma//'.SUPMAIL', ima), 'LONMAX', nbnm, kbid)
!
!         -- ON REGARDE LES NUMEROS PHYSIQUES MAX ET MIN DE LA MAILLE:
        iprem =0
        do 22, i=1,nbnm
        ino=zi(iamail-1+i)
        if ((ino.gt.0) .and. (ino.le.nm)) then
            iprem=iprem+1
            if (iprem .eq. 1) then
                inomax=ino
                inomin=ino
            endif
            if (newn(ino) .gt. newn(inomax)) then
                inomax=ino
            endif
            if (newn(ino) .lt. newn(inomin)) then
                inomin=ino
            endif
        else
            icol=icol+1
        endif
22      continue
!
!
!         -- ON SE SERT DE LA FIN DU VECTEUR .NEWN POUR STOCKER EN FACE
!         DE CHAQUE LAGRANGE LE NUMERO DU NOEUD PHYSIQUE PRES DUQUEL
!         ON DOIT LE DEPLACER (+INOMAX : DERRIERE) (-INOMIN : DEVANT)
!
        do 23, i=1,nbnm
        ino=zi(iamail-1+i)
        if (ino .gt. nm) then
            exilag=.true.
            itypi=zi(iatypl-1+ino-nm)
            if (itypi .eq. -1) then
                newn(ino)=-inomin
            else if (itypi.eq.-2) then
                newn(ino)= inomax
            else
                call assert(.false.)
            endif
        endif
23      continue
!
        if (exilag) then
            zi(iaavap-1+inomin)= 1
            zi(iaavap-1+inomax)= 1
        endif
!
    endif
!
    21 end do
!
    if (icol .eq. 0) goto 9999
!
!
!     -- ON REMPLIT .OLDT AVEC LES NOEUDS DE .OLDN ET LES LAGRANGES:
!     -------------------------------------------------------------
    ico= 0
    do 31, i=1,nm
    iold=oldn(i)
    if (iold .eq. 0) goto 32
    if (zi(iaavap-1+iold) .eq. 1) then
!
        do 33,il=1,nl
        if (newn(nm+il) .eq. -iold) then
            ico = ico+1
            zi(iaoldt-1+ico)=nm+il
        endif
33      continue
!
        ico = ico+1
        zi(iaoldt-1+ico)=iold
!
        do 34,il=1,nl
        if (newn(nm+il) .eq. +iold) then
            ico = ico+1
            zi(iaoldt-1+ico)=nm+il
        endif
34      continue
!
    else
        ico = ico+1
        zi(iaoldt-1+ico)=iold
    endif
    31 end do
32  continue
    nbnore= ico
!
!     -- ON RECOPIE .OLDT DANS .OLDN ET ON REMET .NEWN A JOUR :
!     ---------------------------------------------------------
    do 41, i=1,nbnoma
    newn(i) =0
    oldn(i) =0
    41 end do
!
    do 42, i=1,nbnore
    oldn(i) = zi(iaoldt-1+i)
    newn(zi(iaoldt-1+i)) =i
    42 end do
!
!
9999  continue
!
    call jedetr('&&RELAGM.AVAP')
    call jedetr('&&RELAGM.OLDT')
!
    call jedema()
end subroutine
