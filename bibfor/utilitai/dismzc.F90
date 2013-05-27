subroutine dismzc(questi, nomobz, repi, repkz, ierd)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: repi, ierd
    character(len=*) :: questi, nomobz, repkz
! ----------------------------------------------------------------------
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
!     --     DISMOI( 'Z_CST', MODELE, ... )
!    IN:
!       QUESTI : 'Z_CST'
!       NOMOBZ : NOM D'UN OBJET DE TYPE LIGREL
!    OUT:
!       REPI   : REPONSE ( SI ENTIERE )
!       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
! ----------------------------------------------------------------------
!
    character(len=1) :: k1bid
    character(len=8) :: ma, typma
    character(len=19) :: nolig
    character(len=24) :: nema
    character(len=32) :: repk
    integer :: iadime, iamaco, ianbno, ianoma, iatypm, idnema, ier
    integer :: ii, ilmaco, ima, ino, iocc, itypm, jcoor
    integer :: jima, jnbno, nbma, nbnoma, nbnot, nbpt, numail
    integer :: nunoel, nunota, nutioc, numglm
    real(kind=8) :: z1
! -----  FONCTIONS FORMULES
!     NUMGLM(IMA,INO)=NUMERO GLOBAL DU NOEUD INO DE LA MAILLE IMA
!                     IMA ETANT UNE MAILLE DU MAILLAGE.
    numglm(numail,ino)=zi(iamaco-1+zi(ilmaco+numail-1)+ino-1)
! --------------------------------------------------------------------
    call jemarq()
    call assert(questi.eq.'Z_CST')
!
    nolig = nomobz
    repk = ' '
    repi = 0
    ierd = 0
!
! --- LE MODELE
!
    call jeveuo(nolig//'.LGRF', 'L', ianoma)
    call jelira(nolig//'.LIEL', 'NUTIOC', nutioc, k1bid)
    nema = nolig//'.NEMA'
    call jeexin(nema, ier)
!
! --- LE MAILLAGE
!
    ma = zk8(ianoma-1+1)
    call jeveuo(ma//'.CONNEX', 'L', iamaco)
    call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', ilmaco)
    call jeveuo(ma//'.COORDO    .VALE', 'L', jcoor)
    call jeveuo(ma//'.DIME', 'L', iadime)
    nbnoma = zi(iadime-1+1)
!
! --- ON CREE UN TABLEAU DONT LA COMPOSANTE I VAUDRA 1 SI LE NOEUD I
!     APPARTIENT AU MODELE
!
    call wkvect('&&DISMZC.TRAV.NOEUDS', 'V V I', nbnoma, jnbno)
!
    call jeveuo('&CATA.TE.TYPEMA', 'L', iatypm)
    call jeveuo('&CATA.TM.NBNO', 'L', ianbno)
!
    do 10 iocc = 1, nutioc
!
        call jelira(jexnum(nolig//'.LIEL', iocc), 'LONMAX', nbma, k1bid)
        call jeveuo(jexnum(nolig//'.LIEL', iocc), 'L', jima)
        typma = zk8(iatypm-1+zi(jima+nbma-1))
        call jenonu(jexnom('&CATA.TM.NOMTM', typma), itypm)
        nbpt = zi(ianbno-1+itypm)
        nbma = nbma - 1
!
        do 15 ii = 1, nbma
!
            numail = zi(jima+ii-1)
!
            if (numail .lt. 0) then
! --------- MAILLE TARDIVE: ON RECUPERE NEMA
                if (ier .eq. 0) call u2mess('F', 'UTILITAI_71')
                ima = -numail
                call jeveuo(jexnum(nema, ima), 'L', idnema)
                call jelira(jexnum(nema, ima), 'LONMAX', nbnot, k1bid)
! --------- NEMA NOUS DONNE DIRECTEMENT LE NUMERO DU NOEUD
                nbnot = nbnot-1
                do 22 ino = 1, nbnot
                    nunota = zi(idnema+ino-1)
                    if (nunota .lt. 0) then
                        call u2mess('F', 'UTILITAI_72')
                    endif
                    zi(jnbno+nunota-1) = 1
22              continue
            else
!
! --------- RECUPERATION DU NOMBRE DE NOEUDS ET DE LA LISTE
!           DES NOEUDS DE LA MAILLE NUMAIL
!
                do 20 ino = 1, nbpt
                    nunoel = numglm(numail,ino)
                    zi(jnbno+nunoel-1) = 1
20              continue
            endif
15      continue
10  end do
!
! --- ON RECUPERE LA COORDONNEE Z DU PREMIER NOEUD DU MAILLAGE
!     CONTENU DANS LE MODELE POUR TESTER LES Z SUIVANTS
!
    do 24 ino = 1, nbnoma
        if (zi(jnbno+ino-1) .ne. 0) then
            z1 = zr(jcoor-1+3*(ino-1)+3)
            goto 26
        endif
24  end do
26  continue
!
    do 25 ino = 1, nbnoma
        if (zi(jnbno+ino-1) .ne. 0) then
            if (zr(jcoor-1+3*(ino-1)+3) .ne. z1) goto 30
        endif
25  end do
    repk = 'OUI'
    goto 9999
30  continue
    repk = 'NON'
!
9999  continue
    repkz=repk
    call jedetr('&&DISMZC.TRAV.NOEUDS')
    call jedema()
end subroutine
