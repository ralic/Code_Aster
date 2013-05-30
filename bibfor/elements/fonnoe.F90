subroutine fonnoe(resu, noma, cnxinv, nomobj, typfon,&
                  nbnoff)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/ismali.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/reliem.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=6) :: nomobj
    character(len=8) :: resu, noma, typfon
    character(len=19) :: cnxinv
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
! FONCTION REALISEE:
!
!     CONSTRUCTION DU FOND DE FISSURE A PARTIR DE NOEUDS OU DE GROUPES
!     DE NOEUDS RENSEIGNES DANS DEFI_FOND_FISS
!
!     ENTREES:
!        RESU       : NOM DU CONCEPT RESULTAT DE L'OPERATEUR
!        NOMA       : NOM DU MAILLAGE
!        CNXINV     : CONNECTIVITE INVERSE
!        NOMOBJ     : NOM DU VECTEUR CONTENANT LES DONNEES RELATIVES
!                     AUX NOEUDS
!        TYPFON     : TYPE DE FOND IL PEUT VALOIR OUVERT/FERME/INF/SUP
!     SORTIES:
!        NBNOFF     : NOMBRE DE NOEUDS EN FOND DE FISSURE
!-----------------------------------------------------------------------
!
!
    integer :: jdrvlc, jcncin, jnoe1, jnoe2, jadr
    integer :: iatyma, jtyp
    integer :: k, ityp, j
    integer :: numa, numb
    integer :: jjj, ino, it, nbnoff, nbma, nbmb, na, nb, adra, adrb
    integer :: iret
    character(len=6) :: nompro
    character(len=8) :: k8b, noeud, type, motcle(2), typmcl(2), typmp, valk(8)
    character(len=8) :: typm
    character(len=24) :: noeord, trav
    character(len=24) :: entree, obtrav
    logical :: lfon, test
! DEB-------------------------------------------------------------------
    call jemarq()
    nompro = 'FONNOE'
!
! ---  TYPE DE FOND TRAITE
!      -----------------------------------
!
    lfon = .false.
    if (typfon .eq. 'INF') then
        noeord = resu//'.FOND_INF.NOEU'
        lfon = .true.
    else if (typfon.eq.'SUP') then
        noeord = resu//'.FOND_SUP.NOEU'
        lfon = .true.
    else
        noeord = resu//'.FOND.NOEU'
    endif
!
! ---  RECUPERATIONS RELATIVES AU MAILLAGE
!      -----------------------------------
!
    call jeveuo(noma//'.TYPMAIL', 'L', iatyma)
!
    call jeveuo(jexatr(cnxinv, 'LONCUM'), 'L', jdrvlc)
    call jeveuo(jexnum(cnxinv, 1), 'L', jcncin)
!
! --- CALCUL DU NOMBRE DE NOEUDS
    motcle(1) = 'GROUP_NO'
    motcle(2) = 'NOEUD'
    typmcl(1) = 'GROUP_NO'
    typmcl(2) = 'NOEUD'
    trav = '&&'//nompro//'.NOEUD'
    call reliem(' ', noma, 'NO_NOEUD', 'FOND_FISS', 1,&
                2, motcle, typmcl, trav, nbnoff)
    obtrav = '&&'//nomobj//'.NOEUD'
    call jeexin(obtrav, iret)
    if (iret .ne. 0) then
        call jeveuo(obtrav, 'L', jjj)
        entree = noma//'.NOMNOE'
    else
        obtrav = '&&'//nomobj//'.GROUP_NO'
        call jeveuo(obtrav, 'L', jjj)
        entree = noma//'.GROUPENO'
        call jeveuo(jexnom(entree, zk24(jjj)), 'L', jadr)
    endif
!
    typmp ='        '
    it = 1
    do 210 ino = 1, nbnoff-1
!       NUMERO DU NOEUD INO ET INO+1
        if (iret .ne. 0) then
            call jenonu(jexnom(entree, zk24(jjj-1 + ino )), na)
            call jenonu(jexnom(entree, zk24(jjj-1 + ino+1)), nb)
        else
            na = zi(jadr-1 + ino)
            nb = zi(jadr-1 + ino+1)
        endif
!       NOMBRE DE MAILLES CONNECTEES AU NOEUD INO ET INO+1
        nbma = zi(jdrvlc-1 + na+1) - zi(jdrvlc-1 + na)
        nbmb = zi(jdrvlc-1 + nb+1) - zi(jdrvlc-1 + nb)
!       NUMERO DE LA PREMIERE MAILLE CONNECTEE AU NOEUD INO ET INO+1
        adra = zi(jdrvlc-1 + na)
        adrb = zi(jdrvlc-1 + nb)
!       RECHERCHE DES MAILLES COMMUNES
        do 212 j = 1, nbma
!         POUR LA J IEME MAILLE RELATIVE AU NOEUD INO
            numa = zi(jcncin-1 + adra+j-1)
            do 214 k = 1, nbmb
!           POUR LA K IEME MAILLE RELATIVE AU NOEUD INO+1
                numb = zi(jcncin-1 + adrb+k-1)
                ityp = iatyma-1+numb
!           RECUPERATION DE LA KIEME MAILLE RELATIVE AU NOEUD INO+1
                call jenuno(jexnum('&CATA.TM.NOMTM', zi(ityp)), type)
!           RECUPERATION DE LA K IEME MAILLE RELATIVE AU NOEUD INO+1
                if (type(1:3) .eq. 'SEG ') then
                    if ((it.gt.1) .and. (type.ne.typmp)) then
                        call u2mess('F', 'RUPTURE0_60')
                    endif
                    typmp = type
                    it = it + 1
!             DANS LE CAS LFON (INF ET SUP) IL EST NECESSAIRE D'AVOIR
!             DES MAILLES SEG SI CE N'EST PAS LE CAS ON ECHOUE
                    if (lfon .and. numa .eq. numb) goto 216
                endif
!           DANS LE CAS (OUVERT OU FERME) IL N'EST NECESSAIRE PAS
!           D'AVOIR DES MAILLES SEG
                if (.not.lfon .and. numa .eq. numb) goto 216
214          continue
212      continue
        call u2mess('F', 'RUPTURE0_66')
216      continue
        if (typmp .eq. '        ') then
            numa = zi(jcncin-1 + adra)
            ityp = iatyma-1+numa
            call jenuno(jexnum('&CATA.TM.NOMTM', zi(ityp)), type)
            test = ismali(type)
            if (test) then
                typmp = 'NOE2'
            else
                typmp = 'NOE3'
            endif
        endif
        call jedetr(trav)
210  end do
!
!       CONSTRUCTION DES NOEUDS DU FOND SI "NOEUD" RENSEIGNE
!     -------------------------------------------------------------
!
    call wkvect(noeord, 'G V K8', nbnoff, jnoe1)
    if (iret .ne. 0) then
        do 213 ino = 1, nbnoff
            zk8(jnoe1-1 + ino) = zk24(jjj-1 + ino)(1:8)
213      continue
!
    else
        call jeveuo(jexnom(entree, zk24(jjj)), 'L', jadr)
        do 2223 ino = 1, nbnoff
            call jenuno(jexnum(noma//'.NOMNOE', zi(jadr-1 + ino)), noeud)
            zk8(jnoe1-1 + ino) = noeud
2223      continue
    endif
!
!
!
!     CONSTRUCTION DU TYPE DE MAILLES DES NOEUDS DU FOND DE FISSURE
!     -------------------------------------------------------------
!
    if (nbnoff .eq. 1) typmp = '        '
    call jeexin(resu//'.FOND.TYPE', iret)
    if (iret .eq. 0) then
        call wkvect(resu//'.FOND.TYPE', 'G V K8', 1, jnoe2)
        zk8(jnoe2) = typmp
    else
        call jeveuo(resu//'.FOND.TYPE', 'L', jtyp)
        typm=zk8(jtyp)
        if (typmp .ne. typm) then
            valk(1) = typmp
            valk(2) = typm
            call u2mesk('F', 'RUPTURE0_68', 2, valk)
        endif
    endif
    call jelira(noeord, 'LONUTI', nbnoff, k8b)
!
    call jedema()
end subroutine
