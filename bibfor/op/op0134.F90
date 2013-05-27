subroutine op0134()
    implicit   none
!     ------------------------------------------------------------------
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
!     CALCUL D'UNE FONCTION INTERPRETEE
!     ------------------------------------------------------------------
    include 'jeveux.h'
    include 'asterc/getres.h'
    include 'asterc/gettco.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/calcfo.h'
    include 'asterfort/calcna.h'
    include 'asterfort/foattr.h'
    include 'asterfort/foimpr.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/ordonn.h'
    include 'asterfort/titre.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: ifm, niv, n1, nbvalp, nbvalf, lvalp, lvalf, lnova, nbnova, lprol
    real(kind=8) :: rval
    logical :: compl
    character(len=8) :: k8b, nopn, nopf
    character(len=16) :: nomcmd, typres
    character(len=19) :: nomfon, nomfin, listp, listf, typco
    character(len=24) :: noparp, noparf, valk(3)
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
!
    call infmaj()
    call infniv(ifm, niv)
!
    call getres(nomfon, typres, nomcmd)
!
    call getvid(' ', 'FONCTION', 1, iarg, 1,&
                nomfin, n1)
    call gettco(nomfin, typco)
!
! --- LISTE DES VALEURS DU PARAMETRE
!
    call getvr8(' ', 'VALE_PARA', 1, iarg, 0,&
                rval, n1)
    if (n1 .ne. 0) then
        nbvalp = -n1
        call wkvect('&&OP0134.VALP', 'V V R', nbvalp, lvalp)
        call getvr8(' ', 'VALE_PARA', 1, iarg, nbvalp,&
                    zr(lvalp), n1)
    else
        call getvid(' ', 'LIST_PARA', 1, iarg, 1,&
                    listp, n1)
        call jeveuo(listp//'.VALE', 'L', lvalp)
        call jelira(listp//'.VALE', 'LONUTI', nbvalp, k8b)
    endif
!
! --- NAPPE OU FONCTION
!
    compl = .false.
    if (typco(1:7) .eq. 'FORMULE') then
        if (typco(1:9) .eq. 'FORMULE_C') compl = .true.
        call jelira(nomfin//'.NOVA', 'LONUTI', nbnova, k8b)
        call jeveuo(nomfin//'.NOVA', 'L', lnova)
        if (nbnova .eq. 1) then
            noparp = zk8(lnova)
        else if (nbnova .eq. 2) then
            noparp = zk8(lnova)
            noparf = zk8(lnova+1)
        endif
!
    else if (typco(1:8).eq.'FONCTION') then
        if (typco(1:10) .eq. 'FONCTION_C') compl = .true.
        nbnova = 1
        call jeveuo(nomfin//'.PROL', 'L', lprol)
        noparp = zk24(lprol+2)
!
    else if (typco(1:5).eq.'NAPPE') then
        nbnova = 2
        call jeveuo(nomfin//'.PROL', 'L', lprol)
        noparp = zk24(lprol+2)
        noparf = zk24(lprol+6)
!
    else
        call assert(.false.)
    endif
!
!
    if (nbnova .eq. 1) then
! ------------------------------------------------------------------
!                 FONCTION
! ------------------------------------------------------------------
        call calcfo(compl, nomfin, nomfon, nbvalp, zr(lvalp),&
                    noparp)
!
    else if (nbnova .eq. 2) then
! ------------------------------------------------------------------
!                 NAPPE
! ------------------------------------------------------------------
        call getvr8(' ', 'VALE_PARA_FONC', 1, iarg, 0,&
                    rval, n1)
        if (n1 .ne. 0) then
            nbvalf = -n1
            call wkvect('&&OP0134.VALF', 'V V R', nbvalf, lvalf)
            call getvr8(' ', 'VALE_PARA_FONC', 1, iarg, nbvalf,&
                        zr(lvalf), n1)
        else
            call getvid(' ', 'LIST_PARA_FONC', 1, iarg, 1,&
                        listf, n1)
            if (n1 .ne. 0) then
                call jeveuo(listf//'.VALE', 'L', lvalf)
                call jelira(listf//'.VALE', 'LONUTI', nbvalf, k8b)
            else
                call u2mess('F', 'FONCT0_49')
            endif
        endif
!
!        VERIFIER LA COHERENCE DES NOMS DES PARAMETRES
        call getvtx(' ', 'NOM_PARA', 1, iarg, 1,&
                    nopn, n1)
!        FACULTATIF
        if (n1 .ne. 0 .and. nopn .ne. noparp) then
            valk(1) = nomfin
            valk(2) = noparp
            valk(3) = nopn
            if (typco(1:7) .eq. 'FORMULE') then
                call u2mesk('F', 'FONCT0_58', 3, valk)
            else
                call u2mesk('F', 'FONCT0_59', 3, valk)
            endif
        endif
!
        call getvtx(' ', 'NOM_PARA_FONC', 1, iarg, 1,&
                    nopf, n1)
!        OBLIGATOIRE
        call assert(n1.eq.1)
        if (nopf .ne. noparf) then
            valk(1) = nomfin
            valk(2) = noparf
            valk(3) = nopf
            if (typco(1:7) .eq. 'FORMULE') then
                call u2mesk('F', 'FONCT0_60', 3, valk)
            else
                call u2mesk('F', 'FONCT0_61', 3, valk)
            endif
        endif
!
        call calcna(nomfin, nomfon, nbvalp, zr(lvalp), noparp,&
                    nbvalf, zr(lvalf), noparf)
!
    else
!
        call u2mess('F', 'FONCT0_48')
!
    endif
!
! --- SURCHARGE EVENTUELLE DU .PROL
!
    call foattr(' ', 1, nomfon)
!
! --- VERIFICATION QU'ON A BIEN CREER UNE FONCTION ---
!     ET REMISE DES ABSCISSES EN ORDRE CROISSANT
!
    call ordonn(nomfon, 0)
!
    call titre()
    if (niv .gt. 1) call foimpr(nomfon, niv, ifm, 0, listp)
!
    call jedema()
end subroutine
