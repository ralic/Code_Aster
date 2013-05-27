subroutine rfnoch()
    implicit none
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     OPERATEUR "RECU_FONCTION"   MOT CLE "NOEUD_CHOC"
!     ------------------------------------------------------------------
    include 'jeveux.h'
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterc/getvtx.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/foattr.h'
    include 'asterfort/focrch.h'
    include 'asterfort/foimpr.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/ordonn.h'
    include 'asterfort/titre.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/utnono.h'
    integer :: ifm, niv
    integer :: ibid, n, nc, ng, int, ind, nsst, iret, jrefe1
    character(len=8) :: k8b, noma, sst, basemo, noeud, intitu
    character(len=24) :: valk(2), nogno
    character(len=16) :: parax, paray, nomcmd, typcon
    character(len=19) :: listr, nomfon, resu
    integer :: iarg
!     ------------------------------------------------------------------
    call jemarq()
!
    call infmaj()
    call infniv(ifm, niv)
!
    call getres(nomfon, typcon, nomcmd)
!
    call getvtx(' ', 'INTITULE', 0, iarg, 1,&
                intitu, int)
    call getvid(' ', 'RESU_GENE', 0, iarg, 1,&
                resu, n)
    call getvtx(' ', 'PARA_X', 0, iarg, 1,&
                parax, n)
    call getvtx(' ', 'PARA_Y', 0, iarg, 1,&
                paray, n)
    call getvid(' ', 'LIST_PARA', 0, iarg, 1,&
                listr, ind)
    call getvtx(' ', 'SOUS_STRUC', 0, iarg, 1,&
                sst, nsst)
!
    call getvtx(' ', 'NOEUD_CHOC', 0, iarg, 1,&
                noeud, nc)
    call getvtx(' ', 'GROUP_NO_CHOC', 0, iarg, 1,&
                nogno, ng)
!
    if (nc .ne. 0) then
!
        call focrch(nomfon, resu, noeud, parax, paray,&
                    'G', int, intitu, ind, listr,&
                    sst, nsst, iret)
!
    else
!
        call jeveuo(resu//'.REFD', 'L', jrefe1)
        basemo = zk24(jrefe1+4)(1:8)
        call dismoi('F', 'NOM_MAILLA', basemo, 'RESULTAT', ibid,&
                    noma, iret)
!
        call utnono(' ', noma, 'NOEUD', nogno, noeud,&
                    iret)
        if (iret .eq. 10) then
            call u2mesk('F', 'ELEMENTS_67', 1, nogno)
        else if (iret.eq.1) then
            valk(1) = nogno
            valk(2) = noeud
            call u2mesk('A', 'SOUSTRUC_87', 2, valk)
        endif
!
        call focrch(nomfon, resu, noeud, parax, paray,&
                    'G', int, intitu, ind, listr,&
                    sst, nsst, iret)
    endif
!
    call foattr(' ', 1, nomfon)
!
!     --- VERIFICATION QU'ON A BIEN CREER UNE FONCTION ---
!         ET REMISE DES ABSCISSES EN ORDRE CROISSANT
    call ordonn(nomfon, 0)
!
    call titre()
    if (niv .gt. 1) call foimpr(nomfon, niv, ifm, 0, k8b)
!
    call jedema()
end subroutine
