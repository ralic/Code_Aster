subroutine gverig(noma, nocc, chfond, taillr, config,&
                  lobj2, nomno, coorn, trav1, trav2,&
                  trav3, trav4)
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
! FONCTION REALISEE:
!
!     MOTS CLE FACTEUR THETA:
!
!     POUR CHAQUE NOEUD DU FOND DE FISSURE GAMM0 ON RECUPERE
!     LE TRIPLET ( MODULE(THETA), R_INF, R_SUP )
!
!     PUIS ON VERIFIE:
!                     QUE LES NOMS DE GROUPE OU D'ELEMENTS (NOEUD)
!                     APPARTIENNENT BIEN AU MAILLAGE ET A GAMMA0
!
!                     QU'IL N'Y A PAS DUPLICATION DES ENTITES
!
!                     QUE GAMM0 EST COMPLET
!
!                  ---------------------------------
!
!
!     ------------------------------------------------------------------
! ENTREE:
!        NOMA   : NOM DU MAILLAGE
!        NOCC   : NOMBRE D'OCCURENCES
!        NOMNO  : NOM DE L'OBJET CONTENANT LES NOMS DES NOEUDS
!        CHFOND : NOMS DES NOEUDS
!        TAILLR : TAILLES DE MAILLES CONNECTEES AUX NOEUDS
!        CONFIG : CONFIGURATION DE LA FISSURE
!        COORN  : NOM DE L'OBJET CONTENANT LES COORDONNEES DES NOEUDS
!        LOBJ2  : NOMBRE DE NOEUDS DE GAMM0
!
! SORTIE:
!        R_INF         ( OBJET TRAV1 )
!        R_SUP         ( OBJET TRAV2 )
!        MODULE(THETA) ( OBJET TRAV3 )
!        ABSC_CURV     ( OBJET TRAV4 )
!     ------------------------------------------------------------------
!
    include 'jeveux.h'
!
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/fointe.h'
    include 'asterfort/gabscu.h'
    include 'asterfort/getvem.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/lxlgut.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mesr.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=24) :: chfond, grpno, nomno, coorn, taillr
    character(len=24) :: trav, trav0, trav1, trav2, trav3, trav4, trav5
    character(len=8) :: config, noma, noeud, noeud1, k8b
    character(len=8) :: nompar(1), rinff, rsupf, thetf
    character(len=16) :: motfac, nomcmd, k16b
!
    integer :: jjj, ngro, nent, nsom, iocc, nocc, ndim, lobj2, nbmof
    integer :: igr, ngr, nno, iret, nbpar, ito, nto, noui, jjj2, l2, l, n1
    integer :: nbm, nbmf, iadrno, iatmno, iadrco, iadrt0, iadrt1
    integer :: canoeu, nbre, iadrt2, iadrt3, iadabs, i, iadr, ier, j, num
!
    real(kind=8) :: maxtai, mintai, rinf, rsup, thet, xl, valpar(1), valres
!
    character(len=24) :: valk(2)
    character(len=8) :: k8bid
    real(kind=8) :: valr(2)
    integer :: iarg
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
!
    call jeveuo(chfond, 'L', iadrno)
    call jeveuo(coorn, 'L', iadrco)
!
    call getres(k8b, k16b, nomcmd)
    if (nomcmd .eq. 'CALC_G') then
        motfac = 'THETA'
    else
        motfac = 'THETA_3D'
    endif
    l = len(motfac)
    l2 = lxlgut(motfac)
    nbre = 0
!
! OBJET DEFINISSANT LES GROUP_NO DU MAILLAGE
!
    grpno = noma//'.GROUPENO'
!
! ALLOCATION DE 3 OBJETS DE TRAVAIL
!
    trav0 = '&&VERIFG.GAM0'//'           '
    trav1 = '&&VERIFG.RINF'//'           '
    trav2 = '&&VERIFG.RSUP'//'           '
    trav3 = '&&VERIFG.THET'//'           '
    call wkvect(trav0, 'V V K8', lobj2, iadrt0)
    call wkvect(trav1, 'V V R', lobj2, iadrt1)
    call wkvect(trav2, 'V V R', lobj2, iadrt2)
    call wkvect(trav3, 'V V R', (nbre+1)*lobj2, iadrt3)
!
!
    if (nomcmd .ne. 'CALC_G') then
        do 1 iocc = 1, nocc
!
            call getvtx(motfac(1:l), 'TOUT', iocc, iarg, 0,&
                        k8bid, noui)
            call getvem(noma, 'GROUP_NO', motfac(1:l), 'GROUP_NO', iocc,&
                        iarg, 0, k8bid, ngro)
            call getvem(noma, 'NOEUD', motfac(1:l), 'NOEUD', iocc,&
                        iarg, 0, k8bid, nent)
            nsom = ngro + nent + noui
            if (nsom .eq. ngro) then
                ngro = -ngro
            else if (nsom.eq.nent) then
                nent = -nent
            else if (nsom.eq.noui) then
                noui = -noui
            endif
!
 1      continue
!
        ndim = max(ngro,nent)
        ndim = max(ndim,noui)
    else
        ndim=1
    endif
!
! ALLOCATION D'UN AUTRE OBJET DE TRAVAIL
!
    trav = '&&VERIFG.'//motfac(1:l)
    trav5 = '&&VERIFG.'//motfac(1:l2)//'2'
    call wkvect(trav, 'V V K8', ndim, jjj)
    call wkvect(trav5, 'V V K24', ndim, jjj2)
!
    nbpar = 1
    nompar(1) = 'ABSC'
!
!     CALCUL DES ABSCISSES CURVILIGNES LE LONG DU FOND DE FISSURE
!
    call gabscu(lobj2, coorn, nomno, chfond, xl,&
                trav4)
    call jeveuo(trav4, 'L', iadabs)
!
    do 2 iocc = 1, nocc
!
        call getvr8(motfac(1:l), 'MODULE', iocc, iarg, ndim,&
                    thet, nbm)
        if (nomcmd .eq. 'CALC_G' .and. nbm .ne. 1) then
            thet = 1.d0
        endif
        call getvr8(motfac(1:l), 'R_INF', iocc, iarg, ndim,&
                    rinf, nbm)
        call getvr8(motfac(1:l), 'R_SUP', iocc, iarg, ndim,&
                    rsup, nbm)
        if (nbm .ne. 0 .and. rsup .le. rinf) then
            call u2mess('F', 'RUPTURE1_6')
        endif
        call getvid(motfac(1:l), 'MODULE_FO', iocc, iarg, ndim,&
                    thetf, nbmof)
        call getvid(motfac(1:l), 'R_INF_FO', iocc, iarg, ndim,&
                    rinff, nbmf)
        call getvid(motfac(1:l), 'R_SUP_FO', iocc, iarg, ndim,&
                    rsupf, nbmf)
!
!       RECUPERATION DE RINF ET DE RSUP DANS LA SD FOND_FISS
        if (nbm .eq. 0 .and. nbmf .eq. 0) then
!
            if (config .eq. 'DECOLLEE') call u2mess('F', 'RUPTURE1_7')
            call jeveuo(taillr, 'L', iatmno)
            maxtai = 0.d0
            mintai = zr(iatmno)
            do 11 j = 1, lobj2
                maxtai = max(maxtai,zr(iatmno-1+j))
                mintai = min(mintai,zr(iatmno-1+j))
11          continue
            rinf = 2*maxtai
            rsup = 4*maxtai
            valr(1) = rinf
            valr(2) = rsup
            call u2mesr('I', 'RUPTURE1_5', 2, valr)
            valr(1) = mintai
            valr(2) = maxtai
            if (maxtai .gt. 2*mintai) call u2mesr('A', 'RUPTURE1_16', 2, valr)
        endif
!
!
! MOT CLE TOUT OU COMMANDE 'CALC_G'
!
        if (nomcmd .eq. 'CALC_G') then
            nto=1
        else
            call getvtx(motfac(1:l), 'TOUT', iocc, iarg, ndim,&
                        zk8(jjj), nto)
        endif
!
        do 100 ito = 1, nto
            do 150 j = 1, lobj2
                zk8(iadrt0 + j - 1) = zk8(iadrno + j - 1)
                noeud = zk8(iadrno + j - 1)
                if (nbmf .ne. 0) then
                    call jenonu(jexnom(nomno, zk8(iadrno+j-1)), num)
                    valpar(1) = zr(iadabs + j - 1)
                    call fointe('FM', rinff, nbpar, nompar, valpar,&
                                valres, ier)
                    zr(iadrt1 + j - 1) = valres
                    call fointe('FM', rsupf, nbpar, nompar, valpar,&
                                valres, ier)
                    zr(iadrt2 + j - 1) = valres
                    if (zr(iadrt2 + j - 1) .le. zr(iadrt1 + j - 1)) then
                        call u2mess('F', 'RUPTURE1_6')
                    endif
                    if (nbmof .ne. 0) then
                        call fointe('FM', thetf, nbpar, nompar, valpar,&
                                    valres, ier)
                    else
                        valres = 1.d0
                    endif
                    zr(iadrt3 + j - 1) = valres
                else
                    zr(iadrt1 + j - 1) = rinf
                    zr(iadrt2 + j - 1) = rsup
                    zr(iadrt3 + j - 1) = thet
                endif
150          continue
100      continue
        if (nomcmd .eq. 'CALC_G') goto 2
!
!
!
! MOT CLE GROUP_NO
!
! LE GROUP_NO DOIT APPARTENIR AU MAILLAGE
!
        call getvem(noma, 'GROUP_NO', motfac(1:l), 'GROUP_NO', iocc,&
                    iarg, ndim, zk24(jjj2), ngr)
!
        do 3 igr = 1, ngr
!
            call jeexin(jexnom(grpno, zk24(jjj2+igr-1)), iret)
            if (iret .eq. 0) then
                valk(1) = zk24(jjj2+igr-1)
                valk(2) = noma
                call u2mesk('F', 'RUPTURE1_8', 2, valk)
            else
! LES NOEUDS DE CE GROUP_NO DOIVENT APPARTENIR A GAMMO
!
                call jelira(jexnom(grpno, zk24(jjj2+igr-1)), 'LONUTI', n1, k8bid)
                call jeveuo(jexnom(grpno, zk24(jjj2+igr-1)), 'L', iadr)
                do 4 j = 1, n1
                    call jenuno(jexnum(nomno, zi(iadr+j-1)), noeud1)
                    canoeu = 0
                    do 5 i = 1, lobj2
                        noeud = zk8(iadrno + i - 1)
                        if (noeud .eq. noeud1) then
                            canoeu = canoeu + 1
                            zk8(iadrt0 + i - 1) = noeud1
                            if (nbmf .ne. 0) then
                                call jenonu(jexnom(nomno, zk8(iadrno+i- 1)), num)
                                valpar(1) = zr(iadabs + j - 1)
                                call fointe('FM', rinff, nbpar, nompar, valpar,&
                                            valres, ier)
                                zr(iadrt1 + i - 1) = valres
                                call fointe('FM', rsupf, nbpar, nompar, valpar,&
                                            valres, ier)
                                zr(iadrt2 + i - 1) = valres
                                if (zr(iadrt2 + j - 1) .le. zr(iadrt1 + j - 1)) then
                                    call u2mess('F', 'RUPTURE1_6')
                                endif
                                call fointe('FM', thetf, nbpar, nompar, valpar,&
                                            valres, ier)
                                zr(iadrt3 + i - 1) = valres
                            else
                                zr(iadrt1 + j - 1) = rinf
                                zr(iadrt2 + j - 1) = rsup
                                zr(iadrt3 + j - 1) = thet
                            endif
                        endif
 5                  continue
                    if (canoeu .eq. 0) then
                        call u2mesk('F', 'RUPTURE0_15', 1, noeud1)
                    endif
 4              continue
            endif
 3      continue
!
! MOT CLE NOEUD
        call getvem(noma, 'NOEUD', motfac(1:l), 'NOEUD', iocc,&
                    iarg, ndim, zk8(jjj), nno)
!
        do 6 i = 1, nno
!
            call jenonu(jexnom(nomno, zk8(jjj+i-1)), iret)
            if (iret .eq. 0) then
                valk(1) = zk8(jjj+i-1)
                valk(2) = noma
                call u2mesk('F', 'RUPTURE0_14', 2, valk)
            else
! LES NOEUDS DOIVENT APPARTENIR A GAMMO
                call jenuno(jexnum(nomno, iret), noeud1)
                canoeu = 0
                do 7 j = 1, lobj2
                    noeud = zk8(iadrno+j-1)
                    if (noeud1 .eq. noeud) then
                        canoeu = canoeu + 1
                        zk8(iadrt0 + j - 1) = noeud1
                        if (nbmf .ne. 0) then
                            call jenonu(jexnom(nomno, zk8(iadrno+j-1)), num)
                            valpar(1) = zr(iadabs + j - 1)
                            call fointe('FM', rinff, nbpar, nompar, valpar,&
                                        valres, ier)
                            zr(iadrt1 + j - 1) = valres
                            call fointe('FM', rsupf, nbpar, nompar, valpar,&
                                        valres, ier)
                            zr(iadrt2 + j - 1) = valres
                            if (zr(iadrt2 + j - 1) .le. zr(iadrt1 + j - 1)) then
                                call u2mess('F', 'RUPTURE1_6')
                            endif
                            call fointe('FM', thetf, nbpar, nompar, valpar,&
                                        valres, ier)
                            zr(iadrt3 + j - 1) = valres
                        else
                            zr(iadrt1 + j - 1) = rinf
                            zr(iadrt2 + j - 1) = rsup
                            zr(iadrt3 + j - 1) = thet
                        endif
                    endif
 7              continue
                if (canoeu .eq. 0) then
                    call u2mesk('F', 'RUPTURE0_15', 1, zk8(iadrno+j-1))
                endif
            endif
 6      continue
 2  end do
!
!
! VERIFICATION QUE GAMM0 EST COMPLET
!
    do 8 i = 1, lobj2
        if (zk8(iadrno+ i -1) .ne. zk8(iadrt0+i-1)) then
            call u2mess('F', 'RUPTURE1_9')
        endif
 8  end do
!
! DESTRUCTION D'OBJETS DE TRAVAIL
!
    call jedetr(trav)
    call jedetr(trav0)
    call jedetr(trav5)
!
    call jedema()
end subroutine
