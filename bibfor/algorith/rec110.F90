subroutine rec110(nomres, nomsqu, modgen)
    implicit none
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
!***********************************************************************
!-----------------------------------------------------------------------
!  BUT : < FUSIONNER LES NOEUDS D'INTERFACE D'UN SQUELETTE EXISTANT>
!
!
!-----------------------------------------------------------------------
!
! NOMRES  /I/ : NOM K8 DU MAILLAGE SQUELETTE A MODIFIER
! MODGEN  /I/ : NOM K8 DU MODELE GENERALISE
!
!
!
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/copisd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/mgutdm.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
!
!
!   PARAMETER : REPRESENTE LE NOMBRE MAX DE COMPOSANTES DE LA GRANDEUR
!   SOUS-JACENTE TRAITEE
!
    character(len=24) :: valk(4)
    character(len=8) :: nomres, nomsqu, modgen, tt, lintf, ljntf
    character(len=8) :: k8bid, nomnoe, crit, nomsst
    logical :: fusion
    integer :: iarg
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, i1, i2, iadres, ibid, igr, in
    integer :: incr, inew, iocc, iold, iposi, ireco, iret
    integer :: istac, jltns, jn, jncr, jposi, jstac, lconn
    integer :: lcoord, lcorr, lcort, ldime, lintd, linver, ljntd
    integer :: lrefe, lsk, lsk2, lstac, ltabi, ltabj, lvnew
    integer :: lvold, nbcoor, nbec, nbfuse, nbma, nbmoin, nbn
    integer :: nbnd, nbnd2, nbnew, nbni, nbnj, nbocc, nbreco
    integer :: nbstac, ndist, nnodes, nr, numero
    real(kind=8) :: dist, distij, prec, xii, xj, yii, yj
    real(kind=8) :: zii, zj
!-----------------------------------------------------------------------
    data tt      /'&&REC110'/
!-----------------------------------------------------------------------
!
    call jemarq()
!
! --- RECOPIE DE DES OBJETS SQUELETTE DANS LE NOUVEAU SQUELETTE ---
    call copisd('SQUELETTE', 'G', nomsqu, nomres)
!     IL FAUT MODIFIER .LTNS(1) :
    call jeveuo(nomres//'           .LTNS', 'E', jltns)
    zk24(jltns-1+1)(1:8)=nomres
!
    call jelira(nomres//'         .NOMSST', 'LONUTI', nbstac, k8bid)
    call jeveuo(nomres//'         .NOMSST', 'L', lstac)
    call jeveuo(nomres//'.COORDO    .VALE', 'E', lcoord)
    call jelira(nomres//'.COORDO    .VALE', 'LONUTI', nbcoor, k8bid)
    call jelira(nomres//'.INV.SKELETON   ', 'LONUTI', nbnd2, k8bid)
    call jeveuo(nomres//'.INV.SKELETON   ', 'E', lsk)
    nbnd = nbnd2/2
    lsk2 = lsk + nbnd
!
!     --- CREATION D'UNE LISTE DE CORRESPONDANCE ANCIEN/NOUVEAU NOEUD
    call wkvect(tt//'.CORRES', 'V V I', nbnd, lcort)
!     ET INVERSE
    call wkvect(tt//'.INVER', 'V V I', nbnd, linver)
    do 5 i = 1, nbnd
        zi(lcort-1+i) = i
        zi(linver-1+i) = i
 5  end do
!
!
!     --- TABLEAU DE LISTES DE NOEUDS POUR COMPARAISON ---
    call wkvect(tt//'.TABI', 'V V I', nbnd, ltabi)
    call wkvect(tt//'.TABJ', 'V V I', nbnd, ltabj)
!
! --- LECTURE DE RECO_GLOBAL ---
!
!
    call getvtx('RECO_GLOBAL', 'GROUP_NO_1', 1, iarg, 0,&
                k8bid, igr)
    if (igr .eq. 0) then
!
! --- ON FUSIONNE LES NOEUDS DE TOUTES LES INTERFACES DYNAMIQUES ---
!
!     --- LECTURE DE LA PRECISION
        call getvr8('RECO_GLOBAL', 'PRECISION', 1, iarg, 1,&
                    prec, ibid)
        call getvr8('RECO_GLOBAL', 'DIST_REFE', 1, iarg, 1,&
                    dist, ndist)
        call getvtx('RECO_GLOBAL', 'CRITERE', 1, iarg, 1,&
                    crit, ndist)
        if (ndist .eq. 0) then
!     --- AU CAS OU LA DISTANCE DE REFERENCE N'EST PAS DONNEE,ON DEVRAIT
!         LA LIRE DANS LA SD MAILLAGE (VOIR COMMANDE LIRE_MAILLAGE).
!         ELLE EST POUR L'INSTANT OBLIGATOIRE A LA PREMIERE OCCURENCE
!         DE RECO_GLOBAL
            call u2mess('F', 'ALGORITH10_21')
        endif
        incr = 0
        nbfuse = 0
        nbmoin = 0
        do 80 istac = 1, nbstac
            nomsst = zk8(lstac-1+istac)
            call mgutdm(modgen, nomsst, ibid, 'NOM_LIST_INTERF', ibid,&
                        lintf)
            call jelira(lintf//'.IDC_DEFO', 'LONUTI', nnodes, k8bid)
            call dismoi('F', 'NB_EC', lintf, 'INTERF_DYNA', nbec,&
                        k8bid, iret)
            nbni = nnodes/(2+nbec)
            call jeveuo(lintf//'.IDC_DEFO', 'L', lintd)
            do 10 in = 1, nbni
                numero = zi(lintd-1+in)
                if (zi(lsk-1+numero+incr) .ne. istac) then
                    call u2mesg('E+', 'ALGORITH15_55', 0, ' ', 0,&
                                0, 0, 0.d0)
                    call jenuno(jexnum(nomsqu//'.NOMNOE', numero), k8bid)
                    valk (1) = k8bid
                    valk (2) = lintf
                    valk (3) = nomsst
                    call u2mesg('E', 'ALGORITH15_56', 3, valk, 0,&
                                0, 0, 0.d0)
                endif
                if (zi(lsk2-1+numero+incr) .ne. numero) then
                    call u2mesg('E+', 'ALGORITH15_55', 0, ' ', 0,&
                                0, 0, 0.d0)
                    call jenuno(jexnum(nomsqu//'.NOMNOE', numero), k8bid)
                    valk (1) = k8bid
                    valk (2) = lintf
                    valk (3) = nomsqu
                    call u2mesg('E', 'ALGORITH15_58', 3, valk, 0,&
                                0, 0, 0.d0)
!
                endif
                zi(ltabi-1+in) = numero + incr
10          continue
            incr = numero + incr
!        --- ON SE POSITIONNE EN FIN DE SOUS-STRUCTURE ISTAC ---
20          continue
            if (zi(lsk+incr) .eq. istac) then
                incr = incr + 1
                goto 20
            endif
!        ---
            jncr = incr
            do 70 jstac = istac+1, nbstac
                nomsst = zk8(lstac-1+jstac)
                call mgutdm(modgen, nomsst, ibid, 'NOM_LIST_INTERF', ibid,&
                            ljntf)
                call jelira(ljntf//'.IDC_DEFO', 'LONUTI', nnodes, k8bid)
                call dismoi('F', 'NB_EC', ljntf, 'INTERF_DYNA', nbec,&
                            k8bid, iret)
                nbnj = nnodes/(2+nbec)
                call jeveuo(ljntf//'.IDC_DEFO', 'L', ljntd)
                do 30 jn = 1, nbnj
                    numero = zi(ljntd-1+jn)
                    if (zi(lsk-1+numero+jncr) .ne. jstac) then
                        call u2mesg('E+', 'ALGORITH15_55', 0, ' ', 0,&
                                    0, 0, 0.d0)
                        call jenuno(jexnum(nomsqu//'.NOMNOE', numero), k8bid)
                        valk (1) = k8bid
                        valk (2) = ljntf
                        valk (3) = nomsst
                        call u2mesg('E', 'ALGORITH15_60', 3, valk, 0,&
                                    0, 0, 0.d0)
                    endif
                    if (zi(lsk2-1+numero+jncr) .ne. numero) then
                        call u2mesg('E+', 'ALGORITH15_55', 0, ' ', 0,&
                                    0, 0, 0.d0)
                        call jenuno(jexnum(nomsqu//'.NOMNOE', numero), k8bid)
                        valk (1) = k8bid
                        valk (2) = ljntf
                        valk (3) = nomsqu
                        call u2mesg('E', 'ALGORITH15_62', 3, valk, 0,&
                                    0, 0, 0.d0)
                    endif
                    zi(ltabj-1+jn) = numero + jncr
30              continue
                jncr = numero + jncr
!           --- ON SE POSITIONNE EN FIN DE SOUS-STRUCTURE JSTAC ---
40              continue
                if (zi(lsk+jncr) .eq. jstac) then
                    jncr = jncr + 1
                    goto 40
                endif
!           ---- FUSION DES NOEUDS ---
                do 60 in = 1, nbni
                    iposi = (zi(ltabi-1+in)-1)*3
                    xii = zr(lcoord+iposi)
                    yii = zr(lcoord+iposi+1)
                    zii = zr(lcoord+iposi+2)
                    do 50 jn = 1, nbnj
                        fusion = .true.
                        jposi = (zi(ltabj-1+jn)-1)*3
                        xj = zr(lcoord+jposi)
                        yj = zr(lcoord+jposi+1)
                        zj = zr(lcoord+jposi+2)
                        distij = (xii-xj)**2+(yii-yj)**2+(zii-zj)**2
                        distij = sqrt(abs(distij))
                        if (crit .eq. 'RELATIF') then
                            if (distij .gt. prec*dist) fusion=.false.
                        else
                            if (distij .gt. dist) fusion=.false.
                        endif
                        if (fusion) then
                            i1 = zi(lcort-1+zi(ltabi-1+in))
                            i2 = zi(ltabi-1+in)
                            if (i1 .eq. i2) nbmoin = nbmoin +1
                            zi(lcort-1+zi(ltabi-1+in)) = zi(ltabj-1+ jn)
                            zi(linver-1+zi(ltabj-1+jn)) = zi(ltabi-1+ in)
                            nbfuse = nbfuse + 1
                        endif
50                  continue
60              continue
70          continue
80      end do
!
    else
!
! --- ON FUSIONNE LES INTERFACES DONNEES PAR L'UTILISATEUR ---
!
        call getfac('RECO_GLOBAL', nbreco)
        nbfuse = 0
        nbmoin = 0
        do 170 ireco = 1, nbreco
            call getvtx('RECO_GLOBAL', 'GROUP_NO_1', ireco, iarg, 0,&
                        k8bid, nr)
            if (nr .eq. 0) then
                call u2mesg('F+', 'ALGORITH15_63', 0, ' ', 0,&
                            0, 0, 0.d0)
            endif
!        --- LECTURE DE LA PRECISION
            call getvr8('RECO_GLOBAL', 'PRECISION', ireco, iarg, 1,&
                        prec, ibid)
            call getvr8('RECO_GLOBAL', 'DIST_REFE', ireco, iarg, 1,&
                        dist, ndist)
            call getvtx('RECO_GLOBAL', 'CRITERE', ireco, iarg, 1,&
                        crit, ndist)
            if (ndist .eq. 0) then
!        --- AU CAS OU LA DISTANCE DE REFERENCE N'EST PAS DONNEE,
!            ON DEVRAIT LA LIRE DANS LA SD MAILLAGE.
                crit='RELATIF'
            endif
            call getvtx('RECO_GLOBAL', 'SOUS_STRUC_1', ireco, iarg, 1,&
                        nomsst, ibid)
!        --- RECHERCHE DE LA SOUS-STRUCTURE ---
            istac = 0
90          continue
            istac = istac + 1
            if (istac .le. nbstac) then
                if (zk8(lstac-1+istac) .ne. nomsst) goto 90
            endif
            if (istac .gt. nbstac) then
                valk (1) = nomsst
                call u2mesg('F', 'ALGORITH15_64', 1, valk, 0,&
                            0, 0, 0.d0)
            endif
            call mgutdm(modgen, nomsst, ibid, 'NOM_LIST_INTERF', ibid,&
                        lintf)
            call jenuno(jexnum(lintf//'.IDC_NOMS', 2), k8bid)
            call getvtx('RECO_GLOBAL', 'GROUP_NO_1', ireco, iarg, 1,&
                        nomnoe, ibid)
            if (nomnoe .ne. k8bid) then
                valk (1) = lintf
                valk (2) = nomsst
                valk (3) = k8bid
                valk (4) = nomnoe
                call u2mesg('E', 'ALGORITH15_65', 4, valk, 0,&
                            0, 0, 0.d0)
            endif
            call jelira(lintf//'.IDC_DEFO', 'LONUTI', nnodes, k8bid)
            call dismoi('F', 'NB_EC', lintf, 'INTERF_DYNA', nbec,&
                        k8bid, iret)
            nbni = nnodes/(2+nbec)
!         NBNI = NNODES/3
            call jeveuo(lintf//'.IDC_DEFO', 'L', lintd)
            incr = 0
!        --- ON SE POSITIONNE AVANT ISTAC ---
100          continue
            if (zi(lsk+incr) .ne. istac) then
                incr = incr + 1
                goto 100
            endif
            do 110 in = 1, nbni
                zi(ltabi-1+in) = zi(lintd-1+in) + incr
110          continue
            call getvtx('RECO_GLOBAL', 'SOUS_STRUC_2', ireco, iarg, 1,&
                        nomsst, ibid)
!        --- RECHERCHE DE LA SOUS-STRUCTURE ---
            jstac = 0
120          continue
            jstac = jstac + 1
            if (jstac .le. nbstac) then
                if (zk8(lstac-1+jstac) .ne. nomsst) goto 120
            endif
            if (jstac .gt. nbstac) then
                valk (1) = nomsst
                call u2mesg('F', 'ALGORITH15_66', 1, valk, 0,&
                            0, 0, 0.d0)
            endif
            call mgutdm(modgen, nomsst, ibid, 'NOM_LIST_INTERF', ibid,&
                        ljntf)
            call jenuno(jexnum(ljntf//'.IDC_NOMS', 1), k8bid)
            call getvtx('RECO_GLOBAL', 'GROUP_NO_2', ireco, iarg, 1,&
                        nomnoe, ibid)
            if (nomnoe .ne. k8bid) then
                valk (1) = ljntf
                valk (2) = nomsst
                valk (3) = k8bid
                valk (4) = nomnoe
                call u2mesg('E', 'ALGORITH15_67', 4, valk, 0,&
                            0, 0, 0.d0)
            endif
            call jelira(ljntf//'.IDC_DEFO', 'LONUTI', nnodes, k8bid)
            call dismoi('F', 'NB_EC', ljntf, 'INTERF_DYNA', nbec,&
                        k8bid, iret)
            nbnj = nnodes/(2+nbec)
!         NBNJ = NNODES/3
            call jeveuo(ljntf//'.IDC_DEFO', 'L', ljntd)
            jncr = 0
!        --- ON SE POSITIONNE AVANT JSTAC ---
130          continue
            if (zi(lsk+jncr) .ne. jstac) then
                jncr = jncr + 1
                goto 130
            endif
            do 140 jn = 1, nbnj
                zi(ltabj-1+jn) = zi(ljntd-1+jn) + jncr
140          continue
!           ---- FUSION DES NOEUDS ---
            do 160 in = 1, nbni
                iposi = (zi(ltabi-1+in)-1)*3
                xii = zr(lcoord+iposi)
                yii = zr(lcoord+iposi+1)
                zii = zr(lcoord+iposi+2)
                do 150 jn = 1, nbnj
                    fusion = .true.
                    jposi = (zi(ltabj-1+jn)-1)*3
                    xj = zr(lcoord+jposi)
                    yj = zr(lcoord+jposi+1)
                    zj = zr(lcoord+jposi+2)
                    distij = (xii-xj)**2+(yii-yj)**2+(zii-zj)**2
                    distij = sqrt(abs(distij))
                    if (crit .eq. 'RELATIF') then
                        if (distij .gt. prec*dist) fusion=.false.
                    else
                        if (distij .gt. dist) fusion=.false.
                    endif
                    if (fusion) then
                        i1 = zi(lcort-1+zi(ltabi-1+in))
                        i2 = zi(ltabi-1+in)
                        if (i1 .eq. i2) nbmoin = nbmoin +1
                        zi(lcort-1+zi(ltabi-1+in)) = zi(ltabj-1+jn)
                        zi(linver-1+zi(ltabj-1+jn)) = zi(ltabi-1+in)
                        nbfuse = nbfuse + 1
                    endif
150              continue
160          continue
170      end do
!
! --- FIN DE TRAITEMENT DES OCCURENCES DE RECO_GLOBAL ---
    endif
!
! --- REAJUSTEMENT DE .CORRES ET NOUVEL OBJET DANS LE SQUELETTE ---
!      NBNEW = NBND - NBFUSE
    nbnew = nbnd - nbmoin
    call wkvect(nomres//'.CORRES', 'G V I', nbnew, lcorr)
!     --- MISE EN PLACE DU TABLEAU DE CORRESPONDANCE ---
    inew = 1
    iold = 1
200  continue
    if (inew .le. nbnew .and. iold .le. nbnd) then
        if (zi(linver-1+iold) .ne. iold) then
!           --- NOEUD FUSIONNE : N'EXISTE PLUS DANS LE NOUVEAU MAILLAGE
            iold = iold + 1
            goto 200
        endif
!        --- RECHERCHE DU NOEUD CORRESPONDANT LE PLUS ELOIGNE ---
!            (C'EST CELUI-LA QU'ON RETIENT)
        iadres = zi(lcort-1+iold)
202      continue
        if (iadres .ne. zi(lcort-1+iadres)) then
            iadres = zi(lcort-1+iadres)
            goto 202
        endif
!        --- NOEUD CORRESPONDANT ---
        zi(lcorr-1+inew) = iadres
!        --- ON RETIENT SON INVERSE ---
        zi(linver-1+iadres) = inew
!
        inew = inew + 1
        iold = iold + 1
        goto 200
    endif
!     --- MISE EN PLACE DU TABLEAU INVERSE ---
    do 207 iold = 1, nbnd
        if (zi(lcort-1+iold) .ne. iold) then
!        --- NOEUD FUSIONNE : ON RECHERCHE SON INVERSE ---
!        --- RECHERCHE DU NOEUD CORRESPONDANT LE PLUS ELOIGNE ---
!            (IL DEVIENT LA REFERENCE)
            iadres = zi(lcort-1+iold)
205          continue
            if (iadres .ne. zi(lcort-1+iadres)) then
                iadres = zi(lcort-1+iadres)
                goto 205
            endif
            zi(linver-1+iold) = zi(linver-1+iadres)
        endif
207  end do
!
! --- MISE A JOUR DES OBJETS DU NOUVEAU SQUELETTE ---
! --- COLLECTION .CONNEX
    call jeveuo(nomres//'.DIME', 'L', ldime)
!     NOMBRE DE MAILLES NBMA
    nbma = zi(ldime+2)
    call jelira(nomres//'.CONNEX', 'NUTIOC', nbocc, k8bid)
    do 220 iocc = 1, nbma
        call jeveuo(jexnum(nomres//'.CONNEX', iocc), 'E', lconn)
        call jelira(jexnum(nomres//'.CONNEX', iocc), 'LONMAX', nbn, k8bid)
        do 210 i = 1, nbn
            zi(lconn-1+i) = zi(linver-1+ zi(lconn-1+i))
210      continue
220  end do
!
! --- OBJET  .REFE
    call jeveuo(nomres//'.COORDO    .REFE', 'E', lrefe)
    zk24(lrefe) = nomres
!
! --- OBJET  .DIME
    call jeveuo(nomres//'.DIME', 'E', ldime)
    zi(ldime) = nbnew
!
! --- OBJET  .VALE
    call jedetr(nomres//'.COORDO    .VALE')
    call wkvect(nomres//'.COORDO    .VALE', 'G V R', nbnew*3, lvnew)
    call jeveuo(nomsqu//'.COORDO    .VALE', 'L', lvold)
    do 230 i = 1, nbnew
        iold = zi(lcorr-1+i)
        zr(lvnew-1+(i-1)*3+1) = zr(lvold-1+(iold-1)*3+1)
        zr(lvnew-1+(i-1)*3+2) = zr(lvold-1+(iold-1)*3+2)
        zr(lvnew-1+(i-1)*3+3) = zr(lvold-1+(iold-1)*3+3)
230  end do
!
! --- OBJET .NOMNOE
    call jedetr(nomres//'.NOMNOE')
    call jecreo(nomres//'.NOMNOE', 'G N K8')
    call jeecra(nomres//'.NOMNOE', 'NOMMAX', nbnew, ' ')
    do 240 i = 1, nbnew
        iold = zi(lcorr-1+i)
        call jenuno(jexnum(nomsqu//'.NOMNOE', iold), nomnoe)
        call jecroc(jexnom(nomres//'.NOMNOE', nomnoe))
240  end do
!
    call jedetr(tt//'.TABI')
    call jedetr(tt//'.TABJ')
    call jedetr(tt//'.CORRES')
    call jedetr(tt//'.INVER')
    call jedema()
end subroutine
