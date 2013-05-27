subroutine preml1(neq, n2, diag, delg, col,&
                  xadj, adjncy, parent, adress, supnd,&
                  nnz, qsize, llist, suiv, p,&
                  q, invp, perm, lgind, ddlmoy,&
                  nbsn, optnum, lgadjn, nrl, deb,&
                  vois, suit, ier, nec, prno,&
                  deeq, noeud, ddl, invpnd, permnd,&
                  spndnd, xadjd, matgen)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! person_in_charge: olivier.boiteau at edf.fr
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
!     TOLE CRP_21 CRS_602 CRS_505
    implicit none
!
    include 'jeveux.h'
    include 'asterc/ismaem.h'
    include 'asterc/onmetl.h'
    include 'asterfort/amdapt.h'
    include 'asterfort/amdbar.h'
    include 'asterfort/genmmd.h'
    include 'asterfort/infbav.h'
    include 'asterfort/infmue.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/prmadj.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: neq, diag(0:neq), lgind, lgadjn
    integer :: col(*), xadj(neq+1), adjncy(lgadjn)
    integer :: delg(neq), nbsn, adress(neq), parent(neq)
    integer :: supnd(neq), optnum
    integer :: invp(neq), perm(neq), ier
    integer :: deb(*), vois(*), suit(*)
    integer :: noeud(*), ddl(*), permnd(*), invpnd(*), spndnd(*), xadjd(*)
!     VARIABLES LOCALES
    integer :: i, j, idelta, maxint, nadj, ifet3, ifet4, ilimpi
    integer :: n2, fctnzs, innz, numi, numj, numl, num, ii, iret, ifet1, ifet2
    real(kind=8) :: nbops
    integer :: nnz(1:neq), qsize(neq), llist(neq), suiv(neq)
    integer :: libre, iovflo, ncmpa, ifm, niv, p(neq), q(n2), nrl
    integer :: it
    integer :: nec, prno(*), deeq(*), ino, nbcmp
    logical :: lfeti, matgen, liaiso
!--------------------------------------------------------------
!
!     VERSION RENUMEROTATION PAR NOEUD
    integer :: nbnd, nd, nbnd1, ddlmoy, renum
    integer :: pas, k, ndanc, iddl, sni, ind, nddl
    integer :: nbsd, iaux
    integer :: vali(2)
    integer(kind=4) :: nbnd4, nadj4, nbsn4, fctnz4, lgind4
    character(len=24) :: noxadj, noadjn, noperm, noinvp, nopare, nospnd
    integer :: xadjd4, adjnc4, invpn4, permn4, paren4, spndn4
!
    data noxadj/'&&PREML1.NOMXADJ_PROV   '/
    data noadjn/'&&PREML1.NOMADJN_PROV   '/
    data noperm/'&&PREML1.NOPERM_PROV    '/
    data noinvp/'&&PREML1.NOINVP_PROV    '/
    data nopare/'&&PREML1.NOPARE_PROV    '/
    data nospnd/'&&PREML1.NOSPND_PROV    '/
!****************************************************************
!-----RECUPERATION DU NIVEAU D'IMPRESSION
!
!
    call infniv(ifm, niv)
! FETI OR NOT FETI ?
    call jeexin('&FETI.MAILLE.NUMSD', iret)
    if (iret .ne. 0) then
        call infmue()
        call infniv(ifm, niv)
        lfeti=.true.
    else
        lfeti=.false.
    endif
!--------------------------------------------------------------
!     LA RENUMEROTATION VA ETRE FAITE AVEC LA CONNECTIVITE NODALE
!     ET NON PLUS LA CONNECTIVITE PAR DDL COMME AVANT
!
!     1) ON CALCULE   NOEUD ET DDL A PARTIR DE PRNO ET DEEQ
!     NOEUD(1:NEQ) SURJECTION VERS (1:NBND) OU NBND EST LE NOMBRE
!     DE NOEUDS AU SENS DE LA DISCRETISATION
!     LA FONCTION INVERSE EST DDL(1:NBND)
!     EN CAS DE MATRICE GENERALISEE PRNO ET DEEQ N ONT PAS LE MEME SENS
!     ON NE LES UTILISE PAS MAIS ON SAIT QUE NOEUDS ET DDL SONT
!     IDENTIQUES, LES TABLEAUX DDL ET NOEUD SONT ALORS IMMEDIATS
!     SI ON N'AVAIT PLUS UN DDL PAR NOEUD EN MATRICE GENERALISEE
!     CECI NE SERAIT PLUS VALABLE
    renum=optnum
    if (matgen) then
        nbnd=neq
!        DANS LE CAS MATGEN, ON ASSIMILE LES NOEUDS AUX DDL
!        ON SUPPOSE AUSSI QUE NEQ == N2 IE PAS DE LAGRANGE
!        SI ON AVAIT N2< NEQ ( CERTAINS DELG NON NULS)
!        IL FAUDRAIT P.E DEPLACER LA BOUCLE 3 APRES LA 350
!        AVEC N2 EN PLACE DE NEQ
        do 3 i = 1, neq+1
            ddl(i)=i
            noeud(i)=i
 3      continue
!     RECHERCHE DE PRESENCE DE LIAISON NON DETECTEE PAR PREML0(NRL=0)
!     (NON SIGNALEE PAR DES VALEURS DE DELG NEGATIVES)
        liaiso=.false.
        do 4 i = 1, neq
            if (deeq(2*i) .lt. 0) then
                liaiso=.true.
            endif
 4      continue
        if (liaiso) then
!          EN CAS DE LIAISON DANS UNE MATRICE GENERALISEE
!          LES DDL SONT CORRECTEMENT ORDONNES A PRIORI
!          ON NE FAIT PAS DE RENUMEROTATION
            call u2mess('I', 'ALGELINE3_35')
            renum=3
        endif
    else
        do 1 i = 1, neq
            noeud(i) = 0
 1      continue
        nbnd=0
        i = 1
        ddl(i)= 1
101      continue
!     DO WHILE(I.LE.NEQ)
        if (i .le. neq) then
            if (deeq(2*i) .gt. 0) then
                ino = deeq(2*i-1)
                nbnd = nbnd +1
                nbcmp = prno( (ino-1)*(2+nec) + 2)
                do 2 j = i, i+nbcmp-1
                    noeud(j)= nbnd
 2              continue
                ddl(nbnd+1) = ddl(nbnd) + nbcmp
                pas = nbcmp
            else
                pas = 1
            endif
            i = i+pas
            goto 101
        endif
    endif
    ddlmoy = ( ddl(nbnd+1) - 1 )/nbnd
!--------------------------------------------------------------------
!     2) CALCUL DE (ADJNCY, XADJ) EN DDL DANS LA NUMEROTATION DE 1 À N2
!     COMME DANS LA VERSION INITIALE
!     INITIALISATION DE NNZ : NBRE DE TERMES A AJOUTER
!     POUR CHAQUE LIGNE
    num = 0
    do 320 i = 1, neq
        if (delg(i) .eq. 0) then
            num = num +1
!     ON COMPTE LES NON-ZEROS
            innz=0
            do 315 j = diag(i-1)+1, diag(i)-1
                if (delg(col(j)) .eq. 0) then
!     PARTIE TRIANGULAIRE INFERIEURE
                    innz = innz + 1
!     PARTIE TRIANGULAIRE SUPERIEURE
                    nnz(p(col(j) )) = nnz(p(col(j))) + 1
                endif
315          continue
            nnz(num) = innz
        endif
320  end do
    if (nrl .ne. 0) then
        do 220 j = 1, neq
            it = deb(j)
219          continue
            if (it .gt. 0) then
                nnz(p(j)) = nnz(p(j)) + 1
                it = suit(it)
                goto 219
            endif
220      continue
!     VERIFICATION
!
        do 325 j = 1, neq
!     TERMES A AJOUTER PARTIE SUPERIEURE
            it = deb(j)
324          continue
            if (it .gt. 0) then
                nnz(p(vois(it))) = nnz(p(vois(it))) + 1
                it = suit(it)
                goto 324
            endif
325      continue
    endif
!
    xadj(1) = 1
    do 330 j = 1, n2
        xadj(j+1) = xadj(j) + nnz(j)
        nnz(j) = 0
330  end do
    if ((xadj(neq+1)-1) .gt. lgadjn) then
!     TEST D'ESPACE SUFFISANT DANS ADJNCY
        vali (1) = lgadjn
        vali (2) = xadj(neq+1)-1
        call u2mesg('F', 'ALGELINE4_4', 0, ' ', 2,&
                    vali, 0, 0.d0)
    endif
!
    do 350 j = 1, neq
        if (delg(j) .eq. 0) then
            numj=p(j)
            do 340 ii = diag(j-1)+1, diag(j)-1
                i = col(ii)
                if (delg(i) .eq. 0) then
                    numi=p(i)
                    adjncy(xadj(numj)+nnz(numj)) = numi
                    nnz(numj) = nnz(numj) + 1
                    adjncy(xadj (numi)+nnz(numi)) = numj
                    nnz(numi) = nnz(numi) + 1
                endif
340          continue
            if (nrl .ne. 0) then
                it = deb(j)
344              continue
                if (it .gt. 0) then
                    numl = p(vois(it))
                    adjncy(xadj(numj)+nnz(numj)) = numl
                    nnz(numj) = nnz(numj) + 1
                    adjncy(xadj(numl)+nnz(numl)) = numj
                    nnz(numl) = nnz(numl) + 1
                    it = suit(it)
                    goto 344
                endif
            endif
        endif
350  end do
    nbnd1 = nbnd + 1
    libre = xadj(nbnd1)
    nadj = libre - 1
!-----------------------------------------------------------
!     3) MODIFICATION DE (ADJNCY, XADJ) VOISINAGE PAR DDL EN
!     (ADJNCY,XADJD) VOISINAGE PAR  NOEUD POUR LA RENUMEROTATION
    call prmadj(nbnd, neq, n2, adjncy, xadj,&
                xadjd, llist, q, noeud)
!-----------------------------------------------------------
    nbnd1 = nbnd + 1
    libre = xadjd(nbnd1)
    nadj = libre - 1
    if (renum .eq. 0) then
!----------------------------------MINIMUM DEGRE : GENMMD
        idelta = 0
        maxint = 2*nbnd
!
        call genmmd(nbnd, nbnd1, nadj, xadjd, adjncy,&
                    maxint, idelta, invpnd, permnd, nbsn,&
                    spndnd, adress, parent, lgind, fctnzs,&
                    nbops, nnz, qsize, llist, suiv)
    else if (renum.eq.1) then
!----------------------------------MINIMUM DEGRE : APPROXIMATE MIN DEG
        iovflo = ismaem()
        do 250 i = 1, n2
            qsize(i)=xadjd(i+1)-xadjd(i)
250      continue
        call amdbar(nbnd, xadjd, adjncy, qsize, lgadjn,&
                    libre, suiv, llist, permnd, nnz,&
                    invpnd, parent, ncmpa, adress, iovflo)
        call amdapt(neq, nbnd, nbsn, xadjd, suiv,&
                    invpnd, parent, spndnd, adress, lgind,&
                    fctnzs, nbops, llist, qsize)
    else if (renum.eq.2) then
!----------------------------------METIS 4 : METHODE DE BISSECTION
!
!
!     COPIE EN INTEGER*4
        nbnd4=nbnd
        nadj4=nadj
        call wkvect(noxadj, ' V V S ', nbnd+1, xadjd4)
        call wkvect(noadjn, ' V V S ', max(nadj, 1), adjnc4)
        call wkvect(noinvp, ' V V S ', nbnd, invpn4)
        call wkvect(noperm, ' V V S ', nbnd, permn4)
        call wkvect(nopare, ' V V S ', nbnd, paren4)
        call wkvect(nospnd, ' V V S ', nbnd, spndn4)
        do 401 i = 1, nbnd+1
            zi4(xadjd4+i-1)=xadjd(i)
401      continue
        do 402 i = 1, nadj
            zi4(adjnc4+i-1)=adjncy(i)
402      continue
        call onmetl(nbnd4, nadj4, zi4(xadjd4), zi4(adjnc4), zi4(invpn4),&
                    zi4(permn4), zi4(spndn4), zi4(paren4), nbsn4, nbops,&
                    fctnz4, lgind4, niv)
        nbsn=nbsn4
        fctnzs=fctnz4
        lgind=lgind4
        do 403 i = 1, nbnd
            invpnd(i)=zi4(invpn4 + i-1 )
            permnd(i)=zi4(permn4 + i-1 )
403      continue
        do 404 i = 1, nbsn
            parent(i)=zi4(paren4 + i-1)
404      continue
        do 405 i = 1, nbsn+1
            spndnd(i)=zi4(spndn4 + i-1)
405      continue
!
        call jedetr(noxadj)
        call jedetr(noadjn)
        call jedetr(noinvp)
        call jedetr(noperm)
        call jedetr(nopare)
        call jedetr(nospnd)
    else if (renum.eq.3) then
!-----------------MATRICE GENERALISEE PAS DE RENUMEROTATION
!     ON L'EMULE EN CREANT UN SEUL SUPER NOEUD
!
        nbsn=1
        do 399 i = 1, nbnd
            invpnd(i)=i
            permnd(i)=i
399      continue
        parent(nbsn)=0
        spndnd(1)=1
        spndnd(nbsn+1)=nbnd+1
        lgind=10
        nbops=0.d0
        fctnzs=nbnd*(nbnd+1)/2
    endif
!****************************************************************
!****************************************************************
!.....................................................................
    lgind = lgind * ddlmoy
!     4) TRAITEMENT POUR LE PASSAGE DE LA RENUMEROTATION PAR NOEUD
!     A CELLE PAR DDL
!     PERM, INVP, SUPND SONT RECONSTITUES
    ind=0
    do 415 nd = 1, nbnd
        ndanc = permnd(nd)
        nddl = ddl(ndanc+1) -ddl(ndanc)
        do 410 k = 1, nddl
            ind = ind +1
            perm(ind) = ddl(ndanc) +k - 1
410      continue
415  end do
    if (niv .eq. 2 .and. ind .ne. n2) then
        vali (1) = n2
        vali (2) = ind
        call u2mesg('F', 'ALGELINE4_60', 0, ' ', 2,&
                    vali, 0, 0.d0)
    endif
    do 406 iddl = 1, n2
        invp(perm(iddl)) = iddl
406  end do
!     SUPND
    supnd(nbsn+1)=n2+1
    do 407 sni = 1, nbsn
!        ND 1ER NOEUD DU SNI
        nd = spndnd(sni)
!        ND : ANCIEN NUMERO DE ND
        nd = permnd(nd)
!        IDDL : 1ER DDL DE CE NOEUD
        iddl = ddl(nd)
        supnd(sni) = invp(iddl)
!        INVP(IDDL) 1ER DDL DU SNI
407  end do
!.....................................................................
    if (niv .ge. 1) then
!     WRITE(IFM,*)'RENUMEROTATION PAR MINIMUM DEGRE'//
!     *  'TEMPS CPU',TEMPS(3),
!     *  ' + TEMPS CPU SYSTEME ',TEMPS(6)
    endif
    fctnzs = fctnzs + neq
!
    if (niv .ge. 2) then
        write(ifm,*)'--- RESULTATS DE LA RENUMEROTATION : '
        write(ifm,*)'   --- NOMBRE DE NOEUDS ',nbnd
        write(ifm,*)'   --- LONGUEUR DE LA MATRICE INITIALE ',diag(neq)
        write(ifm,*)'   --- NOMBRE DE SUPERNOEUDS ',nbsn
        if (renum .eq. 2) then
            write(ifm,*)'   --- NOMBRE D''OP. FLOTTANTES ',nbops
        endif
!
! STOCKAGE INFO SI FETI
    endif
    if (lfeti) then
! INTRODUIT POUR PRENDRE EN COMPTE LES TROUS DANS LA LISTE DES SOUS
! -DOMAINES TRAITES EN PARALLELE PAR UN PROCESSEUR
        call jeveuo('&FETI.LISTE.SD.MPI', 'L', ilimpi)
        ilimpi=ilimpi+1
        call jeveuo('&FETI.INFO.STOCKAGE.FIDD', 'E', ifet1)
        nbsd=zi(ifet1+1)
        ifet2=zi(ifet1)
! SI ON COMMENCE LE PROCESSUS, IL FAUT TROUVER LE PREMIER SD CONCERNE
! PAR LE PROCESSUS
        if (ifet2 .eq. 0) then
            do 445 i = 0, nbsd-1
                iaux=zi(ilimpi+i)
                if (iaux .eq. 1) then
                    ifet2=i
                    zi(ifet1)=i
                    goto 446
                endif
445          continue
            call u2mess('A', 'ALGELINE3_26')
446          continue
        endif
        call jeveuo('&FETI.INFO.STOCKAGE.FVAL', 'E', ifet3)
        zi(ifet3+ifet2)=diag(neq)
        call jeveuo('&FETI.INFO.STOCKAGE.FNBN', 'E', ifet4)
        zi(ifet4+ifet2)=nbnd
! MISE A JOUR DES SOMMES FINALES
        zi(ifet3+nbsd)=zi(ifet3+nbsd)+diag(neq)
        zi(ifet4+nbsd)=zi(ifet4+nbsd)+nbnd
        call infbav()
    endif
    ier=0
end subroutine
