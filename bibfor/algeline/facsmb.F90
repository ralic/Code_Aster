subroutine facsmb(nbnd, nbsn, supnd, invsup, parent,&
                  xadj, adjncy, anc, nouv, fils,&
                  frere, local, global, adress, lfront,&
                  nblign, lgsn, debfac, debfsn, chaine,&
                  place, nbass, delg, lgind, ier)
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
! aslint: disable=W1304,W1504
    implicit none
    include 'jeveux.h'
    include 'asterfort/infbav.h'
    include 'asterfort/infmue.h'
    include 'asterfort/infniv.h'
    include 'asterfort/inschn.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mltalc.h'
    integer :: nbnd, nbsn, lgind, nbnd1
    integer :: supnd(nbsn+1), invsup(nbnd), parent(nbsn)
    integer :: xadj(nbnd+1), adjncy(*)
    integer :: anc(nbnd), nouv(nbnd), fils(nbsn), frere(nbsn), delg(nbnd)
    integer(kind=4) :: global(lgind), local(lgind)
    integer :: adress(nbsn+1), debfsn(nbsn+1)
    integer :: lfront(nbsn), nblign(nbsn), lgsn(nbsn), debfac(nbnd+1), ier
    integer :: debut
!
!================================================================
!     FACTORISATION SYMBOLIQUE POUR LA MULTIFRONTALE
!     PRISE EN COMPTE DES DDL LAGRANGE  D'ASTER
!     UTILISATION D'UNE LISTE CHAINEE ORDONNEE
!     POUR RANGER LA LISTE  DES NOEUDS ET DES VOISINS D'UN  SUPERNOEUD
!----------------------------------------------
!
!     3 CAS
!     I) LE SND A SON PREMIER DDL NON LAGRANGE : CAS STANDARD ,
!     ON MET SES VOISINS DANS LA CHAINE. SI UN  DES NDS DU SND
!     EST DANS UNE RELATION LINEAIRE,ON MET LAMBDA2
!     DANS LA CHAINE (79)
!     II) LE PREMIER EST UN DDL LAGRANGE DE BLOQUAGE
!     ON MET DANS LA CHAINE
!     TOUS LES LAGRANGES DU SUPERNOEUD,
!     (LBD1 DE BLOCS ET TOUS LES LBD2)
!     ON CHERCHE PRMNDI 1ER ND NON LAGRANGE DU SND
!     ,ET ON MET PRMNDI ET SES VOISINS DANS LA CHAINE
!     III) C'EST UN SND LBD1 DE RELATION LINEAIRE:
!     IL EST MIS DANS LA CHAINE,
!     AINSI QUE LES NDS DE LA RELATION LINEAIRE ET LE LAMBDA2
!-------------------------------------------------------------
!     MODIFICATION DU 15/09/98
!     AMDBAR A L' AIR DE FAIRE DANS CERTAINS CAS DE L'AMALGAMME I.E.
!     CERTAINS SUPERNOEUDS SONT REGROUPES EN UN SEUL AU PRIX DE CERTAINS
!     ZEROS. IL FAUT ALORS METTRE DANS LA LISTE CHAINEE, LES VOISINS DE
!     TOUS LES NOEUDS DU SN ET NON PLUS CEUX DU PREMIER NOEUD COMME
!     AUPPARAVANT. (ON POUVAIT EN OUBLIER)
!     LE TABLEAU PLACE SERT DE FLAG DANS CETTE INSERTION
!     AFIN DE GAGNER UN PEU DE TEMPS, CAR LA PLUPART DES INSERTIONS
!     SONT REDONDANTES.
!---------------------------------------------------------------
!     SOUS-PROGRAMME APPELLE : INSCHN
!==================================================================
    integer :: chaine(nbnd), place(nbnd), nbass(nbsn)
    integer :: i, k, j, nd, p, sni, andi, sn, suiv, cour
    integer :: ind, ndk, ndi, dli, ilimpi
    integer :: ifm, niv, long, decal, iret, ifet1, ifet2, ifet3, nbsd, iaux
    logical :: lfeti
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call infniv(ifm, niv)
! FETI OR NOT FETI AVEC IMPRESSION NIVEAU 1?
    call jeexin('&FETI.MAILLE.NUMSD', iret)
    if (iret .ne. 0) then
        call infmue()
        call infniv(ifm, niv)
        lfeti=.true.
    else
        lfeti=.false.
    endif
    ier =0
!     CALCUL DE INVSUP FILS ET FRERE
    do 120 i = 1, nbsn
        place(i) = 0
        fils(i) = 0
        frere(i) = 0
        lgsn(i) = supnd(i+1) - supnd(i)
        do 110 j = supnd(i), supnd(i+1) - 1
            invsup(j) = i
110      continue
120  end do
    do 130 nd = 1, nbsn
        p = parent(nd)
        if (p .ne. 0) then
            if (fils(p) .eq. 0) then
                fils(p) = nd
                place(p) = nd
            else
                frere(place(p)) = nd
                place(p) = nd
            endif
        endif
130  end do
!
!
    decal = 0
    adress(1) = 1
    debfac(1) = 1
    do 140 sni = 1, nbsn
        lgsn(sni) = supnd(sni+1) - supnd(sni)
140  end do
!
    adress(1) = 1
    debfac(1) = 1
    nbnd1 = nbnd+1
!     CORRECTION DE NOV 2006 CETTE BOUCLE 311 REMPLACE LA PRECEDENTE
!     INTERNE A LA BOUCLE 310 CELA ENTRAINAIT BEAUCOUP DE TEMPS CPU
    do 311 i = 1, nbnd
        place(i)=0
311  end do
    do 309 ndi = 1, nbsn
        chaine(ndi) = nbnd1
309  end do
    do 310 sni = 1, nbsn
        ndi = supnd(sni)
        andi = anc(ndi)
        chaine(ndi) = nbnd1
        dli=ndi
        debut=ndi
!        LES TERMES INITIAUX DE LA MATRICE SONT MIS DANS LA CHAINE
!        QUE LE DDL ORDINAIRE OU LAGRANGE
        call inschn(andi, dli, xadj, adjncy, chaine,&
                    nouv, place, debut)
        if (delg(andi) .eq. 0) then
!--------------------------------------------------------------
!     1 .....................................   NDI EST UN DDL ORDINAIRE
!
!     ON INSERE AUSSI DANS LA CHAINE LES VOISINS INITIAUX DE TOUTES
!     INCONNNUES DU SUPERNOEUD, AU CAS OU LA RENUMEROTATION
!     FASSE DE L'AMALGAME.
!--------------------------------------------------------------
!
            do 152 dli = ndi+1, supnd(sni+1)-1
                andi = anc(dli)
                if (delg(andi) .ne. 0) goto 151
                call inschn(andi, dli, xadj, adjncy, chaine,&
                            nouv, place, debut)
!
152          continue
151          continue
        endif
!-------------------------------------------------------------
!
!     LES NOEUDS VOISINS DES FILS SONT MIS DANS LA CHAINE
!-------------------------------------------------------------
        sn = fils(sni)
230      continue
!        DO WHILE (SN.NE.0)
        if (sn .ne. 0) then
!           K = ADRESS(SN) + LGSN(SN) + 1 CORRECTION DU 15/03/02
            k = adress(sn) + lgsn(sn)
            ind = 1
            nd = ndi
240          continue
!     DO WHILE (K.LT.ADRESS(SN+1))
            if (k .lt. adress(sn+1)) then
                ndk = global(k)
                if (ndk .gt. ndi) then
                    suiv= nd
235                  continue
                    if (suiv .lt. ndk) then
!     DO WHILE(SUIV.LT.NDK)
                        cour = suiv
                        suiv = chaine(cour)
                        goto 235
                    endif
                    if (suiv .gt. ndk) then
                        chaine(cour) = ndk
                        chaine(ndk) = suiv
                        place(ndk) = 1
                    endif
                    nd = ndk
                endif
                k = k + 1
                goto 240
!     FIN DO WHILE
            endif
            sn = frere(sn)
            goto 230
!     FIN DO WHILE
        endif
        k = 0
        ind = ndi
!     DO WHILE (IND.NE.NBND1) ( FIN DE LA CHAINE)
280      continue
        if (ind .ne. nbnd1) then
!-------------------------------------------------------------
!     VERIFICATION DE LA LONGUEUR DE GLOBAL
!-------------------------------------------------------------
            if ((adress(sni)+k) .gt. lgind) then
                ier = lgind*2
                if (niv .ge. 2) then
                    write(ifm,*)&
     &             'LONGUEUR DE GLOBAL PEUT ETRE INSUFFISANTE '
                    write(ifm,*)'LONGUEUR ALLOUEE :',lgind
                    write(ifm,*)'ON REITERE AVEC :',ier
                endif
                goto 999
            endif
            global(k+adress(sni)) = int(ind, 4)
            place(global(k+adress(sni))) = k + 1
            k = k + 1
            ind = chaine(ind)
            goto 280
!     FIN DO WHILE
        endif
        adress(sni+1) = k + adress(sni)
!...........................................
        sn = fils(sni)
!     DO WHILE (SN.NE.0)
290      continue
        if (sn .ne. 0) then
            call mltalc(local, global, adress, sn, lgsn,&
                        place, sni, supnd, nbass(sn))
!
            sn = frere(sn)
            goto 290
!     FIN DO WHILE
        endif
        nblign(sni) = adress(sni+1) - adress(sni)
        lfront(sni) = nblign(sni) - lgsn(sni)
        long = nblign(sni)
!     ANCIENNE VERSION SANS DGEMV
!     DO 300 K = SUPND(SNI),SUPND(SNI+1) - 1
!     DEBFAC(K+1) = DEBFAC(K) + L
!     L = L - 1
!     300     CONTINUE
!     MODIFS POUR DGEMV
        do 300 k = 1, lgsn(sni)
            nd=supnd(sni) + k-1
            debfac( nd ) = decal +k
            decal = decal+long
300      continue
        debfsn(sni) = debfac(supnd(sni))
!   ON REMET LE TABLEAU PLACE A ZERO ICI AU LIEU DE 311
        do 320 k = adress(sni), (adress(sni+1) - 1)
            place(global(k))=0
320      end do
!
310  end do
!
    debfac(nbnd+1)=decal+1
    debfsn(nbsn+1) = debfac(nbnd+1)
    if (niv .ge. 2) then
        write(ifm,*)'   --- LONGUEUR DE LA MATRICE FACTORISEE ',decal
    endif
! STOCKAGE INFO SI FETI
    if (lfeti) then
! INTRODUIT POUR PRENDRE EN COMPTE LES TROUS DANS LA LISTE DES SOUS
! -DOMAINES TRAITES EN PARALLELE PAR UN PROCESSEUR
        call jeveuo('&FETI.LISTE.SD.MPI', 'L', ilimpi)
        ilimpi=ilimpi+1
        call jeveuo('&FETI.INFO.STOCKAGE.FIDD', 'E', ifet1)
        nbsd=zi(ifet1+1)
! NUMERO DU SOUS-DOMAINE EN COURS - 1
        ifet2=zi(ifet1)
        call jeveuo('&FETI.INFO.STOCKAGE.FVAF', 'E', ifet3)
        zi(ifet3+ifet2)=decal
! MISE A JOUR DES SOMMES FINALES
        zi(ifet3+nbsd)=zi(ifet3+nbsd)+decal
! IL FAUT MAINTENANT TROUVER LE PROCHAIN SD CONCERNE PAR LE PROCESSUS
        do 350 i = ifet2+1, nbsd
            iaux=zi(ilimpi+i)
            if (iaux .eq. 1) then
                zi(ifet1)=i
                goto 351
            endif
350      continue
!
351      continue
        call infbav()
    endif
!
999  continue
!
end subroutine
