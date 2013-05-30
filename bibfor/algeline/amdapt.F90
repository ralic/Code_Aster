subroutine amdapt(neq, nbnd, nbsn, pe, nv,&
                  invp, parent, supnd, adress, lgind,&
                  fctnzs, fctops, llist, nnv)
! person_in_charge: olivier.boiteau at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     DONNEES
!     NEQ : NBRE TOTAL D'INCONNUES (LAGRANGES INCLUS)
!     NBND : NBRE DE DDL  RENUMEROTES PAR AMDBAR (NEQ - LAGRANGES)
!     INVP : INVP(I) : NOUVEAU NUMERO DU DDL  (RESULTATS DE AMDBAR)
!     PE  (RESULTATS DE AMDBAR)
!     NV (RESULTATS DE AMDBAR)
!     ON OUTPUT, PE HOLDS THE ASSEMBLY TREE/FOREST, WHICH IMPLICITLY
!     REPRESENTS A PIVOT ORDER WITH IDENTICAL FILL-IN AS THE ACTUAL
!     ORDER (VIA A DEPTH-FIRST SEARCH OF THE TREE).
!
!     ON OUTPUT:
!     IF NV (I) .GT. 0, THEN I REPRESENTS A NODE IN THE ASSEMBLY TREE,
!     AND THE PARENT OF I IS -PE (I), OR ZERO IF I IS A ROOT.
!     IF NV (I) = 0, THEN (I,-PE (I)) REPRESENTS AN EDGE IN A
!     SUBTREE, THE ROOT OF WHICH IS A NODE IN THE ASSEMBLY TREE.
!
!
!     RESULTATS
!     SUPND(1:NBSN+1) : DEFINITION DES SUPERNOEUDS, LE SN I EST DEFINI
!     PAR ( SUPND(I), SUPND(I+1)-1)
!     PARENT(NBSN) : PARENT(I) EST LE SN PARENT DU SN I
!     ADRESS : POINTEUR DANS LES TABLEAUX GLOBAL ET LOCAL
!     EN FAIT ADRESS SERA RECALCULE DANS FACSMB, IL SERT ICI
!     A EVALUER LGIND.
!
!     LGIND : LONGUEUR DE  GLOBAL (ET LOCAL),
!     FCTNZS      NBRE DE TERMES NON NULS DANS LA FACTORISEE,
!     FCTOPS   NBRE DE D OPERATIONS.
!     (CES VALEURS SONT INDICATIVES)
!     CAR NE PRENANT PAS EN COMPTE LES LAGRANGES.
!
!     TAB DE TRAVAIL
!     LLIST,NNV
!
    implicit none
!
    include 'asterfort/infniv.h'
    include 'asterfort/u2mesg.h'
    integer :: neq, invp(neq), pe(neq+1), nv(neq)
    integer :: nbnd, nbsn, lgind, fctnzs
    real(kind=8) :: fctops
    integer :: parent(*), supnd(neq), adress(*)
!
    integer :: llist(neq), nnv(neq), ifm, niv
    integer :: i, j, k, ncol, nlig, deb, fin, snj, ndi
!
!     NNV EQUIVAUDRA A NV DANS LA NOUVELLE NUMEROTATION
!     PARENT SERA  LE PARENT PAR INCONNUE ET NON PAR SUPERNOEUD
    call infniv(ifm, niv)
    nbsn = 0
    do 100 i = 1, nbnd
        if (nv(i) .ne. 0) nbsn = nbsn + 1
        j = invp(i)
        if (pe(i) .ne. 0) then
            parent(j) = invp(-pe(i))
        else
            parent(j)=0
        endif
        nnv(j) = nv(i)
100  end do
!     NV CONTIENDRA  LA LARGEUR DES SN (LGSN AILLEURS)
    j = 1
    do 110 i = 1, nbsn
        nv(i) = 1
111      continue
        if (nnv(j) .eq. 0) then
            nv(i) = nv(i) + 1
            j = j + 1
            goto 111
        endif
        j = j + 1
110  end do
!
    supnd(1) = 1
    do 120 i = 1, nbsn
        supnd(i+1) = supnd(i) + nv(i)
120  end do
!     LLIST SERA  L INVERSE DE SUPND, APPELE INVSUP AILLEURS
    k = 0
    adress(1) = 1
    do 130 snj = 1, nbsn
        do 125 i = 1, nv(snj)
            k = k + 1
            llist(k) = snj
125      continue
!     CALCUL DE ADRESS : ON CHERCHE DANS LE SN SNJ,
!     LE NOEUD I T.Q. NNV(I) =/= 0, CETTE VALEUR CONTIENT
!     LE "VRAI DEGRE" DE I,(SORTIE DE AMDBAR)
!     ON RAPPELLE VRAI DEGRE = LARGEUR DU SN + NBRE DE VOISINS
!     C A D ADRESS(SN+1) - ADRESS(SN)
        deb = supnd(snj)
        fin = supnd(snj+1)-1
        i= fin
128      continue
        if (i .lt. deb) then
            call u2mesg('F', 'ALGELINE5_6', 0, ' ', 1,&
                        snj, 0, 0.d0)
        endif
        if (nnv(i) .ne. 0) goto 129
        i= i-1
        goto 128
129      continue
        adress(snj+1) = adress(snj) + nnv(i)
130  end do
!
    lgind = adress(nbsn+1) - 1
    fctnzs = 0
    fctops = 0.d0
    do 150 i = 1, nbsn
        ncol = supnd(i+1) - supnd(i)
        nlig = adress(i+1) - adress(i)
        fctnzs = fctnzs + nlig*ncol - (ncol*(ncol+1))/2
        do 140 j = 1, ncol
            nlig = nlig - 1
            fctops = fctops + nlig*(nlig+3)
140      continue
150  end do
!     ON CALCUL PARENT EN SN A PARTIR DE PARENT/NOEUDS
!     NV CONTIENDRA PROVISOIREMENT PARENT/NOEUDS
    do 160 i = 1, nbnd
        nv(i) = parent(i)
160  end do
!
    do 170 i = 1, nbnd
        parent(i) = 0
170  end do
    do 180 snj = 1, nbsn
        deb = supnd(snj)
        fin = supnd(snj+1)-1
        ndi= fin
175      continue
        if (ndi .lt. deb) then
            call u2mesg('F', 'ALGELINE5_6', 0, ' ', 1,&
                        snj, 0, 0.d0)
        endif
        if (nnv(ndi) .ne. 0) goto 177
        ndi= ndi-1
        goto 175
177      continue
!        LE NOEUD NDI EST LE ND REPRESENTATIF DU SN
!        PARENT(SNJ) EST LE SN CONTENANT LE PARENT DE NDI
        if (nv(ndi) .ne. 0) then
            parent(snj) = llist(nv(ndi))
        else
            parent(snj) =0
        endif
!
180  end do
    if (niv .eq. 2) then
        write(ifm,*)'AMDAPT  :  TRAITEMENT DE AMDBAR'
        write(ifm,*)' NOMBRE DE SUPERNOEUDS: ',nbsn
        do 200 i = 1, nbsn
            write(ifm,*) 'SN ',i,' :NDS DE ',supnd(i),' A ',supnd(i+1)&
            -1, ',PARENT ',parent(i),',DEGRE :',adress(i+1)-adress(i)
200      continue
    endif
!
end subroutine
