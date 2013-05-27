subroutine assgcy(nomres, nugene)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!
!***********************************************************************
!    P. RICHARD     DATE 13/10/92
!-----------------------------------------------------------------------
!  BUT:      < ASSEMBLAGE GENERALISEE >
!
!  ASSEMBLER UNE MATRICE A PARTIR D'UNE NUMEROTATION GENERALISEE
!  ET D'UNE OPTION (RIGI_GENE,MASS_GENE,AMOR_GENE)
!
! REMARQUE : L'ASSEMBLAGE DONNE UNE MATRICE ASSEMBLEE LIGNE DE CIEL
!            IL CONSIDERE LES MATRICE ELEMENTAIRE GENERALISEES
!  A ASSEMBLER COMME DES BLOCS
!  CHAQUE MATRICE ELEMENTAIRE POUVANT ETRE CONSTITUE DE PLUSIEURS BLOCS
!  CE QUI SEMBLE COMPLIQUER NETTEMENT LA TACHE POUR LE MOMENT MAIS
!  LE TRAVAIL POUR CONSIDERE UNE MATRICE ASSEMBLEE LIGNE DE CIEL
!     COMME UNE MATRICE ELEMENTAIRE DEVRAIT ETRE MINIME
!
!-----------------------------------------------------------------------
!
! NOM----- / /:
!
! NOMRES   /I/: NOM K8 DE LA MATRICE GENERALISEE RESULTAT
! OPTION   /I/: OPTION DE CALCUL (RIGI_GENE,MASS_GENE)
! NUGENE   /I/: NOM K14 DE LA NUMEROTATION GENERALISEE
!
!
!
!
!
    include 'jeveux.h'
!
    include 'asterfort/jecrec.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/ualfva.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: nomres, modgen
    character(len=14) :: nugene
    character(len=19) :: stolci, resu
!
    integer :: iblo, ldlim, neq, ntbloc, nbloc, iaconl, jrefa, iadesc, iscdi
    integer :: iscbl, ischc, ldblo, n1bloc, n2bloc, i, j, llref
    character(len=8) :: k8bid
!
!-----------------------------------------------------------------------
!
!--------------------------CREATION DU .REFA----------------------------
!
!-----------------------------------------------------------------------
    integer :: jscde
!-----------------------------------------------------------------------
    call jemarq()
!
    resu = nomres
    stolci=nugene//'.SLCS'
!
!--------------------RECUPERATION DU MODE_GENE AMONT--------------------
!
    call jeveuo(nugene//'.NUME.REFN', 'L', llref)
    modgen=zk24(llref)(1:8)
!
!--------------------------CREATION DU .LIME----------------------------
!   POUR L'INSTANT ON DONNE LE NOM DU MODELE GENERALISE
!
    call wkvect(resu//'.LIME', 'G V K24', 1, ldlim)
    zk24(ldlim)=modgen
!
!--------------------RECUPERATION DES CARACTERISTIQUES BLOCS------------
!
    call jeveuo(stolci//'.SCDE', 'L', jscde)
    neq=zi(jscde-1+1)
    ntbloc=zi(jscde-1+2)
    nbloc=zi(jscde-1+3)
!
    call jelibe(stolci//'.SCDE')
!
    call jecrec(resu//'.UALF', 'G V R', 'NU', 'DISPERSE', 'CONSTANT',&
                nbloc)
    call jeecra(resu//'.UALF', 'LONMAX', ntbloc, k8bid)
!
    call wkvect(resu//'.CONL', 'G V R', neq, iaconl)
    do 10 i = 1, neq
        zr(iaconl+i-1) = 1.0d0
10  end do
!
    call wkvect(resu//'.REFA', 'G V K24', 11, jrefa)
    zk24(jrefa-1+11)='MPI_COMPLET'
    zk24(jrefa-1+1)=' '
    zk24(jrefa-1+2)=nugene
    zk24(jrefa-1+8) = 'ASSE'
    zk24(jrefa-1+9) = 'MS'
    zk24(jrefa-1+10) = 'GENE'
!
    call wkvect(resu//'.DESC', 'G V I', 3, iadesc)
    zi(iadesc) = 2
    zi(iadesc+1) = neq
    zi(iadesc+2) = 2
!
! --- RECUPERATION DE LA STRUCTURE DE LA MATR_ASSE_GENE
!
    call jeveuo(stolci//'.SCDI', 'L', iscdi)
    call jeveuo(stolci//'.SCBL', 'L', iscbl)
    call jeveuo(stolci//'.SCHC', 'L', ischc)
!
    do 20 iblo = 1, nbloc
!
        call jecroc(jexnum(resu//'.UALF', iblo))
        call jeveuo(jexnum(resu//'.UALF', iblo), 'E', ldblo)
!
!        BOUCLE SUR LES COLONNES DE LA MATRICE ASSEMBLEE
!
        n1bloc = zi(iscbl+iblo-1)+1
        n2bloc = zi(iscbl+iblo)
!    INITIALISATION DE LA MATRICE GENERALISEE
        do 30 i = n1bloc, n2bloc
            do 40 j = (i-zi(ischc+i-1)+1), i
                zr(ldblo+zi(iscdi+i-1)+j-i-1) = 0.d0
40          continue
30      continue
!
        call jelibe(jexnum(resu//'.UALF', iblo))
20  end do
!
    call ualfva(resu, 'G')
!
    call jedema()
end subroutine
