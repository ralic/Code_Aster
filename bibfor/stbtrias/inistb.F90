subroutine inistb(maxnod, nbtyma, nomail, indic, permut,&
                  limail, indicf, permuf, maxfa)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
!     ====================================================
!A PRESUPER
!
!   ===================================================================
!   !                                                                 !
!   !  AUTEURS:J.M. PROIX                             DATE:20/11/89   !
!!J.F.LAMAUDIERED       DATE 15/05/91
!   !                                                                 !
!   !  LOGISCOPE STATIQUE :09/02/90                                   !
!   !                                                                 !
!   !  LOGISCOPE DYNAMIQUE (35 JEUX TESTS) :21/02/90                  !
!   !                                                                 !
!   ===================================================================
!   !                                                                 !
!   !  FONCTION: INITIALISATION DES NOMS DES MAILLES ASTER-TRIFOU EN  !
!   !            FONCTION DU CODE GRAPHIQUE SUPERTAB I-DEAS 4.0       !
!   !                                                                 !
!   !  REMARQUE: LA LISTE DES CODES DESCRIPTEURS (VERSION 5) EST      !
!   !            DONNEE ICI SEULEMENT A TITRE INDICATIF .             !
!   !            UNE CORRESPONDANCE EST FAITE AU NIVEAU DU SOUS_      !
!   !            PROGRAMME COV4V5 (CORRESPONDANCE DU CODE DESCRIPTEUR !
!   !            EN VERSION 5 ET DU CODE GRAPHIQUE EN VERSION 4       !
!   !                                                                 !
!   !   !!!! PERMUTATIONS EVENTUELLES DES NUMEROTATIONS LOCALES!!!!   !
!   !   !!!!       POUR LES MAILLES ET LES FACES               !!!!   !
!   !                                                                 !
!   !   (CF. DOCUMENT INTERFACE SUPERTAB-ASTER/SUPERTAB-TRIFOU)       !
!   !                                                                 !
!   ===================================================================
!   !                                                                 !
!   !  ROUTINE APPELANTE : PRESUP                                     !
!   !                                                                 !
!   ===================================================================
!   !                                                                 !
!   !                   ***************                               !
!   !                   *  ARGUMENTS  *                               !
!   !                   ***************                               !
!   !                                                                 !
!   !  ************************************************************** !
!   !  *  NOM    *  TYPE * MODE  *ALTERE *         ROLE             * !
!   !  ************************************************************** !
!   !  *         *       *       *       *                          * !
!   !  * MAXNOD  *INTEGER*ENTREE * NON   *NBRE MAXI DE NOEUDS POUR  * !
!   !  *         *       *       *       *  UNE  MAILLE             * !
!   !  *         *       *       *       *                          * !
!   !  * MAXFA   *INTEGER*ENTREE * NON   *NBRE MAXI DE FACES POUR   * !
!   !  *         *       *       *       *  UNE MAILLE
!   !  *         *       *       *       *                          * !
!   !  * NBTYMA  *INTEGER*SORTIE * NON   *NBRE DE TYPE DE MAILLES   * !
!   !  *         *       *       *       *   SUPERTAB               * !
!   !  *         *       *       *       *                          * !
!   !  * NOMAIL  *CHARACT*SORTIE * NON   *NOMS DES MAILES EN FONCT  * !
!   !  *         *       *       *       * DU CODE GRAPHIQUE S.TAB  * !
!   !  *         *       *       *       *                          * !
!   !  *         *       *       *       *                          * !
!   !  * INDIC   *INTEGER*SORTIE * NON   * INDIQUE S'IL FAUT FAIRE  * !
!   !  *         *       *       *       * UNE PERMUTATION POUR LA  * !
!   !  *         *       *       *       * NUMEROTION LOCALE DES    * !
!   !  *         *       *       *       * DES ELEMENTS             * !
!   !  *         *       *       *       *                          * !
!   !  * PERMUT  *INTEGER*SORTIE * NON   * TABLEAU DE PERMUTATION   * !
!   !  *         *       *       *       * POUR LA CONNECTIVITE     * !
!   !  *         *       *       *       *                          * !
!   !  * LIMAIL  *INTEGER*SORTIE * NON   * TABLEAU CONTENANT LE NBRE* !
!   !  *         *       *       *       * DE LIGNES POUR L'ECRITURE* !
!   !  *         *       *       *       * DE CHAQUE TYPE DE MAILLE * !
!   !  *         *       *       *       *                          * !
!   !  * INDICF  *INTEGER*SORTIE * NON   * INDIQUE S'IL FAUT FAIRE  * !
!   !  *         *       *       *       * UNE PERMUTATION POUR LA  * !
!   !  *         *       *       *       * NUMEROTATION LOCALE DES  * !
!   !  *         *       *       *       * FACES                    * !
!   !  *         *       *       *       *                          * !
!   !  * PERMUF  *INTEGER*SORTIE * NON   * TABLEAU DE PERMUTATION   * !
!   !  *         *       *       *       * POUR LA NUMEROTATION     * !
!   !  *         *       *       *       * LOCALE DES FACES         * !
!   !  *         *       *       *       *                          * !
!   !  ************************************************************** !
!   !                                                                 !
!   ===================================================================
!
!  --> DECLARATIONS DES ARGUMENTS
    character(len=8) :: nomail(*)
    integer :: maxfa, maxnod, nbtyma, limail(*)
    integer :: permut(maxnod, *), indic(*), permuf(maxfa, *), indicf(*)
!  --> DECLARATIONS DES VARIABLES LOCALES
    integer :: permu2(20, 3:35), nbnoma(3:35)
    integer :: permu3(6, 14:21), nbnomf(14:21)
!
!  --> DECLARATIONS DES INDICES DE BOUCLES
    integer :: i, j
!
!
!======================= SUPERTAB I-DEAS 4.0 ========================
!
!     CODE GRAPHIQUE     MAILLE             NOEUDS   FACE   TYPE
!     ______________     ______             ______   ____   ____
!
!          1             LINEIQUE              2           LINEAIRE
!          2             TRIANGLE              3           LINEAIRE
!          3             TRIANGLE              6           QUADRATIQUE
!          4             TRIANGLE              9           CUBIQUE
!          5             QUADRILATERE          4           LINEAIRE
!          6             QUADRILATERE          8           QUADRATIQUE
!          7             QUADRILATERE         12           CUBIQUE
!          8             PENTA COQUE EPAISSE   6           LINEAIRE=16
!          9             PENTA COQUE EPAISSE  12           QUADRATIQUE
!         10             PENTA COQUE EPAISSE  18           CUBIQUE
!         11             BRIQUE COQUE EPAISSE  8           LINEAIRE=19
!         12             BRIQUE COQUE EPAISSE 16           QUADRATIQUE
!         13             BRIQUE COQUE EPAISSE 24           CUBIQUE
!         14             TETRAEDRE             4     4     LINEAIRE
!         15             TETRAEDRE            10     4     QUADRATIQUE
!         16             PENTAEDRE             6     5     LINEAIRE
!         17             PENTAEDRE            15     5     QUADRATIQUE
!         18             PENTAEDRE            24     5     CUBIQUE
!         19             HEXAEDRE              8     6     LINEAIRE
!         20             HEXAEDRE             20     6     QUADRATIQUE
!         21             HEXAEDRE             32     6     CUBIQUE
!         22-28 IGNORES
!         29             RESSORT ENTRE NOEUDS  2           LINEAIRE
!         30             RESSORT FIXE AU SOL   1           LINEAIRE
!         31             AMORTIS ENTRE NOEUDS  2           LINEAIRE
!         32             AMORTIS FIXE AU SOL   1           LINEAIRE
!         33             MASSE CONCENTREE      1           LINEAIRE
!         34             BARRE RIGIDE          2           LINEAIRE
!         35             LINEIQUE              3           PARABOLIQUE
!
!
!======================= SUPERTAB I-DEAS 5.0 ========================
!
!     CODE DESCRIPTEUR   MAILLE             NOEUDS   FACE   TYPE
!     ________________   ______             ______   ____   ____
!
!     171                LINEIQUE              2           LINEAIRE
!     91_61_74_81        TRIANGLE              3           LINEAIRE
!     92_62_72_82        TRIANGLE              6           QUADRATIQUE
!     93_63_73           TRIANGLE              9           CUBIQUE
!     94_64_71_84        QUADRILATERE          4           LINEAIRE
!     95_65_75_85        QUADRILATERE          8           QUADRATIQUE
!     96_66_76           QUADRILATERE         12           CUBIQUE
!     101                PENTA COQUE EPAISSE   6           LINEAIRE=16
!     102                PENTA COQUE EPAISSE  12           QUADRATIQUE
!     103                PENTA COQUE EPAISSE  18           CUBIQUE
!     104                BRIQUE COQUE EPAISSE  8           LINEAIRE=19
!     105                BRIQUE COQUE EPAISSE 16           QUADRATIQUE
!     106                BRIQUE COQUE EPAISSE 24           CUBIQUE
!     111                TETRAEDRE             4     4     LINEAIRE
!     118                TETRAEDRE            10     4     QUADRATIQUE
!     112                PENTAEDRE             6     5     LINEAIRE
!     113                PENTAEDRE            15     5     QUADRATIQUE
!     114                PENTAEDRE            24     5     CUBIQUE
!     115                HEXAEDRE              8     6     LINEAIRE
!     116                HEXAEDRE             20     6     QUADRATIQUE
!     117                HEXAEDRE             32     6     CUBIQUE
!     136_137            RESSORT ENTRE NOEUDS  2           LINEAIRE
!     138_139            RESSORT FIXE AU SOL   1           LINEAIRE
!     141                AMORTIS ENTRE NOEUDS  2           LINEAIRE
!     142                AMORTIS FIXE AU SOL   1           LINEAIRE
!     161                MASSE CONCENTREE      1           LINEAIRE
!     121                BARRE RIGIDE          2           LINEAIRE
!     172                LINEIQUE              3           PARABOLIQUE
!
! --> DONNES POUR LA RENUMEROTATION LOCALE DES ELEMENTS
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    data nbnoma/6,9,4,8,12,6,12,18,8,16,24,4,10,6,15,24,8,20,0,0,0,0,&
     &            0,0,0,0,0,0,0,0,0,0,3/
    data (permu2(i,3),i=1,6)/1,4,2,5,3,6/
    data (permu2(i,4),i=1,9)/1,4,5,2,6,7,3,8,9/
    data (permu2(i,6),i=1,8)/1,5,2,6,3,7,4,8/
    data (permu2(i,7),i=1,12)/1,5,6,2,7,8,3,9,10,4,11,12/
    data (permu2(i,9),i=1,12)/1,7,2,8,3,9,4,10,5,11,6,12/
    data (permu2(i,12),i=1,16)/1,9,2,10,3,11,4,12,5,13,6,14,7,15,8,16/
    data (permu2(i,15),i=1,10)/1,5,2,6,3,7,8,9,10,4/
    data (permu2(i,17),i=1,15)/1,7,2,8,3,9,10,11,12,4,13,5,14,6,15/
    data (permu2(i,20),i=1,20)/1,9,2,10,3,11,4,12,13,14,15,16,5,17,&
     &     6,18,7,19,8,20/
    data (permu2(i,35),i=1,3)/1,3,2/
!
! --> DONNEES POUR LA RENUMEROTATION LOCALE DES FACES
    data nbnomf/4,4,5,5,5,6,6,6/
    data (permu3(i,14),i=1,4)/1,2,3,4/
    data (permu3(i,15),i=1,4)/3,1,4,2/
    data (permu3(i,16),i=1,5)/1,2,3,4,5/
    data (permu3(i,17),i=1,5)/1,4,2,5,3/
    data (permu3(i,18),i=1,5)/1,4,2,5,3/
    data (permu3(i,19),i=1,6)/1,2,3,4,5,6/
    data (permu3(i,20),i=1,6)/3,6,1,5,4,2/
    data (permu3(i,21),i=1,6)/3,6,1,5,4,2/
!
! --> FIN DES DONNEES POUR LA RENUMEROTATION
!
    do 1 j = 1, maxnod
        limail(j)=0
 1  end do
!
    do 2 i = 1, maxnod
        indic(i)=-1
 2  end do
!
    do 3 i = 1, 8
        indic(i)=0
 3  end do
!
! JMP 2/5/90
!
    indic(3)=1
    indic(4)=1
    indic(6)=1
    indic(7)=1
!
    indic(9)=1
    indic(12)=1
    indic(15)=1
    indic(17)=1
    indic(20)=1
    indic(35)=1
!
    indic(11)=0
    indic(14)=0
    indic(16)=0
    indic(19)=0
!
    do 4 i = 29, 34
        indic(i)=0
 4  end do
!
    nbtyma=35
!
    do 5 i = 1, nbtyma
        if (indic(i) .eq. 1) then
!
            do 6 j = 1, nbnoma(i)
                permut(j,i)=permu2(j,i)
 6          continue
!
        endif
 5  end do
!
    do 10 i = 1, maxnod
        indicf(i)=-1
10  end do
!
    do 20 i = 1, 13
        indicf(i)=0
20  end do
!
    do 30 i = 14, 21
        indicf(i)=1
30  end do
!
    do 40 i = 29, nbtyma
        indicf(i)=0
40  end do
!
    do 50 i = 1, nbtyma
        if (indicf(i) .eq. 1) then
            do 60 j = 1, nbnomf(i)
                permuf(j,i)=permu3(j,i)
60          continue
        endif
50  end do
!
    nomail( 1)='SEG2'
    nomail( 2)='TRIA3'
    nomail( 3)='TRIA6'
    nomail( 4)='TRIA9'
    nomail( 5)='QUAD4'
    nomail( 6)='QUAD8'
    nomail( 7)='QUAD12'
    nomail( 8)='PENTAC6'
    nomail( 9)='PENTAC12'
    nomail(10)='PENTAC18'
    nomail(11)='HEXACE8'
    nomail(12)='HEXACE16'
    nomail(13)='HEXACE24'
    nomail(14)='TETRA4'
    nomail(15)='TETRA10'
    nomail(16)='PENTA6'
    nomail(17)='PENTA15'
    nomail(18)='PENTA24'
    nomail(19)='HEXA8'
    nomail(20)='HEXA20'
    nomail(21)='HEXA32'
    nomail(22)=' '
    nomail(23)=' '
    nomail(24)=' '
    nomail(25)=' '
    nomail(26)=' '
    nomail(27)=' '
    nomail(28)=' '
    nomail(29)='SEG2'
    nomail(30)='POI1'
    nomail(31)='SEG2'
    nomail(32)='POI1'
    nomail(33)='POI1'
    nomail(34)='SEG2'
    nomail(35)='SEG3'
!
!  --> TABLEAU POUR LES NOMBRES DE LIGNE DECRIVANT CHAQUE MAILLE
    limail(1)=1
    limail(2)=1
    limail(3)=1
    limail(4)=2
    limail(5)=1
    limail(6)=1
    limail(7)=2
    limail(8)=1
    limail(9)=2
    limail(10)=3
    limail(11)=1
    limail(12)=2
    limail(13)=3
    limail(14)=1
    limail(15)=2
    limail(16)=1
    limail(17)=2
    limail(18)=3
    limail(19)=1
    limail(20)=3
    limail(21)=4
    limail(29)=1
    limail(30)=1
    limail(31)=1
    limail(32)=1
    limail(33)=1
    limail(34)=1
    limail(35)=1
end subroutine
