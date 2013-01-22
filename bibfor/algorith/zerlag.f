      SUBROUTINE ZERLAG( TYPC, VECTR, VECTZ, NBDDL, IDEEQ )
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/01/2013   AUTEUR BERRO H.BERRO 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
C                             FONCTION 
C    _________________________________________________________________
C   | ANNULER LES DDL DE LAGRANGE DANS UN VECTEUR (.VALE D'UN CHAMNO) |
C   |_________________________________________________________________|
C
C
C   EXEMPLES : CALL ZERLAG ('R', ZR(JVECT), CBID     , NEQ, ZI(JDEEQ))
C              CALL ZERLAG ('C', RBID     , ZC(JVECT), NEQ, ZI(JDEEQ))
C
C                     DESCRIPTIVE DES VARIABLES
C   ___________________________________________________________________ 
C  | IN > TYPC   : LE TYPE (REEL :'R' OU COMPLEXE 'C') DES CHAMPS  [K1]|
C  |               A TRAITER                                           |
C   ___________________________________________________________________ 
C  | IN > VECTR  : VECTEUR REEL DE TAILLE NBDDL A TRAITER          [R8]|
C  |OUT <                      (SI COMPL.EQ.0 )                        |
C  | IN > VECTZ  : VECTEUR COMPLEXE DE TAILLE NBDDL A TRAITER     [C16]|
C  |OUT <                      (SI COMPL.EQ.1 )                        |
C   ___________________________________________________________________ 
C  | IN > NBDDL  : NOMBRE DE DDL PHYSIQUES / TAILLE DU VECTEUR      [I]|
C  | IN > IDEEQ  : VECTEUR DES DESCRIPTEURS D'EQUATIONS DU NUME_DDL [I]|
C   ___________________________________________________________________ 
C
      INCLUDE 'jeveux.h'
C   ___________________________________________________________________ 
C
C  - 0 - INITIALISATIONS DIVERSES
C   ___________________________________________________________________ 
C
C     0.1 - DECLARATION DES VARIABLES D'ENTREE/SORTIE
C
      INTEGER        NBDDL, IDEEQ(2,NBDDL)
      REAL*8         VECTR(NBDDL)
      COMPLEX*16     VECTZ(NBDDL)
      CHARACTER*1    TYPC
C
C     0.2 - DECLARATION DES VARIABLES LOCALES
C
      INTEGER        I ,ITYP 
C  ____________________________________________________________________ 
C
C  - 1 - RECHERCHE DES DDL DE LAGRANGE ET REMPLISSAGE LES VALEURS DES
C        CORRESPONDANTS PAR DES ZEROS (REELS OU COMPLEXES SELON LE CAS)
C  ____________________________________________________________________ 
C
C     1.1 - CAS REEL
      IF (TYPC.EQ.'R') THEN
        DO 10 I = 1,NBDDL
          ITYP = IDEEQ(2,I)
          IF(ITYP.LE.0) VECTR(I)=0.D0
   10   CONTINUE
      ELSE
C     1.2 - CAS COMPLEXE
        DO 20 I = 1,NBDDL
          ITYP = IDEEQ(2,I)
          IF(ITYP.LE.0) VECTZ(I)=DCMPLX(0.D0,0.D0)
   20   CONTINUE
      ENDIF
C  ____________________________________________________________________ 
C
      END
