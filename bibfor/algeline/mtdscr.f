      SUBROUTINE MTDSCR(NOMMAT)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*) NOMMAT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     ------------------------------------------------------------------
C     ALLOCATION DES DESCRIPTEURS D'UNE MATRICE
C     ------------------------------------------------------------------
C
C IN  NOMMAT  : K19 : NOM DE LA MATRICE
C     ------------------------------------------------------------------
C     CETTE ROUTINE CREE 2 OBJETS DE TRAVAIL SUR LA BASE VOLATILE
C
C     DE NOM  NOMMAT//'.&INT'   VECTEUR D'ENTIER
C             NOMMAT//'.&IN2'   VECTEUR DE K24
C
C     ZI(+0) : INUTILISE
C     ZK24(ZI(+1) : NOM DEVELOPPEUR DE LA MATRICE
C     ZI(+2) : NOMBRE GLOBAL D'EQUATIONS
C     ZI(+3) : TYPE DE VALEURS
C                1 : REELLE
C                2 : COMPLEXE
C     ZI(+4) : PROPRIETE DE SYMETRIE DE LA MATRICE
C                0 : QUELCONQUE
C                1 : SYMETRIQUE
C     ZI(+5) : NOMBRE LOCAL D'EQUATIONS
C     ZI(+6) : INUTILISE
C     ZI(+7) : NOMBRE DE DDLS IMPOSES PAR DES CHARGES CINEMATIQUES DANS
C              LA MATRICE ASSEMBLEE = NELIM
C
C     ZI(+10) : INUTILISE
C     ZI(+11) : INUTILISE
C     ZI(+12) : INUTILISE
C     ZI(+13) : INUTILISE
C     ZI(+14) : LONGUEUR DU BLOC POUR LA MATRICE MORSE
C     ZI(+15) : INUTILISE
C     ZI(+16) : INUTILISE
C     ZI(+17) : INUTILISE
C     ZI(+18) : INUTILISE
C     ------------------------------------------------------------------
C
C
C
C     ----- PARAMETRES DE DEFINITION DES MATRICES ----------------------
      INTEGER IMATD
      CHARACTER*2 TYMA
      CHARACTER*4 KBID
      CHARACTER*14 NU
      CHARACTER*19 MAT19,NOMSTO
C     ------------------------------------------------------------------


C-----------------------------------------------------------------------
      INTEGER IBID ,IER ,IRET ,ISMAEM ,JCCID ,JNEQU ,JREFA 
      INTEGER JSCDE ,JSMDE ,K ,LCCID ,LMAT ,LNOM ,NB1 
      INTEGER NB2 
C-----------------------------------------------------------------------
      CALL JEMARQ()
      MAT19 = NOMMAT


C        ------ ALLOCATION DES OBJETS SI NECESSAIRE :
      CALL JEEXIN(MAT19//'.&INT',IER)
      IF (IER.EQ.0) THEN
        CALL JECREO(MAT19//'.&INT',' V V I')
        CALL JEECRA(MAT19//'.&INT','LONMAX',19,'  ')
      END IF

      CALL JEVEUO(MAT19//'.&INT','E',LMAT)
      DO 10,K = 1,19
        ZI(LMAT-1+K) = ISMAEM()
   10 CONTINUE

      CALL JEEXIN(MAT19//'.&IN2',IER)
      IF (IER.EQ.0) THEN
        CALL WKVECT(MAT19//'.&IN2',' V V K24',1,LNOM)
      END IF

      CALL JEVEUT(MAT19//'.&IN2','E',LNOM)
      ZK24(LNOM) = MAT19


C     -- LMAT+1 :
C     ------------
      ZI(LMAT+1) = LNOM


      CALL JEEXIN(MAT19//'.REFA',IER)
      CALL ASSERT(IER.NE.0)


      CALL JEVEUO(MAT19//'.REFA','L',JREFA)
      NU = ZK24(JREFA-1+2)
      NOMSTO = NU//'.SMOS'

C     -- LMAT+2 ET +5 :
C     ------------
      CALL JEVEUO(NOMSTO//'.SMDE','L',JSMDE)
      CALL JEVEUO(NU//'.NUME.NEQU','L',JNEQU)
      ZI(LMAT+2) = ZI(JNEQU-1+1)
      CALL JEEXIN(NU//'.NUML.NULG',IMATD)
      IF ( IMATD.NE.0 ) THEN
        CALL JEVEUO(NU//'.NUML.NEQU','L',JNEQU)
        ZI(LMAT+5) = ZI(JNEQU-1+1)
      ELSE
        ZI(LMAT+5) = ZI(LMAT+2)
      ENDIF


C     -- LMAT+3 :
C     ------------
      CALL JEEXIN(MAT19//'.VALM',IRET)
C     -- POUR TRAITER LE CAS OU ON A DETRUIT VOLONTAIREMENT LE .VALM
      IF (IRET.GT.0) THEN
        CALL JELIRA(MAT19//'.VALM','TYPE',IBID,KBID)
      ELSE
        CALL JELIRA(MAT19//'.UALF','TYPE',IBID,KBID)
      END IF

      CALL ASSERT(KBID(1:1).EQ.'R' .OR. KBID(1:1).EQ.'C')
      IF (KBID(1:1).EQ.'R') ZI(LMAT+3) = 1
      IF (KBID(1:1).EQ.'C') ZI(LMAT+3) = 2


C     -- LMAT+4 :
C     ------------
      CALL JEEXIN(MAT19//'.VALM',IRET)
C     -- POUR TRAITER LE CAS OU ON A DETRUIT VOLONTAIREMENT LE .VALM
      IF (IRET.GT.0) THEN
        TYMA = ZK24(JREFA-1+9)
        IF (TYMA.EQ.'MS') THEN
          ZI(LMAT+4) = 1

        ELSE IF (TYMA.EQ.'MR') THEN
          ZI(LMAT+4) = 0

        ELSE
          CALL ASSERT(.FALSE.)
        END IF

      ELSE
        CALL JELIRA(MAT19//'.UALF','NMAXOC',NB1,KBID)
        CALL JEVEUO(NU//'.SLCS.SCDE','L',JSCDE)
        NB2 = ZI(JSCDE-1+3)
        IF (NB1.EQ.NB2) THEN
          ZI(LMAT+4) = 1

        ELSE IF (NB1.EQ.2*NB2) THEN
          ZI(LMAT+4) = 0

        ELSE
          CALL ASSERT(.FALSE.)
        END IF

      END IF


C     -- LMAT+7    (SI CHARGES CINEMATIQUES) :
C     -------------------------------------------------
      CALL JEEXIN(MAT19//'.CCID',IER)
      IF (IER.NE.0) THEN
        CALL JEVEUO(MAT19//'.CCID','L',JCCID)
        CALL JELIRA(MAT19//'.CCID','LONMAX',LCCID,KBID)
        ZI(LMAT+7) = ZI(JCCID-1+LCCID+1)
      ELSE
        ZI(LMAT+7) = 0
      END IF


C     -- LMAT+14
C     ----------
      ZI(LMAT+14) = ZI(JSMDE-1+2)


      CALL JEDEMA()
      END
