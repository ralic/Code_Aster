      SUBROUTINE ECHMAT(MATZ,LDIST,RMIN,RMAX)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 26/02/2013   AUTEUR BOITEAU O.BOITEAU 
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
C RESPONSABLE PELLET J.PELLET
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM
      CHARACTER*(*) MATZ
      REAL*8 RMIN,RMAX
      LOGICAL LDIST
C ---------------------------------------------------------------------
C BUT: DONNER LES VALEURS EXTREMES DES VALEURS ABSOLUES
C      DES TERMES NON NULS DE LA DIAGONALE D'UNE MATR_ASSE
C ---------------------------------------------------------------------
C
C     ARGUMENTS:
C
C IN   MATZ  (K19)     : MATR_ASSE A ANALYSER
C IN   LDIST (LOGICAL) : INDIQUE SI LE CALCUL EST DISTRIBUE AU SENS
C                        DONNEE INCOMPLETE PAR PROC
C
C OUT  RMIN  (R8)      : PLUS PETIT TERME NON NUL (EN VALEUR ABSOLUE)
C                        SUR LA DIAGONALE DE MATZ
C OUT  RMAX  (R8)      : PLUS GRAND TERME (EN VALEUR ABSOLUE)
C                        SUR LA DIAGONALE DE MATZ
C ATTENTION : SI LA MATRICE EST IDENTIQUEMENT NULLE, LA ROUTINE
C             RETOURNE :
C               RMAX=0.D0
C               RMIN=R8MAEM ~1.8E308   (RMIN > RMAX !)
C ---------------------------------------------------------------------

C     ------------------------------------------------------------------
      INTEGER JSMDI,NSMHC,JDELGG,JDELGL,JSMHC,NG,IBID,NZ,N,IMATD
      INTEGER JCOL,NLONG,JREFA,JVALM1
      CHARACTER*1 KBID,KTYP,BASE1
      CHARACTER*14 NONU
      CHARACTER*19 MAT19
      REAL*8 R8MAEM,RDIAG
      COMPLEX*16 CBID
C=================================================================
      CALL JEMARQ()

      MAT19=MATZ
      CALL JEVEUO(MAT19//'.REFA','L',JREFA)
      NONU=ZK24(JREFA-1+2)
      CALL JELIRA(NONU//'.SMOS.SMDI','LONMAX',N,KBID)
      CALL JEVEUO(NONU//'.SMOS.SMDI','L',JSMDI)
      NZ=ZI(JSMDI-1+N)
      CALL JEVEUO(NONU//'.SMOS.SMHC','L',JSMHC)
      CALL JELIRA(NONU//'.SMOS.SMHC','LONMAX',NSMHC,KBID)
      CALL ASSERT(NZ.LE.NSMHC)

      CALL JEVEUO(NONU//'.NUME.DELG','L',JDELGG)
      CALL JELIRA(NONU//'.NUME.DELG','LONMAX',NG,KBID)
      CALL JEEXIN(NONU//'.NUML.DELG',IMATD)
      IF ( IMATD.NE.0 ) THEN
        CALL JEVEUO(NONU//'.NUML.DELG','L',JDELGL)
      ELSE
        JDELGL=JDELGG
        CALL ASSERT(NG.EQ.N)
      ENDIF

      CALL JELIRA(MAT19//'.VALM','TYPE',IBID,KTYP)
      CALL JELIRA(MAT19//'.VALM','CLAS',IBID,BASE1)
      CALL JEVEUO(JEXNUM(MAT19//'.VALM',1),'L',JVALM1)
      CALL JELIRA(JEXNUM(MAT19//'.VALM',1),'LONMAX',NLONG,KBID)
      CALL ASSERT(NLONG.EQ.NZ)


C     --CALCUL DE RMIN ET RMAX :
C     -----------------------------
      RMIN=R8MAEM()
      RMAX=-1.D0
C     CALCUL DE RMIN : PLUS PETIT TERME NON NUL DE LA DIAGONALE
C     CALCUL DE RMAX : PLUS GRAND TERME DE LA DIAGONALE
      DO 10,JCOL=1,N
        IF (ZI(JDELGL-1+JCOL).LT.0) THEN
          GOTO 10
        ENDIF

        IF (KTYP.EQ.'R') THEN
          RDIAG=ABS(ZR(JVALM1-1+ZI(JSMDI+JCOL-1)))
        ELSE
          RDIAG=ABS(ZC(JVALM1-1+ZI(JSMDI+JCOL-1)))
        ENDIF
        IF (RDIAG.GT.RMAX)RMAX=RDIAG
        IF (RDIAG.EQ.0.D0)GOTO 10
        IF (RDIAG.LT.RMIN)RMIN=RDIAG
   10 CONTINUE

C     -- SI EXECUTION PARALLELE, IL FAUT COMMUNIQUER :
      IF (LDIST) CALL MPICM1('MPI_MAX','R',1,IBID,IBID,RMAX,CBID)
      IF (LDIST) CALL MPICM1('MPI_MIN','R',1,IBID,IBID,RMIN,CBID)

      CALL JEDEMA()
      END
