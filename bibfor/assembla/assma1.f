      SUBROUTINE ASSMA1(MATAS,LDIST)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_4
C RESPONSABLE PELLET J.PELLET
C--------------------------------------------------------------
C BUT : METTRE A L'ECHELLE LES LIGNES ET COLONNES D'UNE MATR_ASSE
C       CORRESPONDANT AUX DDLS DE LAGRANGE
C
C IN/JXVAR : MATAS (K19) : SD_MATR_ASSE  :
C    -- CREATION DE L'OBJET .CONL
C    -- MODIFICATION DE L'OBJET .VALM
C IN LDIST (LOGICAL): INDIQUE SI LE CALCUL EST DISTRIBUE AU SENS
C                     DONNEE INCOMPLETE PAR PROC
C---------------------------------------------------------------
      CHARACTER*(*) MATAS
C --- DECLARATIONS NORMALISEES JEVEUX ----------------------------
      INTEGER*4 ZI4
      COMMON  / I4VAJE / ZI4(1)
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32,JEXNUM
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C---------------------------------------------------------------
      LOGICAL LMNSY,EXILAG,LDIST
      INTEGER JSMDI,NSMHC,JDELGG,JDELGL,JSMHC,NG,IBID,NZ,N,IMATD
      INTEGER ILIG,JCOL,KTERM,NLONG,JREFA,NVALE,JVALM1,JVALM2,JCONL
      CHARACTER*1 KBID,KTYP,BASE1
      CHARACTER*14 NONU
      CHARACTER*19 MAT19
      REAL*8 RMIN,RMAX,RCOEF,R8MAEM,RDIAG,RCOEF2
C=================================================================
      CALL JEMARQ()



C 1. *  MISE EN MEMOIRE DES OBJETS JEVEUX
C    *  CALCUL DE  :
C        N  : NOMBRE D'EQUATIONS
C        NZ : NOMBRE DE TERMES NON NULS DANS LA MOITIE SUPERIEURE
C        LMNSY : .TRUE.  : LA MATRICE EST NON SYMETRIQUE
C                .FALSE. : LA MATRICE EST SYMETRIQUE
C        KTYP  : 'R'/'C'
C        BASE1 : 'G'/'V'
C    *  QUELQUES VERIFICATIONS DE COHERENCE
C ---------------------------------------------------------------
      MAT19=MATAS
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
      CALL JEVEUO(JEXNUM(MAT19//'.VALM',1),'E',JVALM1)
      CALL JELIRA(JEXNUM(MAT19//'.VALM',1),'LONMAX',NLONG,KBID)
      CALL ASSERT(NLONG.EQ.NZ)

      LMNSY=.FALSE.
      CALL JELIRA(MAT19//'.VALM','NMAXOC',NVALE,KBID)
      IF (NVALE.EQ.2) LMNSY=.TRUE.

      IF (LMNSY) THEN
        CALL JEVEUO(JEXNUM(MAT19//'.VALM',2),'E',JVALM2)
        CALL JELIRA(JEXNUM(MAT19//'.VALM',2),'LONMAX',NLONG,KBID)
        CALL ASSERT(NLONG.EQ.NZ)
      ENDIF



C 2.  CALCUL DU COEFFICIENT DE CONDITIONNEMENT DES LAGRANGES (RCOEF)
C -------------------------------------------------------------------
      RMIN=R8MAEM()
      RMAX=-1.D0
C     CALCUL DE RMIN : PLUS PETIT TERME NON NUL DE LA DIAGONALE
C     CALCUL DE RMAX : PLUS GRAND TERME DE LA DIAGONALE
C     CALCUL DE EXILAG : .TRUE. : IL EXISTE DES DDLS DE LAGRANGE
      EXILAG=.FALSE.
      DO 10,JCOL=1,N
        IF (ZI(JDELGL-1+JCOL).LT.0) THEN
          EXILAG=.TRUE.
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
      IF ( IMATD.NE.0 ) THEN
        EXILAG=.TRUE.
      ENDIF

C     -- S'IL N'Y A PAS DE LAGRANGE, IL N'Y A RIEN A FAIRE :
      IF (.NOT.EXILAG) GOTO 40

      CALL ASSERT(RMIN.GE.0.D0)
C     -- PARFOIS, LA MATRICE EST == 0.
C     -- DANS CE CAS, RCOEF=1.D0
      IF (RMAX.EQ.0.D0) THEN
        RCOEF=1.D0
      ELSE
        CALL ASSERT(RMAX.GE.RMIN)
C       RCOEF1: MOYENNE GEOMETRIQUE DE RMIN ET RMAX
C       RCOEF1=SQRT(RMAX*RMIN)
C       RCOEF2: MOYENNE ARITHMETIQUE DE RMIN ET RMAX
        RCOEF2=0.5D0*(RMAX+RMIN)
C       J'AURAIS PREFERE RCOEF1 MAIS CELA FAIT PLANTER
C       LES TESTS SDNL101A ET SDNV103F
        RCOEF=RCOEF2
      ENDIF

C     -- SI EXECUTION PARALLELE, IL FAUT COMMUNIQUER :
      IF (LDIST) CALL MPICM1('MPI_MAX','R',1,IBID,RCOEF)
      CALL ASSERT(RCOEF.GT.0.D0)


C ---------------------------------------------------------------
      CALL WKVECT(MAT19//'.CONL',BASE1//' V R',NG,JCONL)
      DO 20,JCOL=1,NG
        IF (ZI(JDELGG-1+JCOL).EQ.0) THEN
          ZR(JCONL-1+JCOL)=1.D0
        ELSE
          ZR(JCONL-1+JCOL)=RCOEF
        ENDIF
   20 CONTINUE


C 4.  MISE A L'ECHELLE DE LA MATRICE
C ---------------------------------------------------------------
      JCOL=1
      DO 30,KTERM=1,NZ
        IF (ZI(JSMDI-1+JCOL).LT.KTERM)JCOL=JCOL+1
        ILIG=ZI4(JSMHC-1+KTERM)
        IF (ZI(JDELGL-1+JCOL)+ZI(JDELGL-1+ILIG).LT.0) THEN
          IF (KTYP.EQ.'R') THEN
            ZR(JVALM1-1+KTERM)=RCOEF*ZR(JVALM1-1+KTERM)
          ELSE
            ZC(JVALM1-1+KTERM)=RCOEF*ZC(JVALM1-1+KTERM)
          ENDIF
          IF (LMNSY) THEN
            IF (KTYP.EQ.'R') THEN
              ZR(JVALM2-1+KTERM)=RCOEF*ZR(JVALM2-1+KTERM)
            ELSE
              ZC(JVALM2-1+KTERM)=RCOEF*ZC(JVALM2-1+KTERM)
            ENDIF
          ENDIF
        ENDIF
   30 CONTINUE
      CALL ASSERT(JCOL.EQ.N)


   40 CONTINUE
      CALL JEDEMA()

      END
