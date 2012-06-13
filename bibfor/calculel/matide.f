      SUBROUTINE MATIDE(MATZ,NBCMP,LICMP,MODLAG,TDIAG,VDIAG)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE PELLET
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*) MATZ
      INTEGER NBCMP
      REAL*8 VDIAG
      CHARACTER*8 LICMP(NBCMP),TDIAG
      CHARACTER*16 MODLAG
C TOLE CRP_4
C ---------------------------------------------------------------------
C BUT: METTRE "A L'IDENTITE"  UNE MATR_ASSE SUR CERTAINS DDLS
C ---------------------------------------------------------------------
C     ARGUMENTS:
C MATZ   IN/JXVAR K19  : MATR_ASSE A MODIFIER
C NBCMP  IN       I    : NOMBRE DE COMPOSANTES DE LICMP
C LICMP  IN       K8   : LISTE DES NOMS DES COMPOSANTES
C MODLAG IN       K16  : MODIFICATION OU PAS DES TERMES DE LAGRANGE
C                        VALEURS : MODI_LAGR_OUI OU MODI_LAGR_NON
C TDIAG  IN       K8   : NORMALISATION DES TERMES MODIFIEES 
C                        DE LA DIAGONALE
C                        VALEURS : MAX_ABS, MIN_ABS ou IMPOSE
C VDIAG  IN       R8   : SI TDIAG VAUT IMPOSE ALORS VDIAG PERMET
C                        DE DONNER LA vALEUR A IMPOSER
C ---------------------------------------------------------------------


C     ------------------------------------------------------------------
      INTEGER ILIG,JCOL,KTERM,N,NZ,JREFA,JSMDI,NSMDI,JSMHC,NSMHC
      INTEGER JDELG,N1,NVALE,JVALE,NLONG,JVAL2,IBID,NUCMP,K,JCMP
      INTEGER JDEEQ,JREFN,KCMP,JLDDL,JLLAG
      CHARACTER*1 KBID
      CHARACTER*8 NOMGD,NOCMP
      CHARACTER*14 NONU
      CHARACTER*1 KTYP
      CHARACTER*19 MAT19
      LOGICAL LTYPR,LSYM,ELIML,ELIMC
      REAL*8 KMAX
      COMPLEX*16   CKMAX

C     ------------------------------------------------------------------
      CALL JEMARQ()

      MAT19=MATZ

      CALL JEVEUO(MAT19//'.REFA','L',JREFA)
      NONU=ZK24(JREFA-1+2)

      CALL JEVEUO(NONU//'.SMOS.SMDI','L',JSMDI)
      CALL JELIRA(NONU//'.SMOS.SMDI','LONMAX',NSMDI,KBID)
      CALL JEVEUO(NONU//'.SMOS.SMHC','L',JSMHC)
      CALL JELIRA(NONU//'.SMOS.SMHC','LONMAX',NSMHC,KBID)
      CALL JEVEUO(NONU//'.NUME.DELG','L',JDELG)
      CALL JELIRA(NONU//'.NUME.DELG','LONMAX',N1,KBID)
      CALL ASSERT(N1.EQ.NSMDI)
C     --- CALCUL DE N
      N=NSMDI
C     --- CALCUL DE NZ
      NZ=ZI(JSMDI-1+N)

      CALL ASSERT(NZ.LE.NSMHC)
      CALL JELIRA(MAT19//'.VALM','NMAXOC',NVALE,KBID)
      IF (NVALE.EQ.1) THEN
        LSYM=.TRUE.
      ELSEIF (NVALE.EQ.2) THEN
        LSYM=.FALSE.
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

      CALL JEVEUO(JEXNUM(MAT19//'.VALM',1),'E',JVALE)
      CALL JELIRA(JEXNUM(MAT19//'.VALM',1),'LONMAX',NLONG,KBID)
      CALL ASSERT(NLONG.EQ.NZ)
      IF (.NOT.LSYM) THEN
        CALL JEVEUO(JEXNUM(MAT19//'.VALM',2),'E',JVAL2)
        CALL JELIRA(JEXNUM(MAT19//'.VALM',2),'LONMAX',NLONG,KBID)
        CALL ASSERT(NLONG.EQ.NZ)
      ENDIF

      CALL JELIRA(JEXNUM(MAT19//'.VALM',1),'TYPE',IBID,KTYP)
      LTYPR=(KTYP.EQ.'R')

C     -- CALCUL DE LA LISTE DES DDLS A ELIMINER :
C     -------------------------------------------
      CALL WKVECT('&&MATIDE.LDDLELIM','V V I',N,JLDDL)
      CALL WKVECT('&&MATIDE.LLAG','V V I',N,JLLAG)

      CALL JEVEUO(NONU//'.NUME.DEEQ','L',JDEEQ)
      CALL JEVEUO(NONU//'.NUME.REFN','L',JREFN)
      CALL JELIRA(NONU//'.NUME.DEEQ','LONMAX',N1,KBID)
      NOMGD=ZK24(JREFN-1+2)
      CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP',NOMGD),'L',JCMP)
      CALL ASSERT(N1.EQ.2*N)
      DO 20,K=1,N
        NUCMP=ZI(JDEEQ-1+2*(K-1)+2)
        IF (NUCMP.GT.0) THEN
          NOCMP=ZK8(JCMP-1+NUCMP)
          DO 10,KCMP=1,NBCMP
            IF (NOCMP.EQ.LICMP(KCMP)) THEN
              ZI(JLDDL-1+K)=1
            ENDIF
   10     CONTINUE
        ELSEIF ( MODLAG(1:13) .EQ. 'MODI_LAGR_OUI' ) THEN
          ZI(JLLAG-1+K)=1
        ENDIF
   20 CONTINUE



C     ------------------------------------------------
C     PARCOURS DES TERMES DE LA MATRICE
C     ------------------------------------------------
      JCOL=1
      KMAX = 0.D0
      CKMAX = DCMPLX(0.D0,0.D0)
      IF (TDIAG(1:7) .EQ. 'MAX_ABS') THEN
        IF (LTYPR) THEN
          DO 25,KTERM=1,NZ
            KMAX = MAX(ABS(ZR(JVALE-1+KTERM)),ABS(KMAX))
   25     CONTINUE
          KMAX = KMAX*VDIAG
        ELSE
          DO 26,KTERM=1,NZ
            CKMAX = MAX(ABS(ZC(JVALE-1+KTERM)),ABS(CKMAX))
   26     CONTINUE
          CKMAX = CKMAX*VDIAG
        ENDIF
      ELSEIF (TDIAG(1:7) .EQ. 'MIN_ABS') THEN
        IF (LTYPR) THEN
          KMAX = ABS(ZR(JVALE))
          DO 27,KTERM=1,NZ
            KMAX = MAX(ABS(ZR(JVALE-1+KTERM)),ABS(KMAX))
   27     CONTINUE
          KMAX = KMAX*VDIAG
        ELSE
          CKMAX = ABS(ZC(JVALE))
          DO 28,KTERM=1,NZ
            CKMAX = MAX(ABS(ZC(JVALE-1+KTERM)),ABS(CKMAX))
   28     CONTINUE
          CKMAX = CKMAX*VDIAG
        ENDIF
      ELSEIF (TDIAG(1:6) .EQ. 'IMPOSE') THEN
        KMAX = VDIAG
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
      
      DO 30,KTERM=1,NZ
        IF (ZI(JSMDI-1+JCOL).LT.KTERM)JCOL=JCOL+1
        ILIG=ZI4(JSMHC-1+KTERM)
        ELIMC=.FALSE.
        ELIML=.FALSE.
        IF ((ZI(JLDDL-1+JCOL).EQ.1).AND.(ZI(JLLAG-1+JCOL).EQ.0)
     &  .AND.(ZI(JLLAG-1+ILIG).EQ.0)) ELIMC=.TRUE.
        IF ((ZI(JLDDL-1+ILIG).EQ.1).AND.(ZI(JLLAG-1+ILIG).EQ.0)
     &   .AND.(ZI(JLLAG-1+JCOL).EQ.0)) ELIML=.TRUE.

        IF (ELIMC .OR. ELIML) THEN

C         -- PARTIE TRIANGULAIRE SUPERIEURE :
          IF (JCOL.EQ.ILIG) THEN
            IF (LTYPR) THEN
              ZR(JVALE-1+KTERM)=KMAX
            ELSE
              ZC(JVALE-1+KTERM)=CKMAX
            ENDIF
          ELSE
            IF (LTYPR) THEN
              ZR(JVALE-1+KTERM)=0.D0
            ELSE
              ZC(JVALE-1+KTERM)=DCMPLX(0.D0,0.D0)
            ENDIF
          ENDIF

C         -- PARTIE TRIANGULAIRE INFERIEURE (SI NON-SYMETRIQUE):
          IF (.NOT.LSYM) THEN
            IF (JCOL.EQ.ILIG) THEN
              IF (LTYPR) THEN
                ZR(JVAL2-1+KTERM)=KMAX
             ELSE
                ZC(JVALE-1+KTERM)=CKMAX
              ENDIF
            ELSE
              IF (LTYPR) THEN
                ZR(JVAL2-1+KTERM)=0.D0
              ELSE
                ZC(JVAL2-1+KTERM)=DCMPLX(0.D0,0.D0)
              ENDIF
            ENDIF
          ENDIF
        ENDIF

   30 CONTINUE

      CALL JEDETR('&&MATIDE.LDDLELIM')
      CALL JEDETR('&&MATIDE.LLAG')
      CALL JEDEMA()
      END
