      SUBROUTINE MTMCHC(MATAS,ACTION)
      IMPLICIT NONE
      CHARACTER*(*) MATAS,ACTION
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 22/06/2010   AUTEUR SELLENET N.SELLENET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C OBJET :
C        TRAITEMENT DES CHARGES CINEMATIQUES DANS UNE MATRICE ASSEMBLEE
C        SI ACTION='ELIMF' :
C            - ON UTILISE .CCID POUR :
C                CALCULER .CCVA, .CCLL, .CCII
C                MODIFIER .VALM
C        SI ACTION='ELIML' :
C            - ON UTILISE .CCVA POUR :
C                RETABLIR (EN PARTIE SEULEMENT) .VALM
C                DETRUIRE .CCVA, .CCLL, .CCII
C-----------------------------------------------------------------------
C VAR  MATAS   K*19    : NOM DE LA MATR_ASSE
C IN   ACTION  K*5     : /'ELIMF' /'ELIML'
C-----------------------------------------------------------------------
      CHARACTER*32 JEXNUM
      INTEGER*4 ZI4
      COMMON  /I4VAJE/ZI4(1)
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C----------------------------------------------------------------------
C     VARIABLES LOCALES
C----------------------------------------------------------------------
      CHARACTER*8 KBID
      CHARACTER*14 NU
      CHARACTER*19 MAT,NOMSTO
      INTEGER TYPMAT,IELIM,JELIM,KDEB,KFIN,KKELI,ILIG,JCOL
      INTEGER JSMHC,JSMDI,JVALM,JVALM2,JCCVA,JCCLL,NELIM
      INTEGER JREFA,JNEQU,IEQ,K,DECIEL,NEQ,IER,JNULG
      INTEGER IRET,NBLOCM,JREMP,DECJEL,IREMP,JCCID,KETA,IMATD
      LOGICAL NONSYM
C----------------------------------------------------------------------
      CALL JEMARQ()
      MAT = MATAS
C     CALL CHEKSD('sd_matr_asse',MAT,IRET)
      CALL JEVEUO(MAT//'.REFA','E',JREFA)
      CALL JEEXIN(MAT//'.CCID',IER)
      IF (ZK24(JREFA-1+3).EQ.' ') THEN
         CALL ASSERT(IER.EQ.0)
         GOTO 9999
      ELSE
         CALL ASSERT(IER.GT.0)
      ENDIF

      IF (ACTION.EQ.'ELIMF') THEN
         CALL ASSERT(ZK24(JREFA-1+3).EQ.'ELIML')
         CALL ASMCHC(MAT)
         GO TO 9999
      ELSE IF (ACTION.EQ.'ELIML') THEN
         CALL ASSERT(ZK24(JREFA-1+3).EQ.'ELIMF')
C        TRAITEMENT CI-DESSOUS
      ELSE
         CALL ASSERT(.FALSE.)
      ENDIF


      CALL JEVEUO(MAT//'.CCVA','L',JCCVA)
      CALL JEVEUO(MAT//'.CCLL','L',JCCLL)
      CALL JEVEUO(MAT//'.CCID','L',JCCID)



      NU = ZK24(JREFA-1+2)(1:14)
      CALL JEEXIN(NU//'.NUML.DELG',IMATD)
      IF ( IMATD.NE.0 ) THEN
        CALL JEVEUO(NU//'.NUML.NEQU','L',JNEQU)
        CALL JEVEUO(NU//'.NUML.NULG','L',JNULG)
      ELSE
        CALL JEVEUO(NU//'.NUME.NEQU','L',JNEQU)
      ENDIF
      NEQ = ZI(JNEQU)


      NOMSTO = NU//'.SMOS'
      CALL JEVEUO(NOMSTO//'.SMHC','L',JSMHC)
      CALL JEVEUO(NOMSTO//'.SMDI','L',JSMDI)


C     -- RECUPERATION DE .VALM
C        CALCUL DE TYPMAT ET NONSYM :
C     ------------------------------------
      CALL JELIRA(JEXNUM(MAT//'.VALM',1),'TYPE',IRET,KBID)
      TYPMAT = 1
      IF (KBID(1:1).EQ.'C') TYPMAT = 2
      NONSYM=.FALSE.
      CALL JELIRA(MAT//'.VALM','NMAXOC',NBLOCM,KBID)
      CALL ASSERT(NBLOCM.EQ.1 .OR. NBLOCM.EQ.2)
      IF (NBLOCM.EQ.2) NONSYM=.TRUE.
      CALL JEVEUO(JEXNUM(MAT//'.VALM',1),'E',JVALM)
      IF (NONSYM) CALL JEVEUO(JEXNUM(MAT//'.VALM',2),'E',JVALM2)


C     -- CALCUL DE ELIM(*) ET NELIM :
C     -----------------------------------
C     ELIM    I(*)    : TABLEAU ENTIER DE DIM = NEQ DONNANT LES
C                       LES NUMEROS DES EQUATIONS A ELIMINER ET LEUR
C                       NUMERO D'ELIMINATION
C                       ZI(KKELI-1+IEQ) = / 0      -> PAS ELIMINE
C                                         / IELIM  -> ELIMINE
C     NELIM   I       : NOMBRE D'EQUATIONS DE LA MATRICE A ELIMINER
      CALL WKVECT('&&MTMCHC.ELIM','V V I',NEQ,KKELI)
      CALL JEVEUO(MAT//'.CCID','L',JCCID)
      NELIM=0
      DO 1, IEQ=1,NEQ
         IF ( IMATD.NE.0 ) THEN
           KETA=ZI(JCCID-1+ZI(JNULG+IEQ-1))
         ELSE
           KETA=ZI(JCCID-1+IEQ)
         ENDIF
         CALL ASSERT(KETA.EQ.1 .OR. KETA.EQ.0)
         IF (KETA.EQ.1) THEN
            NELIM=NELIM+1
            ZI(KKELI-1+IEQ)=NELIM
         ELSE
            ZI(KKELI-1+IEQ)=0
         ENDIF
1     CONTINUE


C     -- RECOPIE DE .CCVA DANS .VALM :
C     -----------------------------------------
      CALL WKVECT('&&MTMCHC.REMPLIS','V V I',NELIM,JREMP)
      KFIN=0
      DO 121 JCOL = 1,NEQ
        KDEB = KFIN + 1
        KFIN = ZI(JSMDI-1+JCOL)
        JELIM = ZI(KKELI-1+JCOL)

        IF (JELIM.NE.0) THEN
          DECIEL=ZI(JCCLL-1+3*(JELIM-1)+3)
          DO 111, K=KDEB, KFIN - 1
            ILIG = ZI4(JSMHC-1+K)
            IELIM = ZI(KKELI-1+ILIG)
            IF (IELIM.EQ.0) THEN
               ZI(JREMP-1+JELIM)=ZI(JREMP-1+JELIM)+1
               IREMP=ZI(JREMP-1+JELIM)
               IF (TYPMAT.EQ.1) THEN
                 ZR(JVALM-1+K)=ZR(JCCVA-1+DECIEL+IREMP)
               ELSE
                 ZC(JVALM-1+K)=ZC(JCCVA-1+DECIEL+IREMP)
               ENDIF
            ENDIF
  111     CONTINUE

        ELSE
          DO 112 K = KDEB,KFIN - 1
            ILIG = ZI4(JSMHC-1+K)
            IELIM = ZI(KKELI-1+ILIG)
            DECJEL=ZI(JCCLL-1+3*(IELIM-1)+3)
            IF (IELIM.NE.0) THEN
               ZI(JREMP-1+IELIM)=ZI(JREMP-1+IELIM)+1
               IREMP=ZI(JREMP-1+IELIM)
               IF (TYPMAT.EQ.1) THEN
                 IF (NONSYM) THEN
                   ZR(JVALM2-1+K)=ZR(JCCVA-1+DECJEL+IREMP)
                 ELSE
                   ZR(JVALM-1+K)=ZR(JCCVA-1+DECJEL+IREMP)
                 ENDIF
               ELSE
                 IF (NONSYM) THEN
                   ZC(JVALM2-1+K)=ZC(JCCVA-1+DECJEL+IREMP)
                 ELSE
                   ZC(JVALM-1+K)=ZC(JCCVA-1+DECJEL+IREMP)
                 ENDIF
               ENDIF
            ENDIF
  112     CONTINUE
        END IF

  121 CONTINUE


      ZK24(JREFA-1+3)='ELIML'
      CALL JEDETR(MAT//'.CCVA')
      CALL JEDETR(MAT//'.CCLL')
      CALL JEDETR(MAT//'.CCII')
      CALL JEDETR('&&MTMCHC.REMPLIS')
      CALL JEDETR('&&MTMCHC.ELIM')


 9999 CONTINUE
C     CALL CHEKSD('sd_matr_asse',MAT,IRET)
      CALL JEDEMA()
      END
