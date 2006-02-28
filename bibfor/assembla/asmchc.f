      SUBROUTINE ASMCHC(BASE,MATAS,STOC,NSTOC,MOTC)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) MATAS
      CHARACTER*1 BASE
      CHARACTER*4 MOTC
      INTEGER NSTOC,STOC(*)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 28/02/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C OBJET :
C        TRAITEMENT DES CHARGES CINEMATIQUES DANS UNE MATRICE ASSEMBLEE
C        CONSTRUCTION DU .LLIG ET DU .VALI
C-----------------------------------------------------------------------
C IN   BASE    K*1     : 'G','V' BASE SUR LAQUELLE EST MATAS
C VAR  MATAS   K*19    : NOM DE LA MATR_ASSE
C IN   STOC    I(*)    : TABLEAU ENTIER DE DIM = NEQ DONNANT LES
C                        LES LIGNES A STOCKES ET A ELIMINER ET LEUR
C                        NUMERO DE STOCKAGE
C IN   NSTOC   I       : NOMBRE DE LIGNES DE LA MATRICE A ELIMINER
C IN   MOTC    K*4     : 'ZERO' OU 'CUMU'
C                        'ZERO': SI .VALI OU .LLIG EXISTE DEJA ON
C                        S'ARRETE EN ERREUR 'F'.
C                        'CUMU': ON S'ARRETE PAS TRAITER.
C-----------------------------------------------------------------------
C     FONCTIONS JEVEUX
C-----------------------------------------------------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C-----------------------------------------------------------------------
C     COMMUNS   JEVEUX
C-----------------------------------------------------------------------
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
      CHARACTER*8 KBID,DOCU
      CHARACTER*14 NU
      CHARACTER*19 MAT,NOMSTO
      INTEGER TYPMAT,KSTOC,KDEB,KFIN,JDEB,JFIN,JSTOC,IVALI,LVALI
      INTEGER NEQ,IDHCOL,IDADIA,IDVALE,IDVALI,IDLLIG,IDABLI,IDALIG
      INTEGER IERL,IERI,IERA,IERB,JVALI
C----------------------------------------------------------------------
C                DEBUT DES INSTRUCTIONS
      CALL JEMARQ()
      IF (NSTOC.EQ.0) GOTO 9999
C----------------------------------------------------------------------
      IF (MOTC.EQ.'CUMU')
     +  CALL UTMESS('F','ASMCHC_0','OPTION CUMU NON TRAITEE')
      MAT = MATAS

      CALL JEVEUO(MAT//'.REFA','L',JREFA)
      NU = ZK24(JREFA-1+2)(1:14)
      NOMSTO = NU//'.SMOS'


      CALL JEEXIN(NOMSTO//'.SMHC',IRET2)
      CALL ASSERT(IRET2.GT.0)
      CALL JEVEUO(NOMSTO//'.SMHC','L',IDHCOL)
      CALL JEVEUO(NOMSTO//'.SMDI','L',IDADIA)

      CALL JEVEUO(NU//'.NUME.NEQU','L',IDNEQU)
      NEQ = ZI(IDNEQU)
      CALL JEEXIN(MAT//'.LLIG',IERL)
      CALL JEEXIN(MAT//'.VALI',IERI)
      CALL JEEXIN(MAT//'.ALIG',IERA)
      CALL JEEXIN(MAT//'.ABLI',IERB)
      IF ((IERL+IERI+IERA+IERB).NE.0) THEN
        IF (MOTC.EQ.'ZERO') THEN
C     QUAND ON RECONSTRUIRA LA MATRICE
          CALL UTMESS('F','ASMCHC_1','LA MATRICE POSSEDE DEJA DES'
     +                //' LIGNES ELIMINEES')
        ENDIF
      ENDIF
      CALL WKVECT(MAT//'.LLIG',BASE//' V I ',(1+3*NSTOC),IDLLIG)
      CALL WKVECT(MAT//'.ALIG',BASE//' V I ',NSTOC,IDALIG)
      ZI(IDLLIG) = NSTOC
      IDLLIG = IDLLIG+1
      KDEB = 1

      DO 1 I = 1,NEQ
        KSTOC = STOC(I)
        JDEB = ZI(IDHCOL-1+KDEB)
        KFIN = ZI(IDADIA-1+I)
        IF (KSTOC.NE.0) THEN
          ZI(IDLLIG+3*(KSTOC-1)) = I
          ZI(IDLLIG+3*(KSTOC-1)+1) = JDEB
          ZI(IDLLIG+3*(KSTOC-1)+2) = I
        ENDIF
        DO 10 K = KDEB,KFIN-1
          J = ZI(IDHCOL-1+K)
          JSTOC = STOC(J)
          IF (JSTOC.NE.0) ZI(IDLLIG+3*JSTOC-1) = I
10      CONTINUE
        KDEB = KFIN+1
1     CONTINUE
C
C --- CALCUL DE .ALIG ET DE .ABLI
      IVALI = 0
      DO 2 I = 1,NSTOC
        JDEB =ZI(IDLLIG-1+3*I-1)
        JFIN =ZI(IDLLIG-1+3*I)
        LVALI = JFIN-JDEB+1
        ZI(IDALIG-1+I) = IVALI + 1
        IVALI = IVALI +LVALI
2     CONTINUE
C
C --- MISE A JOUR .ABLI (STOCKAGE SUR 1 BLOC)
      CALL WKVECT(MAT//'.ABLI',BASE//' V I',2,IDABLI)
      ZI(IDABLI) = 0
      ZI(IDABLI+1) = NSTOC
C
C --- MISE A JOUR DE .VALI
      CALL JELIRA(JEXNUM(MAT//'.VALM',1),'TYPE',IRET,KBID)
      CALL JECREC(MAT//'.VALI',BASE//' V '//KBID(1:1),'NU','DISPERSE',
     +            'CONSTANT',1)
      TYPMAT = 1
      IF (KBID(1:1).EQ.'C') TYPMAT = 2
C
C --- REMPLISSAGE DE .VALI
      CALL JECROC(JEXNUM(MAT//'.VALI',1))
      CALL JEECRA(JEXNUM(MAT//'.VALI',1),'LONMAX',IVALI,KBID)
      CALL JEVEUO(JEXNUM(MAT//'.VALI',1),'E',IDVALI)
      CALL JEVEUO(JEXNUM(MAT//'.VALM',1),'E',IDVALE)
C
C---  SAUVEGARDE DES COLONNES A ELIMINER DE LA TRIANGULEE SUPERIEURE
C
      KDEB = 1
      DO 40 I = 1,NEQ
        KSTOC = STOC(I)
        KFIN = ZI(IDADIA-1+I)
        JDEB = ZI(IDHCOL-1+KDEB)
        IF (KSTOC.NE.0) THEN
          IVALI = ZI(IDALIG-1+KSTOC)
          IF (TYPMAT.EQ.1) THEN
            DO 41 K = KDEB , KFIN
              J = ZI(IDHCOL-1+K)
              ZR(IDVALI-1+IVALI+J-JDEB) = ZR(IDVALE-1+K)
41          CONTINUE
          ELSE
            DO 42 K = KDEB , KFIN
              J = ZI(IDHCOL-1+K)
              ZC(IDVALI-1+IVALI+J-JDEB) = ZC(IDVALE-1+K)
42          CONTINUE
          ENDIF
        ENDIF
        KDEB = KFIN+1
40    CONTINUE
C
C---  SAUVEGARDE DES LIGNES A ELIMINER DE LA TRIANGULEE INFERIEURE
C     ET MISE A ZERO
      KDEB = 1
      DO 500 I = 1, NEQ
        KFIN = ZI(IDADIA-1+I)
        IF (TYPMAT.EQ.1) THEN
          DO 5001 K = KDEB,KFIN
            J = ZI(IDHCOL-1+K)
            JSTOC = STOC(J)
            IF (JSTOC.GT.0) THEN
              JDEB = ZI(IDLLIG+3*JSTOC-2)
              JVALI = ZI(IDALIG-1+JSTOC)+I-JDEB
              ZR(IDVALI-1+JVALI)= ZR(IDVALE-1+K)
              ZR(IDVALE-1+K) = 0.D0
            ENDIF
5001      CONTINUE
        ELSE IF(TYPMAT.EQ.2) THEN
          DO 5002 K = KDEB,KFIN
            J = ZI(IDHCOL-1+K)
            JSTOC = STOC(J)
            IF (JSTOC.GT.0) THEN
              JDEB = ZI(IDLLIG+3*JSTOC-2)
              JVALI = ZI(IDALIG-1+JSTOC)+I-JDEB
              ZC(IDVALI-1+JVALI)= ZC(IDVALE-1+K)
              ZC(IDVALE-1+K) = DCMPLX(0.D0,0.D0)
            ENDIF
5002      CONTINUE
        ENDIF
        KDEB = KFIN+1
500   CONTINUE
C
C---  MISE A ZERO DES COLONNES ELIMINER DE LA TRIANGULEE SUPERIEURE
C
      KDEB = 1
      DO 60 I = 1,NEQ
        KSTOC = STOC(I)
        KFIN = ZI(IDADIA-1+I)
        IF (KSTOC.NE.0) THEN
          IF (TYPMAT.EQ.1) THEN
            DO 61 K = KDEB ,KFIN-1
              ZR(IDVALE-1+K) = 0.D0
61          CONTINUE
            ZR(IDVALE-1+KFIN) = 1.D0
          ELSE
            DO 62 K = KDEB ,KFIN-1
              ZC(IDVALE-1+K) = DCMPLX(0.D0,0.D0)
62          CONTINUE
            ZC(IDVALE-1+KFIN) = DCMPLX(1.D0,0.D0)
          ENDIF
        ENDIF
        KDEB = KFIN+1
60    CONTINUE
 9999 CONTINUE
      CALL JEDEMA()
      END
