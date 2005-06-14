      SUBROUTINE LRMIMP( IFM,NIV,NDIM,NOMU,TITRE, GRPNOE, GRPMAI,NOMNOE,
     +                NOMMAI, CONNEX, COOVAL, NBLTIT, NBNOEU, NBMAIL,
     +                NBGRNO, NBGRMA, TYPMAI, NBTYP,  NNOTYP, NOMTYP, 
     +                NMATYP )
      IMPLICIT REAL*8 (A-H,O-Z)
C     IN
      INTEGER         NBGRNO, NBGRMA, NBLTIT, NBNOEU, NBMAIL, NIV, NBTYP
      INTEGER         IFM,NDIM,NMATYP(*),NNOTYP(*)
      CHARACTER*24    COOVAL, GRPNOE, GRPMAI, CONNEX
      CHARACTER*24    TITRE,  NOMMAI, NOMNOE, TYPMAI 
      CHARACTER*8     NOMU,   NOMTYP(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 17/02/99   AUTEUR CABHHST V.LEFEBVRE 
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
C TOLE CRP_21
C     TRANSCRIPTION DES OBJETS D UN MAILLAGE SUR LE FICHIER RESULTAT
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
      CHARACTER*32      JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER         NBMA, NBNO, I, J, N, NNOMAX
      INTEGER         JCNX, JNO, JGNO, JMA,  JGMA, JTIT, JCOO, JTYP
      CHARACTER*32    LISNOE, LISMAI, LISGRN, LISGRM
      CHARACTER*32    COMNOE, COMMAI, COMGRN, COMGRM
      CHARACTER*1     K1
      CHARACTER*8     NOM, TYPEMA
      CHARACTER*24    IMPGNO, IMPGMA, IMPMNO
C     ------------------------------------------------------------------
      CALL JEMARQ ( )
      IMPMNO  = '&&LRMIMP.MAINO'
      IMPGNO  = '&&LRMIMP.NOMNO'
      IMPGMA  = '&&LRMIMP.NOMMA'
C
C     NB NOEUD/TYPE MAX
      NNOMAX = 0
      DO 1 I = 1 , NBTYP
         IF ( NNOTYP(I) .GT. NNOMAX ) NNOMAX =  NNOTYP(I)
 1    CONTINUE
C
C
 701  FORMAT(2X,I8,2X,A8,10X,3(D14.5,2X))
 702  FORMAT(2X,I8,2X,A8,2X,A8,5(2X,A8))
 703  FORMAT(100(30X,5(2X,A8),/))
 704  FORMAT(2X,I8,2X,A8,2X,I8,5(2X,A8))
 801  FORMAT(A80)
 802  FORMAT(/,2X,'------------ LECTURE DU MAILLAGE ',A8,
     +       '   IMPRESSIONS NIVEAU ',I2,' ------------',/)
 803  FORMAT(/,15X,'------  ',A32,'  ------',/)
 804  FORMAT(/,2X,A32,I6)
 806  FORMAT(30X,A8,5X,I6)
 808  FORMAT(30X,5(A8,2X))
C
      LISNOE = 'LISTE DES NOEUDS                '
      LISMAI = 'LISTE DES MAILLES               '
      LISGRN = 'LISTE DES GROUPES DE NOEUDS     '
      LISGRM = 'LISTE DES GROUPES DE MAILLES    '
      COMNOE = 'NOMBRE DE NOEUDS                '
      COMMAI = 'NOMBRE DE MAILLES               '
      COMGRN = 'NOMBRE DE GROUPES DE NOEUDS     '
      COMGRM = 'NOMBRE DE GROUPES DE MAILLES    '
C
 800  CONTINUE
      IF ( NIV .GE. 2 ) THEN
         WRITE(IFM,802)NOMU,NIV
         CALL JEVEUO(TITRE,'L',JTIT)
         DO 910 I = 1 , NBLTIT
            WRITE(IFM,801)ZK80(JTIT+I-1)
 910     CONTINUE
         WRITE(IFM,804)COMNOE,NBNOEU
         WRITE(IFM,804)COMMAI,NBMAIL
         DO 915 I = 1 , NBTYP
            IF(NMATYP(I).NE.0)THEN
               WRITE(IFM,806)NOMTYP(I),NMATYP(I)
            ENDIF
 915     CONTINUE
         IF(NBGRNO.NE.0)THEN
            WRITE(IFM,804)COMGRN,NBGRNO
            DO 920 I = 1 , NBGRNO
               CALL JENUNO(JEXNUM(GRPNOE,I),NOM)
               WRITE(IFM,808)NOM
 920        CONTINUE
         ENDIF
         IF(NBGRMA.NE.0)THEN
            WRITE(IFM,804)COMGRM,NBGRMA
            DO 925 I = 1 , NBGRMA
               CALL JENUNO(JEXNUM(GRPMAI,I),NOM)
               WRITE(IFM,808)NOM
 925        CONTINUE
         ENDIF
C
         WRITE(IFM,803)LISNOE
         N = NBNOEU
         CALL JEVEUO(COOVAL,'L',JCOO)
         DO 930 I = 1 , N
            CALL JENUNO(JEXNUM(NOMNOE,I),NOM)
            WRITE(IFM,701)I,NOM,(ZR(JCOO+(I-1)*3+J-1),J=1,NDIM)
 930     CONTINUE
C
         WRITE(IFM,803)LISMAI
         N = NBMAIL
         CALL JEVEUO(TYPMAI,'L',JTYP)
         CALL WKVECT(IMPMNO,'V V K8',NNOMAX,JNO)
         DO 935 I = 1 , N
             CALL JENUNO(JEXNUM(NOMMAI,I),NOM)
             CALL JEVEUO(JEXNUM(CONNEX,I),'L',JCNX)
             CALL JELIRA(JEXNUM(CONNEX,I),'LONMAX',NBNO,K1)
             CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(JTYP-1+I)),TYPEMA)
             DO 936 J = 1,NBNO
                  CALL JENUNO(JEXNUM(NOMNOE,ZI(JCNX+J-1)),ZK8(JNO+J-1))
 936         CONTINUE
             IF (NBNO.LE.5) THEN
                  WRITE(IFM,702)I,NOM,TYPEMA,(ZK8(JNO+J-1),J=1,NBNO)
             ELSE
                  WRITE(IFM,702)I,NOM,TYPEMA,(ZK8(JNO+J-1),J=1,5)
                  WRITE(IFM,703)(ZK8(JNO+J-1),J=6,NBNO)
             ENDIF
 935     CONTINUE
         CALL JEDETR(IMPMNO)
C
         IF (NBGRNO.NE.0) THEN
            WRITE(IFM,803)LISGRN
            CALL WKVECT(IMPGNO,'V V K8',NBNOEU,JNO)
            DO 940 I = 1 , NBGRNO
               CALL JENUNO(JEXNUM(GRPNOE,I),NOM)
               CALL JEVEUO(JEXNUM(GRPNOE,I),'L',JGNO)
               CALL JELIRA(JEXNUM(GRPNOE,I),'LONMAX',NBNO,K1)
               N = NBNO
               DO 941 J = 1,N
                  CALL JENUNO(JEXNUM(NOMNOE,ZI(JGNO+J-1)),ZK8(JNO+J-1))
 941           CONTINUE
               IF(N.LE.5)THEN
                  WRITE(IFM,704)I,NOM,NBNO,(ZK8(JNO+J-1),J=1,N)
               ELSE
                  WRITE(IFM,704)I,NOM,NBNO,(ZK8(JNO+J-1),J=1,5)
                  WRITE(IFM,703)(ZK8(JNO+J-1),J=6,N)
               ENDIF
 940        CONTINUE
            CALL JEDETR(IMPGNO)
         ENDIF
C
         IF (NBGRMA.NE.0) THEN
            WRITE(IFM,803)LISGRM
            CALL WKVECT(IMPGMA,'V V K8',NBMAIL,JMA)
            DO 945 I = 1 , NBGRMA
               CALL JENUNO(JEXNUM(GRPMAI,I),NOM)
               CALL JEVEUO(JEXNUM(GRPMAI,I),'L',JGMA)
               CALL JELIRA(JEXNUM(GRPMAI,I),'LONMAX',NBMA,K1)
               N = NBMA
               DO 946 J = 1,N
                  CALL JENUNO(JEXNUM(NOMMAI,ZI(JGMA+J-1)),ZK8(JMA+J-1))
 946           CONTINUE
               IF(N.LE.5)THEN
                  WRITE(IFM,704)I,NOM,NBMA,(ZK8(JMA+J-1),J=1,N)
               ELSE
                  WRITE(IFM,704)I,NOM,NBMA,(ZK8(JMA+J-1),J=1,5)
                  WRITE(IFM,703)(ZK8(JMA+J-1),J=6,N)
               ENDIF
 945        CONTINUE
            CALL JEDETR(IMPGMA)
         ENDIF
      ENDIF
C
      CALL JEDETC ('V','&&LRMIMP',1)
      CALL JEDEMA ( )
      END
