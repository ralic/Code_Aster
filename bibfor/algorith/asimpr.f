      SUBROUTINE ASIMPR(NBSUP,MASSE,TCOSUP,TCOSAP,NOMSUP)
      IMPLICIT  REAL*8 (A-H,O-Z)
      INTEGER          IOC,NBSUP,TCOSUP(NBSUP,*),TCOSAP(NBSUP,*)
      CHARACTER*(*)    MASSE
      CHARACTER*8      NOMSUP(NBSUP,*)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/06/2002   AUTEUR CIBHHPD D.NUNEZ 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C     COMMANDE : COMB_SISM_MODAL
C      IMPRIME LES RESULTATS DES REPONSES PRIMAIRES ET SECONDAIRES
C     ------------------------------------------------------------------

C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      LOGICAL      PRIM,SECON
      CHARACTER*4  TYPDIR
      CHARACTER*1  DIREC
      CHARACTER*8  K8BID, NOEU, CMP,NOMA,GRNOEU,TYPCOM,NOREF
      CHARACTER*16 MONACC
      CHARACTER*19 CHEXTR,MOTFAC,KNOEU,DIDI
      CHARACTER*24 OBJ1, OBJ2
      CHARACTER*80 NOMCAS
      COMPLEX*16   CBID
C
      CALL JEMARQ()
      IFM    = IUNIFI('RESULTAT')
      KNOEU  = '&&OP0109.NOM_SUPPOR'
      DIDI   = '&&OP0109.DIRECTION'
C
      CALL GETFAC ( 'COMB_DEPL_APPUI', NDEPL )
      WRITE(IFM,1167)
      WRITE(IFM,1170)
      CALL JEVEUO(KNOEU,'L',JKNO)
      CALL JEVEUO(DIDI,'L',JDIR)
C
C  CAS DES IMPRESSIONS POUR LA REPONSE PRIMAIRE
C
      DO 57 IS = 1,NBSUP
       DO 56 ID = 1,3
        IF (TCOSUP(IS,ID).EQ.1) TYPDIR = 'QUAD'
        IF (TCOSUP(IS,ID).EQ.2) TYPDIR = 'LINE'
        IF (TCOSUP(IS,ID).EQ.3) TYPDIR = 'ABS '
        WRITE(IFM,1180) NOMSUP(IS,ID),TYPDIR
        GOTO 57
 56    CONTINUE
 57   CONTINUE
C
C  CAS DES IMPRESSIONS POUR LA REPONSE SECONDAIRE
C
        CALL DISMOI('F','NOM_MAILLA',MASSE,'MATR_ASSE',IBID,NOMA,IRET)
        OBJ1 = NOMA//'.GROUPENO'
        OBJ2 = NOMA//'.NOMNOE'
        NT = 0 
        NTOU = 0
        NCAS = 0 
        NR = 0
        NC = 0
        NG = 0
        NN = 0 
        NNO = 0
        NGR = 0
        NBNOGR = 0 
        ICOGN = 0
        ICONO = 0 
        WRITE(IFM,1190)
        WRITE(IFM,1200)
        DO 3 IOC = 1,NDEPL
          CALL GETVTX('COMB_DEPL_APPUI','TOUT',IOC,1,0,K8BID ,NT)
          NTOU = NTOU - NT
          CALL GETVIS('COMB_DEPL_APPUI','LIST_CAS',IOC,1,0,IBID,NC)
          IF (NC.NE.0) NCAS = NCAS-NC
 3      CONTINUE
C
C ON RECUPERE TOUS LES CAS A TRAITER DANS LE CAS OU UNE LISTE
C DE CAS EST RENSEIGNEE
        IF (NTOU.EQ.0) THEN
          CALL WKVECT('ASIMPR.LISTE','V V I',NCAS,JCAS)
          JLCAS = JCAS
          DO 4 IOC = 1,NDEPL
            CALL GETVIS('COMB_DEPL_APPUI','LIST_CAS',IOC,1,0,IBID,NC)
            IF (NC.NE.0) THEN
              NC = -NC
              CALL GETVIS('COMB_DEPL_APPUI','LIST_CAS',IOC,1,NC,
     &                    ZI(JLCAS),NC)
              JLCAS = JLCAS + NC
            ENDIF
 4        CONTINUE
C
          DO 18 ICAS = 1, NCAS    
            LCAS = ZI(JCAS+ICAS-1)  
            MOTFAC = 'DEPL_MULT_APPUI'
            CALL GETFAC(MOTFAC,NBOC2)
            DO 20 II =1, NBOC2   
             CALL GETVIS(MOTFAC,'NUME_CAS',II,1,1,NUCAS,NC)
             IF (NUCAS.EQ.LCAS) THEN
               CALL GETVID(MOTFAC,'NOEUD',II,1,0,NOEU,NN)
C RECUPERATION DES DIMENSIONS DES DIFFERENTS TABLEAUX DE 
C STOCKAGE DE NOEUDS ET GRNO
               IF (NN.NE.0) THEN
                 NNO = NNO-NN
               ELSE
                 CALL GETVID(MOTFAC,'GROUP_NO',II,1,0,K8BID,NG)
                 IF (NG.NE.0) NGR = NGR -NG
               ENDIF
             ENDIF
 20        CONTINUE
 18      CONTINUE
        ELSE
          MOTFAC = 'DEPL_MULT_APPUI'
          CALL GETFAC(MOTFAC,NBOC2)
          DO 35 II =1, NBOC2   
            CALL GETVIS(MOTFAC,'NUME_CAS',II,1,1,NUCAS,NC)
            CALL GETVID(MOTFAC,'NOEUD',II,1,0,K8BID,NN)
C RECUPERATION DES DIMENSIONS DES DIFFERENTS TABLEAUX DE 
C STOCKAGE DE NOEUDS ET GRNO
            IF (NN.NE.0) THEN
               NNO = NNO-NN
             ELSE
               CALL GETVID(MOTFAC,'GROUP_NO',II,1,0,K8BID,NG)
               IF (NG.NE.0) NGR = NGR -NG
             ENDIF
 35        CONTINUE
        ENDIF
        CALL WKVECT('ASIMPR.DDLV','V V R',NBOC2,JVALE)
        CALL WKVECT('ASIMPR.DDLC','V V K8',NBOC2,JCMP)
C       
        IF (NNO.NE.0) CALL WKVECT('ASIMPR.NOEUD','V V K8',NNO,JNOE)
        IF (NGR.NE.0) THEN
          CALL WKVECT('ASIMPR.GROUP_NO','V V K8',NGR,JGRN)
          CALL WKVECT('ASIMPR.TRAVLIST','V V I',NGR,JTRAV)
          CALL WKVECT('ASIMPR.LIST_GR','V V I',NGR,IDN)
        ENDIF
C
        ICOGN = JGRN
        ICONO = JNOE
        JTRAV1 = JTRAV
        JVAL1  =JVALE
        JCMP1 = JCMP
C
        IF (NTOU.EQ.0) THEN
          DO 19 ICAS = 1, NCAS   
             DO 30 II = 1, NBOC2   
               CALL GETVIS(MOTFAC,'NUME_CAS',II,1,1,NUCAS,NC)
               IF (NUCAS.EQ.ZI(JCAS+ICAS-1 )) THEN
                 CALL GETVTX(MOTFAC,'NOM_CAS',II,1,1,NOMCAS,NC)
                 CALL GETVID(MOTFAC,'NOEUD_REFE',II,1,1,NOREF,NR)
                 IF (NR.EQ.0) NOREF = '-'
                 CALL GETVR8(MOTFAC,'DX',II,1,0,DX,NX)
                 CALL GETVR8(MOTFAC,'DY',II,1,0,DY,NY)
                 CALL GETVR8(MOTFAC,'DZ',II,1,0,DZ,NZ)
                 IF (NNO.NE.0) THEN
                 IF (NX.NE.0) THEN 
                   CALL GETVR8(MOTFAC,'DX',II,1,1,ZR(JVAL1),NX)
                   ZK8(JCMP1) = 'DX'
                   JVAL1 = JVAL1 + 1
                   JCMP1 = JCMP1 + 1
                 ENDIF
                 IF (NY.NE.0) THEN 
                   CALL GETVR8(MOTFAC,'DY',II,1,1,ZR(JVAL1),NY)
                   ZK8(JCMP1) = 'DY'
                   JVAL1 = JVAL1 + 1
                   JCMP1 = JCMP1 + 1
                 ENDIF
                 IF (NZ.NE.0) THEN 
                   CALL GETVR8(MOTFAC,'DZ',II,1,1,ZR(JVAL1),NZ)
                   ZK8(JCMP1) = 'DZ'
                   JVAL1 = JVAL1 + 1
                   JCMP1 = JCMP1 + 1
                 ENDIF
                 NJJ = JVAL1-JVALE
                   CALL GETVID(MOTFAC,'NOEUD',II,1,0,K8BID,NN)
                   IF (NN.NE.0) THEN
                    CALL GETVID(MOTFAC,'NOEUD',II,1,NN,ZK8(ICONO),NNI)
                    DO 111 INO = 1,NN 
                      NOEU = ZK8(ICONO+INO-1)
                      DO 211 JJ = 1,NJJ
                        DO 311 IS = 1,NBSUP
                         DO 411 ID = 1,3
                          IF (TCOSAP(IS,ID).EQ.1) TYPDIR = 'QUAD'
                          IF (TCOSAP(IS,ID).EQ.2) TYPDIR = 'LINE'
                          IF (TCOSAP(IS,ID).EQ.3) TYPDIR = 'ABS '
                          IF (NOMSUP(IS,ID).EQ.NOEU) THEN
                            WRITE(IFM,1210)NUCAS,TYPDIR,NOEU,
     +                     ZK8(JCMP+JJ-1),ZR(JVALE+JJ-1),NOREF,NOMCAS
                            GOTO 311
                          ENDIF
 411                    CONTINUE
 311                  CONTINUE
 211                 CONTINUE
 111                CONTINUE
                    JVALE = JVAL1
                    JCMP = JCMP1
                    ICONO = ICONO+NN
                  ENDIF
                ENDIF 
                IF (NGR.NE.0) THEN
                  CALL GETVID(MOTFAC,'GROUP_NO',II,1,0,K8BID,NG)
                  IF (NG.NE.0) THEN
                    NNG = -NG
                    CALL GETVID(MOTFAC,'GROUP_NO',II,1,NNG,
     &                       ZK8(ICOGN),NG)
                    ICOGN = ICOGN+NNG
                    DO 121 ITR= 1,NNG
                       ZI(JTRAV1+ITR-1) = NUCAS
 121               CONTINUE
                   JTRAV1 = JTRAV1 +NNG
                  ENDIF
                ENDIF
             ENDIF 
 30        CONTINUE
 19       CONTINUE
       ELSE IF (NTOU.NE.0) THEN
         DO 36 II =1, NBOC2   
           CALL GETVIS(MOTFAC,'NUME_CAS',II,1,1,NUCAS,NC)
           CALL GETVTX(MOTFAC,'NOM_CAS',II,1,1,NOMCAS,NC)
           CALL GETVID(MOTFAC,'NOEUD_REFE',II,1,1,NOREF,NR)
           IF (NR.EQ.0) NOREF = '-'
           CALL GETVR8(MOTFAC,'DX',II,1,0,DX,NX)
           CALL GETVR8(MOTFAC,'DY',II,1,0,DY,NY)
           CALL GETVR8(MOTFAC,'DZ',II,1,0,DZ,NZ)
           IF (NNO.NE.0) THEN 
            CALL GETVID(MOTFAC,'NOEUD',II,1,0,K8BID,NN) 
            IF (NN.NE.0) THEN 
                NNN = -NN
                CALL GETVID(MOTFAC,'NOEUD',II,1,NNN,ZK8(ICONO),NN)
                ICONO = ICONO +NNN
            IF (NX.NE.0) THEN 
              CALL GETVR8(MOTFAC,'DX',II,1,1,ZR(JVAL1),NX)
              ZK8(JCMP1) = 'DX'
              JVAL1 = JVAL1 + 1
              JCMP1 = JCMP1 + 1
           ENDIF
           IF (NY.NE.0) THEN 
              CALL GETVR8(MOTFAC,'DY',II,1,1,ZR(JVAL1),NY)
              ZK8(JCMP1) = 'DY'
              JVAL1 = JVAL1 + 1
              JCMP1 = JCMP1 + 1
            ENDIF
            IF (NZ.NE.0) THEN 
              CALL GETVR8(MOTFAC,'DZ',II,1,1,ZR(JVAL1),NZ)
              ZK8(JCMP1) = 'DZ'
              JVAL1 = JVAL1 + 1
              JCMP1 = JCMP1 + 1
           ENDIF
           NJJ = JVAL1-JVALE
           DO 112 INO = 1,NNN
             NOEU = ZK8(ICONO+INO-1)
             DO 212 JJ = 1, NJJ
               DO 412 IS = 1,NBSUP
                 DO 312 ID = 1,3
                   IF (TCOSAP(IS,ID).EQ.1) TYPDIR = 'QUAD'
                   IF (TCOSAP(IS,ID).EQ.2) TYPDIR = 'LINE'
                   IF (TCOSAP(IS,ID).EQ.3) TYPDIR = 'ABS '
                   IF (NOMSUP(IS,ID).EQ.NOEU) THEN
                       WRITE(IFM,1210)NUCAS,TYPDIR,NOEU,
     +                 ZK8(JCMP+JJ-1),ZR(JVALE+JJ-1),NOREF,NOMCAS
                       GOTO 412
                    ENDIF
 312              CONTINUE
 412            CONTINUE
 212           CONTINUE
 112         CONTINUE
             JVALE = JVAL1
             JCMP = JCMP1
             ICONO = ICONO+NN
           ENDIF
           ENDIF 
           IF (NGR.NE.0) THEN
             CALL GETVID(MOTFAC,'GROUP_NO',II,1,0,K8BID,NG)
             IF (NG.NE.0) THEN
               NNG = -NG
               CALL GETVID(MOTFAC,'GROUP_NO',II,1,NNG,
     &                     ZK8(ICOGN),NG)
                ICOGN = ICOGN+NNG
                DO 113 ITR= 1,NNG
                  ZI(JTRAV1+ITR-1) = NUCAS
 113            CONTINUE
                JTRAV1 = JTRAV1 +NNG
              ENDIF
            ENDIF              
 36      CONTINUE
      ENDIF
      IF ((NTOU.EQ.0).AND.(NGR.NE.0)) THEN
        DO 39 ICAS = 1, NCAS   
          DO 50 II =1, NBOC2   
           CALL GETVIS(MOTFAC,'NUME_CAS',II,1,1,NUCAS,NC)
           IF (NUCAS.EQ.ZI(JCAS+ICAS-1)) THEN        
            DO 70 IGR = 1, NGR
              GRNOEU = ZK8(JGRN+IGR-1)
              CALL JELIRA(JEXNOM(OBJ1,GRNOEU),'LONMAX',NN,K8BID)
              ZI(IDN-1+IGR) = NN
              NBNOGR = NBNOGR+NN
 70         CONTINUE
           ENDIF
 50      CONTINUE
 39     CONTINUE
      ENDIF
C
      IF ((NTOU.NE.0).AND.(NGR.NE.0)) THEN
        DO 51 II =1, NBOC2   
          DO 71 IGR = 1, NGR
            GRNOEU = ZK8(JGRN+IGR-1)
            CALL JELIRA(JEXNOM(OBJ1,GRNOEU),'LONMAX',NN,K8BID)
            ZI(IDN-1+IGR) = NN
            NBNOGR = NBNOGR+NN
 71       CONTINUE
 51     CONTINUE
      ENDIF

      IF (NBNOGR.NE.0) 
     &  CALL WKVECT('ASIMPR.NOEUDGR','V V K8',NBNOGR,INOE)
C
      IF ((NTOU.EQ.0).AND.(NGR.NE.0)) THEN
        DO 40 II =1, NGR
         DO 140 ICAS = 1,NBOC2
         NUCAS = ZI(JTRAV+II-1)
         CALL GETVIS(MOTFAC,'NUME_CAS',ICAS,1,1,NUCAS1,NC)
         IF (NUCAS .EQ. NUCAS1) THEN
         CALL GETVTX(MOTFAC,'NOM_CAS',ICAS,1,1,NOMCAS,NC)
         CALL GETVID(MOTFAC,'NOEUD_REFE',ICAS,1,1,NOREF,NR)
         IF (NR.EQ.0) NOREF = '-'
         CALL GETVR8(MOTFAC,'DX',ICAS,1,0,DX,NX)
         CALL GETVR8(MOTFAC,'DY',ICAS,1,0,DY,NY)
         CALL GETVR8(MOTFAC,'DZ',ICAS,1,0,DZ,NZ)
         IF (NX.NE.0) THEN 
            CALL GETVR8(MOTFAC,'DX',ICAS,1,1,ZR(JVAL1),NX)
            ZK8(JCMP1) = 'DX'
            JVAL1 = JVAL1 + 1
            JCMP1 = JCMP1 + 1
         ENDIF
         IF (NY.NE.0) THEN 
           CALL GETVR8(MOTFAC,'DY',ICAS,1,1,ZR(JVAL1),NY)
           ZK8(JCMP1) = 'DY'
           JVAL1 = JVAL1 + 1
           JCMP1 = JCMP1 + 1
         ENDIF
         IF (NZ.NE.0) THEN 
           CALL GETVR8(MOTFAC,'DZ',ICAS,1,1,ZR(JVAL1),NZ)
           ZK8(JCMP1) = 'DZ'
           JVAL1 = JVAL1 + 1
           JCMP1 = JCMP1 + 1
         ENDIF
           NJJ = JVAL1-JVALE
         IF (NBNOGR.GT.0) THEN
            CALL JEVEUO(JEXNOM(OBJ1,ZK8(JGRN+II-1)),'L',JDGN)
            DO 90 J = 1,ZI(IDN+II-1)
              CALL JENUNO(JEXNUM(OBJ2,ZI(JDGN-1+J)),NOEU)
              DO 790 JJ = 1, NJJ
               DO 490 IS = 1,NBSUP
                 DO 590 ID = 1,3
                   IF (TCOSAP(IS,ID).EQ.1) TYPDIR = 'QUAD'
                   IF (TCOSAP(IS,ID).EQ.2) TYPDIR = 'LINE'
                   IF (TCOSAP(IS,ID).EQ.3) TYPDIR = 'ABS '
                   IF (NOMSUP(IS,ID).EQ.NOEU) THEN
                     WRITE(IFM,1210)NUCAS,TYPDIR,NOEU,
     +                ZK8(JCMP+JJ-1),ZR(JVALE+JJ-1),NOREF,NOMCAS
                       GOTO 490
                    ENDIF
 590             CONTINUE
 490           CONTINUE
 790         CONTINUE
 90        CONTINUE
             JVALE = JVAL1
             JCMP = JCMP1
          ENDIF
          ENDIF
 140     CONTINUE
 40     CONTINUE
 29    CONTINUE
      ELSE IF ((NTOU.NE.0).AND.(NGR.NE.0)) THEN
        DO 45 II =1, NGR
         DO 145 ICAS = 1,NBOC2
           NUCAS = ZI(JTRAV+II-1)
           CALL GETVIS(MOTFAC,'NUME_CAS',ICAS,1,1,NUCAS1,NC)
         IF (NUCAS .EQ. NUCAS1) THEN
           CALL GETVTX(MOTFAC,'NOM_CAS',ICAS,1,1,NOMCAS,NC)
           CALL GETVID(MOTFAC,'NOEUD_REFE',ICAS,1,1,NOREF,NR)
           IF (NR.EQ.0) NOREF = '-'
           CALL GETVR8(MOTFAC,'DX',ICAS,1,0,DX,NX)
           CALL GETVR8(MOTFAC,'DY',ICAS,1,0,DY,NY)
           CALL GETVR8(MOTFAC,'DZ',ICAS,1,0,DZ,NZ)
           IF (NX.NE.0) THEN 
            CALL GETVR8(MOTFAC,'DX',ICAS,1,1,ZR(JVAL1),NX)
            ZK8(JCMP1) = 'DX'
            JVAL1 = JVAL1 + 1
            JCMP1 = JCMP1 + 1
           ENDIF
           IF (NY.NE.0) THEN 
            CALL GETVR8(MOTFAC,'DY',ICAS,1,1,ZR(JVAL1),NY)
            ZK8(JCMP1) = 'DY'
            JVAL1 = JVAL1 + 1
            JCMP1 = JCMP1 + 1
           ENDIF
           IF (NZ.NE.0) THEN 
            CALL GETVR8(MOTFAC,'DZ',ICAS,1,1,ZR(JVAL1),NZ)
            ZK8(JCMP1) = 'DZ'
            JVAL1 = JVAL1 + 1
            JCMP1 = JCMP1 + 1
           ENDIF
           
           NJJ = JVAL1-JVALE
           IF (NBNOGR.GT.0) THEN
             CALL JEVEUO(JEXNOM(OBJ1,ZK8(JGRN+II-1)),'L',JDGN)
             DO 190 J = 1,ZI(IDN+II-1)
               CALL JENUNO(JEXNUM(OBJ2,ZI(JDGN-1+J)),NOEU)
               DO  290 JJ = 1, NJJ
                DO 390 IS = 1,NBSUP
                 DO 690 ID = 1,3
                   IF (TCOSAP(IS,ID).EQ.1) TYPDIR = 'QUAD'
                   IF (TCOSAP(IS,ID).EQ.2) TYPDIR = 'LINE'
                   IF (TCOSAP(IS,ID).EQ.3) TYPDIR = 'ABS '
                   IF (NOMSUP(IS,ID).EQ.NOEU) THEN 
                       WRITE(IFM,1210)NUCAS,TYPDIR,NOEU,
     +                 ZK8(JCMP+JJ-1),ZR(JVALE+JJ-1),NOREF,NOMCAS
                       GOTO 390
                   ENDIF
 690             CONTINUE
 390           CONTINUE
 290          CONTINUE
 190         CONTINUE
             JVALE = JVAL1
             JCMP = JCMP1
            ENDIF
            ENDIF
 145        CONTINUE
 45      CONTINUE
        ENDIF


 1167 FORMAT(/,1X,'--- COMPOSANTE PRIMAIRE ---')
 1170 FORMAT(1X,
     +      'SUPPORT  COMBINAISON')
 1180 FORMAT(A10,A10)
 1190 FORMAT(/,1X,' --- COMPOSANTE SECONDAIRE ---')
 1200 FORMAT(1X,
     + '  CAS   COMBINAISON   SUPPORT     CMP        VALEUR       '
     +  ' NOEUD_REFE  NOM_CAS')
 1210 FORMAT(1P,1X,I5,5X,A8,5X,A8,3X,A8,2X,D12.5,3X,A8,3X,A80)
C
      CALL JEDETC('V','ASIMPR',1)
      CALL JEDEMA()
      END
