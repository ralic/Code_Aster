      SUBROUTINE ASCLIG ( MAILLA, NOMRES, ICMD, ACOU, ASEP, RCIN, FLAG, 
     +                    GEOM, MATER, IERUSR )
      IMPLICIT   NONE
      INTEGER         IERUSR, ICMD
      CHARACTER*8     MAILLA, GEOM, MATER
      CHARACTER*16    NOMRES
      REAL*8          ACOU, ASEP, RCIN
      LOGICAL         FLAG
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 23/02/2004   AUTEUR CIBHHLV L.VIVAN 
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
C TOLE  CRP_20
C                   MACR_ASCOUF_CALC
C
C       --- POST TRAITEMENT SOUS-EPAISSEURS:   POST_RELEVE_T  ---
C                                              IMPR_TABLE     ---
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI        
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32     JEXNOM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C     ------------------------------------------------------------------
      INTEGER      ISEP0, IGR, ISEP, IPOS, NBGRNO, I8, I9, I, J, NBSEP,
     &             K, I16V(5), NBPARA, JPARA, IRET, I8V(6), I80, ICOOR,
     &             NBNOL, JGRN, LT1
      REAL*8       VECTY(3), PI, R8PI, ACOUR, ASEPR, X, Y, Z, ANGSEC,
     &             VSIN, VCOS
      LOGICAL      PREM, PPM, PPMPB ,PSILON, PTRESC, PSIRAD, PSICIR
      CHARACTER*2  NUME,SECT(3)
      CHARACTER*4  LIG(8)
      CHARACTER*8  K8B, NOMGRN, NOPOSD, TABLIG(100,3), COMPO(6),
     &             SECMOY(3), SECINV(3), SECRCM(3),MOT(5),CRIT
      CHARACTER*16 NOMCHA,IMPRTA(6),OPER(3), REP(3)
      CHARACTER*24 GRPNOE, COORD
      CHARACTER*32 JEXNUM
      CHARACTER*80 TEXTE
C     ------------------------------------------------------------------
      DATA SECT    / 'MI ','TU ','GV '/
      DATA LIG     / 'FDRO','EXDR','EXTR','EXGA',
     &               'FGAU','INGA','INTR','INDR'/
      DATA NOMCHA  / 'SIEF_ELNO_ELGA' /
      DATA IMPRTA  / 'INTITULE' , ' ',' ',
     &               ' ', ' ' , 'TRESCA' /
      DATA MOT     / 'QUANTITE','MOMENT_0','OUI','ORIG','EXTR'/
      DATA OPER    / 'EXTRACTION','PM_PB','MOYENNE' /
      DATA COMPO   / 'SIXX', 'SIYY', 'SIZZ', 'SIXY', 'SIXZ', 'SIYZ' /
      DATA CRIT    / 'MAXI' /
      DATA REP     / 'LOCAL', 'GLOBAL','CYLINDRIQUE' /
      DATA I8V     / 6*8  /
      DATA I16V    / 5*16 /
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      PI = R8PI()
      PPM    = .FALSE.
      PPMPB  = .FALSE.
      PTRESC = .FALSE.
      PSILON = .FALSE.
      PSIRAD = .FALSE.
      PSICIR = .FALSE.
C
      I8  = 8
      I9  = 9
      I80 = 80
      GRPNOE = MAILLA//'.GROUPENO        '
      COORD  = MAILLA//'.COORDO    .VALE'
      CALL JEVEUO (COORD,'L',ICOOR)
      CALL JELIRA ( GRPNOE, 'NUTIOC', NBGRNO, K8B )
      CALL JEEXIN ( '&&OPS020.NOM_PARA',IRET)
C
      IF (IRET.NE.0) THEN
       CALL JELIRA ( '&&OPS020.NOM_PARA','LONMAX',NBPARA,K8B)
       CALL JEVEUO ( '&&OPS020.NOM_PARA','L',JPARA)
       DO 1 K = 1, NBPARA
         IF (ZK16(JPARA+K-1).EQ.'TRESCA_MEMBRANE ')  PPM    = .TRUE.
         IF (ZK16(JPARA+K-1).EQ.'TRESCA_MFLE     ')  PPMPB  = .TRUE.
         IF (ZK16(JPARA+K-1).EQ.'TRESCA          ')  PTRESC = .TRUE.
         IF (ZK16(JPARA+K-1).EQ.'SI_LONG         ')  PSILON = .TRUE.
         IF (ZK16(JPARA+K-1).EQ.'SI_RADI         ')  PSIRAD = .TRUE.
         IF (ZK16(JPARA+K-1).EQ.'SI_CIRC         ')  PSICIR = .TRUE.
1      CONTINUE
      ELSE
        PPM   = .TRUE.
        PPMPB = .TRUE.
        PTRESC = .TRUE.
        PSILON = .TRUE.
        PSIRAD = .TRUE.
        PSICIR = .TRUE.
      END IF
C
C
C     MOYENNE_RCCM, INVARIANT ET MOYENNE SUR LES LIGAMENTS DANS
C     L'EPAISSEUR
C
      DO 5 K=1,3
C
      NBSEP = 0
      ISEP0 = 0
 10   CONTINUE
      PREM = .TRUE.
      ISEP0 = ISEP0 + 1
C
C     PRELEVEMENTS DES LIGAMENTS CIRCONFERENTIELS ET LONGITUDINAUX
C     DE LA SOUS-EPAISSEUR
C
      DO 20 IGR = 1,NBGRNO
         ISEP = 0
         CALL JENUNO ( JEXNUM(GRPNOE,IGR),NOMGRN )
         IF (NOMGRN(1:3).EQ.'CIR'. OR.
     +       NOMGRN(1:3).EQ.'LON') THEN
             IPOS = INDEX(NOMGRN,'_')-1
             READ(NOMGRN(4:IPOS),'(I2)') ISEP
         ELSE IF ( NOMGRN(1:5).EQ.'PCENT' ) THEN
             READ(NOMGRN(6:8),'(I2)') ISEP
         ELSE IF ( NOMGRN(1:4).EQ.'FGAU'.AND.
     +             NOMGRN(5:6).NE.'GV'.AND.
     +             NOMGRN(5:6).NE.'TU'.AND.
     +             NOMGRN(5:6).NE.'MI' ) THEN
             READ(NOMGRN(5:6),'(I2)') ISEP
         END IF
         IF (ISEP.EQ.ISEP0) THEN
           IF (ISEP.GT.NBSEP) NBSEP = ISEP
           IF (PREM) THEN
             ICMD = ICMD + 1
             CALL GCNCON ( '.' , NOPOSD )
             TABLIG(ISEP,K) = NOPOSD             
             IF (K.EQ.1) THEN
               CALL SMDCMD ( ICMD, NOPOSD, 'POST_RCCM', IERUSR )
               CALL PUTVID ( 'MATER',        1, MATER, IERUSR )
               CALL PUTVID ( 'MAILLAGE',     1, MAILLA, IERUSR )
               CALL PUTVTX ( 'TYPE_RESU_MECA',1,'EVOLUTION',I9,IERUSR)
               CALL PUTVTX ( 'OPTION', 1, OPER(2) ,I8, IERUSR)        
               CALL SMDMCF ( 'TRANSITOIRE', IERUSR )
                CALL PUTVID ( 'RESULTAT', 1, NOMRES, IERUSR )
                CALL PUTVTX ( 'NOM_CHAM', 1, NOMCHA , I16V, IERUSR)
               CALL SMFMCF ( IERUSR )                      
             ELSE
               CALL SMDCMD ( ICMD, NOPOSD, 'POST_RELEVE_T', IERUSR )
             END IF
             PREM = .FALSE.
           END IF
           IF (NOMGRN(1:4).NE.'FGAU') THEN
C
C    PRELEVEMENTS DES LIGAMENTS LONGI ET CIRCONF POUR UNE SOUS-EP
C    ELLIPTIQUE
C     
              IF (K.EQ.2.OR.K.EQ.3) THEN               
                CALL SMDMCF ( 'ACTION', IERUSR )
                CALL PUTVID ( 'RESULTAT', 1, NOMRES, IERUSR )
                CALL PUTVTX ( 'NOM_CHAM', 1, NOMCHA , I16V, IERUSR)
              ELSE
                CALL SMDMCF ( 'SEGMENT', IERUSR )
              END IF      
              CALL PUTVTX ( 'INTITULE', 1, NOMGRN , I8, IERUSR)
              CALL PUTVID ( 'GROUP_NO', 1, NOMGRN, IERUSR )
              IF (K.EQ.2) THEN              
                CALL PUTVTX ( 'INVARIANT', 1, MOT(3) ,I8, IERUSR)
                CALL PUTVTX ( 'OPERATION', 1, OPER(1) ,I16V,IERUSR)
              ELSE IF (K.EQ.3 .AND. FLAG) THEN
C
C DEFINITION DU REPERE LOCAL POUR LES CONTRAINTES :
C
C * LE PREMIER AXE EST DONNE PAR LE LIGAMENT (AXE X = AXE RADIAL)
C * LE SECOND EST DONNE PAR AXE_Y ET DEFINIT ICI UN AXE NORMAL A 
C   LA SECTION SOIT L'AXE LONGITUDINAL (AXE Y)
C * L'AXE Z EST DEDUIT DES DEUX AUTRES ET FORME L'AXE TANGENT A LA 
C   SECTION SOIT L'AXE CIRCONFERENTIEL (AXE Z)
C              
             IF (NOMGRN(1:3).EQ.'CIR'.OR.NOMGRN(1:5).EQ.'PCENT') THEN
C
C AXE_Y POUR UN TUBE :
C
              IF (GEOM(1:4).EQ.'TUBE') THEN
                 VECTY(1) = 0.0D0
                 VECTY(2) = 0.0D0
                 VECTY(3) = 1.0D0
              ELSE
C
C AXE_Y POUR UN COUDE :
C
                VECTY(1) = SIN(ASEPR)
                VECTY(2) = 0.0D0
                VECTY(3) = COS(ASEPR)
              END IF
C              
            ELSE IF (NOMGRN(1:3).EQ.'LON') THEN
C
C AXE_Y POUR UN TUBE :
C
              IF (GEOM(1:4).EQ.'TUBE') THEN
                 VECTY(1) = 0.0D0
                 VECTY(2) = 0.0D0
                 VECTY(3) = 1.0D0
              ELSE
C
C AXE_Y POUR UN COUDE :
C
                CALL JEVEUO ( JEXNOM(GRPNOE,'FGAUTU'),'L', JGRN )
                LT1 = -ZR(ICOOR+3*(ZI(JGRN)-1)+2)
                CALL JEVEUO ( JEXNOM(GRPNOE,NOMGRN),'L', JGRN )
                CALL JELIRA ( JEXNOM(GRPNOE,NOMGRN),'LONMAX',NBNOL,K8B)
C  ON CALCULE L'ANGLE DE LA SECTION POUR TOUS LES NOEUDS DU 
C  LIGAMENTS (TEST) MAIS ON NE GARDE QUE LA DERNIERE VALEUR
                DO 6 I=1 , NBNOL
                  X = ZR(ICOOR+3*(ZI(JGRN+I-1)-1))
                  Y = ZR(ICOOR+3*(ZI(JGRN+I-1)-1)+1)
                  Z = ZR(ICOOR+3*(ZI(JGRN+I-1)-1)+2)
                  IF (Z.GT.LT1) THEN
C  CAS OU LE LIGAMENT LONGI EST DANS L'EMBOUT P1
                    ANGSEC = 0.D0
                  ELSE IF (X.LT.(-RCIN)) THEN
C  CAS OU LE LIGAMENT LONGI EST DANS L'EMBOUT P2
                    ANGSEC = ACOU*PI/180.0D0
                  ELSE
C  CAS OU LE LIGAMENT EST DANS LA PARTIE COUDE
                    VCOS = COS((-LT1-Z)/ ( SQRT ((X+RCIN)**2+Y**2 )))
                    VSIN = SIN((-LT1-Z)/ ( SQRT ((X+RCIN)**2+Y**2 )))
                    ANGSEC = ATAN2(VSIN,VCOS)
                  END IF   
C                  WRITE(6,*) 'NOM DU LIGAMENT : ',NOMGRN
C                  WRITE(6,*) 'POINT No',I
C                  WRITE(6,*) 'ANGLE EN DEGRES DE LA SECTION CONTENANT'
C     +               //' LE POINT DU LIGAMENT : ',ANGSEC*180.D0/PI
6               CONTINUE                
                VECTY(1) = SIN(ANGSEC)
                VECTY(2) = 0.0D0
                VECTY(3) = COS(ANGSEC)
              END IF
C                                   
            END IF
C                                          
                CALL PUTVTX ( 'NOM_CMP'  , 6, COMPO   ,I8V , IERUSR)
                CALL PUTVTX ( 'OPERATION', 1, OPER(3) ,I16V, IERUSR)
                CALL PUTVTX ( 'REPERE', 1, REP(1), I16V, IERUSR)
                CALL PUTVR8 ( 'VECT_Y', 3, VECTY, IERUSR)
             ELSE IF (K.EQ.3 .AND. .NOT. FLAG) THEN
                CALL PUTVTX ( 'NOM_CMP'  , 6, COMPO   ,I8V , IERUSR)
                CALL PUTVTX ( 'OPERATION', 1, OPER(3),I16V,IERUSR)
              END IF
            CALL SMFMCF ( IERUSR )
           END IF
         END IF
C
 20   CONTINUE
C
C     PRELEVEMENTS DES LIGAMENTS TOUS LES 45 DEGRES DANS LA SECTION
C     CONTENANT LA SOUS-EPAISSEUR
C
      ASEPR = ASEP*PI/180.D0
      IF (.NOT.PREM) THEN
        DO 40 I=1,8
           CALL CODENT(ISEP0,'G',NUME)
           NOMGRN = LIG(I)//NUME
           IF (K.EQ.2.OR.K.EQ.3) THEN                  
            CALL SMDMCF ( 'ACTION', IERUSR )
             CALL PUTVID ( 'RESULTAT', 1, NOMRES, IERUSR )
             CALL PUTVTX ( 'NOM_CHAM', 1, NOMCHA , I16V, IERUSR)
           ELSE
             CALL SMDMCF ( 'SEGMENT', IERUSR )
           END IF 
           CALL PUTVTX ( 'INTITULE', 1, NOMGRN , I8, IERUSR)
           CALL PUTVID ( 'GROUP_NO', 1, NOMGRN, IERUSR )
           IF (K.EQ.2) THEN
             CALL PUTVTX ( 'INVARIANT', 1, MOT(3) ,I8, IERUSR)
             CALL PUTVTX ( 'OPERATION', 1, OPER(1) ,I16V,IERUSR)
           ELSE IF (K.EQ.3 .AND. FLAG) THEN
C
C DEFINITION DU REPERE LOCAL POUR LES CONTRAINTES :
C
C * LE PREMIER AXE EST DONNE PAR LE LIGAMENT (AXE X = AXE RADIAL)
C * LE SECOND EST DONNE PAR AXE_Y ET DEFINIT ICI UN AXE NORMAL A LA
C   LA SECTION SOIT L'AXE LONGITUDINAL (AXE Y)
C * L'AXE Z EST DEDUIT DES DEUX AUTRES ET FORME L'AXE TANGENT A LA 
C   SECTION SOIT L'AXE CIRCONFERENTIEL (AXE Z)
C
C AXE_Y POUR UN TUBE :
C
              IF (GEOM(1:4).EQ.'TUBE') THEN
                 VECTY(1) = 0.0D0
                 VECTY(2) = 0.0D0
                 VECTY(3) = 1.0D0
              ELSE
C
C AXE_Y POUR UN COUDE :
C
                VECTY(1) = SIN(ASEPR)
                VECTY(2) = 0.0D0
                VECTY(3) = COS(ASEPR)
              END IF
C              
              CALL PUTVTX ( 'NOM_CMP'  , 6, COMPO   ,I8V , IERUSR)
              CALL PUTVTX ( 'OPERATION', 1, OPER(3),I16V,IERUSR)
              CALL PUTVTX ( 'REPERE', 1, REP(1), I16V, IERUSR)
              CALL PUTVR8 ( 'VECT_Y', 3, VECTY, IERUSR)
             ELSE IF (K.EQ.3 .AND. .NOT. FLAG) THEN
              CALL PUTVTX ( 'NOM_CMP'  , 6, COMPO   ,I8V , IERUSR)
              CALL PUTVTX ( 'OPERATION', 1, OPER(3),I16V,IERUSR)
            END IF
           CALL SMFMCF ( IERUSR )
           TEXTE = 'TABLE DE POST-TRAITEMENT SECTION SOUS-EPAISSEUR'
           CALL PUTVTX ( 'TITRE', 1, TEXTE , I80, IERUSR)
 40     CONTINUE
        CALL SMFCMD ( IERUSR )
        GOTO 10
      ELSE
        GOTO 30
      END IF
 30   CONTINUE
C
 5    CONTINUE
C
C     PRELEVEMENTS DES LIGAMENTS SUR LES SECTIONS MI,TU ET GV
C     LES 8 LIGAMENTS SONT TOUS LES 45 DEGRES
C
      ACOUR = ACOU*PI/180.0D0
      DO 130 I = 1,3
C
        IF (GEOM(1:4).EQ.'TUBE') THEN
          VECTY(1) = 0.0D0
          VECTY(2) = 0.0D0
          VECTY(3) = 1.0D0
        ELSE
          IF (I .EQ. 1) THEN
C         MI
            VECTY(1) = SIN(ACOUR/2.0D0)
            VECTY(2) = 0.0D0
            VECTY(3) = COS(ACOUR/2.0D0)
          ELSEIF (I .EQ. 2) THEN
C         TU
            VECTY(1) = 0.0D0
            VECTY(2) = 0.0D0
            VECTY(3) = 1.0D0
          ELSEIF (I .EQ. 3) THEN
C         GV
            VECTY(1) = SIN(ACOUR)
            VECTY(2) = 0.0D0
            VECTY(3) = COS(ACOUR)
          ENDIF
        END IF 
C
        ICMD = ICMD + 1
        CALL GCNCON ( '.' , NOPOSD )
        SECRCM(I) = NOPOSD
        CALL SMDCMD ( ICMD, NOPOSD, 'POST_RCCM', IERUSR )
        CALL PUTVID ( 'MATER',        1, MATER, IERUSR )
        CALL PUTVID ( 'MAILLAGE',     1, MAILLA, IERUSR )        
        CALL PUTVTX ( 'TYPE_RESU_MECA', 1, 'EVOLUTION', I9, IERUSR)
        CALL PUTVTX ( 'OPTION', 1, OPER(2) ,I8, IERUSR)        
        CALL SMDMCF ( 'TRANSITOIRE', IERUSR )
          CALL PUTVID ( 'RESULTAT', 1, NOMRES, IERUSR )
          CALL PUTVTX ( 'NOM_CHAM', 1, NOMCHA , I16V, IERUSR)
        CALL SMFMCF ( IERUSR )                                
C
C       MOYENNE RCCM SUR LES SECTIONS MI,TU ET GV
C        
        DO 140 J=1,8
           NOMGRN = LIG(J)//SECT(I)
           CALL SMDMCF ( 'SEGMENT', IERUSR )
             CALL PUTVTX ( 'INTITULE', 1, NOMGRN , I8, IERUSR)
             CALL PUTVID ( 'GROUP_NO', 1, NOMGRN, IERUSR )
           CALL SMFMCF ( IERUSR )
 140    CONTINUE
        TEXTE ='TABLE DE POST-TRAITEMENT MOYENNE RCCM SECTION '//SECT(I)
        CALL PUTVTX ( 'TITRE', 1, TEXTE , I80, IERUSR)
        CALL SMFCMD ( IERUSR )
C
        ICMD = ICMD + 1
        CALL GCNCON ( '.' , NOPOSD )
        SECINV(I) = NOPOSD
        CALL SMDCMD ( ICMD, NOPOSD, 'POST_RELEVE_T', IERUSR )
C
C       INVARIANTS SUR LES SECTIONS MI,TU ET GV
C        
        DO 150 J=1,8
           NOMGRN = LIG(J)//SECT(I)
           CALL SMDMCF ( 'ACTION', IERUSR )
             CALL PUTVTX ( 'INTITULE', 1, NOMGRN , I8, IERUSR)
             CALL PUTVID ( 'GROUP_NO', 1, NOMGRN, IERUSR )
             CALL PUTVID ( 'RESULTAT', 1, NOMRES, IERUSR )
             CALL PUTVTX ( 'NOM_CHAM', 1, NOMCHA , I16V, IERUSR)
             CALL PUTVTX ( 'INVARIANT', 1, MOT(3) ,I8, IERUSR)
             CALL PUTVTX ( 'OPERATION', 1, OPER(1),I16V, IERUSR)
           CALL SMFMCF ( IERUSR )
 150    CONTINUE
        TEXTE = 'TABLE DE POST-TRAITEMENT INVARIANTS SECTION '//SECT(I)
        CALL PUTVTX ( 'TITRE', 1, TEXTE , I80, IERUSR)
        CALL SMFCMD ( IERUSR )
C
        ICMD = ICMD + 1
        CALL GCNCON ( '.' , NOPOSD )
        SECMOY(I) = NOPOSD
        CALL SMDCMD ( ICMD, NOPOSD, 'POST_RELEVE_T', IERUSR )
C
C       MOYENNES CONTRAINTES SUR LES SECTIONS MI,TU ET GV
C        
        DO 160 J=1,8
           NOMGRN = LIG(J)//SECT(I)
           CALL SMDMCF ( 'ACTION', IERUSR )
             CALL PUTVTX ( 'INTITULE', 1, NOMGRN , I8, IERUSR)
             CALL PUTVTX ( 'REPERE', 1, REP(1), I16V, IERUSR)
             CALL PUTVR8 ( 'VECT_Y', 3, VECTY, IERUSR)
             CALL PUTVID ( 'GROUP_NO', 1, NOMGRN, IERUSR )
             CALL PUTVID ( 'RESULTAT', 1, NOMRES, IERUSR )
             CALL PUTVTX ( 'NOM_CHAM', 1, NOMCHA , I16V, IERUSR)
             CALL PUTVTX ( 'NOM_CMP'  , 6, COMPO   ,I8V  , IERUSR)
             CALL PUTVTX ( 'OPERATION', 1, OPER(3) , I16V, IERUSR)
           CALL SMFMCF ( IERUSR )
 160    CONTINUE
        TEXTE = 'TABLE DE POST-TRAITEMENT MOYENNE SECTION '//SECT(I)
        CALL PUTVTX ( 'TITRE', 1, TEXTE , I80, IERUSR)
        CALL SMFCMD ( IERUSR )
C
 130  CONTINUE
C
C    IMPRESSION DES VALEURS MAXIMALES POUR CHAQUE SOUS-EPAISSEUR
C
      DO 170 I=1, NBSEP
C
      IF (PPM) THEN
       ICMD = ICMD + 1
       CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
         CALL PUTVID ( 'TABLE', 1, TABLIG(I,1), IERUSR )
         IMPRTA(2) = 'PM'
         CALL PUTVTX ( 'NOM_PARA', 2, IMPRTA, I16V, IERUSR )
         CALL SMDMCF ( 'FILTRE', IERUSR )
          CALL PUTVTX ( 'NOM_PARA', 1, IMPRTA(2), I16V, IERUSR )
          CALL PUTVTX ( 'CRIT_COMP', 1, CRIT, I8, IERUSR )
         CALL SMFMCF ( IERUSR )
       CALL SMFCMD ( IERUSR )
      END IF
C
      IF (PPMPB) THEN
        ICMD = ICMD + 1
        CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
          CALL PUTVID ( 'TABLE', 1, TABLIG(I,1) , IERUSR )
          IMPRTA(2) = 'PMB'
          IMPRTA(3) = 'LIEU'
          CALL PUTVTX ( 'NOM_PARA', 2, IMPRTA, I16V, IERUSR )
          CALL SMDMCF ( 'FILTRE', IERUSR )
            CALL PUTVTX ( 'NOM_PARA', 1, IMPRTA(3), I16V, IERUSR )
            CALL PUTVTX ( 'VALE_K', 1, MOT(4), I8, IERUSR )
          CALL SMFMCF ( IERUSR )
           CALL SMDMCF ( 'FILTRE', IERUSR )
            CALL PUTVTX ( 'NOM_PARA', 1, IMPRTA(2), I16V, IERUSR )
            CALL PUTVTX ( 'CRIT_COMP', 1, CRIT, I8, IERUSR )
          CALL SMFMCF ( IERUSR )
       CALL SMFCMD ( IERUSR )
C
        ICMD = ICMD + 1
        CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
          CALL PUTVID ( 'TABLE', 1, TABLIG(I,1) , IERUSR )
          IMPRTA(2) = 'PMB'
          IMPRTA(3) = 'LIEU'          
          CALL PUTVTX ( 'NOM_PARA', 2, IMPRTA, I16V, IERUSR )
          CALL SMDMCF ( 'FILTRE', IERUSR )
            CALL PUTVTX ( 'NOM_PARA', 1, IMPRTA(3), I16V, IERUSR )
            CALL PUTVTX ( 'VALE_K', 1, MOT(5), I8, IERUSR )
          CALL SMFMCF ( IERUSR )
          CALL SMDMCF ( 'FILTRE', IERUSR )
            CALL PUTVTX ( 'NOM_PARA', 1, IMPRTA(2), I16V, IERUSR )
            CALL PUTVTX ( 'CRIT_COMP', 1, CRIT, I8, IERUSR )
          CALL SMFMCF ( IERUSR )
        CALL SMFCMD ( IERUSR )
      END IF
C
      IF (PSIRAD) THEN
        ICMD = ICMD + 1
        CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
          CALL PUTVID ( 'TABLE', 1, TABLIG(I,3) , IERUSR )
          IMPRTA(2) = 'SIXX'
         CALL PUTVTX ( 'NOM_PARA', 2, IMPRTA, I16V, IERUSR )
          CALL SMDMCF ( 'FILTRE', IERUSR )
            CALL PUTVTX ( 'NOM_PARA', 1, MOT(1), I8, IERUSR )
            CALL PUTVTX ( 'VALE_K'  , 1, MOT(2), I8, IERUSR )
          CALL SMFMCF ( IERUSR )
          CALL SMDMCF ( 'FILTRE', IERUSR )
            CALL PUTVTX ( 'NOM_PARA' , 1, COMPO(1), I8, IERUSR )
            CALL PUTVTX ( 'CRIT_COMP', 1, CRIT     , I8, IERUSR )
          CALL SMFMCF ( IERUSR )
        CALL SMFCMD ( IERUSR )
      END IF
C
      IF (PSILON) THEN
        ICMD = ICMD + 1
        CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
          CALL PUTVID ( 'TABLE', 1, TABLIG(I,3) , IERUSR )
          IMPRTA(2) = 'SIYY'
         CALL PUTVTX ( 'NOM_PARA', 2, IMPRTA, I16V, IERUSR )
          CALL SMDMCF ( 'FILTRE', IERUSR )
            CALL PUTVTX ( 'NOM_PARA', 1, MOT(1), I8, IERUSR )
            CALL PUTVTX ( 'VALE_K'  , 1, MOT(2), I8, IERUSR )
          CALL SMFMCF ( IERUSR )
          CALL SMDMCF ( 'FILTRE', IERUSR )
            CALL PUTVTX ( 'NOM_PARA' , 1, COMPO(2), I8, IERUSR )
            CALL PUTVTX ( 'CRIT_COMP', 1, CRIT     , I8, IERUSR )
          CALL SMFMCF ( IERUSR )
        CALL SMFCMD ( IERUSR )
      END IF
C
      IF (PSICIR) THEN
        ICMD = ICMD + 1
        CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
          CALL PUTVID ( 'TABLE', 1, TABLIG(I,3) , IERUSR )
          IMPRTA(2) = 'SIZZ'
         CALL PUTVTX ( 'NOM_PARA', 2, IMPRTA, I16V, IERUSR )
          CALL SMDMCF ( 'FILTRE', IERUSR )
            CALL PUTVTX ( 'NOM_PARA', 1, MOT(1), I8, IERUSR )
            CALL PUTVTX ( 'VALE_K'  , 1, MOT(2), I8, IERUSR )
          CALL SMFMCF ( IERUSR )
          CALL SMDMCF ( 'FILTRE', IERUSR )
            CALL PUTVTX ( 'NOM_PARA' , 1, COMPO(3), I8, IERUSR )
            CALL PUTVTX ( 'CRIT_COMP', 1, CRIT     , I8, IERUSR )
          CALL SMFMCF ( IERUSR )
        CALL SMFCMD ( IERUSR )
      END IF
C      
      IF (PTRESC) THEN
       ICMD = ICMD + 1
       CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
         CALL PUTVID ( 'TABLE', 1, TABLIG(I,2), IERUSR )
         IMPRTA(2) = 'NOEUD'
         IMPRTA(3) = 'TRESCA'
         CALL PUTVTX ( 'NOM_PARA',3, IMPRTA, I16V, IERUSR )
         CALL SMDMCF ( 'FILTRE', IERUSR )
           CALL PUTVTX ( 'NOM_PARA' , 1, IMPRTA(6),I8, IERUSR )
           CALL PUTVTX ( 'CRIT_COMP', 1, CRIT  , I8, IERUSR )
         CALL SMFMCF ( IERUSR )
       CALL SMFCMD ( IERUSR )
      END IF
C
 170  CONTINUE
C
C     IMPRESSION DES RESULTATS POUR CHAQUE SOUS-EPAISSEUR
C
      DO 175 I=1, NBSEP
C
      IF (PPM) THEN
        ICMD = ICMD + 1
        CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
          CALL PUTVID ( 'TABLE', 1,TABLIG(I,1) , IERUSR )
          IMPRTA(2) = 'PM'
          IMPRTA(3) = 'LIEU'          
          CALL PUTVTX ( 'NOM_PARA', 2, IMPRTA, I16V, IERUSR )
          CALL SMDMCF ( 'FILTRE', IERUSR )
            CALL PUTVTX ( 'NOM_PARA', 1, IMPRTA(3), I16V, IERUSR )
            CALL PUTVTX ( 'VALE_K'  , 1, MOT(4), I8, IERUSR )
          CALL SMFMCF ( IERUSR )
        CALL SMFCMD ( IERUSR )
      END IF
C
      IF (PPMPB) THEN
        ICMD = ICMD + 1
        CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
          CALL PUTVID ( 'TABLE', 1, TABLIG(I,1) , IERUSR )
          IMPRTA(2) = 'PMB'
          IMPRTA(3) = 'LIEU'
          CALL PUTVTX ( 'NOM_PARA', 2, IMPRTA, I16V, IERUSR )
          CALL SMDMCF ( 'FILTRE', IERUSR )
            CALL PUTVTX ( 'NOM_PARA', 1, IMPRTA(3), I16V, IERUSR )
            CALL PUTVTX ( 'VALE_K', 1, MOT(4), I8, IERUSR )
          CALL SMFMCF ( IERUSR )
        CALL SMFCMD ( IERUSR )
C
        ICMD = ICMD + 1
        CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
          CALL PUTVID ( 'TABLE', 1, TABLIG(I,1), IERUSR )
          IMPRTA(2) = 'PMB'
          IMPRTA(3) = 'LIEU'
          CALL PUTVTX ( 'NOM_PARA', 2, IMPRTA, I16V, IERUSR )
          CALL SMDMCF ( 'FILTRE', IERUSR )
            CALL PUTVTX ( 'NOM_PARA', 1, IMPRTA(3), I16V, IERUSR )
            CALL PUTVTX ( 'VALE_K', 1, MOT(5), I8, IERUSR )
          CALL SMFMCF ( IERUSR )
        CALL SMFCMD ( IERUSR )
      END IF
C
      IF (PSIRAD) THEN
        ICMD = ICMD + 1
        CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
          CALL PUTVID ( 'TABLE', 1,TABLIG(I,3) , IERUSR )
          IMPRTA(2) = 'SIXX'
          CALL PUTVTX ( 'NOM_PARA', 2, IMPRTA, I16V, IERUSR )
          CALL SMDMCF ( 'FILTRE', IERUSR )
            CALL PUTVTX ( 'NOM_PARA', 1, MOT(1), I8, IERUSR )
            CALL PUTVTX ( 'VALE_K'  , 1, MOT(2), I8, IERUSR )
          CALL SMFMCF ( IERUSR )
        CALL SMFCMD ( IERUSR )
      END IF
C        
      IF (PSILON) THEN
        ICMD = ICMD + 1
        CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
          CALL PUTVID ( 'TABLE', 1,TABLIG(I,3) , IERUSR )
          IMPRTA(2) = 'SIYY'
          CALL PUTVTX ( 'NOM_PARA', 2, IMPRTA, I16V, IERUSR )
          CALL SMDMCF ( 'FILTRE', IERUSR )
            CALL PUTVTX ( 'NOM_PARA', 1, MOT(1), I8, IERUSR )
            CALL PUTVTX ( 'VALE_K'  , 1, MOT(2), I8, IERUSR )
          CALL SMFMCF ( IERUSR )
        CALL SMFCMD ( IERUSR )
      END IF
C
      IF (PSICIR) THEN
        ICMD = ICMD + 1
        CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
          CALL PUTVID ( 'TABLE', 1,TABLIG(I,3) , IERUSR )
          IMPRTA(2) = 'SIZZ'
          CALL PUTVTX ( 'NOM_PARA', 2, IMPRTA, I16V, IERUSR )
          CALL SMDMCF ( 'FILTRE', IERUSR )
            CALL PUTVTX ( 'NOM_PARA', 1, MOT(1), I8, IERUSR )
            CALL PUTVTX ( 'VALE_K'  , 1, MOT(2), I8, IERUSR )
          CALL SMFMCF ( IERUSR )
        CALL SMFCMD ( IERUSR )
      END IF
C
      IF (PTRESC) THEN
       ICMD = ICMD + 1
       CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
         CALL PUTVID ( 'TABLE', 1, TABLIG(I,2), IERUSR )
         IMPRTA(1) = 'NOEUD'
         IMPRTA(2) = 'TRESCA'
         IMPRTA(3) = 'INTITULE'
         CALL PUTVTX ( 'NOM_PARA',3, IMPRTA, I16V, IERUSR )
         IMPRTA(1) = 'INTITULE'
         CALL PUTVTX ( 'PAGINATION',1, IMPRTA, I16V, IERUSR )
       CALL SMFCMD ( IERUSR )
      END IF
C
 175  CONTINUE
C
C     IMPRESSION DES RESULTATS POUR LES SECTIONS MI, TU ET GV
C
      DO 180 I=1,3
C
      IF (PPM) THEN
        ICMD = ICMD + 1
        CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
          CALL PUTVID ( 'TABLE', 1, SECRCM(I), IERUSR )
          IMPRTA(2) = 'PM'
          IMPRTA(3) = 'LIEU'
          CALL PUTVTX ( 'NOM_PARA', 2, IMPRTA, I16V, IERUSR )
          CALL SMDMCF ( 'FILTRE', IERUSR )
            CALL PUTVTX ( 'NOM_PARA', 1, IMPRTA(3), I16V, IERUSR )
            CALL PUTVTX ( 'VALE_K'  , 1, MOT(4), I8, IERUSR )
          CALL SMFMCF ( IERUSR )          
        CALL SMFCMD ( IERUSR )
      END IF
C
      IF (PPMPB) THEN
        ICMD = ICMD + 1
        CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
          CALL PUTVID ( 'TABLE', 1, SECRCM(I), IERUSR )
          IMPRTA(2) = 'PMB'
          IMPRTA(3) = 'LIEU'
          CALL PUTVTX ( 'NOM_PARA', 2, IMPRTA, I16V, IERUSR )
          CALL SMDMCF ( 'FILTRE', IERUSR )
            CALL PUTVTX ( 'NOM_PARA', 1, IMPRTA(3), I16V, IERUSR )
            CALL PUTVTX ( 'VALE_K', 1, MOT(4), I8, IERUSR )
          CALL SMFMCF ( IERUSR )
        CALL SMFCMD ( IERUSR )
C
        ICMD = ICMD + 1
        CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
          CALL PUTVID ( 'TABLE', 1, SECRCM(I) , IERUSR )
          IMPRTA(2) = 'PMB'
          IMPRTA(3) = 'LIEU'
          CALL PUTVTX ( 'NOM_PARA', 2, IMPRTA, I16V, IERUSR )
          CALL SMDMCF ( 'FILTRE', IERUSR )
            CALL PUTVTX ( 'NOM_PARA', 1, IMPRTA(3), I16V, IERUSR )
            CALL PUTVTX ( 'VALE_K', 1, MOT(5), I8, IERUSR )
          CALL SMFMCF ( IERUSR )
        CALL SMFCMD ( IERUSR )
      END IF
C
      IF (PSIRAD) THEN
        ICMD = ICMD + 1
        CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
          CALL PUTVID ( 'TABLE', 1, SECMOY(I), IERUSR )
          IMPRTA(2) = 'SIXX'
          CALL PUTVTX ( 'NOM_PARA', 2, IMPRTA, I16V, IERUSR )
          CALL SMDMCF ( 'FILTRE', IERUSR )
            CALL PUTVTX ( 'NOM_PARA', 1, MOT(1), I8, IERUSR )
            CALL PUTVTX ( 'VALE_K'  , 1, MOT(2), I8, IERUSR )
          CALL SMFMCF ( IERUSR )
        CALL SMFCMD ( IERUSR )
      END IF
C
      IF (PSILON) THEN
        ICMD = ICMD + 1
        CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
          CALL PUTVID ( 'TABLE', 1, SECMOY(I), IERUSR )
          IMPRTA(2) = 'SIYY'
          CALL PUTVTX ( 'NOM_PARA', 2, IMPRTA, I16V, IERUSR )
          CALL SMDMCF ( 'FILTRE', IERUSR )
            CALL PUTVTX ( 'NOM_PARA', 1, MOT(1), I8, IERUSR )
            CALL PUTVTX ( 'VALE_K'  , 1, MOT(2), I8, IERUSR )
          CALL SMFMCF ( IERUSR )
        CALL SMFCMD ( IERUSR )
      END IF
C
      IF (PSICIR) THEN
        ICMD = ICMD + 1
        CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
          CALL PUTVID ( 'TABLE', 1, SECMOY(I), IERUSR )
          IMPRTA(2) = 'SIZZ'
          CALL PUTVTX ( 'NOM_PARA', 2, IMPRTA, I16V, IERUSR )
          CALL SMDMCF ( 'FILTRE', IERUSR )
            CALL PUTVTX ( 'NOM_PARA', 1, MOT(1), I8, IERUSR )
            CALL PUTVTX ( 'VALE_K'  , 1, MOT(2), I8, IERUSR )
          CALL SMFMCF ( IERUSR )
        CALL SMFCMD ( IERUSR )
      END IF
C
      IF (PTRESC) THEN
       ICMD = ICMD + 1
       CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
         CALL PUTVID ( 'TABLE', 1,SECINV(I) , IERUSR )
         IMPRTA(1) = 'NOEUD'
         IMPRTA(2) = 'TRESCA'
         IMPRTA(3) = 'INTITULE'
         CALL PUTVTX ( 'NOM_PARA',3, IMPRTA, I16V, IERUSR )
         IMPRTA(1) = 'INTITULE'
         CALL PUTVTX ( 'PAGINATION', 1, IMPRTA, I16V, IERUSR )
       CALL SMFCMD ( IERUSR )
      END IF
C
 180  CONTINUE
C
 9999 CONTINUE
      CALL JEDEMA()
      END
