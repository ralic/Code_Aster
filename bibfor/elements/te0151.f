      SUBROUTINE TE0151 ( OPTION , NOMTE )
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
      IMPLICIT  REAL*8  (A-H,O-Z)
      CHARACTER*(*)       OPTION , NOMTE
C     ------------------------------------------------------------------
C MODIF ELEMENTS  DATE 16/01/2012   AUTEUR PELLET J.PELLET 
C TOLE CRP_6
C     CALCUL
C       - ENERGIE DE DEFORMATION
C       - ENERGIE CINETIQUE
C     POUR LES ELEMENTS DE POUTRE D'EULER ET DE TIMOSHENKO.
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C        'EPOT_ELEM' : ENERGIE DE DEFORMATION
C        'ECIN_ELEM' : ENERGIE CINETIQUE
C IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
C        'MECA_POU_D_E' : POUTRE DROITE D'EULER       (SECTION VARIABLE)
C        'MECA_POU_D_T' : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
C        'MECA_POU_C_T' : POUTRE COURBE DE TIMOSHENKO(SECTION CONSTANTE)
C     ------------------------------------------------------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      PARAMETER    (             NBRES = 3 )
      REAL*8              VALRES(NBRES)
      INTEGER CODRES(NBRES)
      CHARACTER*1  STOPZ(3)
      CHARACTER*4  FAMI
      CHARACTER*8  NOMPAR,NOMRES(NBRES),NOMAIL,FAMIL,POUM
      CHARACTER*16 CH16
      REAL*8       UL(12), UG(12), PGL(3,3), KLC(12,12), KLV(78)
      REAL*8       PGL1(3,3), PGL2(3,3), EPSTHE
      INTEGER      IADZI,IAZK24,KPG,SPT
C     ------------------------------------------------------------------
      DATA NOMRES / 'E' , 'NU' , 'RHO' /
C     ------------------------------------------------------------------
      ZERO   = 0.D0
      UN     = 1.D0
      DEUX   = 2.D0
C     ------------------------------------------------------------------
C
C     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
      CALL JEVECH ('PMATERC', 'L', LMATER)
      DO 10 I = 1, NBRES
         VALRES(I) = ZERO
 10   CONTINUE
C
      FAMI = 'RIGI'
      NPG = 3
      IF (NOMTE.EQ.'MECA_POU_C_T') NPG = 2
C
      CALL MOYTEM(FAMI,NPG,1,'+',VALPAR,IRET)
      CALL VERIFM(FAMI,NPG,1,'+',ZI(LMATER),'ELAS',1,EPSTHE,IRET)
      NBPAR  = 1
      NOMPAR = 'TEMP'
      FAMIL='FPG1'
      KPG=1
      SPT=1
      POUM='+'
C
      CALL RCVALB(FAMIL,KPG,SPT,POUM,ZI(LMATER),' ','ELAS',NBPAR,NOMPAR,
     &              VALPAR,2,NOMRES,VALRES,CODRES,1)
      CALL RCVALB(FAMIL,KPG,SPT,POUM,ZI(LMATER),' ','ELAS',NBPAR,NOMPAR,
     &              VALPAR,1,NOMRES(3),VALRES(3),CODRES(3),1)
C
      E      = VALRES(1)
      XNU    = VALRES(2)
      RHO    = VALRES(3)
      G = E / ( DEUX * ( UN + XNU ) )
C
C     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
      CALL JEVECH ('PCAGNPO', 'L',LSECT)
      LSECT = LSECT-1
      ITYPE =  NINT(ZR(LSECT+23))
C
C     --- SECTION INITIALE ---
      A     =  ZR(LSECT+ 1)
      XIY   =  ZR(LSECT+ 2)
      XIZ   =  ZR(LSECT+ 3)
      ALFAY =  ZR(LSECT+ 4)
      ALFAZ =  ZR(LSECT+ 5)
C     EY    = -ZR(LSECT+ 6)
C     EZ    = -ZR(LSECT+ 7)
      XJX   =  ZR(LSECT+ 8)
C
C     --- SECTION FINALE ---
      LSECT2 = LSECT + 11
      A2     = ZR(LSECT2+ 1)
      XIY2   = ZR(LSECT2+ 2)
      XIZ2   = ZR(LSECT2+ 3)
      ALFAY2 = ZR(LSECT2+ 4)
      ALFAZ2 = ZR(LSECT2+ 5)
      EY     = -(ZR(LSECT+6)+ZR(LSECT2+6))/DEUX
      EZ     = -(ZR(LSECT+7)+ZR(LSECT2+7))/DEUX
      XJX2   = ZR(LSECT2+ 8)
C
C     --- RECUPERATION DES ORIENTATIONS ---
      CALL JEVECH ('PCAORIE', 'L',LORIEN)
C
C     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
      CALL JEVECH ('PGEOMER', 'L',LX)
      LX = LX - 1
      XL = SQRT( (ZR(LX+4)-ZR(LX+1))**2
     &  + (ZR(LX+5)-ZR(LX+2))**2 + (ZR(LX+6)-ZR(LX+3))**2 )
      IF( XL .EQ. ZERO ) THEN
        CALL TECAEL(IADZI,IAZK24)
        NOMAIL = ZK24(IAZK24-1+3)(1:8)
        CALL U2MESK('F','ELEMENTS2_43',1,NOMAIL)
      ENDIF
C
      IF     ( NOMTE .EQ. 'MECA_POU_D_E' )  THEN
C        --- POUTRE DROITE D'EULER A 6 DDL ---
         ISTRUC = 1
         NNO = 2
         NC  = 6
         ALFAY  = ZERO
         ALFAZ  = ZERO
         ALFAY2 = ZERO
         ALFAZ2 = ZERO
         CALL MATROT ( ZR(LORIEN) , PGL )
      ELSEIF ( NOMTE .EQ. 'MECA_POU_D_T' ) THEN
C        --- POUTRE DROITE DE TIMOSKENKO A 6 DDL ---
         ISTRUC = 1
         NNO = 2
         NC  = 6
         CALL MATROT ( ZR(LORIEN) , PGL )
      ELSEIF ( NOMTE .EQ. 'MECA_POU_C_T' ) THEN
C        --- POUTRE COURBE DE TIMOSKENKO A 6 DDL ---
         ISTRUC = 1
         NNO = 1
         NC  = 6
         CALL JEVECH ('PCAARPO', 'L',LRCOU)
         RAD    = ZR(LRCOU)
         ANGARC = ZR(LRCOU+1)
         XFL    = ZR(LRCOU+2)
         XFLY   = XFL
         XFLZ   = XFL
         IF (XFL.EQ.ZERO) THEN
             XFLY   = ZR(LRCOU+4)
             XFLZ   = ZR(LRCOU+6)
         ENDIF
         ANGS2  = ASIN( XL / ( DEUX * RAD ) )
         ANG    = ANGS2 * DEUX
         XL     = RAD * ANG
         X2IY   = XIY
         X2IZ   = XIZ
         XIY    = XIY  / XFLY
         XIZ    = XIZ  / XFLZ
         XIY2   = XIY2 / XFLY
         XIZ2   = XIZ2 / XFLZ
         CALL MATRO2 ( ZR(LORIEN) , ANGARC , ANGS2 , PGL1 , PGL2 )
      ELSE
         CH16 = NOMTE
         CALL U2MESK('F','ELEMENTS2_42',1,CH16)
      ENDIF
C
      IF (OPTION.NE.'ECIN_ELEM') THEN
        CALL JEVECH ('PDEPLAR', 'L', JDEPL)
        DO 20 I = 1,12
          UG(I) = ZR(JDEPL+I-1)
 20     CONTINUE
      ELSE
        STOPZ(1)='O'
        STOPZ(2)='N'
        STOPZ(3)='O'
        CALL TECACH(STOPZ,'PVITESR',1,JVITE,IRET)
C IRET NE PEUT VALOIR QUE 0 (TOUT EST OK) OU 2 (CHAMP NON FOURNI)
        IF (IRET.EQ.0) THEN
          DO 21 I = 1,12
            UG(I) = ZR(JVITE+I-1)
 21       CONTINUE
        ELSE
          CALL TECACH(STOPZ,'PDEPLAR',1,JDEPL,IRET)
          IF (IRET.EQ.0) THEN
            DO 22 I = 1,12
              UG(I) = ZR(JDEPL+I-1)
 22         CONTINUE
          ELSE
            CALL U2MESK('F','ELEMENTS2_1',1,OPTION)
          ENDIF
        ENDIF
      ENDIF
C
C     --- MATRICE DE ROTATION PGL
C     --- VECTEUR DEPLACEMENT OU VITESSE LOCAL  UL = PGL * UG
      IF ( ITYPE .EQ. 10 ) THEN
         CALL UTPVGL ( NNO, NC, PGL1, UG, UL )
         CALL UTPVGL ( NNO, NC, PGL2, UG(7), UL(7) )
      ELSE
         CALL UTPVGL ( NNO, NC, PGL, UG, UL )
      ENDIF
C
C                    --- ENERGIE DE DEFORMATION ----
C
      IF( OPTION .EQ. 'EPOT_ELEM' ) THEN
         CALL JEVECH ('PENERDR', 'E', JENDE)
C
C        --- CALCUL DE LA MATRICE DE RIGIDITE LOCALE
         IF ( ITYPE .EQ. 0 ) THEN
C           --- POUTRE DROITE A SECTION CONSTANTE ---
            CALL PTKA01(KLV,E,A,XL,XIY,XIZ,XJX,
     &                      G,ALFAY,ALFAZ,EY,EZ,ISTRUC)
         ELSE IF ( ITYPE .EQ. 1 .OR. ITYPE .EQ. 2 ) THEN
C           --- POUTRE DROITE A SECTION VARIABLE (TYPE 1 OU 2) ---
            CALL PTKA02(ITYPE,KLV,E,A,A2,XL,
     &                  XIY,XIY2,XIZ,XIZ2,XJX,XJX2,G,
     &                  ALFAY,ALFAY2,ALFAZ,ALFAZ2,EY,EZ,ISTRUC)
         ELSE IF  ( ITYPE .EQ. 10 ) THEN
C           --- POUTRE COURBE A SECTION CONSTANTE ---
            CALL PTKA10(KLV,E,A,XIY,XIZ,XJX,
     &                      G,ALFAY,ALFAZ,RAD,ANG,ISTRUC)
         ENDIF
C
C        ---- MATRICE RIGIDITE LIGNE > MATRICE RIGIDITE CARRE
         CALL VECMA  (KLV,78,KLC,12)
C        --- ENERGIE DE DEFORMATION
         IF = 1
         CALL PTENPO(12,UL,KLC,ZR(JENDE),ITYPE,IF)
         IF (EPSTHE.NE.ZERO) THEN
           CALL PTENTH(UL,XL,EPSTHE,12,KLC,ITYPE,ENERTH)
           ZR(JENDE) = ZR(JENDE) - ENERTH
         ENDIF
C
C                     --- ENERGIE CINETIQUE ----
C
      ELSEIF( OPTION .EQ. 'ECIN_ELEM' ) THEN
         CALL JEVECH ('PENERCR', 'E', JENDE)
         CALL JEVECH ('PMASDIA', 'L', JMASD)
         CALL JEVECH ('POMEGA2' , 'L', JFREQ)
         KANL = ZI(JMASD)
C
C        --- CALCUL DE LA MATRICE DE MASSE LOCALE
         IF ( RHO.NE.ZERO ) THEN
C           --- KANL = 0 MASSES CONCENTREES
C           --- KANL = 1 MASSES COHERENTES
            IF ( ITYPE.LT.10 ) THEN
C              --- POUTRE DROITE SECTION CONSTANTE OU VARIABLE (1 OU 2)
               CALL PTMA01( KANL,ITYPE,KLV,ISTRUC,RHO,E,
     &                       A,A2,XL,XIY,XIY2,XIZ,XIZ2,G,
     &                       ALFAY,ALFAY2,ALFAZ,ALFAZ2,EY,EZ )
            ELSEIF ( ITYPE.EQ.10 ) THEN
C              --- POUTRE COURBE SECTION CONSTANTE ---
               CALL PTMA10 ( KLV,RHO,A,XL,X2IY,X2IZ )
            ENDIF
C
C           ---- MATRICE MASSE LIGNE > MATRICE MASSE CARRE
            CALL VECMA  (KLV,78,KLC,12)
C           --- ENERGIE CINETIQUE
            IF = 1
            CALL PTENCI(12,UL,KLC,ZR(JFREQ),ZR(JENDE),ITYPE,KANL,IF)
C
         ELSEIF ( CODRES(3).NE.0) THEN
            CALL U2MESS('F','ELEMENTS3_31')
         ENDIF
C
      ELSE
         CH16 = OPTION
         CALL U2MESK('F','ELEMENTS2_47',1,CH16)
      ENDIF
C
      END
