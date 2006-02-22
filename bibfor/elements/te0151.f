      SUBROUTINE TE0151 ( OPTION , NOMTE )
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
      IMPLICIT  REAL*8  (A-H,O-Z)
      CHARACTER*(*)       OPTION , NOMTE
C     ------------------------------------------------------------------
C MODIF ELEMENTS  DATE 21/02/2006   AUTEUR FLANDI L.FLANDI 
C TOLE CRP_6
C     CALCUL
C       - ENERGIE DE DEFORMATION
C       - ENERGIE CINETIQUE
C     POUR LES ELEMENTS DE POUTRE D'EULER ET DE TIMOSHENKO.
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C        'EPOT_ELEM_DEPL' : ENERGIE DE DEFORMATION
C        'ECIN_ELEM_DEPL' : ENERGIE CINETIQUE
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
      PARAMETER    (             NBRES = 4 )
      REAL*8              VALRES(NBRES)
      CHARACTER*2    BL2, CODRES(NBRES)
      CHARACTER*8  NOMPAR,NOMRES(NBRES)
      CHARACTER*16 CH16
      REAL*8       UL(12), UG(12), PGL(3,3), KLC(12,12), KLV(78)
      REAL*8       PGL1(3,3), PGL2(3,3)
C     ------------------------------------------------------------------
      DATA NOMRES / 'E' , 'NU' , 'ALPHA' , 'RHO' /
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
      CALL TECACH('ONN','PTEMPER',1,ITEMPE,IRET)
      IF ( ITEMPE .EQ. 0 ) THEN
         NBPAR  = 0
         NOMPAR = ' '
         VALPAR = ZERO
      ELSE
         NBPAR  = 1
         NOMPAR = 'TEMP'
         VALPAR = 0.5D0*(ZR(ITEMPE)+ZR(ITEMPE+1))
      ENDIF
C
      BL2 = '  '
      CALL RCVALA(ZI(LMATER),' ','ELAS',NBPAR,NOMPAR,VALPAR,2,
     +              NOMRES,VALRES,CODRES,'FM')
      CALL RCVALA(ZI(LMATER),' ','ELAS',NBPAR,NOMPAR,VALPAR,1,
     +              NOMRES(3),VALRES(3),CODRES(3),BL2)
      IF ( CODRES(3) .NE. 'OK' ) VALRES(3) = ZERO
      CALL RCVALA(ZI(LMATER),' ','ELAS',NBPAR,NOMPAR,VALPAR,1,
     +              NOMRES(4),VALRES(4),CODRES(4),'FM')
C
      E      = VALRES(1)
      XNU    = VALRES(2)
      ALPHAT = VALRES(3)
      RHO    = VALRES(4)
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
     +  + (ZR(LX+5)-ZR(LX+2))**2 + (ZR(LX+6)-ZR(LX+3))**2 )
      IF( XL .EQ. ZERO ) THEN
         CH16 = ' ?????????'
         CALL UTMESS('F','ELEMENTS DE POUTRE (TE0144)',
     +   'NOEUDS CONFONDUS POUR UN ELEMENT: '//CH16(:8))
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
         CALL UTMESS('F','ELEMENTS DE POUTRE (TE0144)',
     +                   '"'//CH16//'"    NOM D''ELEMENT INCONNU.')
      ENDIF
C
      CALL JEVECH ('PDEPLAR', 'L', JDEPL)
      DO 20 I = 1,12
         UG(I) = ZR(JDEPL+I-1)
 20   CONTINUE
C
C     --- MATRICE DE ROTATION PGL
C     --- VECTEUR DEPLACEMENT LOCAL  UL = PGL * UG
      IF ( ITYPE .EQ. 10 ) THEN
         CALL UTPVGL ( NNO, NC, PGL1, UG, UL )
         CALL UTPVGL ( NNO, NC, PGL2, UG(7), UL(7) )
      ELSE
         CALL UTPVGL ( NNO, NC, PGL, UG, UL )
      ENDIF
C
C                    --- ENERGIE DE DEFORMATION ----
C
      IF( OPTION .EQ. 'EPOT_ELEM_DEPL' ) THEN
         CALL JEVECH ('PENERDR', 'E', JENDE)
C
C        --- CALCUL DE LA MATRICE DE RIGIDITE LOCALE
         IF ( ITYPE .EQ. 0 ) THEN
C           --- POUTRE DROITE A SECTION CONSTANTE ---
            CALL PTKA01(KLV,E,A,XL,XIY,XIZ,XJX,
     +                      G,ALFAY,ALFAZ,EY,EZ,ISTRUC)
         ELSE IF ( ITYPE .EQ. 1 .OR. ITYPE .EQ. 2 ) THEN
C           --- POUTRE DROITE A SECTION VARIABLE (TYPE 1 OU 2) ---
            CALL PTKA02(ITYPE,KLV,E,A,A2,XL,
     +                  XIY,XIY2,XIZ,XIZ2,XJX,XJX2,G,
     +                  ALFAY,ALFAY2,ALFAZ,ALFAZ2,EY,EZ,ISTRUC)
         ELSE IF  ( ITYPE .EQ. 10 ) THEN
C           --- POUTRE COURBE A SECTION CONSTANTE ---
            CALL PTKA10(KLV,E,A,XIY,XIZ,XJX,
     +                      G,ALFAY,ALFAZ,RAD,ANG,ISTRUC)
         ENDIF
C
C        ---- MATRICE RIGIDITE LIGNE > MATRICE RIGIDITE CARRE
         CALL VECMA  (KLV,78,KLC,12)
C        --- ENERGIE DE DEFORMATION
         IF = 1
         CALL PTENPO(12,UL,KLC,ZR(JENDE),ITYPE,IF)
         IF (ALPHAT.NE.ZERO) THEN
           CALL PTENTH(UL,XL,ALPHAT,12,KLC,ITYPE,ENERTH)
           ZR(JENDE) = ZR(JENDE) - ENERTH
         ENDIF
C
C                     --- ENERGIE CINETIQUE ----
C
      ELSEIF( OPTION .EQ. 'ECIN_ELEM_DEPL' ) THEN
         CALL JEVECH ('PENERCR', 'E', JENDE)
         CALL JEVECH ('PMASDIA', 'L', JMASD)
         CALL JEVECH ('PFREQR' , 'L', JFREQ)
         KANL = ZI(JMASD)
C
C        --- CALCUL DE LA MATRICE DE MASSE LOCALE
         IF ( RHO.NE.ZERO ) THEN
C           --- KANL = 0 MASSES CONCENTREES
C           --- KANL = 1 MASSES COHERENTES
            IF ( ITYPE.LT.10 ) THEN
C              --- POUTRE DROITE SECTION CONSTANTE OU VARIABLE (1 OU 2)
               CALL PTMA01( KANL,ITYPE,KLV,ISTRUC,RHO,E,
     +                       A,A2,XL,XIY,XIY2,XIZ,XIZ2,G,
     +                       ALFAY,ALFAY2,ALFAZ,ALFAZ2,EY,EZ )
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
         ELSEIF ( CODRES(4)(1:2).NE.'OK') THEN
            CALL UTMESS('F','ELEMENTS DE POUTRE (TE0151)',
     +                            'PAS DE VALEUR UTILISATEUR POUR RHO')
         ENDIF
C
      ELSE
         CH16 = OPTION
         CALL UTMESS('F','ELEMENTS DE POUTRE (TE0151)',
     +                   'L''OPTION "'//CH16//'" EST INCONNUE')
      ENDIF
C
      END
