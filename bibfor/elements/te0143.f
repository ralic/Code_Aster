      SUBROUTINE TE0143(OPTION,NOMTE)
      IMPLICIT NONE
      CHARACTER*(*)     OPTION,NOMTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 10/05/2012   AUTEUR CHEIGNON E.CHEIGNON 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE  CRP_6
C-----------------------------------------------------------------------
C     CALCULE LA MATRICE DE RIGIDITE GEOMETRIQUE ELEMENTAIRE DES
C     ELEMENTS DE POUTRE D'EULER ET DE TIMOSHENKO
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C        'RIGI_MECA_GE' : CALCUL DE LA MATRICE DE RIGIDITE
C IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
C       'MECA_POU_D_E'  : POUTRE DROITE D'EULER       (SECTION VARIABLE)
C       'MECA_POU_D_T'  : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
C       'MECA_POU_C_T'  : POUTRE COURBE DE TIMOSHENKO(SECTION CONSTANTE)
C       'MECA_POU_D_EM' : POUTRE DROITE MULTIFIBRE D EULER (SECT. CONST)
C       'MECA_POU_D_TG' : POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
C       'MECA_POU_D_TGM': POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
C                         MULTI-FIBRES SECTION CONSTANTE
C     ------------------------------------------------------------------
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      CHARACTER*8  NOMAIL
      CHARACTER*16 CH16
      REAL*8       A  , XIY , XIZ , EZ, EY
      REAL*8       A2 , XIY2, XIZ2, XL
      REAL*8       RAD, ANG, ANGARC, ANGS2, XFLY, XFLZ
      REAL*8       PGL(3,3), PGL1(3,3), PGL2(3,3), MAT(105)
      REAL*8       ITYPE, IYR2, IZR2, XFL, B(14),KSI1, D1B3(2,3)
      REAL*8       ZERO,TRIGOM,SIGMA(14)
      INTEGER      LSECT,LSECT2,LORIEN,NNO,NC,I,LMAT,NCOMP
      INTEGER      LRCOU,LDEP,KP,ADR,LX,IADZI,IAZK24,NPG,IPLOUF,ISTRXR
C     ------------------------------------------------------------------
C
      ZERO = 0.0D0
      ITYPE= 0
C
C     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
      CALL JEVECH ('PCAGNPO', 'L',LSECT)
      LSECT = LSECT-1
      IF (NOMTE.EQ.'MECA_POU_D_TG' .AND.
     &    NOMTE.EQ.'MECA_POU_D_TGM' ) THEN
         ITYPE =  NINT(ZR(LSECT+23))
      ENDIF
C
C     --- SECTION INITIALE ---
      A     =  ZR(LSECT+1)
      XIY   =  ZR(LSECT+2)
      XIZ   =  ZR(LSECT+3)

      IF(NOMTE.EQ.'MECA_POU_D_TG'.OR.
     &   NOMTE.EQ.'MECA_POU_D_TGM'  ) THEN
         EY = -ZR(LSECT+6)
         EZ = -ZR(LSECT+7)
         IYR2= ZR(LSECT+10)
         IZR2= ZR(LSECT+11)
      ELSE
C     --- SECTION FINALE ---
         LSECT2 = LSECT + 11
         A2     = ZR(LSECT2+1)
         XIY2   = ZR(LSECT2+2)
         XIZ2   = ZR(LSECT2+3)
         EY     = -(ZR(LSECT+6)+ZR(LSECT2+6))/2.D0
         EZ     = -(ZR(LSECT+7)+ZR(LSECT2+7))/2.D0
      ENDIF
C
C     --- RECUPERATION DES ORIENTATIONS ---
      CALL JEVECH ('PCAORIE', 'L',LORIEN)
C
C     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
      CALL JEVECH ('PGEOMER', 'L',LX)
      LX = LX - 1
      XL = SQRT( (ZR(LX+4)-ZR(LX+1))**2
     &  + (ZR(LX+5)-ZR(LX+2))**2 + (ZR(LX+6)-ZR(LX+3))**2 )
      IF( XL .EQ. 0.D0 ) THEN
        CALL TECAEL(IADZI,IAZK24)
        NOMAIL = ZK24(IAZK24-1+3)(1:8)
        CALL U2MESK('F','ELEMENTS2_43',1,NOMAIL)
      ENDIF
C
      IF     ( NOMTE .EQ. 'MECA_POU_D_E' )  THEN
C        --- POUTRE DROITE D'EULER A 6 DDL ---
         NNO = 2
         NC  = 6
         CALL MATROT ( ZR(LORIEN) , PGL )
      ELSEIF ( NOMTE .EQ. 'MECA_POU_D_T' ) THEN
C        --- POUTRE DROITE DE TIMOSKENKO A 6 DDL ---
         NNO = 2
         NC  = 6
         CALL MATROT ( ZR(LORIEN) , PGL )
      ELSEIF ( NOMTE .EQ. 'MECA_POU_C_T' )  THEN
         NNO = 2
         NC  = 6
C        --- POUTRE COURBE DE TIMOSHENKO A 6 DDL ---
         CALL U2MESS('F','ELEMENTS3_28')
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
         ANGS2  = TRIGOM('ASIN', XL / ( 2.D0 * RAD ) )
         ANG    = ANGS2 * 2.D0
         XL     = RAD * ANG
         XIY    = XIY  / XFLY
         XIZ    = XIZ  / XFLZ
         XIY2   = XIY2 / XFLY
         XIZ2   = XIZ2 / XFLZ
         CALL MATRO2 ( ZR(LORIEN) , ANGARC , ANGS2 , PGL1 , PGL2 )

      ELSE IF(NOMTE.EQ.'MECA_POU_D_TG'.OR.
     &        NOMTE.EQ.'MECA_POU_D_TGM'  ) THEN
         NNO = 2
         NC  = 7
         CALL MATROT ( ZR(LORIEN) , PGL )
      ELSE
         CH16 = NOMTE
         CALL U2MESK('F','ELEMENTS2_42',1,CH16)
      ENDIF
C
      DO 10 I = 1, 105
         MAT(I) = 0.D0
10    CONTINUE
C
C     --- CALCUL DES MATRICES ELEMENTAIRES ----
      IF ( OPTION .EQ. 'RIGI_MECA_GE' ) THEN
C
C        --- CALCUL DE LA MATRICE DE RIGIDITE GEOMETRIQUE ---
         CALL JEVECH('PEFFORR','L',LDEP)
         CALL JEVECH('PMATUUR','E',LMAT)
         IF(NOMTE.EQ.'MECA_POU_D_T' .OR.
     &      NOMTE.EQ.'MECA_POU_D_E' .OR.
     &      NOMTE.EQ.'MECA_POU_C_T'  ) THEN
C           NOMBRE DE POINTS DE GAUSS
            CALL ELREF4(' ','RIGI',IPLOUF,IPLOUF,IPLOUF,
     &            NPG,IPLOUF,IPLOUF,IPLOUF,IPLOUF)
            CALL ASSERT( (NPG.EQ.2).OR.(NPG.EQ.3) )
            IF ( NPG .EQ. 2 ) THEN
               DO 15 I=1,NC
                  SIGMA(I)    = ZR(LDEP+I-1)
                  SIGMA(I+NC) = ZR(LDEP+NC+I-1)
15             CONTINUE
            ELSE
               DO 17 I=1,NC
                  SIGMA(I)    = ZR(LDEP+I-1)
                  SIGMA(I+NC) = ZR(LDEP+NC+NC+I-1)
17             CONTINUE
            ENDIF
            IF ( ITYPE.NE.10 )  THEN
               CALL PTKG00(SIGMA,A,A2,XIZ,XIZ2,XIY,XIY2,XL,EY,EZ,MAT)
            ELSE
               CALL U2MESS('A','ELEMENTS3_28')
            ENDIF

         ELSEIF( NOMTE.EQ.'MECA_POU_D_TG') THEN
            CALL JSPGNO(XL, ZR(LDEP), B)
            DO 20 I = 1, 7
               B(I) = -B(I)
20          CONTINUE
            CALL PTKG20(B,A,XIZ,XIY,IYR2,IZR2,XL,EY,EZ,MAT)

         ELSEIF( NOMTE.EQ.'MECA_POU_D_TGM') THEN
C     --- RECUPERATION DES CARACTERISTIQUES DES FIBRES
            CALL JEVECH('PNBSP_I','L',I)

C     ON PROJETTE AVEC LES FCTS DE FORME
C     SUR LES NOEUDS DEBUT ET FIN DE L'ELEMENT
C     POUR LE POINT 1
            KSI1 = -SQRT( 5.D0 / 3.D0 )
            D1B3(1,1) = KSI1*(KSI1-1.D0)/2.0D0
            D1B3(1,2) = 1.D0-KSI1*KSI1
            D1B3(1,3) = KSI1*(KSI1+1.D0)/2.0D0
C     POUR LE POINT 2
            KSI1 = SQRT( 5.D0 / 3.D0 )
            D1B3(2,1) = KSI1*(KSI1-1.D0)/2.0D0
            D1B3(2,2) = 1.D0-KSI1*KSI1
            D1B3(2,3) = KSI1*(KSI1+1.D0)/2.0D0
C
C     ON RECUPERE LES EFFORTS GENERALISE CF OPTION SIEF_ELNO
            CALL JEVECH('PSTRXRR','L',ISTRXR)
C     NOMBRE DE COMPOSANTE PAR PDG DANS LE CHAMPS PSTRXRR
            NCOMP = 18
C       AU NOEUD 1 ON RECUPERE   -EFFORT STOCKE
C       AU NOEUD 2 ON RECUPERE   +EFFORT STOCKE
            DO 210 I = 1 , NC
               B(I)    = ZERO
               B(I+NC) = ZERO
               DO 212 KP = 1 , 3
                  ADR = ISTRXR+NCOMP*(KP-1)+I-1
                  B(I)   = B(I)   -ZR(ADR)*D1B3(1,KP)
                  B(I+NC)= B(I+NC)+ZR(ADR)*D1B3(2,KP)
212            CONTINUE
210         CONTINUE
            CALL PTKG20(B,A,XIZ,XIY,IYR2,IZR2,XL,EY,EZ,MAT)
         ENDIF
C
C        --- PASSAGE DU REPERE LOCAL AU REPERE GLOBAL ---
         IF ( ITYPE.EQ.10 ) THEN
            CALL CHGREP ( 'LG', PGL1, PGL2, MAT, ZR(LMAT) )
         ELSE
            CALL UTPSLG ( NNO, NC, PGL, MAT, ZR(LMAT) )
         ENDIF
C
      ELSE
         CH16 = OPTION
         CALL U2MESK('F','ELEMENTS2_47',1,CH16)
      ENDIF
C
      END
