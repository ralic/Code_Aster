      SUBROUTINE TE0348(OPTION,NOMTE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     OPTION,NOMTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/04/2004   AUTEUR JMBHH01 J.M.PROIX 
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
C     CALCUL DES CHARGES DE PESANTEUR POUR L'ELEMENT POU_D_TG
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C        'CHAR_MECA_PESA_R'  : CALCUL DES CHARGES DE PESANTEUR
C        'CHAR_MECA_TEMP_R'  : CALCUL DES CHARGES THERMIQUE
C IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
C        'MECA_POU_D_TG' : POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
C     ------------------------------------------------------------------
C
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
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      REAL*8       A, XL, XIY, XIZ, ALFAY, ALFAZ, EY, EZ, XJX, XJG
      REAL*8       ZERO, VALPAR(3), VALRES(3)
      REAL*8       COEF, F, TEMP, E, NU, RHO, G, ALPHA
      REAL*8       PGL(3,3), FE(14), MAT(105)
      REAL*8       Q(14), QQ(14), BSM(14,14), DE(14)
      CHARACTER*2  CODRET(3)
      CHARACTER*8  NOMPAR(3), NOMRES(3)
C     ------------------------------------------------------------------
      ZERO = 0.0D0
      NNO   = 2
      NC    = 7
C     ------------------------------------------------------------------
C
C     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
      CALL JEVECH ('PGEOMER','L',LX)
      LX = LX - 1
      XL = SQRT( (ZR(LX+4)-ZR(LX+1))**2
     +           + (ZR(LX+5)-ZR(LX+2))**2 + (ZR(LX+6)-ZR(LX+3))**2 )
      IF( XL .EQ. ZERO ) THEN
         CALL UTMESS('F','ELEMENTS DE POUTRE (TE0348)',
     +                  'NOEUDS CONFONDUS POUR UN ELEMENT')
      ENDIF
C
C     --- RECUPERATION DES ORIENTATIONS ---
      CALL JEVECH ('PCAORIE', 'L',LORIEN)
C
C     --- MATRICE DE PASSAGE DU REPERE GLOBAL AU REPERE LOCAL: PGL ---
      CALL MATROT ( ZR(LORIEN) , PGL )
C
C     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
      CALL JEVECH('PCAGNPO','L',LSECT)
      LSECT = LSECT - 1
      A     =  ZR(LSECT+1)
      XIY   =  ZR(LSECT+2)
      XIZ   =  ZR(LSECT+3)
      ALFAY =  ZR(LSECT+4)
      ALFAZ =  ZR(LSECT+5)
      EY    = -ZR(LSECT+6)
      EZ    = -ZR(LSECT+7)
      XJX   =  ZR(LSECT+8)
      XJG   =  ZR(LSECT+12)
C
C     --- LE MATERIAU ---
      CALL JEVECH('PMATERC','L',LMATE)
      CALL TECACH('NNN','PTEMPER',1,ITEMPE,IRET)
      IF ( ITEMPE .EQ. 0 ) THEN
         NBPAR     = 0
         NOMPAR(1) = ' '
         VALPAR(1) = ZERO
      ELSE
         NBPAR     = 1
         NOMPAR(1) = 'TEMP'
         VALPAR(1) = ZR(ITEMPE)
      ENDIF
C
C     ------------------------------------------------------------------
C
      IF ( OPTION.EQ.'CHAR_MECA_PESA_R') THEN
C
         CALL RCVALA(ZI(LMATE),' ','ELAS',0,NOMPAR,VALPAR,
     +                                1,'RHO',RHO,CODRET,'FM')
C
         CALL JEVECH('PPESANR','L',LPESA)
         DO 100 I=1,3
             Q(I)    = RHO *  ZR(LPESA) * ZR(LPESA+I)
             Q(I+3)  = ZERO
             Q(I+7)  = RHO *  ZR(LPESA) * ZR(LPESA+I)
             Q(I+10) = ZERO
 100     CONTINUE
         Q(7)  = ZERO
         Q(14) = ZERO
C
C        --- PASSAGE REPERE LOCAL DU VECTEUR FORCE ---
         CALL UTPVGL ( NNO, NC, PGL, Q(1), QQ(1) )
C
C        ---A CAUSE DES CHARGEMENTS VARIABLE ---
         COEF = A
C
C        ---UN CAS DE CHARGE DE PESANTEUR SE PASSE EN REPERE GLOBAL ---
C        -- ELEMENTS DROITS A SECTION CONSTANTE - ON TIENT COMPTE DES
C        -- EFFORTS TRANCHANTS
C        --- LA CHARGE EST CONSTANTE OU VARIE LINEAIREMENT ---
         COEF    =  COEF * XL
         FE(1)   =  ( QQ(1)/3.0D0 + QQ(8)/6.0D0 ) * COEF
         FE(8)   =  ( QQ(1)/6.0D0 + QQ(8)/3.0D0 ) * COEF
         FE(2)   =  ( 7.0D0*QQ(2) + 3.0D0*QQ(9) ) * COEF / 20.0D0
         FE(9)   =  ( 3.0D0*QQ(2) + 7.0D0*QQ(9) ) * COEF / 20.0D0
         FE(3)   =  ( 7.0D0*QQ(3) + 3.0D0*QQ(10)) * COEF / 20.0D0
         FE(10)  =  ( 3.0D0*QQ(3) + 7.0D0*QQ(10)) * COEF / 20.0D0
         COEF    =   COEF * XL
         FE(4)   =   ZERO
         FE(7)   =   ZERO
         FE(11)  =   ZERO
         FE(14)  =   ZERO
         FE(5)   = -( QQ(3)/20.0D0+ QQ(10)/30.0D0) * COEF
         FE(12)  =  ( QQ(3)/30.0D0+ QQ(10)/20.0D0) * COEF
         FE(6)   =  ( QQ(2)/20.0D0+ QQ(9) /30.0D0) * COEF
         FE(13)  = -( QQ(2)/30.0D0+ QQ(9) /20.0D0) * COEF
C
C     ------------------------------------------------------------------
C
      ELSEIF ( OPTION.EQ.'CHAR_MECA_TEMP_R') THEN
        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
        NOMRES(3) = 'ALPHA'
        CALL RCVALA(ZI(LMATE),' ','ELAS',NBPAR,NOMPAR,VALPAR,3,NOMRES,
     &                                     VALRES,CODRET, 'FM' )
         E      = VALRES(1)
         NU     = VALRES(2)
         G      = E / ( 2.0D0 * ( 1.0D0 + NU ) )
         ALPHA  = VALRES(3)
C
         DO 200 I = 1 , 105
            MAT(I) = ZERO
 200     CONTINUE
         CALL PTKA21(MAT,E,A,XL,XIY,XIZ,XJX,XJG,G,ALFAY,ALFAZ,EY,EZ)
C
C        --- REMPLISSAGE DE LA MATRICE CARREE ---
         IND = 0
         DO 202 I = 1, 14
            DE(I) = ZERO
            DO 204 J = 1, I-1
               IND = IND + 1
               BSM(I,J) = MAT(IND)
               BSM(J,I) = MAT(IND)
 204        CONTINUE
            IND = IND + 1
            BSM(I,I) = MAT(IND)
 202     CONTINUE
C
C        --- TEMPERATURE DE REFERENCE ---
         CALL JEVECH('PTEREF','L',LTREF)
C
C        --- TEMPERATURE EFFECTIVE ---
         CALL JEVECH('PTEMPER','L',LTEMP)
C
         TEMP = ZR(LTEMP) - ZR(LTREF)
C
         F = ALPHA * TEMP
         DE(1) = -F * XL
         DE(8) = -DE(1)
C
C        --- CALCUL DES FORCES INDUITES ---
         DO 206 I = 1 , 7
            FE(I)   = ZERO
            FE(I+7) = ZERO
            DO 208 J = 1 , 7
               FE(I)   = FE(I)   + BSM(I,J)     * DE(J)
               FE(I+7) = FE(I+7) + BSM(I+7,J+7) * DE(J+7)
 208       CONTINUE
 206    CONTINUE
C
      ENDIF
C
C     --- PASSAGE DU REPERE LOCAL AU REPERE GLOBAL ---
      CALL JEVECH ('PVECTUR','E',LVECT)
      CALL UTPVLG ( NNO, NC, PGL, FE(1), ZR(LVECT) )
C
      END
