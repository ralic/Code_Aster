      SUBROUTINE CLFFCH(ALIAS,TYPE,NNO,XI,YI,ZI,XIN,YIN,ZIN,TN,
     &                  AJX,AJY,AJZ,BJXX,BJYY,BJZZ,BJXY,BJXZ,BJYZ,IDER)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/04/2011   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C......................................................................C
C......................................................................C
C                                                                      C
C BUT:   CALCUL DES FONCTIONS DE FORMES ET DE LEURS DERIVEES           C
C        AU POINT DE COORDONNEES XI,YI,ZI                              C
C                                                                      C
C ATTENTION: CETTE ROUTINE EST SPECIFIQUE DES ELEMENTS HOMOGENEISE     C
C                            INI100                                    C
C                                                                      C
C ENTREES                                                              C
C      NNO         : NOMBRE DE NOEUDS                                  C
C      ALIAS       : NOM D'ALIAS DE L'ELEMENT :HEXA20 OU HEXA8         C
C      TYPE        : TYPE DE LA FONCTION DE FORME : POUTRE OU FLUIDE   C
C      XI,YI,ZI    : POINT DE CALCUL DES F FORMES ET DERIVEES          C
C      XIN,YIN,ZIN : COORDONNEES INTRINSEQUES                          C
C      IDER        : INDICATEUR DE CALCUL DES DERIVEES                 C
C                    IDER = 0 CALCUL DE TN                             C
C                    IDER = 1 CALCUL DE TN, AJ*                        C
C                    IDER = 2 CALCUL DE TN AJ* BJ*                     C
C                                                                      C
C SORTIES                                                              C
C      TN  : FONCTIONS DE FORMES EN XI,YI,ZI                           C
C      AJ (AJX, AJY, AJZ) : DERIVEES DES F FORMES EN XI,YI,ZI          C
C      BJ (BJXX ... BJYZ) : DERIVEES SECONDES DES F FORMES EN XI,YI,ZI C
C......................................................................C
C......................................................................C
C
      IMPLICIT     NONE
      CHARACTER*6  ALIAS,TYPE
      REAL*8       TN(1),AJX(1),AJY(1),AJZ(1),XIN(1),YIN(1),ZIN(1),
     &             BJXX(1),BJYY(1),BJZZ(1),BJXY(1),BJXZ(1),BJYZ(1),
     &             XI,YI,ZI
      INTEGER      IDER, NNO
C----------------------------------------------------------------------
      REAL*8       X0,Y0,Z0,FXY,FXYDX,FXYDY,FXYDXY,
     &             FZ,FZDZ,FZD2Z,F,FDX,FDY,FDZ
      INTEGER      I
C----------------------------------------------------------------------
C
      IF ( TYPE .EQ. 'POUTRE' ) THEN
C
C     -------------------------------------------------------------
C     --- F. DE FORME ASSOCIEES AUX DDLS DE FLEXION DES POUTRES ---
C     -------------------------------------------------------------
C
        DO 10 I = 1, NNO
C
           X0 = XI*XIN(I)
           Y0 = YI*YIN(I)
           Z0 = ZI*ZIN(I)
C
           FXY     = (1.D0+X0) * (1.D0+Y0) * 0.25D0
           FXYDX   = XIN(I)    * (1.D0+Y0) * 0.25D0
           FXYDY   = (1.D0+X0) * YIN(I)    * 0.25D0
           FXYDXY  = XIN(I)    * YIN(I)    * 0.25D0
C
C          FONCTIONS DE FORME A DERIVEE NULLE AU BORD
C
           FZ      = - 0.25D0 * Z0*Z0*Z0     + 0.75D0 * Z0   +   0.5D0
           FZDZ    = - 0.75D0 * Z0*Z0*ZIN(I) + 0.75D0 * ZIN(I)
           FZD2Z   = - 1.5D0  * Z0*ZIN(I)*ZIN(I)
C
           TN(I)   = FXY   * FZ
C
           IF (IDER.GT.0) THEN
             AJX(I) = FXYDX * FZ
             AJY(I) = FXYDY * FZ
             AJZ(I) = FXY   * FZDZ
           ENDIF
C
           IF (IDER.GT.1) THEN
             BJXX(I) = 0.D0
             BJYY(I) = 0.D0
             BJZZ(I) = FXY    * FZD2Z
             BJXY(I) = FXYDXY * FZ
             BJXZ(I) = FXYDX  * FZDZ
             BJYZ(I) = FXYDY  * FZDZ
           ENDIF
C
C          FONCTIONS DE FORME A VALEUR NULLE AU BORD
C
           FZ    = 0.25D0*( -ZIN(I) - ZI + ZIN(I)*ZI*ZI  + ZI*ZI*ZI )
           FZDZ  = 0.25D0*( -1.D0 + 2.D0*ZIN(I)*ZI + 3.D0*ZI*ZI )
           FZD2Z = 0.25D0*( 2.D0*ZIN(I) + 6.D0*ZI )
C
           TN(I+8)   = FXY   * FZ
C
           IF (IDER.GT.0) THEN
             AJX(I+8) = FXYDX * FZ
             AJY(I+8) = FXYDY * FZ
             AJZ(I+8) = FXY   * FZDZ
           ENDIF
C
           IF (IDER.GT.1) THEN
             BJXX(I+8) = 0.D0
             BJYY(I+8) = 0.D0
             BJZZ(I+8) = FXY    * FZD2Z
             BJXY(I+8) = FXYDXY * FZ
             BJXZ(I+8) = FXYDX  * FZDZ
             BJYZ(I+8) = FXYDY  * FZDZ
           ENDIF
 10      CONTINUE
C
      ELSE IF ( TYPE .EQ. 'FLUIDE' ) THEN
C     --------------------------------------------------------------
      IF ( ALIAS .EQ. 'HEXA20' ) THEN
C
C     --------------------------------------------------------------
C     --- FONCTIONS DE FORMES AUX DDL FLUIDE DE LA MAILLE HEXA20 ---
C     --------------------------------------------------------------
C
        DO 20 I = 1, NNO
           X0 = XI*XIN(I)
           Y0 = YI*YIN(I)
           Z0 = ZI*ZIN(I)
C
           IF ( I .LE. 8 ) THEN
             F     = 0.125D0 * (1.D0+X0) * (1.D0+Y0) * (1.D0+Z0)
     &                       * (X0+Y0+Z0-2.D0)
             FDX   = 0.125D0             * (1.D0+Y0) * (1.D0+Z0)
     &                       * (2.D0*XI + XIN(I)*(Z0+Y0-1.D0))
             FDY   = 0.125D0 * (1.D0+X0)             * (1.D0+Z0)
     &                       * (2.D0*YI + YIN(I)*(X0+Z0-1.D0))
             FDZ   = 0.125D0 * (1.D0+X0) * (1.D0+Y0)
     &                       * (2.D0*ZI + ZIN(I)*(X0+Y0-1.D0))
C
           ELSE IF ((I.EQ.9).OR.(I.EQ.11).OR.
     &              (I.EQ.17).OR.(I.EQ.19)) THEN
             F     =  0.25D0 * (1.D0-XI*XI) * (1.D0+Y0) * (1.D0+Z0)
             FDX   = -0.5D0  *  XI          * (1.D0+Y0) * (1.D0+Z0)
             FDY   =  0.25D0 * (1.D0-XI*XI) *  YIN(I)   * (1.D0+Z0)
             FDZ   =  0.25D0 * (1.D0-XI*XI) * (1.D0+Y0) *  ZIN(I)
C
           ELSE IF ((I.EQ.10).OR.(I.EQ.12).OR.
     &              (I.EQ.18).OR.(I.EQ.20)) THEN
             F     =  0.25D0 * (1.D0-YI*YI) * (1.D0+X0) * (1.D0+Z0)
             FDY   = -0.5D0  *  YI          * (1.D0+X0) * (1.D0+Z0)
             FDX   =  0.25D0 * (1.D0-YI*YI) *  XIN(I)   * (1.D0+Z0)
             FDZ   =  0.25D0 * (1.D0-YI*YI) * (1.D0+X0) *  ZIN(I)
C
           ELSE IF ((I.EQ.13).OR.(I.EQ.14).OR.
     &              (I.EQ.15).OR.(I.EQ.16)) THEN
             F     =  0.25D0 * (1.D0-ZI*ZI) * (1.D0+X0) * (1.D0+Y0)
             FDZ   = -0.5D0  *  ZI          * (1.D0+X0) * (1.D0+Y0)
             FDX   =  0.25D0 * (1.D0-ZI*ZI) *  XIN(I)   * (1.D0+Y0)
             FDY   =  0.25D0 * (1.D0-ZI*ZI) * (1.D0+X0) *  YIN(I)
           ENDIF
C
           TN(I) = F
           IF (IDER.GT.0) THEN
             AJX(I) = FDX
             AJY(I) = FDY
             AJZ(I) = FDZ
           ENDIF
 20     CONTINUE
C
      ELSE IF ( ALIAS .EQ. 'HEXA8 ') THEN
C
C     ----------------------------------------------------
C     --- FONCTIONS DE FORMES ASSOCIEES A LA GEOMETRIE ---
C     ---    ET AUX DDL FLUIDE DE LA MAILLE HEXA8      ---
C     ----------------------------------------------------
C
        DO 30 I = 1, NNO
           X0 = XI*XIN(I)
           Y0 = YI*YIN(I)
           Z0 = ZI*ZIN(I)
C
           TN(I) = (1.D0+X0) * (1.D0+Y0) * (1.D0+Z0) * 0.125D0

           IF (IDER.GT.0) THEN
             AJX(I) = XIN(I) * (1.D0+Y0) * (1.D0+Z0) * 0.125D0
             AJY(I) = YIN(I) * (1.D0+X0) * (1.D0+Z0) * 0.125D0
             AJZ(I) = ZIN(I) * (1.D0+X0) * (1.D0+Y0) * 0.125D0
           ENDIF
           IF (IDER.GT.1) THEN
             BJXX(I) = 0.D0
             BJYY(I) = 0.D0
             BJZZ(I) = 0.D0
             BJXY(I) = XIN(I) * YIN(I) * (1.D0+Z0) * 0.125D0
             BJXZ(I) = XIN(I) * ZIN(I) * (1.D0+Y0) * 0.125D0
             BJYZ(I) = YIN(I) * ZIN(I) * (1.D0+X0) * 0.125D0
           ENDIF
 30   CONTINUE
      ENDIF
C     ---------------------------------------------------------
      ELSE
         CALL U2MESS('F','ELEMENTS_20')
      ENDIF
C
      END
