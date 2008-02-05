      SUBROUTINE PTFOP1 ( ITYPE, COEF1, COEF2, XL, RAD, ANGS2, GLOBAL,
     &                    QQ, FE )
      IMPLICIT  NONE
      INTEGER             ITYPE
      REAL*8              COEF1, COEF2, XL, RAD, ANGS2, FE(12), QQ(12)
      LOGICAL             GLOBAL
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 05/02/2008   AUTEUR FLEJOU J-L.FLEJOU 
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
C
C     ------------------------------------------------------------------
      REAL*8   SECT1, SECT2, COEF, XX1, XX2
C     ------------------------------------------------------------------
C
      SECT1 = COEF1
      SECT2 = COEF2
C
C     --- CALCUL DES FORCES NODALES EQUIVALENTES EN REPERE LOCAL ---
      IF ( ITYPE .EQ. 0 .OR. ITYPE.EQ.30.OR. ITYPE.EQ.20) THEN
C        -- ELEMENTS DROITS A SECTION CONSTANTE - ON TIENT COMPTE DES
C        -- EFFORTS TRANCHANTS
C        --- LA CHARGE EST CONSTANTE OU VARIE LINEAIREMENT ---
         SECT1   =  SECT1 * XL
         FE(1)   =  ( QQ(1)/3.D0 + QQ(7)/6.D0 ) * SECT1
         FE(7)   =  ( QQ(1)/6.D0 + QQ(7)/3.D0 ) * SECT1
         FE(2)   =  ( 7.D0*QQ(2) + 3.D0* QQ(8)) * SECT1 / 20.D0
         FE(8)   =  ( 3.D0*QQ(2) + 7.D0* QQ(8)) * SECT1 / 20.D0
         FE(3)   =  ( 7.D0*QQ(3) + 3.D0* QQ(9)) * SECT1 / 20.D0
         FE(9)   =  ( 3.D0*QQ(3) + 7.D0* QQ(9)) * SECT1 / 20.D0
         SECT1   =  SECT1 * XL
         FE(4)   =   0.D0
         FE(10)  =   0.D0
         FE(5)   = -( QQ(3)/20.D0+ QQ(9)/30.D0) * SECT1
         FE(11)  =  ( QQ(3)/30.D0+ QQ(9)/20.D0) * SECT1
         FE(6)   =  ( QQ(2)/20.D0+ QQ(8)/30.D0) * SECT1
         FE(12)  = -( QQ(2)/30.D0+ QQ(8)/20.D0) * SECT1
C
      ELSE IF ( ITYPE .EQ. 1 ) THEN
C        --- ELEMENTS DROITS A SECTION VARIABLE TYPE 1 /
C        ---  ON NE TIENT PAS COMPTE DE EFFORTS TRANCHANTS
         FE(1)   =   QQ(1) * ( SECT1/3.D0 + SECT2/6.D0) * XL
         FE(2)   =   QQ(2) * ( 7.D0*SECT1 + 3.D0*SECT2) * XL / 20.D0
         FE(3)   =   QQ(3) * ( 7.D0*SECT1 + 3.D0*SECT2) * XL / 20.D0
         FE(4)   =   0.D0
         FE(5)   =  -QQ(3) * ( SECT1/20.D0 + SECT2/30.D0) * XL * XL
         FE(6)   =   QQ(2) * ( SECT1/20.D0 + SECT2/30.D0) * XL * XL
         FE(7)   =   QQ(7) * ( SECT1/6.D0 + SECT2/3.D0) * XL
         FE(8)   =   QQ(8) * ( 3.D0*SECT1 + 7.D0*SECT2) * XL /20.D0
         FE(9)   =   QQ(9) * ( 3.D0*SECT1 + 7.D0*SECT2) * XL /20.D0
         FE(10) =    0.D0
         FE(11) =    QQ(9) * ( SECT1/30.D0 + SECT2/20.D0) * XL * XL
         FE(12) =   -QQ(8) * ( SECT1/30.D0 + SECT2/20.D0) * XL * XL
      ELSE IF ( ITYPE .EQ. 2 ) THEN
C        ---  ELEMENTS DROITS A SECTION VARIABLE TYPE 2 /
C        ---  ON NE TIENT PAS COMPTE DE EFFORTS TRANCHANTS
         COEF   =   SQRT( SECT1*SECT2)
         FE(1)  =   QQ(1) * (SECT1/4.D0+SECT2/12.D0+COEF/6.D0) * XL
         FE(2)  =  QQ(2) * (8.D0*SECT1+2.D0*SECT2+5.D0*COEF) * XL/30.D0
         FE(3)  =  QQ(3) * (8.D0*SECT1+2.D0*SECT2+5.D0*COEF) * XL/30.D0
         FE(4)  =  0.D0
         FE(5)  = -QQ(3) * (2.D0*SECT1+SECT2+2.D0*COEF)*XL*XL/60.D0
         FE(6)  =  QQ(2) * (2.D0*SECT1+SECT2+2.D0*COEF)*XL*XL/60.D0
         FE(7)  =  QQ(7) * (SECT1/12.D0+SECT2/4.D0+COEF/6.D0) * XL
         FE(8)  =  QQ(8) * (2.D0*SECT1+8.D0*SECT2+5.D0*COEF) * XL/30.D0
         FE(9)  =  QQ(9) * (2.D0*SECT1+8.D0*SECT2+5.D0*COEF) * XL/30.D0
         FE(10) =  0.D0
         FE(11) =  QQ(9) * (SECT1+2.D0*SECT2+2.D0*COEF)*XL*XL/60.D0
         FE(12) = -QQ(8) * (SECT1+2.D0*SECT2+2.D0*COEF)*XL*XL/60.D0
      ELSE IF ( ITYPE .EQ. 10 ) THEN
C        --- ELEMENTS COURBES ---
C        --- CAS DES CHARGES REPARTIES RADIALEMENT ---
         IF ( GLOBAL ) THEN
            XX1 = SECT1 * XL / 2.D0
            XX2 = XX1 * XL / 6.D0
            FE(1) =  QQ(1) * XX1
            FE(2) =  QQ(2) * XX1
            FE(3) =  QQ(3) * XX1
            FE(4) =  0.D0
            FE(5) = -QQ(3) * XX2
            FE(6) =  QQ(2) * XX2
            FE(7) =  QQ(7) * XX1
            FE(8) =  QQ(8) * XX1
            FE(9) =  QQ(9) * XX1
            FE(10)=  0.D0
            FE(11)=  QQ(9) * XX2
            FE(12)= -QQ(8) * XX2
         ELSE
            XX1 = SECT1 * RAD * ANGS2
            XX2 = XX1 * RAD * ANGS2 / 3.D0
            FE(1)  =  QQ(1) * XX1
            FE(2)  =  QQ(2) * XX1
            FE(3)  =  QQ(3) * XX1
            FE(4)  =  0.D0
            FE(5)  = -QQ(3) * XX2
            FE(6)  =  QQ(2) * XX2
            FE(7)  =  QQ(7) * XX1
            FE(8)  =  QQ(8) * XX1
            FE(9)  =  QQ(9) * XX1
            FE(10) =  0.D0
            FE(11) =  QQ(9) * XX2
            FE(12) = -QQ(8) * XX2
         ENDIF
      ELSE
         CALL U2MESS('F','ELEMENTS2_48')
      ENDIF
C
      END
