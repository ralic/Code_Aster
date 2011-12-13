      SUBROUTINE UTBTAB (RAZ,NA,MB,A,B,XAB,BTAB)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 06/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) RAZ
      INTEGER  NA , MB
      REAL*8   A(NA,NA) , B(NA,MB) , XAB(NA,MB)
      REAL*8   BTAB(MB,MB)
C     ------------------------------------------------------------------
C     PRODUIT BT . A . B - A CARREE - B RECTANGULAIRE
C     ------------------------------------------------------------------
CIN   K4  RAZ  'ZERO' : ON FAIT BTAB = 0    + BT*A.B
C              'CUMU' : ON FAIT BTAB = BTAB + BT*A.B
CIN   I   NA   ORDRE DE A
CIN   I   MB   NB DE COLONNES DE B
CIN   R   A    MATRICE A           (NA,NA)
CIN   R   B    MATRICE B           (NA,MB)
CIN   R   XAB  ZONE DE TRAVAIL XAB (NA,MB)
COUT  R   BTAB PRODUIT BT . A . B  (MB,MB)
C     ------------------------------------------------------------------
      CHARACTER*4   RAZ2
C --DEB
      RAZ2=RAZ
C
      CALL R8INIR(NA*MB,0.0D0,XAB,1)
      DO 15 I = 1 , NA
         DO 15 K = 1 , NA
            DO 15 J = 1 , MB
               XAB(I,J) = XAB(I,J) + A(I,K) * B(K,J)
   15 CONTINUE
C
      IF (RAZ2.EQ.'ZERO') CALL R8INIR(MB*MB,0.0D0,BTAB,1)
C
      DO 25 I = 1 , MB
         DO 25 K = 1 , NA
            DO 25 J = 1 , MB
               BTAB(I,J) = BTAB(I,J) + B(K,I) * XAB(K,J)
   25 CONTINUE
      END
