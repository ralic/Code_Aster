      SUBROUTINE UTCTAB (RAZ,NA,MB,MC,A,B,C,XAB,CTAB)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
      IMPLICIT NONE
      INTEGER  NA , MB , MC
      CHARACTER*(*) RAZ
      REAL*8   A(NA,NA) , B(NA,MB) , C(NA,MC) , XAB(NA,MB)
      REAL*8   CTAB(MC,MB)
C     ------------------------------------------------------------------
C     PRODUIT CT . A . B - A CARREE - B ET C  RECTANGULAIRE
C     ------------------------------------------------------------------
CIN   K4  RAZ  'ZERO' : ON FAIT CTAB = 0    + CT*A.B
C              'CUMU' : ON FAIT CTAB = CTAB + CT*A.B
CIN   I   NA   ORDRE DE A
CIN   I   MB   NB DE COLONNES DE B
CIN   I   MC   NB DE COLONNES DE C
CIN   R   A    MATRICE A           (NA,NA)
CIN   R   B    MATRICE B           (NA,MB)
CIN   R   C    MATRICE C           (NA,MC)
CIN   R   XAB  ZONE DE TRAVAIL XAB (NA,MB)
COUT  R   CTAB PRODUIT CT . A . B  (MC,MB)
C     ------------------------------------------------------------------
      CHARACTER*4   RAZ2
C --DEB
C-----------------------------------------------------------------------
      INTEGER I ,J ,K 
C-----------------------------------------------------------------------
      RAZ2=RAZ
C
      CALL R8INIR(NA*MB,0.0D0,XAB,1)
      DO 15 I = 1 , NA
         DO 15 K = 1 , NA
            DO 15 J = 1 , MB
               XAB(I,J) = XAB(I,J) + A(I,K) * B(K,J)
   15 CONTINUE
C
      IF (RAZ2.EQ.'ZERO') CALL R8INIR(MC*MB,0.0D0,CTAB,1)
C
      DO 25 I = 1 , MC
         DO 25 K = 1 , NA
            DO 25 J = 1 , MB
               CTAB(I,J) = CTAB(I,J) + C(K,I) * XAB(K,J)
   25 CONTINUE
      END
