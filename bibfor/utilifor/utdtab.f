      SUBROUTINE UTDTAB (RAZ,NA,NB,MB,MD,A,B,D,XAB,DTAB)
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
      INTEGER  NA , MB , MD
      CHARACTER*(*) RAZ
      REAL*8   A(NA,NB) , B(NB,MB) , D(NA,MD) , XAB(NA,MB)
      REAL*8   DTAB(MD,MB)
C     ------------------------------------------------------------------
C     PRODUIT DT . A . B  - A B ET D  RECTANGULAIRES
C     ------------------------------------------------------------------
CIN   K4  RAZ  'ZERO' : ON FAIT DTAB = 0    + DT*A.B
C              'CUMU' : ON FAIT DTAB = DTAB + DT*A.B
CIN   I   NA   NB DE LIGNES DE A
CIN   I   NB   NB DE COLONNES DE A
CIN   I   MB   NB DE COLONNES DE B
CIN   I   MD   NB DE COLONNES DE C
CIN   R   A    MATRICE A           (NA,NB)
CIN   R   B    MATRICE B           (NB,MB)
CIN   R   D    MATRICE D           (NA,MD)
CIN   R   XAB  ZONE DE TRAVAIL XAB (NA,MB)
COUT  R   DTAB PRODUIT DT . A . B  (MD,MB)
C     ------------------------------------------------------------------
      CHARACTER*4   RAZ2
C --DEB
C-----------------------------------------------------------------------
      INTEGER I ,J ,K ,NB 
C-----------------------------------------------------------------------
      RAZ2=RAZ
C
      CALL R8INIR(NA*MB,0.0D0,XAB,1)
      DO 15 I = 1 , NA
         DO 15 K = 1 , NB
            DO 15 J = 1 , MB
               XAB(I,J) = XAB(I,J) + A(I,K) * B(K,J)
   15 CONTINUE
C
      IF (RAZ2.EQ.'ZERO') CALL R8INIR(MD*MB,0.0D0,DTAB,1)
C
      DO 25 I = 1 , MD
         DO 25 K = 1 , NA
            DO 25 J = 1 , MB
               DTAB(I,J) = DTAB(I,J) + D(K,I) * XAB(K,J)
   25 CONTINUE
      END
