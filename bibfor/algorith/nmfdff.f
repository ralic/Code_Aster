      SUBROUTINE NMFDFF(NDIM,NNO,AXI,G,R,RIGI,MATSYM,FR,VFF,DFF,DEF,PFF)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/11/2011   AUTEUR PROIX J-M.PROIX 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C ----------------------------------------------------------------------
C     BUT:  CALCUL DES PRDUITS DU GRADIENT DE TRANSFORMATION FR
C     PAR LES DERIVEES DE FONCITONS DE FORME DFF POUR NMDLOG ET NMGR3D
C     CONFIGURATION LAGRANGIENNE. 
C ----------------------------------------------------------------------
C IN  NDIM    : DIMENSION DU PROBLEME : 2 OU 3
C IN  DFF     : DERIVEE DES FONCTIONS DE FORME
C IN  FR      : GRADIENT TRANSFORMATION   
C OUT DEF     : PRODUITS F*DFF
C OUT PFF     : PRODUITS DFF*DFF
C
      IMPLICIT NONE
      INTEGER NDIM,NNO,N,NMAX,I,M,G
      REAL*8  DFF(NNO,*),VFF(NNO,*),FR(3,3),RAC2,R
      REAL*8  DEF(2*NDIM,NNO,NDIM),PFF(2*NDIM,NNO,NNO)
      LOGICAL AXI,RIGI,MATSYM

      RAC2=SQRT(2.D0)
      
      IF (NDIM.EQ.3) THEN
      
          DO 40 N=1,NNO
             DO 30 I=1,3
              DEF(1,N,I) =  FR(I,1)*DFF(N,1)
              DEF(2,N,I) =  FR(I,2)*DFF(N,2)
              DEF(3,N,I) =  FR(I,3)*DFF(N,3)
              DEF(4,N,I) = (FR(I,1)*DFF(N,2) + FR(I,2)*DFF(N,1))/RAC2
              DEF(5,N,I) = (FR(I,1)*DFF(N,3) + FR(I,3)*DFF(N,1))/RAC2
              DEF(6,N,I) = (FR(I,2)*DFF(N,3) + FR(I,3)*DFF(N,2))/RAC2
 30          CONTINUE
 40       CONTINUE

          IF (RIGI) THEN
             DO 125 N=1,NNO
                IF(MATSYM) THEN
                 NMAX = N
                ELSE
                  NMAX = NNO
                ENDIF
                DO 126 M=1,NMAX
                 PFF(1,N,M) =  DFF(N,1)*DFF(M,1)
                 PFF(2,N,M) =  DFF(N,2)*DFF(M,2)
                 PFF(3,N,M) =  DFF(N,3)*DFF(M,3)
                 PFF(4,N,M) =(DFF(N,1)*DFF(M,2)+DFF(N,2)*DFF(M,1))/RAC2
                 PFF(5,N,M) =(DFF(N,1)*DFF(M,3)+DFF(N,3)*DFF(M,1))/RAC2
                 PFF(6,N,M) =(DFF(N,2)*DFF(M,3)+DFF(N,3)*DFF(M,2))/RAC2
 126            CONTINUE
 125         CONTINUE
          ENDIF

      ELSEIF (NDIM.EQ.2) THEN
      
         DO 41 N=1,NNO
            DO 31 I=1,2
               DEF(1,N,I) =  FR(I,1)*DFF(N,1)
               DEF(2,N,I) =  FR(I,2)*DFF(N,2)
               DEF(3,N,I) =  0.D0
               DEF(4,N,I) = (FR(I,1)*DFF(N,2) + FR(I,2)*DFF(N,1))/RAC2
 31         CONTINUE
 41      CONTINUE
C 5.2.5 - TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1
         IF (AXI) THEN
            DO 50 N=1,NNO
               DEF(3,N,1) = FR(3,3)*VFF(N,G)/R
 50         CONTINUE
         ENDIF
         IF (RIGI) THEN
            DO 135 N=1,NNO
              IF(MATSYM) THEN
               NMAX = N
              ELSE
               NMAX = NNO
              ENDIF
              DO 136 M=1,NMAX
               PFF(1,N,M) =  DFF(N,1)*DFF(M,1)
               PFF(2,N,M) =  DFF(N,2)*DFF(M,2)
               PFF(3,N,M) = 0.D0
               PFF(4,N,M) =(DFF(N,1)*DFF(M,2)+DFF(N,2)*DFF(M,1))/RAC2
 136          CONTINUE
 135        CONTINUE
          ENDIF

      ENDIF
      END
