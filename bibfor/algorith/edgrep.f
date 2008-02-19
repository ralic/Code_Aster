      SUBROUTINE EDGREP (TYPMOD,COORD,ANIC,ANI)

      IMPLICIT REAL*8 (A-H,O-Z)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/02/2008   AUTEUR CANO V.CANO 
C RESPONSABLE CANO V.CANO

      REAL*8         COORD(3),ANIC(6,6),ANI(6,6)
      CHARACTER*8   TYPMOD(2)

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

C ----------------------------------------------------------------------
C    MODELE VISCOPLASTIQUE SANS SEUIL DE EDGAR
C    TRANSFORMATION DE LA MATRICE ANIC DANS LE REPERE PRINCIPAL
C  IN  NDIM  : DIMENSION DU PROBLEME
C  IN  COORD : COORDONNEES DU POINT DE GAUSS
C  IN  ANIC  : MATRICE D ANISOTROPIE DANS LE REPERE CYLINDRIQUE

C  OUT ANI  : MATRICE D ANISOTROPIE DANS LE REPERE PRINCIPAL
C ----------------------------------------------------------------------
     
      INTEGER I,J,K,L
      INTEGER IND(3,3)
      REAL*8  R8PREM,ZERO,NORME,COST,SINT       
      REAL*8  PAS(3,3),PASI(3,3)        
      DATA    IND/1,4,5,
     &            4,2,6,
     &            5,6,3/
           
      IF (TYPMOD(1)(1:4).EQ.'AXIS') THEN
        DO 5 I=1,6
          DO 6 J=1,6
            ANI(I,J)=ANIC(I,J)
 6        CONTINUE
 5      CONTINUE
        ANI(1,2)=ANIC(1,3)
        ANI(1,3)=ANIC(1,2)
        ANI(2,1)=ANIC(3,1)
        ANI(2,2)=ANIC(3,3)        
        ANI(3,1)=ANIC(2,1)        
        ANI(3,3)=ANIC(2,2)
        ANI(4,4)=ANIC(5,5)
        ANI(5,5)=ANIC(4,4)
      ELSE
        ZERO=R8PREM()
      
        NORME=SQRT((COORD(1)**2)+(COORD(2)**2))
        IF (NORME.LE.ZERO) THEN
          COST=0.D0
          SINT=0.D0
        ELSE
          COST=COORD(1)/NORME
          SINT=COORD(2)/NORME
        ENDIF

C MATRICE DE PASSAGE PAS ET INVERSE PASI
C DU REPERE X Y Z AU REPERE CYLINDRIQUE R T Z
      
        DO 10 I=1,3
          DO 20 J=1,3
            PAS(I,J)=0.D0
            PASI(I,J)=0.D0
 20       CONTINUE
 10     CONTINUE         
        PAS(1,1)=COST
        PAS(1,2)=-SINT
        PAS(2,1)=SINT
        PAS(2,2)=COST
        PAS(3,3)=1.D0

        PASI(1,1)=COST
        PASI(1,2)=SINT
        PASI(2,1)=-SINT
        PASI(2,2)=COST
        PASI(3,3)=1.D0

        DO 30 I=1,3
          DO 40 J=1,3
            DO 50 K=1,3
              DO 60 L=1,3
                IF ((J.GE.I).AND.(L.GE.K))THEN              
                ANI(IND(I,J),IND(K,L))=
     &       ANIC(1,1)*PAS(I,1)*PASI(1,J)*PASI(1,K)*PAS(L,1)
     &      +ANIC(2,2)*PAS(I,2)*PASI(2,J)*PASI(2,K)*PAS(L,2)
     &      +ANIC(3,3)*PAS(I,3)*PASI(3,J)*PASI(3,K)*PAS(L,3)
     &      +ANIC(4,4)*(PAS(I,1)*PASI(2,J) + PAS(I,2)*PASI(1,J))
     &                *(PASI(1,K)*PAS(L,2) + PASI(2,K)*PAS(L,1))
     &      +ANIC(5,5)*(PAS(I,1)*PASI(3,J) + PAS(I,3)*PASI(1,J))
     &                *(PASI(1,K)*PAS(L,3) + PASI(3,K)*PAS(L,1))
     &      +ANIC(6,6)*(PAS(I,2)*PASI(3,J) + PAS(I,3)*PASI(2,J))
     &                *(PASI(2,K)*PAS(L,3) + PASI(3,K)*PAS(L,2))
     &      +ANIC(1,2)*(PAS(I,1)*PASI(1,J)*PASI(2,K)*PAS(L,2)+
     &                  PAS(I,2)*PASI(2,J)*PASI(1,K)*PAS(L,1))
     &      +ANIC(1,3)*(PAS(I,1)*PASI(1,J)*PASI(3,K)*PAS(L,3)+
     &                  PAS(I,3)*PASI(3,J)*PASI(1,K)*PAS(L,1))
     &      +ANIC(2,3)*(PAS(I,2)*PASI(2,J)*PASI(3,K)*PAS(L,3)+
     &                  PAS(I,3)*PASI(3,J)*PASI(2,K)*PAS(L,2))
                ENDIF
 60           CONTINUE
 50         CONTINUE
 40       CONTINUE
 30     CONTINUE
      
      ENDIF      
      END
