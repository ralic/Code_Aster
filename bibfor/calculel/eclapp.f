      SUBROUTINE ECLAPP(NDIM, NNO2, LONMIN, COOR)
      
      IMPLICIT NONE
      INTEGER NDIM, NNO2
      REAL*8  COOR(NDIM,NNO2), LONMIN
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 16/12/2004   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C        CONTROLE DE L'APPLATISSEMENT DES ELEMENTS AVEC ECLA_PG
C ----------------------------------------------------------------------
C IN  NDIM    I  DIMENSION DE L'ESPACE
C IN  NNO2    I  NOMBRE DE NOEUDS DE L'ELEMENT
C IN  LONMIN  R  LONGUEUR MINIMALE (LONGUEUR DES MEDIANES)
C VAR COOR    R  COORDONNEES DES NOEUDS
C                 IN  -> VALEURS INITIALES
C                 OUT -> VALEURS APRES CORRECTION (AFFINITE)
C ----------------------------------------------------------------------

      REAL*8  CORR, L1, L2, D1(3),D2(3),PREC
      REAL*8  DNRM2
      INTEGER I
      PARAMETER (PREC = 1.D-5)
C ----------------------------------------------------------------------

      
      
      IF (NNO2.EQ.4) THEN
      
        DO 10 I = 1,NDIM
          D1(I) = (COOR(I,2)+COOR(I,3)-COOR(I,1)-COOR(I,4))/2
          D2(I) = (COOR(I,3)+COOR(I,4)-COOR(I,1)-COOR(I,2))/2
 10     CONTINUE
         
        L1 = DNRM2(NDIM,D1,1)
        L2 = DNRM2(NDIM,D2,1)
        
C      ELEMENTS PLATS        
        IF (MIN(L1,L2)/MAX(L1,L2) .LT. PREC) THEN
          IF (L1.LT.L2) THEN
            CALL R8INIR(NDIM,0.D0,D1,1)
            D1(1) =  D2(2)
            D1(2) = -D2(1)
            CORR  = LONMIN/2.D0/DNRM2(NDIM,D1,1)
            DO 12 I = 1,NDIM
              COOR(I,1) = COOR(I,1) - CORR*D1(I)
              COOR(I,2) = COOR(I,2) + CORR*D1(I)
              COOR(I,3) = COOR(I,3) + CORR*D1(I)
              COOR(I,4) = COOR(I,4) - CORR*D1(I)
 12         CONTINUE
          ELSE
            CALL R8INIR(NDIM,0.D0,D2,1)
            D2(1) = -D1(2)
            D2(2) =  D1(1)
            CORR  = LONMIN/2.D0/DNRM2(NDIM,D2,1)
            DO 15 I = 1,NDIM
              COOR(I,1) = COOR(I,1) - CORR*D2(I)
              COOR(I,2) = COOR(I,2) - CORR*D2(I)
              COOR(I,3) = COOR(I,3) + CORR*D2(I)
              COOR(I,4) = COOR(I,4) + CORR*D2(I)
 15         CONTINUE
          END IF
          GOTO 9999
        END IF

C      ELEMENTS EPAIS
        IF (L1.LT.LONMIN) THEN
          CORR = LONMIN/L1
          DO 20 I = 1,NDIM
            COOR(I,1) = CORR*COOR(I,1)+(1-CORR)/2*(COOR(I,1)+COOR(I,2))
            COOR(I,2) = CORR*COOR(I,2)+(1-CORR)/2*(COOR(I,1)+COOR(I,2))
            COOR(I,3) = CORR*COOR(I,3)+(1-CORR)/2*(COOR(I,3)+COOR(I,4))
            COOR(I,4) = CORR*COOR(I,4)+(1-CORR)/2*(COOR(I,3)+COOR(I,4))
 20       CONTINUE
        END IF
        
        IF (L2.LT.LONMIN) THEN
          CORR = LONMIN/L2
          DO 30 I = 1,NDIM
            COOR(I,1) = CORR*COOR(I,1)+(1-CORR)/2*(COOR(I,1)+COOR(I,4))
            COOR(I,2) = CORR*COOR(I,2)+(1-CORR)/2*(COOR(I,2)+COOR(I,3))
            COOR(I,3) = CORR*COOR(I,3)+(1-CORR)/2*(COOR(I,2)+COOR(I,3))
            COOR(I,4) = CORR*COOR(I,4)+(1-CORR)/2*(COOR(I,1)+COOR(I,4))
 30       CONTINUE
        ENDIF
        
      ENDIF

 9999 CONTINUE
      END
