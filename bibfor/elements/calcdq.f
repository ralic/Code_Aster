      SUBROUTINE CALCDQ(PROJ, NUB, NU ,D, PQX, PQY, PQZ, DQ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/01/2007   AUTEUR DESROCHES X.DESROCHES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      INTEGER PROJ,I,II
      REAL*8 NU,NUB,PQX(4),PQY(4),PQZ(4),D(6,6),DQ(72)
      REAL*8 DX(6),DY(6),DZ(6)
C.......................................................................
C
C     BUT:  CALCUL  DE L'INCREMENT DQ POUR ACTUALISER LES CONTRAINTES 
C           EN HYPO-ELASTICITE 3D POUR LE HEXA8 SOUS INTEGRE
C           STABILITE PAR ASSUMED STRAIN (3 VARIANTES)
C.......................................................................
C IN  PROJ     : NOMBRE DE NOEUDS DE L'ELEMENT
C IN  NUB      : NUB = NU/(1-NU)
C IN  NU       : COEFFICIENT DE POISSON
C IN  D        : NOMBRE DE POINTS DE GAUSS
C IN  PQX      : INCREMENT DE DEFORMATIONS GENERALISEES EN X
C IN  PQY      : INCREMENT DE DEFORMATIONS GENERALISEES EN Y
C IN  PQZ      : INCREMENT DE DEFORMATIONS GENERALISEES EN Z
C OUT  DQ      : INCREMENT SERVANT AU CALCUL DES CONTRAINTES

C         QUAS4 SANS PROJECTION
C         ---------------------
          IF (PROJ.EQ.0) THEN
            DO 1 I = 1,6
              II = 12*(I-1)
              DQ(II+1) = D(I,1)*PQX(1)+ D(I,4)*PQY(1)
     &                                + D(I,5)*PQZ(1)
              DQ(II+2) = D(I,4)*PQX(1)+ D(I,2)*PQY(1)
     &                                + D(I,6)*PQZ(1)
              DQ(II+3) = D(I,5)*PQX(1)+ D(I,6)*PQY(1)
     &                                + D(I,3)*PQZ(1)
              DQ(II+4) = D(I,1)*PQX(2)+ D(I,4)*PQY(2)
     &                                + D(I,5)*PQZ(2)
              DQ(II+5) = D(I,4)*PQX(2)+ D(I,2)*PQY(2)
     &                                + D(I,6)*PQZ(2)
              DQ(II+6) = D(I,5)*PQX(2)+ D(I,6)*PQY(2)
     &                                + D(I,3)*PQZ(2)
              DQ(II+7) = D(I,1)*PQX(3)+ D(I,4)*PQY(3)
     &                                + D(I,5)*PQZ(3)
              DQ(II+8) = D(I,4)*PQX(3)+ D(I,2)*PQY(3)
     &                                + D(I,6)*PQZ(3)
              DQ(II+9) = D(I,5)*PQX(3)+ D(I,6)*PQY(3)
     &                                + D(I,3)*PQZ(3)
              DQ(II+10) = D(I,1)*PQX(4)+ D(I,4)*PQY(4)
     &                                 + D(I,5)*PQZ(4)
              DQ(II+11) = D(I,4)*PQX(4)+ D(I,2)*PQY(4)
     &                                 + D(I,6)*PQZ(4)
              DQ(II+12) = D(I,5)*PQX(4)+ D(I,6)*PQY(4)
     &                                 + D(I,3)*PQZ(4)
  1        CONTINUE

C         ADS
C         ---
          ELSE IF (PROJ.EQ.1) THEN
            DO 2 I = 1,6
              II = 12*(I-1)
              DX(I) = (2.0D0*D(I,1)    -D(I,2)    -D(I,3))/3.0D0
              DY(I) = ( -D(I,1) +2.0D0*D(I,2)     -D(I,3))/3.0D0
              DZ(I) = (  -D(I,1)    -D(I,2) +2.0D0*D(I,3))/3.0D0
              DQ(II+1) = DX(I)*PQX(1) + D(I,4)*PQY(1)
     &                                + D(I,5)*PQZ(1)
              DQ(II+2) = D(I,4)*PQX(1) + DY(I)*PQY(1)
              DQ(II+3) = D(I,5)*PQX(1) + DZ(I)*PQZ(1)
              DQ(II+4) = DX(I)*PQX(2) + D(I,4)*PQY(2)
              DQ(II+5) = D(I,4)*PQX(2) + DY(I)*PQY(2)
     &                                 + D(I,6)*PQZ(2)
              DQ(II+6) = D(I,6)*PQY(2) + DZ(I)*PQZ(2)
              DQ(II+7) = DX(I)*PQX(3) + D(I,5)*PQZ(3)
              DQ(II+8) = DY(I)*PQY(3) + D(I,6)*PQZ(3)
              DQ(II+9) = D(I,5)*PQX(3)+ D(I,6)*PQY(3)
     &                                + DZ(I)*PQZ(3)
              DQ(II+10) = DX(I)*PQX(4)
              DQ(II+11) = DY(I)*PQY(4)
              DQ(II+12) = DZ(I)*PQZ(4)
  2        CONTINUE
C
C         ASBQI
C         -----
          ELSE IF (PROJ.EQ.2) THEN
            DO 4 I = 1,6
              II = 12*(I-1)
              DX(I) =     D(I,1) -NU*D(I,2) -NU*D(I,3)
              DY(I) = -NU*D(I,1)    +D(I,2) -NU*D(I,3)
              DZ(I) = -NU*D(I,1) -NU*D(I,2)    +D(I,3)
              DQ(II+1) = DX(I)*PQX(1) + D(I,4)*PQY(1)
     &                                + D(I,5)*PQZ(1)
              DQ(II+2) = D(I,4)*PQX(1) + (D(I,2)-D(I,3)*NUB)*PQY(1)
              DQ(II+3) = D(I,5)*PQX(1) + (D(I,3)-D(I,2)*NUB)*PQZ(1)
              DQ(II+4) = (D(I,1)-D(I,3)*NUB)*PQX(2) + D(I,4)*PQY(2)
              DQ(II+5) = D(I,4)*PQX(2) + DY(I)*PQY(2)
     &                                 + D(I,6)*PQZ(2)
              DQ(II+6) = D(I,6)*PQY(2) + (D(I,3)-D(I,1)*NUB)*PQZ(2)
              DQ(II+7) = (D(I,1)-D(I,2)*NUB)*PQX(3) + D(I,5)*PQZ(3)
              DQ(II+8) = (D(I,2)-D(I,1)*NUB)*PQY(3) + D(I,6)*PQZ(3)
              DQ(II+9) = D(I,5)*PQX(3)+ D(I,6)*PQY(3)
     &                                + DZ(I)*PQZ(3)
              DQ(II+10) = DX(I)*PQX(4)
              DQ(II+11) = DY(I)*PQY(4)
              DQ(II+12) = DZ(I)*PQZ(4)
  4        CONTINUE

          END IF
        END 
