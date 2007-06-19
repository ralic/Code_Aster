      SUBROUTINE DXSIR2 ( NPG, NBCOU, NPGH, T2EV , CDLE , CDLC )
      IMPLICIT  NONE
      INTEGER             NPG, NBCOU, NPGH
      REAL*8              T2EV(2,2), CDLE(*), CDLC(*)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/12/2003   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C     PASSAGE DES CONTRAINTES OU DEFORMATIONS DU REPERE INTRINSEQUE DE
C     L'ELEMENT AU REPERE LOCAL DE LA COQUE
C     ------------------------------------------------------------------
C     IN  NE    I      NOMBRE DE POINTS A TRAITER
C     IN  T2EV  R  2,2  MATRICE DE PASSAGE ELEMENT - VARIETE
C     IN  CDLE R    *   SIXX SIYY SIXY SIXZ SIYZ
C     OUT CDLC R    *   SIXX SIYY SIXY SIXZ SIYZ
C     ------------------------------------------------------------------
      INTEGER   IP, IC, IG, ICPG
      REAL*8    SIMELE(4) , SIMELC(4)
      REAL*8    XAB(2,2)
C
      DO 10 IP = 1 , NPG

         DO 20 IC = 1 , NBCOU

            DO 30 IG = 1 , NPGH

               ICPG = 6*NPGH*NBCOU*(IP-1) + 6*NPGH*(IC-1) + 6*(IG-1)

               SIMELE(1) = CDLE(1+ICPG)
               SIMELE(2) = CDLE(4+ICPG)
               SIMELE(3) = CDLE(4+ICPG)
               SIMELE(4) = CDLE(2+ICPG)
C
               CALL UTBTAB ('ZERO',2,2,SIMELE,T2EV,XAB,SIMELC)
C
               CDLC(1+ICPG) = SIMELC(1)
               CDLC(2+ICPG) = SIMELC(4)
               CDLC(3+ICPG) = 0.D0
               CDLC(4+ICPG) = SIMELC(2)
               CDLC(5+ICPG) = CDLE(5+ICPG) * T2EV(1,1) +
     &                        CDLE(6+ICPG) * T2EV(2,1)
               CDLC(6+ICPG) = CDLE(5+ICPG) * T2EV(1,2) +
     &                        CDLE(6+ICPG) * T2EV(2,2)
C
  30        CONTINUE
C
  20     CONTINUE
C
  10  CONTINUE
C
      END
