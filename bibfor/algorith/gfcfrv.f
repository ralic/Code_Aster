      SUBROUTINE GFCFRV ( XRE, HRUG, CF )
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/10/2003   AUTEUR BOYERE E.BOYERE 
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
C
C     CALCUL DU COEFFICIENT DE FROTTEMENT CF EN FONCTION DE LA VITESSE
C-----------------------------------------------------------------------
      REAL*8 CF, XRE, REC, HRUG
C     ------------------------------------------------------------------
C
      REC = ( 17.85D0 / HRUG )**1.143D0
C
      IF ( XRE .LE. 0.0D0 ) THEN
         CF = 0.0D0
C
      ELSEIF ( XRE .LE. 2000.0D0) THEN
C ------ REGIME LAMINAIRE
         CF = 16.0D0 / XRE
C 
      ELSEIF ((XRE.GT.2000.0D0).AND.(XRE.LE.4000.0D0)) THEN
C ------ REGIME DE TRANSITION LAMINAIRE TURBULENT
         CF = 7.45D-4 * XRE**(0.312D0)
C
      ELSEIF ((XRE.GT.4000.0D0).AND.(XRE.LE.REC)) THEN
         IF ( XRE .LE. 1.0D5 ) THEN
C --------- REGIME TURBULENT HYDRAULIQUEMENT LISSE RE<1E5
            CF = 0.079D0 * XRE**(-0.25D0)
C
         ELSE
C --------- REGIME TURBULENT HYDRAULIQUEMENT LISSE RE>1E5
           CF = 0.96381D0 * 0.25D0 / (1.8D0*LOG10(XRE)-1.64D0)**2
C
         ENDIF
C
      ELSE
         IF ( REC .LE. 1.0D5 ) THEN
C --------- REGIME TURBULENT PLEINEMENT RUGUEUX RE<1E5
           CF = 0.079D0 * REC**(-0.25D0)
C
         ELSE
C --------- REGIME TURBULENT PLEINEMENT RUGUEUX RE>1E5
           CF = 0.96381D0 * 0.25D0 / (1.8D0*LOG10(REC)-1.64D0)**2
C
         ENDIF
       ENDIF 
C
      END
