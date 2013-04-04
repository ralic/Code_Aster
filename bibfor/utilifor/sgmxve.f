      REAL*8 FUNCTION SGMXVE(NBTERM,VECT)
      IMPLICIT NONE
      INTEGER  NBTERM
      REAL*8   VECT(*)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 02/04/2013   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C
C              SIGNE DE LA VALEUR ABSOLUE MAXIMALE
C
C  IN
C     NBTERM   :  NOMBRE DE VALEURS
C     VECT     :  TABLEAU
C  OUT
C     SGMXVE   : 1.0D0 OU -1.0D0
C
C --- ------------------------------------------------------------------
C
      INTEGER     II
      REAL*8      VMAX
C --- ------------------------------------------------------------------
C
      VMAX = VECT(1)
      DO 10 II = 2 , NBTERM
         IF ( ABS(VECT(II)).GE.ABS(VMAX) ) VMAX = VECT(II)
10    CONTINUE
      IF ( VMAX.GE.0.0D0 ) THEN
         SGMXVE =  1.0D0
      ELSE
         SGMXVE = -1.0D0
      ENDIF
      END
