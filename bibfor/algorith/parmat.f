      SUBROUTINE PARMAT(NBM,DT,AMOR,PULS,PULSD,S0,Z0,SR0,
     &                  ZA1,ZA2,ZA3)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/05/2000   AUTEUR KXBADNG T.KESTENS 
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
C-----------------------------------------------------------------------
C DESCRIPTION : CALCUL DES PARAMETRES (ALGO METHODE INTEGRALE)
C -----------
C               APPELANTS : CALCMI, CALTRA
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER     NBM
      REAL*8      DT, AMOR(*), PULS(*), PULSD(*)
      COMPLEX*16  S0(*), Z0(*), SR0(*), ZA1(*), ZA2(*), ZA3(*)
C
C VARIABLES LOCALES
C -----------------
      INTEGER     I
      REAL*8      KSI0
C
C FONCTIONS INTRINSEQUES
C ----------------------
C     INTRINSIC   DCMPLX, EXP, SQRT
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      DO 10 I = 1, NBM
C
         KSI0 = AMOR(I)/(2.0D0*PULS(I))
         IF ( KSI0.GE.1.0D0 ) THEN
            KSI0 =  0.99D0
         ELSE IF ( KSI0.LE.-1.0D0 ) THEN
            KSI0 = -0.99D0
         ENDIF
         PULSD(I) = PULS(I) * SQRT( 1.0D0 - KSI0*KSI0 )
         S0(I)    = DCMPLX( -KSI0*PULS(I), PULSD(I) )
         SR0(I)   = S0(I) * DT
         Z0(I)    = EXP( SR0(I) )
C
         ZA1(I) = (Z0(I)-1.0D0) / S0(I)
         ZA2(I) = (1.0D0/SR0(I)) - (1.0D0/(Z0(I)-1.0D0))
         ZA3(I) = 1.0D0 - ZA2(I)
C
  10  CONTINUE
C
C --- FIN DE PARMAT.
      END
