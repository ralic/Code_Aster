      FUNCTION PRGONF(BIOT,BETAM,PREF,P1)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/07/2008   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C ROUTINE PRGONF
C CALCUL DE LA PRESSION DE GONFLEMENT POUR L ARGILE GONFLANTE 
C ======================================================================
C 
      IMPLICIT NONE
      REAL*8 BIOT,BETAM,PREF,P1,PRGONF,DERF
      REAL*8 PI,RPI,S,RBETAM,ERFCFO,R8PI
      
      PI = R8PI()
      RPI=SQRT(PI)
      RBETAM=SQRT(BETAM)
      BETAM=RBETAM*RBETAM
      S=P1/PREF
      DERF = (1.D0-ERFCFO(S*RBETAM))
      IF ( S.GT.0.D0) THEN
       PRGONF=PREF*BIOT*((RPI/(2.D0*RBETAM))*DERF
     >     +(0.5D0/BETAM)*(1.D0-EXP(-BETAM*S*S)))
      ELSE
       PRGONF=PREF*BIOT*S
      ENDIF
      END
