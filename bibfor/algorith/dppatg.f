      FUNCTION DPPATG (MATER, PMOINS)
C
      IMPLICIT      NONE
      REAL*8        MATER(4,2), PMOINS, DPPATG
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/02/2004   AUTEUR ROMEO R.FERNANDES 
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
C ======================================================================
C --- BUT : INITIALISATION POUR L OPERATEUR TANGENT POUR LA LOI --------
C --- DRUCKER-PRAGER PARABOLIQUE SOUS RIGI_MECA_TANG -------------------
C ======================================================================
      REAL*8   UN, DEUX, TROIS, YOUNG, NU, TROISK, DEUXMU, PHI, C, PULT
      REAL*8   ALPHA2, SIX, ALPHA
C ======================================================================
      PARAMETER  ( UN    = 1.0D0 )
      PARAMETER  ( DEUX  = 2.0D0 )
      PARAMETER  ( TROIS = 3.0D0 )
      PARAMETER  ( SIX   = 6.0D0 )
C ======================================================================
      YOUNG  = MATER(1,1)
      NU     = MATER(2,1)
      TROISK = YOUNG / (UN-DEUX*NU)
      DEUXMU = YOUNG / (UN+NU)
      ALPHA2 = MATER(1,2)
      PHI    = MATER(2,2)
      C      = MATER(3,2)
      PULT   = MATER(4,2)
      ALPHA  = DEUX*SIN(PHI)/(TROIS-SIN(PHI))
      IF ( PMOINS.LT.PULT ) THEN
         DPPATG = - ( TROIS*DEUXMU/DEUX + TROIS*TROISK*ALPHA*ALPHA -
     +      DEUX*SIX*C*COS(PHI)/(TROIS-SIN(PHI))*
     +      (UN-(UN-ALPHA2)*PMOINS/PULT)*(UN-ALPHA2)/PULT )
      ELSE
         DPPATG = - ( TROIS*DEUXMU/DEUX + TROIS*TROISK*ALPHA*ALPHA )
      ENDIF
C ======================================================================
      END
