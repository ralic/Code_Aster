      SUBROUTINE DFLLIN(MCFACT,IECHEC,CMMAXI)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/09/2011   AUTEUR ABBAS M.ABBAS 
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
C
      IMPLICIT NONE
      CHARACTER*16 MCFACT
      INTEGER      IECHEC
      REAL*8       CMMAXI
C
C ----------------------------------------------------------------------
C
C OPERATEUR DEFI_LIST_INST
C
C LECTURE DES PARAMETRES L'ACTION DE TYPE INTERPENETRATION
C
C ----------------------------------------------------------------------
C
C
C IN  MCFACT : MOT-CLEF FACTEUR POUR LIRE L'ECHEC
C IN  IECHEC : NUMERO OCCURRENCE ECHEC
C OUT CMMAXI : VALEUR DE COEF_MULT_MAXI
C
C ----------------------------------------------------------------------
C
      INTEGER IRET
C
C ----------------------------------------------------------------------
C

C
C --- INITIALISATIONS
C
      CMMAXI = 0.D0
C
C --- OPTIONS DE L'ACTION
C
      CALL GETVR8(MCFACT,'COEF_MAXI',IECHEC,1,1,CMMAXI,IRET)
C
      END
