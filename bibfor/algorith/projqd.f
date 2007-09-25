      SUBROUTINE PROJQD(ARETT1,ARETT2,ARETT3,ARETT4,DIAG)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/09/2007   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      INTEGER      DIAG(2)      
      INTEGER      ARETT1(3),ARETT2(3),ARETT3(3),ARETT4(3)      
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT - MAIT/ESCL - QUA)
C
C VERIFIE SI ON SE PROJETTE SUR UNE DIAGONALE DU QUADRANGLE QUAND ON
C LE DECOUPE EN TRIANLES
C
C ----------------------------------------------------------------------
C
C
C IN  ARETT1 : DETECTION DE PROJECTION SUR ARETE (TRIANGLE 1)
C                (1: SUR L'ARETE, 0: NON)
C IN  ARETT2 : DETECTION DE PROJECTION SUR ARETE (TRIANGLE 2)
C                (1: SUR L'ARETE, 0: NON)
C IN  ARETT3 : DETECTION DE PROJECTION SUR ARETE (TRIANGLE 3)
C                (1: SUR L'ARETE, 0: NON)
C IN  ARETT4 : DETECTION DE PROJECTION SUR ARETE (TRIANGLE 4)
C                (1: SUR L'ARETE, 0: NON)
C OUT DIAG   : DETECTION SUR PSEUDO-DIAGONALE
C                (1: SUR DIAGONALE, 0: NON)
C               DIAG(1)    : SEGMENT AC
C               DIAG(2)    : SEGMENT BD
C                       
C
C ----------------------------------------------------------------------
C
C
C --- INITIALISATIONS
C
      DIAG(1)   = 0
      DIAG(2)   = 0      
C
C --- PROJECTION SUR LES PSEUDOS-DIAGONALES ?
C
      IF (ARETT1(3).EQ.1) THEN
        DIAG(1) = 1
      ENDIF
      IF (ARETT2(1).EQ.1) THEN
        DIAG(1) = 1
      ENDIF
      IF (ARETT3(2).EQ.1) THEN
        DIAG(2) = 1
      ENDIF
      IF (ARETT4(3).EQ.1) THEN
        DIAG(2) = 1
      ENDIF   
C
      END
