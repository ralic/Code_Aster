      SUBROUTINE LCCAGA(LOI,CARGAU)
      IMPLICIT NONE

      CHARACTER*4     CARGAU
      CHARACTER*16    LOI

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/10/2008   AUTEUR GENIAUT S.GENIAUT 
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
C     ----------------------------------------------------------------
C     CHOIX DES CARACTERISTIQUES
C     ----------------------------------------------------------------
C
C     IN  
C         LOI : NOM DE LA LOI DE COMPORTEMENT
C     OUT 
C         CARGAUS : CHAINE DE CARACTERES PRECISANT CERTAINES 
C                     CARACTERISTIQUES DE LA RESOLUTION DES SYSTEMES
C                     LINEAIRES (1ER ARGUMENT DE MGAUSS)
C     ----------------------------------------------------------------
C
      IF (LOI.EQ.'VISCOCHAB') THEN

C       METHODE 'S' : SURE
        CARGAU = 'NCSP'

      ELSE

C       METHODE 'W' : RATEAU
        CARGAU = 'NCWP'      

      ENDIF

      END
