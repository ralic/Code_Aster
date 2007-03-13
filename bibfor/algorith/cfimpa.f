      SUBROUTINE CFIMPA(SUBAPP,IMESG )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/03/2007   AUTEUR ABBAS M.ABBAS 
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
      IMPLICIT      NONE
      CHARACTER*6   SUBAPP
      INTEGER       IMESG  
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES DISCRETES)
C
C CENTRALISATION DES ROUTINES D'AFFICHAGE EN MODE DEBUG
C AFFICHAGE POUR ASSERT
C
C ----------------------------------------------------------------------
C
C
C IN  IFM    : UNITE D'IMPRESSION (EN GENERAL 6: FICHIER MESSAGE)
C IN  SUBAPP : SOUS-ROUTINE D'APPEL
C IN  IMESG  : NUMERO DE MESSAGE DANS LA SOUS-ROUTINE
C
C ----------------------------------------------------------------------
C
      CHARACTER*8 VALK
      INTEGER     VALI
      INTEGER     IFM,NIV
      REAL*8      VALR
C
C ----------------------------------------------------------------------
C 
      CALL INFNIV(IFM,NIV)
C
      CALL U2MESG('F','CONTACTDEBG_98',1,VALK,
     &                                 1,VALI,
     &                                 0,VALR)
C
      END
