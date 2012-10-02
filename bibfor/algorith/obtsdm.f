      SUBROUTINE OBTSDM(LISNOM,TYPCOZ,MARQ  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/10/2012   AUTEUR DESOZA T.DESOZA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INCLUDE       'jeveux.h'
      CHARACTER*24  LISNOM
      CHARACTER*(*) TYPCOZ
      CHARACTER*1   MARQ
C
C ----------------------------------------------------------------------
C
C ROUTINE UTILITAIRE (GESTION STRUCTS - TABLEAU POUR IMPRESSION)
C
C AFFECTATION D'UNE MARQUE DANS UNE COLONNE
C
C ----------------------------------------------------------------------
C
C
C IN  LISNOM : REPERTOIRE DES NOMS DE LA STRUCT TABLEAU POUR IMPRESSION
C IN  TYPCOL : CODE TYPE DE LA COLONNE
C IN  MARQ   : MARQUAGE DE LA COLONNE
C
C ----------------------------------------------------------------------
C
      INTEGER      JLISNO
      CHARACTER*24 SDCOLO,TYPCOL
C
C ----------------------------------------------------------------------
C
      TYPCOL = TYPCOZ
      CALL JEVEUO(JEXNOM(LISNOM,TYPCOL),'L',JLISNO)
      SDCOLO = ZK24(JLISNO)
C
C --- AFFECTATION MARQUE
C
      CALL OBSETK(SDCOLO,'MARQUE',MARQ)
C
      END
