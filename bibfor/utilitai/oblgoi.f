      SUBROUTINE OBLGOI(SDLIST,ISTRU ,NOMSTZ)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 02/10/2012   AUTEUR DESOZA T.DESOZA 
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
      CHARACTER*24  SDLIST
      INTEGER       ISTRU
      CHARACTER*(*) NOMSTZ
C
C ----------------------------------------------------------------------
C
C ROUTINE UTILITAIRE (GESTION STRUCT - LISTE DE STRUCTS)
C
C RECUPERATION D'UN STRUCT DANS UNE LISTE DE STRUCTS
C ACCES PAR INDICE
C
C ----------------------------------------------------------------------
C
C
C IN  SDLIST : NOM DE LA LISTE
C IN  ISTRU  : INDICE DANS LA LISTE DES STRUCTS
C OUT NOMSTZ : NOM DU STRUCT IDENTIFIE DANS LA LISTE
C
C ----------------------------------------------------------------------
C
      CHARACTER*24 LISNOM
      INTEGER      JLISNO
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- REPERTOIRE DES NOMS
C
      CALL OBGETO(SDLIST,'NOM_STRUCTS',LISNOM)
C
C --- LECTURE
C
      CALL JEVEUO(LISNOM,'L',JLISNO)
      NOMSTZ = ZK24(JLISNO-1+ISTRU)
C
      CALL JEDEMA()
      END
