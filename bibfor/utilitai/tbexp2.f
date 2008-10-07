      SUBROUTINE TBEXP2 ( NOMTA,PARA)
      IMPLICIT   NONE
      CHARACTER*(*)       NOMTA, PARA
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 07/10/2008   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C      EXISTENCE D'UN PARAMETRE DANS UNE TABLE.
C ----------------------------------------------------------------------
C IN  : NOMTA  : NOM DE LA STRUCTURE "TABLE".
C IN  : PARA   : PARAMETRE A CHERCHER
C ----------------------------------------------------------------------
      CHARACTER*4 TYPPAR
      CHARACTER*24 VALK(2)
      LOGICAL  EXIST
C DEB------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
      CALL TBEXIP(NOMTA,PARA,EXIST,TYPPAR)
C
      IF(.NOT.EXIST)THEN
        VALK (1) = PARA
        VALK (2) = NOMTA
        CALL U2MESG('F', 'UTILITAI6_93',2,VALK,0,0,0,0.D0)
      ENDIF
C  
      CALL JEDEMA()
      END
