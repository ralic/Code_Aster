      SUBROUTINE OBGETT(NOMSTR,TYPESZ)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 18/09/2012   AUTEUR ABBAS M.ABBAS 
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
      IMPLICIT     NONE
      INCLUDE      'jeveux.h'
      CHARACTER*24  NOMSTR
      CHARACTER*(*) TYPESZ
C
C ----------------------------------------------------------------------
C
C ROUTINE UTILITAIRE (GESTION STRUCT)
C
C LECTURE DU TYPE DE LA SD
C
C ----------------------------------------------------------------------
C
C
C IN  NOMSTR : NOM DU STRUCT
C OUT TYPESD : TYPE DE LA SD
C
C ----------------------------------------------------------------------
C
      CHARACTER*24 SDDESK
      INTEGER      JSDESK
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      SDDESK = NOMSTR(1:19)//'.DESK'
      CALL JEVEUO(SDDESK,'L',JSDESK)
      TYPESZ = ZK24(JSDESK-1+1)
C
      CALL JEDEMA()
      END
