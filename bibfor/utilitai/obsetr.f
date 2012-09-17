      SUBROUTINE OBSETR(NOMSTR,NOMPAZ,VALR  )
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
      CHARACTER*(*) NOMPAZ
      REAL*8        VALR
C
C ----------------------------------------------------------------------
C
C ROUTINE UTILITAIRE (GESTION STRUCT)
C
C ECRITURE D'UN REEL
C
C ----------------------------------------------------------------------
C
C
C IN  NOMSTR : NOM DU STRUCT
C IN  NOMPAR : NOM DU PARAMETRE
C IN  VALR   : VALEUR DU PARAMETRE DE TYPE REEL
C
C ----------------------------------------------------------------------
C
      CHARACTER*24 SDVALR
      INTEGER      JSVALR
      CHARACTER*24 NOMPAR
      CHARACTER*1  TYPPAR
      INTEGER      INDICE
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      INDICE = 0
      NOMPAR = NOMPAZ
      TYPPAR = ' '
C
C --- VALEURS PARAMETRES REELS
C
      SDVALR = NOMSTR(1:19)//'.VALE'
C
C --- REPERAGE PARAMETRE DANS LA SD
C
      CALL OBPARA(NOMSTR,NOMPAR,INDICE,TYPPAR)
C
C --- ECRITURE
C
      IF (TYPPAR.EQ.'R') THEN
        CALL JEVEUO(SDVALR,'E',JSVALR)
        ZR(JSVALR-1+INDICE) = VALR
      ELSE
        WRITE(6,*) 'TYPE INCONNU: ',TYPPAR
        CALL ASSERT(.FALSE.)
      ENDIF
C
      CALL JEDEMA()
      END
