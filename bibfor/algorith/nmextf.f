      SUBROUTINE NMEXTF(MOTFAC,IOCC  ,EXTRCP)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/01/2011   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT      NONE
      CHARACTER*16  MOTFAC
      INTEGER       IOCC 
      CHARACTER*8   EXTRCP
C
C ----------------------------------------------------------------------
C
C ROUTINE *_NON_LINE (EXTRACTION - LECTURE)
C
C LECTURE TYPE EXTRACTION SUR LES COMPOSANTES
C
C ----------------------------------------------------------------------
C
C
C IN  MOTFAC : MOT-FACTEUR POUR LIRE 
C IN  IOCC   : OCCURRENCE DU MOT-CLEF FACTEUR MOTFAC
C OUT EXTRCP : TYPE D'EXTRACTION SUR LES COMPOSANTES
C               ' ' POUR LES VALEURS OU NOM DE LA FORMULE
C
C ----------------------------------------------------------------------
C
      CHARACTER*8  TYPEXT
      INTEGER      N1
C
C ----------------------------------------------------------------------
C
      CALL GETVTX(MOTFAC,'EVAL_CMP',IOCC,1,1,TYPEXT,N1    )
      IF (TYPEXT.EQ.'VALE') THEN
        EXTRCP = ' '
      ELSEIF (TYPEXT.EQ.'FORMULE') THEN
        CALL GETVID(MOTFAC,'FORMULE',IOCC,1,1,EXTRCP,N1    )
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      END
