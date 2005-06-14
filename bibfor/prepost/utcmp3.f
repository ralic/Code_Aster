      SUBROUTINE UTCMP3 ( NBCMP, NOMCMP, NUMCMP )
      IMPLICIT   NONE
      INTEGER             NBCMP, NUMCMP(*)
      CHARACTER*(*)       NOMCMP(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 23/05/2000   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C
C ----------------------------------------------------------------------
      INTEGER      I, IRET
      CHARACTER*8  NOM
C     ------------------------------------------------------------------
C
      DO 10 I = 1 , NBCMP
         NOM = NOMCMP(I)
         NUMCMP(I) = 0
C
         CALL LXLIIS ( NOM(2:8), NUMCMP(I), IRET )
C
         IF ( IRET .NE. 0 ) THEN
           CALL UTDEBM('F','UTCMP3','ERREURS DONNEES')
           CALL UTIMPK('L','COMPOSANTE INCONNUE ',1,NOM)
           CALL UTIMPK('S',' POUR LA GRANDEUR ',1,'VARI_R')
           CALL UTFINM()
         ENDIF
 10   CONTINUE
C
      END
