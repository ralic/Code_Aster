      SUBROUTINE DBGCHA(VALINC,INSTAP,ITERAT)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF DEBUG  DATE 15/10/2012   AUTEUR TARDIEU N.TARDIEU 
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
      IMPLICIT NONE
      REAL*8       INSTAP
      INTEGER      ITERAT
      CHARACTER*19 VALINC(*)
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME)
C
C    IMPRESSION D'UN CHAMP POUR DEBUG
C
C ----------------------------------------------------------------------
C
C
C IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
C IN  INSTAP : INSTANT CORRESPONDANT AU CHAMP
C IN  ITERAT : ITERATION CORRESPONDANT AU CHAMP
C
      CHARACTER*19 DEPPLU
      CHARACTER*8  INSTXT,ITETXT
      INTEGER      CODRET
      LOGICAL      DBG
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      
      DBG=.FALSE.

      IF (DBG) THEN
        CALL NMCHEX(VALINC,'VALINC','DEPPLU',DEPPLU)
        CALL CODREE(INSTAP,'G',INSTXT)
        CALL CODENT(ITERAT,'G',ITETXT)
        CALL IRCHMD(80,DEPPLU,'REEL','INST:'//INSTXT//'ITERAT:'//ITETXT,
     &                                                           CODRET)
      ENDIF
      
      CALL JEDEMA()
      END
