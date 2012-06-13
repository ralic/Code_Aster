      SUBROUTINE NMCSOL(NBEXCI,LVISS)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C
C       NMCSOL -- DETERMINE LA PRESENCE DE CHARGES FORCE_SOL
C
C -------------------------------------------------------
C    NBEXCI       - IN     NB CHARGES EXCIT
C    LVISS        - OUT    - LOGIQUE
C                - JXVAR -      -   LA  CHARGE EST ENRICHIE
C                                   DU VECTEUR ASSEMBLE DONT LE NOM
C                                   EST STOCKE DANS L'OBJET
C                                   CHAR//'CHME.VEASS'
C -------------------------------------------------------
C
C.========================= DEBUT DES DECLARATIONS ====================
C
C -----  ARGUMENTS
      INCLUDE 'jeveux.h'
      INTEGER       NBEXCI
      LOGICAL       LVISS
C ------ VARIABLES LOCALES
      CHARACTER*8   CHARGE
      INTEGER       IARG,IOCC,IRET,NC
C
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ()
C
      LVISS=.FALSE.
      DO 1 IOCC=1,NBEXCI
        CALL GETVID('EXCIT','CHARGE',IOCC,IARG,1,CHARGE,NC)
        CALL JEEXIN(CHARGE//'.CHME.VEISS',IRET)
        IF (IRET.NE.0) THEN
          LVISS=.TRUE.
          GOTO 2
        ENDIF          
 1    CONTINUE
 2    CONTINUE
C
      CALL JEDEMA()
C.============================ FIN DE LA ROUTINE ======================
      END
