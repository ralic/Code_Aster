      SUBROUTINE LISDBL(LISCHA)
C
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
C
      IMPLICIT     NONE
      INCLUDE 'jeveux.h'
      CHARACTER*19 LISCHA
C
C ----------------------------------------------------------------------
C
C ROUTINE UTILITAIRE (LISTE_CHARGES)
C
C VERIFICATIONS DES DOUBLONS
C
C ----------------------------------------------------------------------
C
C
C IN  LISCHA : SD LISTE DES CHARGES
C
C
C
C
      INTEGER      ICHAR1,ICHAR2,NBCHAR
      CHARACTER*8  CHARG1,CHARG2
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- NOMBRE DE CHARGES
C
      CALL LISNNB(LISCHA,NBCHAR)
C
C --- BOUCLE SUR LES CHARGES
C
      DO 10 ICHAR1 = 1,NBCHAR
        CALL LISLCH(LISCHA,ICHAR1,CHARG1)
        DO 20 ICHAR2 = 1,NBCHAR
          IF (ICHAR1.NE.ICHAR2) THEN
            CALL LISLCH(LISCHA,ICHAR2,CHARG2)
            IF (CHARG1.EQ.CHARG2) THEN
              CALL U2MESK('F','CHARGES5_2',1,CHARG1)
            ENDIF
          ENDIF
  20    CONTINUE
  10  CONTINUE
C
      CALL JEDEMA()
      END
