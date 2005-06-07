      SUBROUTINE LGLORD (SIG1, SIG2, SIG3)
C
      IMPLICIT    NONE
      REAL*8      SIG1, SIG2, SIG3
C =================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/06/2003   AUTEUR CIBHHBC R.FERNANDES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C =================================================================
C --- BUT : ORDONNER LES CONTRAINTES PRINCIPALES ------------------
C ------- : TEL QUE |SIG1| > |SIG2| > |SIG3| ----------------------
C =================================================================
C IN/OUT : SIG1 :  CONTRAINTE MAXIMALE ----------------------------
C ------ : SIG2 :  CONTRAINTE INTERMEDIAIRE -----------------------
C ------ : SIG3 :  CONTRAINTE MINIMALE ----------------------------
C =================================================================
      REAL*8        TMP
C =================================================================
      CALL JEMARQ ()
C =================================================================
      IF (ABS(SIG3).GT.ABS(SIG1)) THEN
         TMP  = SIG1
         SIG1 = SIG3
         SIG3 = TMP
      ENDIF
      IF (ABS(SIG2).GT.ABS(SIG1)) THEN
         TMP  = SIG1
         SIG1 = SIG2
         SIG2 = TMP
      ENDIF
      IF (ABS(SIG3).GT.ABS(SIG2)) THEN
         TMP  = SIG2
         SIG2 = SIG3
         SIG3 = TMP
      ENDIF
C =================================================================
      CALL JEDEMA ()
C =================================================================
      END
