       SUBROUTINE INFMUE ( )
       IMPLICIT REAL*8 (A-H,O-Z)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 29/02/96   AUTEUR CIBHHRA R.MEDDOURI 
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
C      MOT CLE INFO
C-----------------------------------------------------------------------
C      UTILITAIRE D'INHIBITION D'UNE IMPRESSION
C-----------------------------------------------------------------------
C      PAS D'ARGUMENTS 
C-----------------------------------------------------------------------
C------DEBUT DU COMMON INF001-------------------------------------------
C      NIVUTI    :NIVEAU DEMANDE PAR L'UTILISATEUR  : 1 OU 2
C      NIVPGM    :NIVEAU ACCESSIBLE AU PROGRAMMEUR  : 0 , 1 OU 2 
C      UNITE     :UNITE LOGIQUE DU FICHIER MESSAGE
C  
       INTEGER      NIVUTI,NIVPGM,UNITE
       COMMON/INF001/NIVUTI,NIVPGM,UNITE
C-----FIN DE INF001-----------------------------------------------------
C
        IF (NIVUTI.EQ.1)   NIVPGM = 0
       END
