      SUBROUTINE RC32RS ( PMPB, SN, SNET, FATIGU )
      IMPLICIT   NONE
      LOGICAL             PMPB, SN, SNET, FATIGU
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 24/05/2004   AUTEUR CIBHHLV L.VIVAN 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C     ------------------------------------------------------------------
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
C     STOCKAGE DES RESULTATS DANS LA TABLE DE SORTIE
C
C     ------------------------------------------------------------------
      CHARACTER*8   NOMRES
      CHARACTER*16  CONCEP, NOMCMD
C DEB ------------------------------------------------------------------
C
      CALL GETRES ( NOMRES , CONCEP , NOMCMD )
C
      CALL TBCRSD ( NOMRES, 'G' )
C
C     -----------------------------------------------------------------
C
      IF ( FATIGU ) THEN
C
         CALL RC32R1 ( NOMRES )
C
C     -----------------------------------------------------------------
      ELSEIF ( PMPB .AND. SN .AND. SNET ) THEN
C
        CALL RC32R7 ( NOMRES )
C
C     -----------------------------------------------------------------
      ELSEIF ( PMPB .AND. SN ) THEN
C
        CALL RC32R2 ( NOMRES )
C
C     -----------------------------------------------------------------
      ELSEIF ( PMPB ) THEN
C
        CALL RC32R3 ( NOMRES )
C
C     -----------------------------------------------------------------
      ELSEIF ( SN .AND. SNET ) THEN
C
        CALL RC32R6 ( NOMRES )
C
C     -----------------------------------------------------------------
      ELSEIF ( SN ) THEN
C
        CALL RC32R4 ( NOMRES )
C
C     -----------------------------------------------------------------
      ELSEIF ( SNET ) THEN
C
        CALL RC32R5 ( NOMRES )
C
      ENDIF
C
      END
