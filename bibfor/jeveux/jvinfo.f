      INTEGER FUNCTION JVINFO ( KACTIO, INFO )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 23/06/2003   AUTEUR D6BHHJP J.P.LEFEBVRE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)             KACTIO
      INTEGER                           INFO
C ----------------------------------------------------------------------
C DEFINITION DU NIVEAU DES IMPRESSIONS JEVEUX
C
C IN  KACTIO  = AFFECT AFFECTATION DU NIVEAU DES IMPRESSIONS
C             = INIT   MISE A 0
C             = RECUP  RECUPERATION DU NIVEAU DES IMPRESSIONS
C IN  INFO   VALEUR DU NIVEAU DES IMPRESSIONS (AFFECT UNIQUEMENT)
C ----------------------------------------------------------------------
      INTEGER          IFNIVO, NIVO
      COMMON /JVNIVO/  IFNIVO, NIVO
C DEB ------------------------------------------------------------------
      CHARACTER*8 K8CH
      K8CH=KACTIO
      IF ( IFNIVO .NE. 22021986) THEN
        NIVO = 0
        IFNIVO = 22021986
      ENDIF
      IF ( K8CH(1:6) .EQ. 'AFFECT' ) THEN
        NIVO = INFO
      ELSE IF ( K8CH(1:4) .EQ. 'INIT' ) THEN
        NIVO = 0
      ELSE IF ( K8CH(1:5) .EQ. 'RECUP' ) THEN
      ENDIF
      JVINFO = NIVO
C FIN ------------------------------------------------------------------
      END
