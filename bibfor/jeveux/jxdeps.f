      SUBROUTINE JXDEPS ( IADINI , IADFIN , LSO )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
C MODIF JEVEUX  DATE 27/06/2011   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C     DEPLACEMENT D'UNE ZONE MEMOIRE 
C     ------------------------------------------------------------------
C IN  IADINI : ADRESSE INITIALE
C IN  IADFIN : ADRESSE CIBLE
C IN  LSO : LONGUEUR DE LA ZONE A DEPLACER
C     ------------------------------------------------------------------
C TOLE CRP_18 CRS_508
      IMPLICIT REAL*8 (A-H,O-Z)
C             ROUTINE AVEC ADHERENCE SYSTEME    CRAY
C             FONCTION(S) UTILISEE(S) : IAND
C
      INTEGER             IADINI , IADFIN , LSO
C     ------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C     ------------------------------------------------------------------
      INTEGER          MSLOIS
      COMMON /JENVJE/  MSLOIS
      INTEGER          LBIS , LOIS , LOLS , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOR8 , LOC8
C DEB ------------------------------------------------------------------
      IF ( IAND ( JK1ZON + IADINI - 1 , MSLOIS ) .EQ. 0 .AND.
     +     IAND ( JK1ZON + IADFIN - 1 , MSLOIS ) .EQ. 0 .AND.
     +     IAND ( LSO , MSLOIS ) .EQ. 0          ) THEN
         JINI = ( JK1ZON + IADINI - 1 ) / LOIS + 1
         JFIN = ( JK1ZON + IADFIN - 1 ) / LOIS + 1
         IF ( JINI .GT. JFIN ) THEN
CCDIR$ IVDEP
            DO 20 I = 0 , (LSO / LOIS) - 1
              ISZON( JFIN + I ) = ISZON( JINI + I )
   20       CONTINUE
         ELSE IF ( JINI .LT. JFIN ) THEN
CCDIR$ IVDEP
            DO 21 I = (LSO / LOIS) - 1 , 0 , -1
              ISZON( JFIN + I ) = ISZON( JINI + I )
   21       CONTINUE
         ENDIF
      ELSE
         IF ( IADINI .GT. IADFIN ) THEN
            DO 30 I = 0 , LSO - 1
               K1ZON(JK1ZON + IADFIN + I) = K1ZON(JK1ZON + IADINI + I)
   30       CONTINUE
         ELSE IF ( IADINI .LT. IADFIN ) THEN
            DO 31 I = LSO-1 , 0 , - 1
               K1ZON(JK1ZON + IADFIN + I) = K1ZON(JK1ZON + IADINI + I)
   31       CONTINUE
         ENDIF
      ENDIF
C FIN ------------------------------------------------------------------
      END
