      SUBROUTINE JXLIBM ( ISZON , LISZON )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 10/03/98   AUTEUR VABHHTS J.PELLET 
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
C TOLE  CFT_889  CRS_505
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             ISZON(*) , LISZON
C     ==================================================================
      INTEGER          IADA
      COMMON /IALLJE/  IADA
C     ------------------------------------------------------------------
      CHARACTER *75    CMESS
      CHARACTER *6     PGME
      PARAMETER      ( PGME   = 'JXLIBM' )
C     ------------------------------------------------------------------
C             ROUTINE AVEC ADHERENCE SYSTEME    CRAY
C
C
C     ==================================================================
      IERCOD = 0
      CALL HPCHECK ( IERCOD )
      IF ( IERCOD .NE. 0 ) THEN
         IF ( IERCOD .EQ. -5 ) THEN
            CMESS = 'MOT DE CONTROLE INCORRECT POUR UNE ZONE ALLOUEE'
         ELSE IF ( IERCOD .EQ. -6 ) THEN
            CMESS = 'MOT DE CONTROLE INCORRECT POUR UNE ZONE LIBEREE'
         ELSE
            CMESS = 'ERREUR VERIFICATION DE ZONE'
         ENDIF
         CALL JVMESS ( 'S' , PGME//'01' , CMESS )
      ELSE
         IZERO = 0
         IERR = 0
         CALL  HPDEALLC ( IADA , IERR , IZERO )
         IF ( IERR .NE. 0 ) THEN
            IF      ( IERR .EQ. -3 ) THEN
              CMESS = 'ADRESSE INCORRECTE'
            ELSE IF ( IERR .EQ. -4 ) THEN
              CMESS = 'ZONE DEJA LIBEREE'
            ELSE IF ( IERR .EQ. -5 ) THEN
              CMESS = 'ADRESSE DIFFERENTE D''UN DEBUT DE ZONE'
            ELSE IF ( IERR .EQ. -7 ) THEN
              CMESS = 'ECRASEMENT DU DEBUT DE LA ZONE SUIVANTE'
            ENDIF
            CALL JVMESS ( 'S' , PGME//'02' , CMESS )
         ENDIF
      ENDIF
C     ==================================================================
      END
