      SUBROUTINE PRCCM0 ( NBOPT, OPTION, NBFT, NPARA, NOPARA, TYPARA,  
     +                  RCCMPM, RCCMSN, SNTHER, FATIZH, FATISP, TYPTAB )
      IMPLICIT   NONE
      INTEGER             NBOPT, NBFT, NPARA
      LOGICAL             RCCMPM, RCCMSN, SNTHER, FATIZH, FATISP
      CHARACTER*8         TYPARA(*)
      CHARACTER*16        OPTION(*), NOPARA(*), TYPTAB
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 17/09/97   AUTEUR CIBHHLV L.VIVAN 
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
C     POST_RCCM: LISTE DES PARAMETRES DE SORTIE DE LA TABLE
C     ------------------------------------------------------------------
      INTEGER       I, J, N1
      CHARACTER*8   K8B
C DEB ------------------------------------------------------------------
C
      FATIZH = .FALSE.
      FATISP = .FALSE.
      RCCMSN = .FALSE.
      RCCMPM = .FALSE.
      SNTHER = .FALSE.
C
      DO 10 I = 1 , NBOPT
         IF ( OPTION(I) .EQ. 'PM_PB' ) THEN
            RCCMPM = .TRUE.
         ELSEIF ( OPTION(I) .EQ. 'SN' ) THEN
            RCCMSN = .TRUE.
            DO 12 J = 1 , NBFT
               CALL GETVID ( 'TRANSITOIRE', 'RESU_SIGM_THER', J,1,0,
     +                                                        K8B, N1 )
               IF ( N1 .NE. 0 ) SNTHER = .TRUE.
 12         CONTINUE
         ELSEIF ( OPTION(I) .EQ. 'FATIGUE_ZH210' ) THEN
            FATIZH = .TRUE.
         ELSEIF ( OPTION(I) .EQ. 'FATIGUE_SPMAX' ) THEN
            FATISP = .TRUE.
         ENDIF
 10   CONTINUE
C
      CALL PRCCM4 ( RCCMPM, RCCMSN, SNTHER, FATIZH, FATISP, 
     +                      TYPTAB, NPARA, NOPARA, TYPARA )
C
      END
