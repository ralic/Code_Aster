      SUBROUTINE FOECFF(IUL, ORG,PAS,NPS,VAR,FON,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           IUL,           NPS
      CHARACTER*(*)            ORG
      REAL*8                       PAS,    VAR(*),FON(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 05/01/95   AUTEUR J2BHHMB C.MASSERET 
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
C     ECRITURE DES VALEURS DU SIGNAL SELON SON ORGANISATION
C     ------------------------------------------------------------------
C IN  IUL    : IS : UNITE LOGIQUE DE LECTURE
C IN  ORG    : K4 : ORGANISATION DES VALEURS
C       'VFVF'
C       'VVFF'
C IN  NPS    : IS : NOMBRE DE POINTS DECRIVANT LE SIGNAL
C IN  PAS    : R8 : PAS DECRIVANT LE SIGNAL
C OUT VAR    : R8 : TABLEAU DES ABSCISSES
C OUT FON    : R8 : TABLEAU DES ORDONNEES
C     ------------------------------------------------------------------
      CHARACTER*4  CH4
C
      NLIG = NPS / 9
      IF ( MOD(NPS,9) .NE. 0) NLIG=NLIG+1
C
      IF (ORG .EQ. 'VFVF') THEN
         DO 10 IL = 1,NLIG
            IL1 =  1+9*(IL-1)
            IL2 =  MIN(9*IL,NPS)
            WRITE(IUL,'(9(0PF8.4))') (VAR(K),K=IL1,IL2)
            WRITE(IUL,'(9(0PF8.4))') (FON(K),K=IL1,IL2)
  10     CONTINUE
C
      ELSEIF (ORG .EQ. 'VVFF') THEN
C
         DO 20 IL = 1,NLIG
            IL1 =  1+9*(IL-1)
            IL2 =  MIN(9*IL,NPS)
            WRITE(IUL,'(9(0PF8.4))') (VAR(K),K=IL1,IL2)
  20     CONTINUE
         DO 21 IL = 1,NLIG
            IL1 =  1+9*(IL-1)
            IL2 =  MIN(9*IL,NPS)
            WRITE(IUL,'(9(0PF8.4))') (FON(K),K=IL1,IL2)
  21     CONTINUE
C
      ELSEIF (ORG .EQ. 'FFFF') THEN
C
        IF ( PAS .EQ. 0.D0 ) THEN
           CALL UTMESS('F',' ','VOUS N''AVEZ PAS DEFINI LE "PAS".')
        ENDIF
        VAR(1 ) = 0.D0
        DO 30 IL = 2,NPS
          VAR(IL) = VAR(IL-1)+PAS
  30    CONTINUE
        DO 31 IL = 1,NLIG
           IL1 =  1+9*(IL-1)
           IL2 =  MIN(9*IL,NPS)
            WRITE(IUL,'(9(0PF8.4))') (FON(K),K=IL1,IL2)
  31    CONTINUE
C
      ELSEIF (ORG .EQ. 'V1F1') THEN
C
         DO 41 IL = 1,NPS
            WRITE(IUL,'(2(0PF8.4))') VAR(IL),FON(IL)
  41     CONTINUE
C
      ELSE
C
         CH4 = ORG
         CALL UTMESS('F','ORG',' ORGANISATION = "'//CH4//'"  INCONNUE')
C
      ENDIF
      END
