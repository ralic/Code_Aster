      SUBROUTINE PRSOUL(CH1,IDEB,NPOS)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*1       CH1
      INTEGER               IDEB,NPOS
C
C     ------------------------------------------------------------------
C     SOULIGNER UNE ZONE PARTANT DE IDEB, SUR NPOS POSITION
C     ------------------------------------------------------------------
C IN  CH1   : CH*1   : SYMBOLE A METTRE EN EVIDENCE
C IN  IDEB  : IS     : DEBUT DU SOULIGNAGE
C IN  NPOS  : IS     : NOMBRE DE "TRAIT"
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 06/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         -
C     ROUTINE(S) FORTRAN     :
C         WRITE   LEN    MAX
C     ------------------------------------------------------------------
C FIN PRSOUL
C     ------------------------------------------------------------------
C
C     --- COMMUN D'IMPRESSIONS SUPERVISEUR -----------------------------
      PARAMETER          ( MXUNIT = 10)
      INTEGER         UNIT(MXUNIT), NBUNIT
      COMMON /PRCN00/ UNIT   , NBUNIT , NBCOLS
C     ------------------------------------------------------------------
C
C     --- AFFICHAGE SUR "MXCOLS" COLONNES ------------------------------
      PARAMETER   (MXCOLS = 132 )
      CHARACTER*(MXCOLS)  BLANC, TIRET
      CHARACTER*6                       DEBUT
      CHARACTER*1                              FIN
      COMMON /PRCC00/     BLANC, TIRET, DEBUT, FIN
C     ------------------------------------------------------------------
C
      IDEB1 = MAX(1,IDEB)-1
      IBL   = NBCOLS - IDEB1 - NPOS - LEN(DEBUT)
      DO 10 IUNIT = 1 , NBUNIT
         IFR = UNIT(IUNIT)
         IF ( IDEB1 .GT. 0 ) THEN
            WRITE(IFR,'(1X,130A)') DEBUT,BLANC(1:IDEB1),(CH1,I=1,NPOS),
     +                             BLANC(1:IBL) ,FIN
         ELSE
            WRITE(IFR,'(1X,130A)') DEBUT,(CH1,I=1,NPOS),
     +                                                BLANC(1:IBL) ,FIN
         ENDIF
  10  CONTINUE
C
      END
