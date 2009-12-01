      SUBROUTINE PRTITR(CH,TEXTE )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     CH,TEXTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 25/10/2004   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
C     ECRITURE D'UN TITRE
C          CH1 = "G"  CADRE A GAUCHE
C          CH1 = "C"  CADRE AU CENTRE
C          CH1 = "D"  CADRE A DROITE
C     ------------------------------------------------------------------
C IN  CH1   : CH*1   : SYMBOLE A METTRE EN EVIDENCE
C IN  TEXTE : CH*(*) : TEXTE
C IN  UNIT  : CH*(*) : TABLEAU DES UNITES LOGIQUES ACTIVES
C IN  NBUNIT: CH*(*) : NOMBRE D'UNITES LOGIQUES ACTIVES
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         -
C     ROUTINE(S) FORTRAN     :
C         WRITE   LEN
C     ------------------------------------------------------------------
C FIN PRTITR
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
      CHARACTER*1       CH1,CH2
C
      CH1 = CH(1:1)
      CH2 = ' '
      IF ( LEN(CH).GT.1 ) CH2 = CH(2:2)
      LONG  = MIN (LEN(TEXTE),NBCOLS)
      IF ( CH1.EQ.'C') THEN
C
         IBL   = (NBCOLS - LONG ) / 2
         DO 10 IUNIT = 1 , NBUNIT
            IFR = UNIT(IUNIT)
            WRITE(IFR,'(1X,2A)') BLANC(1:IBL), TEXTE(1:LONG)
            IF ( CH2 .EQ. 'S' )
     +                 WRITE(IFR,'(1X,2A)') BLANC(1:IBL), TIRET(1:LONG)
            WRITE(IFR,'(1X)')
   10    CONTINUE
C
      ELSEIF ( CH1.EQ.'G') THEN
C
         DO 20 IUNIT = 1 , NBUNIT
            IFR = UNIT(IUNIT)
            WRITE(IFR,'(1X,1A)') TEXTE(1:LONG)
            IF ( CH2 .EQ. 'S' )  WRITE(IFR,'(1X,1A)') TIRET(1:LONG)
            WRITE(IFR,'(1X)')
   20    CONTINUE
C
      ELSEIF ( CH1.EQ.'D') THEN
C
         DO 30 IUNIT = 1 , NBUNIT
            IFR = UNIT(IUNIT)
            IBL = NBCOLS - LONG
            WRITE(IFR,'(1X,2A)')  BLANC(1:IBL), TEXTE(1:LONG)
            IF ( CH2 .EQ. 'S' )
     +                WRITE(IFR,'(1X,2A)')  BLANC(1:IBL), TIRET(1:LONG)
            WRITE(IFR,'(1X)')
   30    CONTINUE
      ENDIF
      END
