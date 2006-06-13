      SUBROUTINE JVDEBM ( CH1 , SPGLU , TEXTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 05/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER* 1        CH1
      CHARACTER*(*)             SPGLU , TEXTE
C     ==================================================================
      INTEGER          MXUNIT,      MXNUML
      PARAMETER      ( MXUNIT = 7 , MXNUML = 4 )
      INTEGER          UNIT(MXUNIT,MXNUML), NBUNIT(MXUNIT)
      COMMON /JVINIF/  UNIT               , NBUNIT
      INTEGER          MXCOLS , ITABU , LIGCOU , COLCOU , IDF
      COMMON /JVINIP/  MXCOLS , ITABU , LIGCOU , COLCOU , IDF
      PARAMETER      ( NT = 10 )
      CHARACTER*132    TAMPON
      COMMON /JVTAMP/  TAMPON(NT)
      INTEGER          LDEB
      COMMON /JVDEB /  LDEB
C     ------------------------------------------------------------------
      CHARACTER *80    SPG,SPGSAU
      INTEGER          CDEB , CFIN , ICOMAL
      DATA SPGSAU,ICOMAL/' ',0/
C     ------------------------------------------------------------------
      IDF  = INDEX('EFIDASX',CH1)
      IF ( IDF .LE. 0 ) THEN
        TAMPON(9)  = '<S> ERREUR DE PROGRAMMATION'
        TAMPON(10) = '    APPEL A JVDEBM ERRONE  '
        IFIC = UNIT(2,1)
        IF ( IFIC .GT. 0 ) WRITE(IFIC,'(1X,A)') (TAMPON(K),K=1,10)
        CALL JXABOR()
      ELSE IF ( IDF .EQ. 5 ) THEN
        LL=MIN(LEN(SPGLU),80)
        IF (SPGSAU(1:LL).EQ.SPGLU(1:LL) ) THEN
          ICOMAL=ICOMAL+1
          IF(ICOMAL.EQ.6) THEN
            LIGCOU = 1
            COLCOU = 80
            TAMPON(1) (1 : COLCOU) =
     &         '<A> < PLUS DE 5 FOIS LE MEME MESSAGE D''ALARME>'
            CALL JVVTAM
            IDF=7
            GOTO 200
          ELSE IF(ICOMAL.GT.6) THEN
            IDF=7
            GOTO 200
          ENDIF
        ELSE
          ICOMAL=0
          SPGSAU(1:LL) = SPGLU(1:LL)
        END IF
      ENDIF
C
      DO 1 K = 1 , NT
         TAMPON(K) = ' '
 1    CONTINUE
C
      LIGCOU = 1
      LL  = MIN(MXCOLS-10,LEN(SPGLU))
      SPG = SPGLU(1:LL)
      COLCOU = LDEB + LL + 3
      TAMPON(1) (1 : COLCOU) = '<'//CH1//'> <'//SPG(1:LL)//'>  '
      CALL JVRTAM ( TEXTE )
C
 200  CONTINUE
      END
