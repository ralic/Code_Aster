      SUBROUTINE UTDEBM ( CH1 , SPGLU , TEXTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 11/05/2005   AUTEUR MCOURTOI M.COURTOIS 
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
      IMPLICIT NONE
      CHARACTER* (*)        CH1
      CHARACTER*(*)             SPGLU , TEXTE
C     ==================================================================
      INTEGER          MXUNIT,      MXNUML
      PARAMETER      ( MXUNIT = 8 , MXNUML = 4 )
      INTEGER          UNIT(MXUNIT,MXNUML), NBUNIT(MXUNIT)
      COMMON /UTINIF/  UNIT               , NBUNIT
      INTEGER          MXCOLS , ITABU , LIGCOU , COLCOU , IDF
      COMMON /UTINIP/  MXCOLS , ITABU , LIGCOU , COLCOU , IDF
      INTEGER          NEXCEP
      COMMON /UTEXC /  NEXCEP
      INTEGER          NT
      PARAMETER      ( NT = 10 )
      CHARACTER*132    TAMPON
      COMMON /UTTAMP/  TAMPON(NT)
      INTEGER          LDEB
      COMMON /UTDEB /  LDEB
C     COMPTEUR D'ERREURS POUR POUVOIR ARRETER EN F S'IL N'Y A QUE DES E
      INTEGER          NBERRF,NBERRE
      COMMON /UTNBER/  NBERRF,NBERRE
C
      CHARACTER *80    SPGSAU
      COMMON /SPGSAV / SPGSAU
      INTEGER          ICOMAL
      COMMON /COMSAV / ICOMAL
C
      LOGICAL LEXP
C     ------------------------------------------------------------------
      CHARACTER *80    SPG, SPG1
      CHARACTER *20    VERS
      CHARACTER *16    NOMCMD,DATE
      CHARACTER *8     K8B
      INTEGER          CDEB , CFIN
      INTEGER          IFIC,INIVO,IUTIL,IVERS,K,LC,LL,LXLGUT         
C     ------------------------------------------------------------------
      CALL GETRES(K8B,K8B,NOMCMD)
C --- 'Z' (IDF=8) = LEVEE D'EXCEPTION
      IDF  = INDEX('EFIDASXZ',CH1(1:1))
C     --- SI EXCEPTION, NEXCEP EST FIXE PAR COMMON VIA UTEXCP/UTDEXC
      IF ( IDF.NE.8 ) THEN
         NEXCEP = 21
      ENDIF
      IF ( IDF .EQ. 5) THEN
        LL=MIN(LEN(SPGLU),80)
        IF (SPGSAU(1:LL).EQ.SPGLU(1:LL) ) THEN
          ICOMAL=ICOMAL+1
          IF(ICOMAL.EQ.6) THEN
            LIGCOU = 1
            COLCOU = 80
            TAMPON(1) (1 : COLCOU) =
     &         '<A> < PLUS DE 5 FOIS LE MEME MESSAGE D''ALARME>'
            CALL UTVTAM
            IDF=7
            GOTO 200
          ELSE IF(ICOMAL.GT.6) THEN
            IDF=7
            LIGCOU = 1
            COLCOU = 80
            GOTO 200
          ENDIF
        ELSE
          ICOMAL=0
          SPGSAU(1:LL) = SPGLU(1:LL)
        END IF
      ELSE IF ( IDF .LE. 0 ) THEN
        TAMPON(9) = '<F> ERREUR DE PROGRAMMATION'
        TAMPON(10) = '    APPEL A UTDEBM ERRONE  '
        IFIC = UNIT(2,1)
        IF ( IFIC .GT. 0 ) WRITE(IFIC,'(1X,A)') (TAMPON(K),K=1,10)
        CALL JXABOR()
      ENDIF
C
      DO 1 K = 1 , NT
         TAMPON(K) = ' '
 1    CONTINUE
C
      LIGCOU = 1
      LL  = MIN(MXCOLS-10,LXLGUT(SPGLU))
      SPG = SPGLU(1:LL)
C -- MESSAGE:     E             F             A 
      IF ( IDF.EQ.1 .OR. IDF.EQ.2 .OR. IDF.EQ.5 ) THEN
        IF (IDF.EQ.1) NBERRE=NBERRE+1
        IF (IDF.EQ.2) NBERRF=NBERRF+1
        LC  = LXLGUT(NOMCMD)
        SPG1 = NOMCMD(1:LC)
        IF ( SPG1 .EQ. SPG ) THEN
          COLCOU = LDEB + LL + 3
          TAMPON(1) (1 : COLCOU) =
     +                     '<'//CH1(1:1)//'> <'//SPG(1:LL)//'>  '
        ELSE
          COLCOU = LDEB + LL + 3 + LC + 3
          TAMPON(1) (1 : COLCOU) = '<'//CH1(1:1)//'> <'//SPG1(1:LC)//
     &                             '> <'//SPG(1:LL)//'>  '
        ENDIF
C -- MESSAGE:          S             Z
      ELSE IF ( IDF.EQ.6 .OR. IDF.EQ.8 ) THEN
        IF (IDF.EQ.1) NBERRE=NBERRE+1
        IF (IDF.EQ.2) NBERRF=NBERRF+1
        LC  = LXLGUT(NOMCMD)
        SPG1 = NOMCMD(1:LC)
        IF ( SPG1 .EQ. SPG ) THEN
          COLCOU = LDEB + LL + 3
          TAMPON(1) (1 : COLCOU) = '<'//SPG(1:LL)//'>  '
        ELSE
          COLCOU = LDEB + LL + 3 + LC + 3
          TAMPON(1) (1 : COLCOU) = '<'//SPG1(1:LC)//
     &                             '> <'//SPG(1:LL)//'>  '
        ENDIF
      ELSE
        COLCOU = LDEB + LL + 3
        TAMPON(1) (1 : COLCOU) = '<'//CH1(1:1)//'> <'//SPG(1:LL)//'>  '
      ENDIF
      IF ( IDF.EQ.2 ) THEN
        CALL VERSIO(IVERS,IUTIL,INIVO,DATE,LEXP)
        WRITE(VERS,'(I2,''.'',I2,''.'',I2,'' '',A10)')
     &        IVERS,IUTIL,INIVO,DATE(1:10)
        LIGCOU = 2
        TAMPON(2) = TAMPON(1)
        TAMPON(1) = '<'//CH1(1:1)//'> <ASTER '//VERS//'>  '
      ENDIF
      IF ( IDF.EQ.6 .OR. IDF.EQ.8 ) THEN
        CALL VERSIO(IVERS,IUTIL,INIVO,DATE,LEXP)
        WRITE(VERS,'(I2,''.'',I2,''.'',I2,'' '',A10)')
     &        IVERS,IUTIL,INIVO,DATE(1:10)
        LIGCOU = 2
        TAMPON(2) = TAMPON(1)
        TAMPON(1) = '<ASTER '//VERS//'>  '
        IF( IDF.EQ.8 ) THEN
          LC  = LXLGUT(TAMPON(1))
          TAMPON(1) = TAMPON(1)(1:LC)//' <EXCEPTION LEVEE>  '
        ENDIF
      ENDIF
100   CONTINUE
      CALL UTRTAM ( TEXTE )
C
200   CONTINUE
      END 
