      SUBROUTINE FOLEFO(IFSIG, ORG,PAS,NPS,FVA,FFO,NOMCMD,
     +                  VAR,FON,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           IFSIG,               NPS
      INTEGER           IER
      CHARACTER*(*)            ORG,    FVA,FFO, NOMCMD
      REAL*8                       PAS
      REAL*8            VAR(*),FON(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 06/04/2004   AUTEUR DURAND C.DURAND 
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
C     LECTURE DES VALEURS DU SIGNAL SELON SON ORGANISATION
C     ------------------------------------------------------------------
C IN  IFSIG  : IS : UNITE LOGIQUE DE LECTURE
C IN  ORG    : K4 : ORGANISATION DES VALEURS
C       'VFVF'
C       'VVFF'
C       'FFFF'
C       'V1F1'
C IN  NPS    : IS : NOMBRE DE POINTS DECRIVANT LE SIGNAL
C IN  PAS    : R8 : PAS DECRIVANT LE SIGNAL
C IN  FVA    : K3 : FORMAT DE LECTURE VARIABLE * OU FIXE N CARACTERES
C IN  FFO    : K3 : FORMAT DE LECTURE FONCTION * OU FIXE N CARACTERES
C IN  NOMCMD : K16: NOM DE LA COMMANDE APPELANTE
C OUT VAR    : R8 : TABLEAU DES ABSCISSES
C OUT FON    : R8 : TABLEAU DES ORDONNEES
C OUT IER    : IS : CODE RETOUR
C     ------------------------------------------------------------------
      CHARACTER*4  CORG
      CHARACTER*9  CHVAR,CHFON
      CHARACTER*16 CHNOMC

C
      CORG   = ORG
      CHNOMC = NOMCMD
      CHVAR  = '(NNFII.0)'
      CHFON  = '(NNFII.0)'
      IF ( CORG .EQ. 'FFFF' ) THEN
        IF ( FFO(1:1) .NE. 'R' ) THEN
          IER = IER + 1
          CALL UTMESS('E',CHNOMC//'(ERREUR 41)','FORMAT FFO :'//
     +                ' OBLIGATOIREMENT RII AVEC ORG : '//CORG//'.')
        ELSE IF ( PAS .EQ. 0.D0 ) THEN
          IER = IER + 1
          CALL UTMESS('F',CHNOMC//'(ERREUR 43)',
     +                'PAS OBLIGATOIRE AVEC ORG : '//CORG//'.')
        ELSE
          READ(FFO(2:3), '(I2)' ) IFO
          NTF  =  72 / IFO
          NLIF = NPS / NTF
          IF ( MOD(NPS,NTF) .NE. 0) NLIF = NLIF + 1
          WRITE(CHFON(2:3), '(I2)' ) NTF
          CHFON(5:6) = FFO(2:3)
          VAR(1 ) = 0.D0
          DO 30 IL = 2 , NPS
            VAR(IL) = VAR(IL-1)+PAS
  30      CONTINUE
        ENDIF
      ELSE IF ( CORG .EQ. 'VFVF' .OR.
     +          CORG .EQ. 'V1V1' .OR.
     +          CORG .EQ. 'VVFF'       ) THEN
        IF ( FVA(1:1) .NE. 'R' .AND. FFO(1:1) .NE. 'R' ) THEN
          IER = IER + 1
          CALL UTMESS('E',CHNOMC//'(ERREUR 41)','FORMAT FVA ET FFO :'//
     +                ' OBLIGATOIREMENT RII AVEC ORG : '//CORG//'.')
        ELSE
          READ(FVA(2:3), '(I2)' ) IVA
          READ(FFO(2:3), '(I2)' ) IFO
          IF ( CORG .EQ. 'VFVF' .OR. CORG .EQ. 'V1F1' ) THEN
            IF ( IVA .NE. IFO ) THEN
              IER = IER + 1
              CALL UTMESS('E',CHNOMC//'(ERREUR 42)',
     +                    'FORMAT FVA ET FFO :'//
     +                    ' OBLIGATOIREMENT IDENTIQUES AVEC ORG : '
     +                    //CORG//'.')
            ENDIF
          ENDIF
        ENDIF
        NTV  =  72 / IVA
        NLIV = NPS / NTV
        IF ( MOD(NPS,NTV) .NE. 0) NLIV = NLIV + 1
        WRITE(CHVAR(2:3), '(I2)' ) NTV
        CHVAR(5:6) = FVA(2:3)
        NTF  =  72 / IFO
        NLIF = NPS / NTF
        IF ( MOD(NPS,NTF) .NE. 0) NLIF = NLIF + 1
        WRITE(CHFON(2:3), '(I2)' ) NTF
        CHFON(5:6) = FFO(2:3)
      ELSE
         CALL UTMESS('F',CHNOMC//'(ERREUR 44)',
     +              ' ORGANISATION = '//CORG//' INCONNUE')
      ENDIF
C
      IF ( CORG .EQ. 'VVFF') THEN
         DO 10 IL = 1,NLIV
            IL1 =  1+NTV*(IL-1)
            IL2 =  MIN(NTV*IL,NPS)
            READ(IFSIG,CHVAR,END=900) (VAR(K),K=IL1,IL2)
  10     CONTINUE
      ENDIF
      IF ( CORG .EQ. 'VFVF' .OR. CORG .EQ. 'V1F1' ) THEN
         DO 15 IL = 1,NLIF
            IL1 =  1+NTF*(IL-1)
            IL2 =  MIN(NTF*IL,NPS)
            READ(IFSIG,CHVAR,END=900) (VAR(K),K=IL1,IL2)
            READ(IFSIG,CHFON,END=900) (FON(K),K=IL1,IL2)
  15     CONTINUE
      ELSE
         DO 20 IL = 1,NLIF
            IL1 =  1+NTF*(IL-1)
            IL2 =  MIN(NTF*IL,NPS)
            READ(IFSIG,CHFON,END=900) (FON(K),K=IL1,IL2)
  20     CONTINUE
      ENDIF
         WRITE(6,*) ' DOUZE PREMIERS TERMES '
         WRITE(6,*) (VAR(K),FON(K), K= 1,12)
         WRITE(6,*) ' SIX DERNIERS TERMES '
         WRITE(6,*) (VAR(K),FON(K), K= NPS-5,NPS)
C
      GOTO 9999
  900 CONTINUE
      CALL UTMESS('F',' ','FICHIER DE SIGNAL INCOMPLET.')
 9999 CONTINUE
      END
