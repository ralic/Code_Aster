      SUBROUTINE TE0517(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 06/05/2003   AUTEUR CIBHHPD D.NUNEZ 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16 OPTION,NOMTE
C ----------------------------------------------------------------------
C     CALCUL DES OPTIONS POUR L'ELEMENT POU_D_TGM (MULTI-FIBRES)
C        VARI_ELNO_ELGA
C        SIEF_ELNO_ELGA
C        FORC_NODA
C
C IN  OPTION : OPTION DE CALCUL
C IN  NOMTE  : NOM DU TYPE ELEMENT
C ----------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER        ZI,JTAB(7)
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
C --------- FIN DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      INTEGER NC,NNO
      PARAMETER ( NC = 7 , NNO = 2 )
      REAL*8 ZERO
      PARAMETER ( ZERO = 0.0D+00)

      REAL*8 PGL(3,3),FL(2*NC), D1B3(2,3),KSI1,TMAX(2),TMIN(2),XIY,XIZ

      INTEGER NBFIB,KP,ADR,NCOMP,I
      INTEGER ICHN,ICHG,ICOMPO,NBVAR,LGPG,ICGP,ICONTN,IORIEN,IVECTU
C
C ----------------------------------------------------------------------
C
C     --- RECUPERATION DES CARACTERISTIQUES DES FIBRES
C     NOMBRE DE VARIABLES PAR POINT DE GAUSS EN PLUS DU NBFIB
      NCOMP = 7
      CALL JEVECH('PNBSP_I','L',I)
      NBFIB = ZI(I)
      CALL JEVECH('PFIBRES','L',JACF)
      NCARFI = 3

C     ON PROJETTE AVEC LES FCTS DE FORME
C     SUR LES NOEUDS DEBUT ET FIN DE L'ELEMENT
C     POUR LE POINT 1
      KSI1 = -SQRT( 5.D0 / 3.D0 )
      D1B3(1,1) = KSI1*(KSI1-1.D0)/2.0D0
      D1B3(1,2) = 1.D0-KSI1*KSI1
      D1B3(1,3) = KSI1*(KSI1+1.D0)/2.0D0
C     POUR LE POINT 2
      KSI1 = SQRT( 5.D0 / 3.D0 )
      D1B3(2,1) = KSI1*(KSI1-1.D0)/2.0D0
      D1B3(2,2) = 1.D0-KSI1*KSI1
      D1B3(2,3) = KSI1*(KSI1+1.D0)/2.0D0

C     --------------------------------------
      IF ( OPTION .EQ. 'VARI_ELNO_ELGA' ) THEN

C  CETTE OPTION EXISTE DANS LE CATALOGUE :
C  A QUOI SERT-ELLE, POUR CET ELEMENT ?

        CALL UTMESS('F','TE0517',
     &              'OPTION "VARI_ELNO_ELGA" IMPOSSIBLE ACTUELLEMENT')

C        CALL JEVECH ( 'PVARINR', 'E', ICHN   )
C        CALL JEVECH ( 'PVARIGR', 'L', ICHG   )
C        CALL JEVECH ( 'PCOMPOR', 'L', ICOMPO )
C --- NOMBRE DE VARIABLE INTERNE DE LA LOI DE COMPORTEMENT
C        READ (ZK16(ICOMPO-1+2),'(I16)') NBVAR

C        CALL TECACH('OON','PVARINR',7,JTAB,IRET)

C        LGPG = NBVAR*NBFIB
C        DO 110 I = 1,LGPG
C          DO 112 KP = 1 , 3
C            ADR = ICHG-1+LGPG*(KP-1)+I
C            ZR(ICHN     +I-1)=ZR(ICHN     +I-1)+ZR(ADR)*D1B3(1,KP)
C            ZR(ICHN+LGPG+I-1)=ZR(ICHN+LGPG+I-1)+ZR(ADR)*D1B3(2,KP)
C112       CONTINUE
C110     CONTINUE

C     --------------------------------------
      ELSEIF ( OPTION .EQ. 'SIEF_ELNO_ELGA' .OR.
     &         OPTION .EQ. 'FORC_NODA'  ) THEN

        IF (OPTION .EQ. 'SIEF_ELNO_ELGA' ) THEN
          CALL JEVECH('PCONTRR','L',ICGP)
          CALL JEVECH('PSIEFNOR','E',ICONTN)
        ELSE
          CALL JEVECH('PCONTMR','L',ICGP)
          CALL JEVECH('PCAORIE','L',IORIEN)
          CALL JEVECH('PVECTUR','E',IVECTU)
        ENDIF

C !!!   MAGOUILLE POUR STOCKER LES FORCES INTEGREES !!!
C       ELLES SONT DEJA CALCULEES ET STOCKEES APRES LES FIBRES
C         ON RECUPERE L'EFFORT STOCKE AUX NOEUDS 1 ET 2
        DO 210 I = 1 , NC
          FL(I)    = ZERO
          FL(I+NC) = ZERO
          DO 212 KP = 1 , 3
            ADR = ICGP+(NBFIB+NCOMP)*(KP-1)+ NBFIB + I - 1
            FL(I)   = FL(I)   +ZR(ADR)*D1B3(1,KP)
            FL(I+NC)= FL(I+NC)+ZR(ADR)*D1B3(2,KP)
212       CONTINUE
210     CONTINUE
C !!!   FIN MAGOUILLE !!!

        IF (OPTION .EQ. 'SIEF_ELNO_ELGA' ) THEN
C         TAUX DE TRAVAIL CORRESPONDANT AUX EFFORTS GENERALISES
C         CARACTERISTIQUE MECANIQUE DE LA SECTION
          XIY = ZERO
          XIZ = ZERO
          AA = ZERO
          DO 220 I = 1 , NBFIB
            ADR = JACF+(I-1)*NCARFI
            AA = AA + ZR(ADR+2)
            XIY = XIY + ZR(ADR+2)*ZR(ADR+1)*ZR(ADR+1)
            XIZ = XIZ + ZR(ADR+2)*ZR(ADR)*ZR(ADR)
220       CONTINUE

          ADR = JACF
          TMAX(1)=FL(1)/AA +FL( 5)*ZR(ADR+1)/XIY -FL( 6)*ZR(ADR)/XIZ
          TMIN(1)=TMAX(1)
          TMAX(2)=FL(8)/AA +FL(12)*ZR(ADR+1)/XIY -FL(13)*ZR(ADR)/XIZ
          TMIN(2)=TMAX(2)
          DO 230 I= 2 , NBFIB
            ADR = JACF+(I-1)*NCARFI
            SIGM=FL(1)/AA +FL( 5)*ZR(ADR+1)/XIY -FL( 6)*ZR(ADR)/XIZ
            IF ( SIGM .GT.  TMAX(1) ) TMAX(1) = SIGM
            IF ( SIGM .LT.  TMIN(1) ) TMIN(1) = SIGM
            SIGM=FL(8)/AA +FL(12)*ZR(ADR+1)/XIY -FL(13)*ZR(ADR)/XIZ
            IF ( SIGM .GT.  TMAX(2) ) TMAX(2) = SIGM
            IF ( SIGM .LT.  TMIN(2) ) TMIN(2) = SIGM
230      CONTINUE

          DO 310 I = 1 , NC
            ZR(ICONTN+I-1) = FL(I)
310       CONTINUE
          ZR(ICONTN+(NC+1)-1) = TMAX(1)
          ZR(ICONTN+(NC+2)-1) = TMIN(1)
          DO 312 I = (NC+1) , 2*NC
            ZR(ICONTN+2+I-1) = FL(I)
312       CONTINUE
          ZR(ICONTN+2*(NC+1)+1-1) = TMAX(2)
          ZR(ICONTN+2*(NC+1)+2-1) = TMIN(2)

        ELSEIF ( OPTION .EQ. 'FORC_NODA' ) THEN
C         AU NOEUD 1 ON DOIT PRENDRE -FORCE POUR VERIFIER L'EQUILIBRE
          DO 410 I = 1 , NC
            FL(I) = - FL(I)
410       CONTINUE
          CALL MATROT(ZR(IORIEN),PGL)
          CALL UTPVLG(NNO,NC,PGL,FL,ZR(IVECTU))
        ENDIF

      ENDIF

      END
