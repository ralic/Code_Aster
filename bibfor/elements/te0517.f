      SUBROUTINE TE0517(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 15/12/2003   AUTEUR MJBHHPE J.L.FLEJOU 
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
      REAL*8 SIGFIB

      INTEGER NBFIB,KP,ADR,NCOMP,I,CARA,NE,JACF,NCARFI
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
        DO 200 I = 1 , NC
          FL(I)    = ZERO
          FL(I+NC) = ZERO
          DO 202 KP = 1 , 3
            ADR = ICGP+(NBFIB+NCOMP)*(KP-1)+ NBFIB + I - 1
            FL(I)   = FL(I)   +ZR(ADR)*D1B3(1,KP)
            FL(I+NC)= FL(I+NC)+ZR(ADR)*D1B3(2,KP)
202       CONTINUE
200     CONTINUE
C !!!   FIN MAGOUILLE !!!

C !!!   A CAUSE DE LA PLASTIFICATION DE LA SECTION LES EFFORTS
C          N,MFY,MFZ DOIVENT ETRE RECALCULES POUR LES NOEUDS 1 ET 2
        FL(1)    = ZERO
        FL(5)    = ZERO
        FL(6)    = ZERO
        FL(1+NC) = ZERO
        FL(5+NC) = ZERO
        FL(6+NC) = ZERO

C       POUR LES NOEUDS 1 ET 2
C          CALCUL DES CONTRAINTES
C          CALCUL DES EFFORTS GENERALISES A PARTIR DES CONTRAINTES
        DO 220 NE = 1 , 2
          DO 230 I= 1 , NBFIB
             SIGFIB = ZERO
             DO 240 KP = 1 , 3
              ADR = ICGP+(NBFIB+NCOMP)*(KP-1) + I - 1
              SIGFIB = SIGFIB + ZR(ADR)*D1B3(NE,KP)
240          CONTINUE
             IF ( I .EQ. 1 ) THEN
               TMAX(NE) = SIGFIB
               TMIN(NE) = SIGFIB
             ELSE
               IF ( SIGFIB .GT. TMAX(NE) ) TMAX(NE) = SIGFIB
               IF ( SIGFIB .LT. TMIN(NE) ) TMIN(NE) = SIGFIB
             ENDIF
             ADR  = NC*(NE-1)
             CARA = JACF+(I-1)*NCARFI
             FL(1+ADR) = FL(1+ADR) + SIGFIB*ZR(CARA+2)
             FL(5+ADR) = FL(5+ADR) + SIGFIB*ZR(CARA+2)*ZR(CARA+1)
             FL(6+ADR) = FL(6+ADR) - SIGFIB*ZR(CARA+2)*ZR(CARA)
230       CONTINUE
220     CONTINUE

        IF (OPTION .EQ. 'SIEF_ELNO_ELGA' ) THEN
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
