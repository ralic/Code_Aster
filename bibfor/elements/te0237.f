      SUBROUTINE TE0237(OPTION,NOMTE)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*16 OPTION,NOMTE
C ......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C .  - FONCTION REALISEE:      CONTRAINTES PLANES AUX NOEUDS
C .                            COQUE 1D
C .                        OPTION  : 'SIEF_ELGA'
C .                                  'EPSI_ELGA'
C .                        ELEMENT: MECXSE3,METCSE3,METDSE3
C .  - ARGUMENTS:
C .      DONNEES:      OPTION       -->  OPTION DE CALCUL
C .                    NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................

      CHARACTER*8 ELREFE
      CHARACTER*8 NOMRES(3),NOMPAR
      INTEGER ICODRE(3)
      REAL*8 E,NU,TPG,TPGMOY,TPGINF,TPGSUP,VALPAR,TREF
      REAL*8 X3,EPS(5),C1,C2,H,EPSTHE,KI(3),NIV
      REAL*8 E11,E22,K11,K22,EP11,EP22,EP12,ESX3
      REAL*8 DFDX(3),VALRES(3)
      REAL*8 JAC,R,COSA,SINA,COUR,CORREC,ZMIN,HIC
      INTEGER I,K,KP,IGEOM,IMATE,ICACO,IDEPL,ICONT,NBPAR,IDEFOR,ITAB(7)
      INTEGER NNO,NPG,IDFDK,IVF,IRET,IRET1,IRET2,IRET3,IDEC,INTE,NPGE

C-----------------------------------------------------------------------
      INTEGER ICOU ,IPOIDS ,IRET4 ,ISP ,JGANO ,JNBSPI ,NBCMP 
      INTEGER NBCOU ,NDIM ,NNOS 
      REAL*8 R8NNEM ,SI11 ,SI12 ,SI22 ,ZIC 
C-----------------------------------------------------------------------
      CALL ELREF1(ELREFE)

      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDK,JGANO)

      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PCACOQU','L',ICACO)
      CALL JEVECH('PDEPLAR','L',IDEPL)
      CALL JEVECH('PNBSP_I','L',JNBSPI)
      NBCOU=ZI(JNBSPI-1+1)
      IF (NBCOU.LE.0) CALL U2MESS('F','ELEMENTS_12')
      IF (NBCOU.GT.30) CALL U2MESS('F','ELEMENTS3_50')

      IF (OPTION.EQ.'EPSI_ELGA') THEN
        CALL TECACH('OOO','PDEFOPG' ,7,ITAB,IRET)
        IDEFOR=ITAB(1)
      ELSEIF (OPTION.EQ.'SIEF_ELGA') THEN
        CALL JEVECH('PMATERC','L',IMATE)
        CALL TECACH('OOO','PCONTRR' ,7,ITAB,IRET)
        ICONT=ITAB(1)
        CALL RCVARC(' ','TEMP','REF','RIGI',1,1,TREF,IRET)
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

      NBCMP=ITAB(2)/ITAB(3)
      CALL ASSERT(NBCMP.GT.0)

      H=ZR(ICACO)
C---- COTE MINIMALE SUR L'EPAISSEUR
      ZMIN=-H/2.D0
C---- EPAISSEUR DE CHAQUE COUCHE
      HIC=H/NBCOU
      CORREC=ZR(ICACO+2)
C NOMBRE DE POINT DE GAUSS DANS LA TRANCHE
C (POUR RESTER COHERENT AVEC SIEF_ELGA EN PLASTICITE )
      NPGE=3
      KI(1)=-1.D0
      KI(2)=0.D0
      KI(3)=1.D0
C
      DO 50 ICOU=1,NBCOU
        DO 40 INTE=1,NPGE
          NIV=KI(INTE)

          IF (INTE.EQ.1) THEN
            ZIC=ZMIN+(ICOU-1)*HIC
          ELSEIF (INTE.EQ.2) THEN
            ZIC=ZMIN+HIC/2.D0+(ICOU-1)*HIC
          ELSE
            ZIC=ZMIN+HIC+(ICOU-1)*HIC
          ENDIF
          X3=ZIC

          DO 30 KP=1,NPG
            K=(KP-1)*NNO
            IDEC=NBCMP*(KP-1)*NPGE*NBCOU+
     &                                NBCMP*(ICOU-1)*NPGE+NBCMP*(INTE-1)
            CALL DFDM1D(NNO,ZR(IPOIDS+KP-1),ZR(IDFDK+K),ZR(IGEOM),DFDX,
     &                  COUR,JAC,COSA,SINA)

            DO 10 I=1,5
              EPS(I)=0.D0
   10       CONTINUE
            R=0.D0
            DO 20 I=1,NNO
              EPS(1)=EPS(1)+DFDX(I)*ZR(IDEPL+3*I-3)
              EPS(2)=EPS(2)+DFDX(I)*ZR(IDEPL+3*I-2)
              EPS(3)=EPS(3)+DFDX(I)*ZR(IDEPL+3*I-1)
              EPS(4)=EPS(4)+ZR(IVF+K+I-1)*ZR(IDEPL+3*I-3)
              EPS(5)=EPS(5)+ZR(IVF+K+I-1)*ZR(IDEPL+3*I-1)
              R=R+ZR(IVF+K+I-1)*ZR(IGEOM+2*I-2)
   20       CONTINUE
C
            E11=EPS(2)*COSA-EPS(1)*SINA
            K11=EPS(3)
            ESX3=EPS(5)+EPS(1)*COSA+EPS(2)*SINA
            IF (NOMTE.EQ.'MECXSE3') THEN
              E22=EPS(4)/R
              K22=-EPS(5)*SINA/R
              EP22=(E22+X3*K22)/(1.D0+(CORREC*X3*COSA/R))
            ELSE
              E22=0.D0
              K22=0.D0
              EP22=0.D0
            ENDIF
C
            EP11=(E11+X3*K11)/(1.D0+(CORREC*X3*COUR))
            EP12=ESX3/(1.D0+(CORREC*X3*COUR))
C
            IF (OPTION.EQ.'EPSI_ELGA') THEN
              ZR(IDEFOR+IDEC-1+1)=EP11
              ZR(IDEFOR+IDEC-1+2)=EP22
              ZR(IDEFOR+IDEC-1+3)=EP12

            ELSEIF (OPTION.EQ.'SIEF_ELGA') THEN
C
C         -- RECUPERATION DES PARAMETRES MATERIAU :
C
C         ---- RECUPERATION DE LA TEMPERATURE POUR LE MATERIAU:
C         ---- SI LA TEMPERATURE EST CONNUE AUX POINTS DE GAUSS :
              ISP=3*(ICOU-1)
              CALL RCVARC(' ','TEMP','+','RIGI',KP,ISP+1,TPGINF,IRET1)
              CALL RCVARC(' ','TEMP','+','RIGI',KP,ISP+2,TPGMOY,IRET2)
              CALL RCVARC(' ','TEMP','+','RIGI',KP,ISP+3,TPGSUP,IRET3)
              IRET4=IRET1+IRET2+IRET3
              CALL ASSERT(IRET4.EQ.0 .OR. IRET4.EQ.3)

C         ---- UTILISATION DE 4 POINTS DE GAUSS DANS L'EPAISSEUR
C         ---- COMME POUR LA LONGUEUR

              IF (IRET4.EQ.0) THEN
                TPG=TPGSUP*NIV*(1.D0+NIV)/2.D0+TPGMOY*(1.D0-(NIV)**2)-
     &              TPGINF*NIV*(1.D0-NIV)/2.D0
              ELSE
                TPG=R8NNEM()
              ENDIF
              VALPAR=TPG
              NBPAR=1
              NOMPAR='TEMP'

              NOMRES(1)='E'
              NOMRES(2)='NU'
              NOMRES(3)='ALPHA'
              CALL RCVALB('RIGI',1,1,'+',ZI(IMATE),' ','ELAS',NBPAR,
     &                    NOMPAR,VALPAR,2,NOMRES,VALRES,ICODRE,1)
              CALL RCVALB('RIGI',1,1,'+',ZI(IMATE),' ','ELAS',NBPAR,
     &                    NOMPAR,VALPAR,1,NOMRES(3),VALRES(3),ICODRE(3),
     &                    0)
              E=VALRES(1)
              NU=VALRES(2)
              IF (IRET4.EQ.0) THEN
                IF ((ICODRE(3).NE.0) .OR. (IRET.EQ.1)) THEN
                  CALL U2MESS('F','CALCULEL_15')
                ELSE
                  EPSTHE=(TPG-TREF)*VALRES(3)*E/(1.D0-NU)
                ENDIF
              ELSE
                EPSTHE=0.D0
              ENDIF
C
C         -- FIN RECUPERATION DES PARAMETRES MATERIAU :
C
              C1=E/(1.D0+NU)
              C2=C1/(1.D0-NU)

              IF (NOMTE(3:4).EQ.'CX') THEN
                SI11=C2*(EP11+NU*EP22)-EPSTHE
                SI22=C2*(EP22+NU*EP11)-EPSTHE
              ELSEIF (NOMTE.EQ.'METDSE3 ') THEN
                SI11=C2*EP11-EPSTHE
                SI22=C2*NU*EP11-EPSTHE
              ELSE
                SI11=E*(EP11-EPSTHE)
                SI12=0.D0
              ENDIF
              SI12=C1*EP12
C
              ZR(ICONT+IDEC-1+1)=SI11
              ZR(ICONT+IDEC-1+2)=SI22
              ZR(ICONT+IDEC-1+4)=SI12
C
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
C
   30     CONTINUE
C
   40   CONTINUE
   50 CONTINUE
C
      END
