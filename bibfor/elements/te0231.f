      SUBROUTINE TE0231(OPTION,NOMTE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16 OPTION,NOMTE
C ......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 06/05/2003   AUTEUR CIBHHPD D.NUNEZ 
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
C .  - FONCTION REALISEE:  CALCUL DES EFFORTS GENERALISES AUX NOEUDS
C .                                              OU AUX POINTS DE GAUSS
C .                        COQUE 1D
C .                        OPTIONS : 'SIEF_ELGA_DEPL  '
C .                                  'EFGE_ELNO_DEPL  '
C .                        ELEMENT: MECXSE3,METCSE3,METDSE3
C .  - ARGUMENTS:
C .      DONNEES:      OPTION       -->  OPTION DE CALCUL
C .                    NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C --------- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS NORMALISEES JEVEUX -----------------------
C
      CHARACTER*24  CARAC,FF
      CHARACTER*24  CHMAT
      CHARACTER*8   NOMRES(3),ELREFE
      CHARACTER*2   BL2, CODRET(3)
      REAL*8        E,NU,TPG,TPG1,TPG2,TPG3
      REAL*8        X3,EPS(5),C,H,DILAT,VALRES(3)
      REAL*8        E11,E22,K11,K22,EP11,EP22
      REAL*8        DFDX(3),EFFOPG(24)
      REAL*8        JAC,R,COSA,SINA,COUR
      INTEGER       I,K,KP,IGEOM,IMATE,ICACO,IDEPL
      INTEGER       ICARAC,NNO,NPG1,IFF,IDFDK,ITEMP,IVF,ITREF,NCMP
      INTEGER       ICOPS,IP,CORREC,ITAB(8)
C
      CALL ELREF1(ELREFE)

C
      CARAC = '&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO = ZI(ICARAC)
      NPG1 = ZI(ICARAC+2)
C
      FF = '&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PCACOQU','L',ICACO)
      CALL JEVECH('PDEPLAR','L',IDEPL)
      CALL TECACH ('OON','PTEMPER',8,ITAB,IRET)
      ITEMP=ITAB(1)
      CALL JEVECH('PTEREF','L',ITREF)
      IF (OPTION.EQ.'EFGE_ELNO_DEPL') THEN
        CALL JEVECH('PEFFORR','E',IEFFOR)
      ELSE
        CALL JEVECH('PCONTRR','E',IEFFOR)
      END IF
C
      BL2    = '  '
      H      = ZR(ICACO)
CJMP  CORREC = CORRECTION DE METRIQUE = 0 (NON) OU 1 (OUI)
CJMP  CORREC = ZR(ICACO+2)
      CORREC = NINT(ZR(ICACO+2))
C
      DO 300 I = 1,NPG1*6
        EFFOPG(I) = 0.D0
  300 CONTINUE
C
      IVF = IFF + NPG1
      IDFDK = IVF + NPG1*NNO
      ICOPS = IDFDK + NPG1*NNO
C
      DO 200 KP = 1,NPG1
        K = (KP-1)*NNO
        CALL DFDM1D(NNO,ZR(IFF+KP-1),ZR(IDFDK+K),ZR(IGEOM),DFDX,COUR,
     +              JAC,COSA,SINA)
        TPG1 = 0.D0
        TPG2 = 0.D0
        TPG3 = 0.D0
        DO 129 I = 1,NNO
          CALL DXTPIF(ZR(ITEMP+3*(I-1)),ZL(ITAB(8)+3*(I-1)))
          TPG1 = TPG1 + ZR(ITEMP+3*I-3)*ZR(IVF+K+I-1)
          TPG2 = TPG2 + ZR(ITEMP+3*I-2)*ZR(IVF+K+I-1)
          TPG3 = TPG3 + ZR(ITEMP+3*I-1)*ZR(IVF+K+I-1)
  129   CONTINUE
        DO 140 I = 1,5
          EPS(I) = 0.D0
  140   CONTINUE
        R = 0.D0
        DO 150 I = 1,NNO
          EPS(1) = EPS(1) + DFDX(I)      *ZR(IDEPL+3*I-3)
          EPS(2) = EPS(2) + DFDX(I)      *ZR(IDEPL+3*I-2)
          EPS(3) = EPS(3) + DFDX(I)      *ZR(IDEPL+3*I-1)
          EPS(4) = EPS(4) + ZR(IVF+K+I-1)*ZR(IDEPL+3*I-3)
          EPS(5) = EPS(5) + ZR(IVF+K+I-1)*ZR(IDEPL+3*I-1)
          R      = R      + ZR(IVF+K+I-1)*ZR(IGEOM+2*I-2)
  150   CONTINUE
C
        E11 = EPS(2)*COSA - EPS(1)*SINA
        K11 = EPS(3)
        IF (NOMTE(3:4).EQ.'CX') THEN
          E22 = EPS(4)/R
          K22 = -EPS(5)*SINA/R
        ELSE
          E22 = 0.D0
          K22 = 0.D0
        END IF
C
C---- UTILISATION DE 4 POINTS DE GAUSS DANS L'EPAISSEUR
C---- COMME POUR LA LONGUEUR
C
        DO 130 IP = 1,NPG1
          X3 = ZR(ICOPS-1+IP)
          TPG = TPG1* (1.D0- (X3)**2) + TPG3*X3* (1.D0+X3)/2.D0 -
     +          TPG2*X3* (1.D0-X3)/2.D0
          X3 = X3*H/2.D0
          EP11 = (E11+X3*K11)/ (1.D0 + (CORREC*X3*COUR))
          NOMRES(1) = 'E'
          NOMRES(2) = 'NU'
          NOMRES(3) = 'ALPHA'
          CALL RCVALA ( ZI(IMATE),'ELAS',1,'TEMP',TPG,2,NOMRES,
     +                  VALRES, CODRET, 'FM' )
          CALL RCVALA ( ZI(IMATE),'ELAS',1,'TEMP',TPG,1,NOMRES(3),
     +                  VALRES(3), CODRET(3), BL2 )
          E  = VALRES(1)
          NU = VALRES(2)
          IF (CODRET(3).NE.'OK') THEN
            DILAT = 0.D0
          ELSE
            TPG = TPG - ZR(ITREF)
            DILAT = VALRES(3)*E/ (1.D0-NU)
          END IF
C
          C = E/ (1.D0-NU*NU)
          IF (NOMTE(3:4).EQ.'CX') THEN
            EP22 = (E22+X3*K22)/ (1.D0 + (CORREC*COSA*X3/R))
            EFFOPG(6* (KP-1)+1) = EFFOPG(6* (KP-1)+1) +
     +                            ZR(IFF-1+IP)* (H/2.D0)*
     +                            (C* (EP11+NU*EP22)-DILAT*TPG)
            EFFOPG(6* (KP-1)+2) = EFFOPG(6* (KP-1)+2) +
     +                            ZR(IFF-1+IP)* (H/2.D0)*
     +                            (C* (NU*EP11+EP22)-DILAT*TPG)
            EFFOPG(6* (KP-1)+4) = EFFOPG(6* (KP-1)+4) +
     +                            ZR(IFF-1+IP)*X3* (H/2.D0)*
     +                            (C* (EP11+NU*EP22)-DILAT*TPG)
            EFFOPG(6* (KP-1)+5) = EFFOPG(6* (KP-1)+5) +
     +                            ZR(IFF-1+IP)*X3* (H/2.D0)*
     +                            (C* (NU*EP11+EP22)-DILAT*TPG)
          ELSE IF (NOMTE(1:8).EQ.'METCSE3 ') THEN
            EFFOPG(6* (KP-1)+1) = EFFOPG(6* (KP-1)+1) +
     +                            ZR(IFF-1+IP)* (H/2.D0)*
     +                            (E*(EP11-DILAT*TPG))
            EFFOPG(6* (KP-1)+4) = EFFOPG(6* (KP-1)+4) +
     +                            ZR(IFF-1+IP)*X3* (H/2.D0)*
     +                            (E*(EP11-DILAT*TPG))
            EFFOPG(6* (KP-1)+2) = 0.D0
            EFFOPG(6* (KP-1)+5) = 0.D0
          ELSE
            EFFOPG(6* (KP-1)+1) = EFFOPG(6* (KP-1)+1) +
     +                            ZR(IFF-1+IP)* (H/2.D0)*
     +                            (C*EP11-DILAT*TPG)
            EFFOPG(6* (KP-1)+2) = EFFOPG(6* (KP-1)+2) +
     +                            ZR(IFF-1+IP)* (H/2.D0)*
     +                            (C*NU*EP11-DILAT*TPG)
            EFFOPG(6* (KP-1)+4) = EFFOPG(6* (KP-1)+4) +
     +                            ZR(IFF-1+IP)*X3* (H/2.D0)*
     +                            (C*EP11-DILAT*TPG)
            EFFOPG(6* (KP-1)+5) = EFFOPG(6* (KP-1)+5) +
     +                            ZR(IFF-1+IP)*X3* (H/2.D0)*
     +                            (C*NU*EP11-DILAT*TPG)
          END IF
C
  130   CONTINUE
        EFFOPG(6* (KP-1)+3) = 0.D0
        EFFOPG(6* (KP-1)+6) = 0.D0
C
  200 CONTINUE
C
      IF (OPTION(8:9).EQ.'NO') THEN
         NCMP = 6
         CALL PPGANO(NNO,NPG1,NCMP,EFFOPG,ZR(IEFFOR))
      ELSE
         DO 289 I = 1,NPG1
            ZR(IEFFOR-1+6* (I-1)+1) = EFFOPG(6* (I-1)+1)
            ZR(IEFFOR-1+6* (I-1)+2) = EFFOPG(6* (I-1)+2)
            ZR(IEFFOR-1+6* (I-1)+4) = EFFOPG(6* (I-1)+4)
            ZR(IEFFOR-1+6* (I-1)+5) = EFFOPG(6* (I-1)+5)
  289    CONTINUE
      END IF
C
      END
