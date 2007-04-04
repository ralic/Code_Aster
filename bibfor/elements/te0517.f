      SUBROUTINE TE0517(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/04/2007   AUTEUR ABBAS M.ABBAS 
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
      IMPLICIT NONE
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
      INTEGER        ZI
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
      INTEGER ICOMPO,ICGP,ICONTN,IORIEN,IVECTU
C
      INTEGER IDEPLM,IDEPLP,IGEOM,IRET,ISECT,IMATE,K,KK,NPG
      REAL*8  UTG(2*NC),XUG(6),XD(3),ANG1(3),DDOT,EY,EZ,TEMP,R8BID
      REAL*8  XL,XL2,TET1,TET2,ALFA1,BETA1,GAMMA1,GAMMA
      REAL*8  XLS2,D1B(NC,2*NC),CO(3),AA,E,NU,G,ALFAY,ALFAZ,PHIY,PHIZ
      LOGICAL REACTU
C ----------------------------------------------------------------------
C
C     --- RECUPERATION DES CARACTERISTIQUES DES FIBRES
C     NOMBRE DE VARIABLES PAR POINT DE GAUSS EN PLUS DU NBFIB
      NCOMP = 7
      CALL JEVECH('PNBSP_I','L',I)
      NBFIB = ZI(I)
      IF ( ZI(I+1).NE.1 ) THEN
        CALL U2MESS('F','SENSIBILITE_52')
      ENDIF
      CALL JEVECH('PFIBRES','L',JACF)
      NCARFI = 3

C     --- NOMBRE DE POINT DE GAUSS
      NPG = 3

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

        CALL U2MESS('F','ELEMENTS4_16')

C     --------------------------------------
      ELSEIF ( OPTION .EQ. 'SIEF_ELNO_ELGA' ) THEN

        CALL JEVECH('PCONTRR','L',ICGP)
        CALL JEVECH('PSIEFNOR','E',ICONTN)

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

        DO 310 I = 1 , NC
          ZR(ICONTN+I-1) = FL(I)
310     CONTINUE
        ZR(ICONTN+(NC+1)-1) = TMAX(1)
        ZR(ICONTN+(NC+2)-1) = TMIN(1)
        DO 312 I = (NC+1) , 2*NC
          ZR(ICONTN+2+I-1) = FL(I)
312     CONTINUE
        ZR(ICONTN+2*(NC+1)+1-1) = TMAX(2)
        ZR(ICONTN+2*(NC+1)+2-1) = TMIN(2)

C     --------------------------------------
      ELSEIF ( OPTION .EQ. 'FORC_NODA' ) THEN

        CALL JEVECH('PCONTMR','L',ICGP)
        CALL JEVECH('PCAORIE','L',IORIEN)
        CALL JEVECH('PVECTUR','E',IVECTU)

C ---------- MODIF ---------------------------------------------------
C --- IL FAUT TENIR COMPTE DU CHGT DE GEOMETRIE
C --- POUR CALCULER LA MATRICE DE PASSAGE

        CALL JEVECH('PCOMPOR','L',ICOMPO)

        REACTU = (ZK16(ICOMPO+2)(1:9) .EQ. 'REAC_GEOM')

        CALL JEVECH('PGEOMER','L',IGEOM)

        IF ( REACTU ) THEN
           CALL TECACH('ONN','PDEPLMR',1,IDEPLM,IRET)
           IF ( IRET .NE. 0 ) THEN
               DO 100 I = 1,14
                  UTG(I) = 0.D0
100            CONTINUE
           ELSE
               DO 102 I = 1,14
                  UTG(I) = ZR(IDEPLM-1+I)
102            CONTINUE
           ENDIF

           CALL TECACH('ONN','PDEPLPR',1,IDEPLP,IRET)
           IF ( IRET .EQ. 0 ) THEN
               DO 104 I = 1,14
                  UTG(I) = UTG(I) + ZR(IDEPLP-1+I)
104            CONTINUE
           ENDIF

           DO 110 I = 1,3
             XUG(I) = UTG(I) + ZR(IGEOM-1+I)
             XUG(I+3) = UTG(I+7) + ZR(IGEOM-1+I+3)
110        CONTINUE

           CALL VDIFF(3,XUG(4),XUG(1),XD)
           XL2=DDOT(3,XD,1,XD,1)
           XL = SQRT(XL2)
           TET1=DDOT(3,UTG(4),1,XD,1)
           TET2=DDOT(3,UTG(11),1,XD,1)
           TET1 = TET1/XL
           TET2 = TET2/XL
           CALL ANGVX(XD,ALFA1,BETA1)
           GAMMA = ZR(IORIEN+2)
           GAMMA1 = GAMMA + (TET1+TET2)/2.D0
           ANG1(1) = ALFA1
           ANG1(2) = BETA1
           ANG1(3) = GAMMA1
        ELSE
           CALL VDIFF(3,ZR(IGEOM-1+4),ZR(IGEOM),XD)
           XL2=DDOT(3,XD,1,XD,1)
           XL = SQRT(XL2)
           ANG1(1) = ZR(IORIEN+0)
           ANG1(2) = ZR(IORIEN+1)
           ANG1(3) = ZR(IORIEN+2)
        END IF
C          CALL MATROT(ZR(IORIEN),PGL)
        CALL MATROT(ANG1,PGL)
C -------FIN MODIF ---------------------------------------------------

        CALL JEVECH('PCAGNPO','L',ISECT)

C     -- RECUPERATION DES CARACTERISTIQUES DE LA SECTION
        AA    = ZR(ISECT)
        XIY   = ZR(ISECT + 1)
        XIZ   = ZR(ISECT + 2)
        ALFAY = ZR(ISECT + 3)
        ALFAZ = ZR(ISECT + 4)
C     -- PASSAGE DE G (CENTRE DE GRAVITE) A C (CENTRE DE TORSION)
        EY = -ZR(ISECT + 5)
        EZ = -ZR(ISECT + 6)

        CALL JEVECH('PMATERC','L',IMATE)
        CALL MOYTEM('RIGI',NPG,1,'+',TEMP)
        CALL MATELA(ZI(IMATE),' ',1,TEMP,E,NU,R8BID)

        G = E / (2.D0*(1.D0+NU))

        PHIY = E*XIZ*12.D0*ALFAY/ (XL*XL*G*AA)
        PHIZ = E*XIY*12.D0*ALFAZ/ (XL*XL*G*AA)

        XLS2 = 0.5D0 * XL
C     POIDS DES POINTS DE GAUSS
        CO(1) = 5.D0/9.D0
        CO(2) = 8.D0/9.D0
        CO(3) = 5.D0/9.D0

        CALL R8INIR(2*NC,0.D0,FL,1)

        DO 400 KP = 1,3
           CALL JSD1FF(KP,XL,PHIY,PHIZ,D1B)
           KK = ICGP-1+(NBFIB+NCOMP)*(KP-1) + NBFIB

           DO 410 K = 1,2*NC
              DO 420 I = 1,NC
                 FL(K)=FL(K) + XLS2*ZR(KK+I)*D1B(I,K)*CO(KP)
420           CONTINUE
410        CONTINUE
400     CONTINUE

        DO 430 I = 1,2
           FL(7*(I-1)+4)=FL(7*(I-1)+4)-EZ*FL(7*(I-1)+2)+EY*FL(7*(I-1)+3)
430     CONTINUE

        CALL UTPVLG(NNO,NC,PGL,FL,ZR(IVECTU))


      ENDIF

      END
