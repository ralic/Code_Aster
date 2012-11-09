      SUBROUTINE TE0295(OPTION,NOMTE)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C      TOLE CRP_20

      IMPLICIT NONE

C ----------------------------------------------------------------------
C FONCTION REALISEE:  CALCUL DES FACTEURS D'INTENSITÉS DE CONTRAINTES
C                     A PARTIR DE LA FORME BILINEAIRE SYMETRIQUE G ET
C                      DES DEPLACMENTS SINGULIERS EN FOND DE FISSURE

C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C.......................................................................
C
      INCLUDE 'jeveux.h'

      INTEGER ICODRE(3)
      CHARACTER*4  FAMI
      CHARACTER*8  NOMRES(3),NOMPAR(4)
      CHARACTER*16 NOMTE,OPTION,PHENOM, COMPOR(4), VALK
C
      REAL*8   EPSI,R8PREM
      REAL*8   DFDI(60),F(3,3),EPS(6),FNO(81),E1(3),E2(3),E3(3)
      REAL*8   DUDM(3,4),DFDM(3,4),DTDM(3,4),DER(4)
      REAL*8   U1L(3),U2L(3),U3L(3), DFVDM(3,4)
      REAL*8   DU1DM(3,4),DU2DM(3,4),DU3DM(3,4)
      REAL*8   P(3,3),INVP(3,3)
      REAL*8   COURB(3,3,3)
      REAL*8   RHO,OM,OMO,RBID,E,NU,ALPHA,TREF
      REAL*8   THET,TG(27),TPN(20),TGDM(3),TTRG,LA,MU, KA
      REAL*8   XG,YG,ZG,FF
      REAL*8   C1,C2,C3, RG, PHIG
      REAL*8   VALRES(3),DEVRES(3)
      REAL*8   COEFF,COEFF3
      REAL*8   GUV,GUV1,GUV2,GUV3,K1,K2,K3,G,POIDS
      REAL*8   NORME,K3A,TTRGV,TGVDM(3)
      REAL*8   VALPAR(4),LSNG,LSTG
C
      INTEGER  IPOIDS,IVF,IDFDE,NNO,KP,NPG,COMPT,IER, NNOS
      INTEGER  JGANO,IEPSR,ICOMP,ISIGI,IBALO,ICOUR
      INTEGER  IGEOM,ITHET, IGTHET, IROTA,IPESA,IDEPL,IRET
      INTEGER  IMATE,IFORC,IFORF,ITEMPS,K,I,J,KK,L,NDIM,INO
      INTEGER   JLSN,JLST,IRET1,IRET2,TRIGI,TNOEU

      LOGICAL   LCOUR,FONC


C DEB ------------------------------------------------------------------

      CALL JEMARQ()
      EPSI = R8PREM()

      FAMI = 'RIGI'
      CALL ELREF4(' ',FAMI,NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
      CALL JEVECH('PTHETAR','L',ITHET)
      G = 0.D0
      K1= 0.D0
      K2= 0.D0
      K3= 0.D0
      COEFF=1.D0
      COEFF3=1.D0
      CALL JEVECH('PGTHETA','E',IGTHET)

C - PAS DE CALCUL DE G POUR LES ELEMENTS OU LA VALEUR DE THETA EST NULLE
      COMPT = 0
      DO 10 I = 1,NNO
        THET = 0.D0
        DO 11 J = 1,NDIM
          THET = THET + ABS(ZR(ITHET+NDIM*(I-1)+J-1))
 11     CONTINUE
        IF (THET.LT.EPSI) COMPT = COMPT + 1
 10   CONTINUE
      IF (COMPT.EQ.NNO) GOTO 9999
C
C RECUPERATION CHARGE, MATER...
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PDEPLAR','L',IDEPL)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PCOMPOR','L',ICOMP)
      CALL JEVECH('PBASLOR','L',IBALO)
      CALL JEVECH('PCOURB','L',ICOUR)
      CALL JEVECH('PLSN','L',JLSN)
      CALL JEVECH('PLST','L',JLST)

      IF (OPTION.EQ.'CALC_K_G_F') THEN
        FONC = .TRUE.
        CALL JEVECH('PFFVOLU','L',IFORF)
        CALL JEVECH('PTEMPSR','L',ITEMPS)
        NOMPAR(1) = 'X'
        NOMPAR(2) = 'Y'
        VALPAR(NDIM+1) = ZR(ITEMPS)
        IF (NDIM.EQ.2) THEN
          NOMPAR(3) = 'INST'
        ELSEIF (NDIM.EQ.3) THEN
          NOMPAR(3) = 'Z'
          NOMPAR(4) = 'INST'
        ENDIF
        CALL TECACH('ONN','PEPSINF',1,IEPSR,IRET)
      ELSE
        FONC =.FALSE.
        CALL JEVECH('PFRVOLU','L',IFORC)
        CALL TECACH('ONN','PEPSINR',1,IEPSR,IRET)
      ENDIF

      DO 20 I = 1,4
        COMPOR(I) = ZK16(ICOMP+I-1)
 20   CONTINUE


       IF ((COMPOR(3) .EQ. 'GROT_GDEP')
     &    .OR. (COMPOR(4) (1:9) .EQ. 'COMP_INCR')
     &    .OR. (COMPOR(1) .NE. 'ELAS')) THEN
       CALL U2MESS('F','RUPTURE1_24')
       END IF

      ISIGI=0
      CALL TECACH('ONN','PPESANR',1,IPESA,IRET)
      CALL TECACH('ONN','PROTATR',1,IROTA,IRET)
      IF (.NOT.FONC) CALL TECACH('ONN','PSIGINR',1,ISIGI,IRET)
      IF (ISIGI.NE.0) THEN
        VALK='G_BILI'
        CALL U2MESK('F','RUPTURE1_13',1,VALK)
      END IF

      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      NOMRES(3) = 'ALPHA'

      CALL RCVARC(' ','TEMP','REF','RIGI',1,1,TREF,TRIGI)
      TNOEU=TRIGI
      DO 645 KP = 1,NPG
        CALL RCVARC(' ','TEMP','+','RIGI',KP,1,TG(KP),IRET1)
        TRIGI=TRIGI+IRET1
  645 CONTINUE

      DO 646 KP = 1,NNO
        CALL RCVARC(' ','TEMP','+','NOEU',KP,1,TPN(KP),IRET2)
        TNOEU=TNOEU+IRET2
  646 CONTINUE

C - RECUPERATION DES CHARGES ET DEFORMATIONS INITIALES ----------------
C
      IF (FONC) THEN
        DO 50 I = 1,NNO
          DO 30 J = 1,NDIM
            VALPAR(J) = ZR(IGEOM+NDIM*(I-1)+J-1)
 30       CONTINUE
          DO 40 J = 1,NDIM
            KK = NDIM*(I-1) + J
            CALL FOINTE('FM',ZK8(IFORF+J-1),NDIM+1,NOMPAR,VALPAR,
     &                                               FNO(KK),IER)
 40       CONTINUE
 50     CONTINUE
      ELSE
        DO 8000 I = 1,NNO
          DO 6000 J = 1,NDIM
            FNO(NDIM*(I-1)+J) = ZR(IFORC+NDIM*(I-1)+J-1)
 6000     CONTINUE
 8000   CONTINUE
      END IF
      IF ((IPESA.NE.0).OR.(IROTA.NE.0)) THEN
        CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,ICODRE)
        CALL RCVALB('RIGI',1,1,'+',ZI(IMATE),' ',PHENOM,
     &              1,' ',RBID,1,'RHO',RHO,
     &              ICODRE,1)
        IF (IPESA.NE.0) THEN
          DO 60 I=1,NNO
            DO 61 J=1,NDIM
              KK = NDIM*(I-1)+J
              FNO(KK)=FNO(KK)+RHO*ZR(IPESA)*ZR(IPESA+J)
 61         CONTINUE
 60       CONTINUE
        ENDIF
        IF (IROTA.NE.0) THEN
          OM = ZR(IROTA)
          DO 70 I=1,NNO
            OMO = 0.D0
            DO 71 J=1,NDIM
              OMO = OMO + ZR(IROTA+J)* ZR(IGEOM+NDIM*(I-1)+J-1)
 71        CONTINUE
            DO 72 J=1,NDIM
              KK = NDIM*(I-1)+J
              FNO(KK)=FNO(KK)+RHO*OM*OM*(ZR(IGEOM+KK-1)-OMO*ZR(IROTA+J))
 72         CONTINUE
 70       CONTINUE
        ENDIF
      ENDIF


C-----------------------------------------------------------------------
C     BOUCLE SUR LES POINTS DE GAUSS
      DO 100 KP=1,NPG

        L  = (KP-1)*NNO
        XG = 0.D0
        YG = 0.D0
        ZG = 0.D0
        LSNG=0.D0
        LSTG=0.D0
        DO 110 I=1,3
          TGDM(I) = 0.D0
          TGVDM(I) = 0.D0
          DO 111 J=1,4
            DUDM(I,J) = 0.D0
            DU1DM(I,J)= 0.D0
            DU2DM(I,J)= 0.D0
            DU3DM(I,J)= 0.D0
            DTDM(I,J) = 0.D0
            DFDM(I,J) = 0.D0
            DFVDM(I,J) = 0.D0
 111      CONTINUE
 110    CONTINUE

C   CALCUL DES ELEMENTS CINEMATIQUES (MATRICES F ET E) EN UN PT DE GAUSS

        CALL NMGEOM (NDIM,NNO,.FALSE.,.FALSE.,ZR(IGEOM),KP,
     &               IPOIDS,IVF,IDFDE,
     &               ZR(IDEPL),.TRUE.,POIDS,DFDI,F,EPS,RBID)

C - CALCULS DES GRADIENTS DE U (DUDM),THETA (DTDM) ET FORCE(DFDM)
C   DU GRADIENT DE TEMPERATURE AUX POINTS DE GAUSS (TGDM)
C   ET LEVEL SETS

        DO 120 I=1,NNO
          DER(1) = DFDI(I)
          DER(2) = DFDI(I+NNO)
          DER(3) = DFDI(I+2*NNO)
          DER(4) = ZR(IVF+L+I-1)

          XG = XG + ZR(IGEOM-1+NDIM*(I-1)+1)*DER(4)
          YG = YG + ZR(IGEOM-1+NDIM*(I-1)+2)*DER(4)
          ZG = ZG + ZR(IGEOM-1+NDIM*(I-1)+3)*DER(4)

          LSNG = LSNG + ZR(JLSN-1+I) * DER(4)
          LSTG = LSTG + ZR(JLST-1+I) * DER(4)

          IF (TNOEU.EQ.0) THEN
            DO 121 J=1,NDIM
              TGDM(J) = TGDM(J) + TPN(I)*DER(J)
121         CONTINUE
          ENDIF
          DO 122 J=1,NDIM
            DO 123 K=1,NDIM
              DUDM(J,K) = DUDM(J,K) + ZR(IDEPL+NDIM*(I-1)+J-1)*DER(K)
              DTDM(J,K) = DTDM(J,K) + ZR(ITHET+NDIM*(I-1)+J-1)*DER(K)
              DFDM(J,K) = DFDM(J,K) + FNO(NDIM*(I-1)+J)*DER(K)
123       CONTINUE
            DUDM(J,4) = DUDM(J,4) + ZR(IDEPL+NDIM*(I-1)+J-1)*DER(4)
            DTDM(J,4) = DTDM(J,4) + ZR(ITHET+NDIM*(I-1)+J-1)*DER(4)
            DFDM(J,4) = DFDM(J,4) + FNO(NDIM*(I-1)+J)*DER(4)
122       CONTINUE
120     CONTINUE

C       RECUPEATION DES DONNEES MATERIAUX
        IF (TRIGI.EQ.0) THEN
          TTRG = TG(KP) - TREF
        ELSE
          TTRG=0.D0
        ENDIF
        TTRGV = 0.D0
        CALL RCVAD2 (FAMI,KP,1,'+',ZI(IMATE),'ELAS',
     &               3,NOMRES,VALRES,DEVRES,ICODRE)
        IF (ICODRE(3).NE.0) THEN
          VALRES(3)= 0.D0
          DEVRES(3)= 0.D0
        ENDIF
        E     = VALRES(1)
        NU    = VALRES(2)
        ALPHA = VALRES(3)
        K3A = ALPHA * E / (1.D0-2.D0*NU)

        LA = NU*E/((1.D0+NU)*(1.D0-2.D0*NU))
        MU = E/(2.D0*(1.D0+NU))
C       EN DP
        KA=3.D0-4.D0*NU
        COEFF=E/(1.D0-NU*NU)
        COEFF3=2.D0 * MU
C       EN CP
C       KA=(3.D0-NU)/(1.D0+NU)
C       COEFF=E
C       COEFF3=2.D0 * MU

        C1 = LA + 2.D0 * MU
        C2 = LA
        C3 = MU

C       BASE LOCALE ASSOCIÉE AU POINT DE GAUSS KP
C       (E1=GRLT,E2=GRLN,E3=E1^E2)
        DO 124 I=1,3
          E1(I)=0.D0
          E2(I)=0.D0
          DO 130 INO=1,NNO
            FF=ZR(IVF-1+NNO*(KP-1)+INO)
            E1(I) = E1(I)+ZR(IBALO-1+9*(INO-1)+I+3)* FF
            E2(I) = E2(I)+ZR(IBALO-1+9*(INO-1)+I+6)* FF
 130      CONTINUE
 124    CONTINUE

C       NORMALISATION DE LA BASE
        CALL NORMEV(E1,NORME)
        CALL NORMEV(E2,NORME)
        CALL PROVEC(E1,E2,E3)
C
C       CALCUL DE LA MATRICE DE PASSAGE P TQ 'GLOBAL' = P * 'LOCAL'
        DO 125 I=1,3
          P(I,1)=E1(I)
          P(I,2)=E2(I)
          P(I,3)=E3(I)
 125    CONTINUE

C       CALCUL DE L'INVERSE DE LA MATRICE DE PASSAGE : INV=TRANSPOSE(P)
        DO 126 I=1,3
          DO 127 J=1,3
            INVP(I,J)=P(J,I)
 127      CONTINUE
 126    CONTINUE

C       RECUPERATION DU TENSEUR DE COURBURE
        DO 128 I=1,3
          DO 129 J=1,3
            COURB(I,1,J)=ZR(ICOUR-1+3*(I-1)+J)
            COURB(I,2,J)=ZR(ICOUR-1+3*(I+3-1)+J)
            COURB(I,3,J)=ZR(ICOUR-1+3*(I+6-1)+J)

 129      CONTINUE
 128    CONTINUE
C       PRISE EN COMPTE DE LA COURBURE
        LCOUR=.TRUE.

C       COORDONNÉES POLAIRES DU POINT
        RG=SQRT(LSNG**2+LSTG**2)

        IF (RG.GT.R8PREM()) THEN
C         LE POINT N'EST PAS SUR LE FOND DE FISSURE
          PHIG = SIGN(1.D0,LSNG) * ABS(ATAN2(LSNG,LSTG))
          IRET=1
        ELSE
C         LE POINT EST SUR LE FOND DE FISSURE :
C         L'ANGLE N'EST PAS DÉFINI, ON LE MET À ZÉRO
C         ON NE FERA PAS LE CALCUL DES DÉRIVÉES
          PHIG=0.D0
          IRET=0
        ENDIF
C
C       ON A PAS PU CALCULER LES DERIVEES DES FONCTIONS SINGULIERES
C       CAR ON SE TROUVE SUR LE FOND DE FISSURE
        CALL ASSERT(IRET.NE.0)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       ------------------------------------------------
C       CALCUL DES CHAMPS AUXILIAIRES ET DE LEURS DERIVEES
C       -----------------------------------------------------

C       PRISE EN COMPTE DE LA COURBURE
        LCOUR=.TRUE.
C       RECUPERATION DU TENSEUR DE COURBURE
        CALL JEVECH('PCOURB','L',ICOUR)
        DO 500 I=1,NDIM
          DO 501 J=1,NDIM
            COURB(I,1,J)=ZR(ICOUR-1+NDIM*(I-1)+J)
            COURB(I,2,J)=ZR(ICOUR-1+NDIM*(I+3-1)+J)
            COURB(I,3,J)=ZR(ICOUR-1+NDIM*(I+6-1)+J)
 501      CONTINUE
 500    CONTINUE


        CALL CHAUXI(NDIM,MU,KA,RG,PHIG,INVP,LCOUR,COURB,
     &              DU1DM,DU2DM,DU3DM,U1L,U2L,U3L)

C-----------------------------------------------------------------------
C       CALCUL DE G, K1, K2, K2 AU POINT DE GAUSS
C-----------------------------------------------------------------------

        GUV = 0.D0
        CALL GBIL3D(DUDM,DUDM,DTDM,DFDM,DFDM,TGDM,TGDM,
     &              TTRG,TTRG,POIDS,C1,C2,C3,K3A,0.D0,0.D0,GUV)
        G = G + GUV
C
        GUV1 = 0.D0
        CALL GBIL3D(DUDM,DU1DM,DTDM,DFDM,DFVDM,TGDM,TGVDM,
     &              TTRG,TTRGV,POIDS,C1,C2,C3,K3A,0.D0,0.D0,GUV1)
        K1 = K1 + GUV1
C
        GUV2 = 0.D0
        CALL GBIL3D(DUDM,DU2DM,DTDM,DFDM,DFVDM,TGDM,TGVDM,
     &              TTRG,TTRGV,POIDS,C1,C2,C3,K3A,0.D0,0.D0,GUV2)
        K2 = K2 + GUV2
C
        GUV3 = 0.D0
        CALL GBIL3D(DUDM,DU3DM,DTDM,DFDM,DFVDM,TGDM,TGVDM,
     &              TTRG,TTRGV,POIDS,C1,C2,C3,K3A,0.D0,0.D0,GUV3)
        K3 = K3 + GUV3

100   CONTINUE

9999  CONTINUE

      K1 = K1 * COEFF
      K2 = K2 * COEFF
      K3 = K3 * COEFF3
C
      ZR(IGTHET)    = G
      ZR(IGTHET+1) = K1 / SQRT(COEFF)
      ZR(IGTHET+2) = K2 / SQRT(COEFF)
      ZR(IGTHET+3) = K3 / SQRT(COEFF3)
      ZR(IGTHET+4) = K1
      ZR(IGTHET+5) = K2
      ZR(IGTHET+6) = K3

      CALL JEDEMA()

C FIN ------------------------------------------------------------------
      END
