      SUBROUTINE TE0505 ( OPTION , NOMTE )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
C                          OPTION : 'CHAR_THER_TNL'
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
      REAL*8             DFDX(9),DFDY(9),POIDS,R,TPG,RBID
      REAL*8             ULOC(2,9),DBETA,UL(2,10),JACOB(10)
      REAL*8             BETAA,TPN,BETAI,DUPGDX(9),DUPGDY(9)
      REAL*8             XKPT,XKPTT(9),DTPGDX(9),DTPGDY(9)
      REAL*8             VECT(50),RES(50),DBPGDX(9),DBPGDY(9)
      REAL*8             XR,XRR,XAUX,RR,TPG0,XK1,XK0,PN,PNP1
      INTEGER            KP,I,K,ITEMPS,IVECTT,IFON(3),IGEOM,IMATE
      INTEGER            IVITE,ITEMP,ITEMPI,ILAGRM,ILAGRP,IVERES
      INTEGER            NBVF,JVALF,IDIM
      INTEGER            NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO
      LOGICAL            LTEATT
C DEB ------------------------------------------------------------------
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
C
      CALL JEVECH('PGEOMER','L',IGEOM )
      CALL JEVECH('PMATERC','L',IMATE )
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PTEMPER','L',ITEMP )
      CALL JEVECH('PTEMPEI','L',ITEMPI)
      CALL JEVECH('PLAGRM ','L',ILAGRM)
      CALL JEVECH('PVITESR','L',IVITE )
      CALL JEVECH('PLAGRP ','E',ILAGRP)
      CALL JEVECH('PVECTTR','E',IVECTT)
      CALL JEVECH('PRESIDU','E',IVERES)
C
      CALL NTFCMA(ZI(IMATE),IFON)
      NBVF  = ZI(IFON(1))
      JVALF = ZI(IFON(1) + 2)
      XR   = 0.D0
      DO 22 I = 1 , NBVF
      XAUX = ZR(JVALF + I - 1)
      CALL RCFODI(IFON(1), XAUX, RBID, XRR)
      IF (XRR .GT. XR) THEN
      XR = XRR
      END IF
 22   CONTINUE
      RR  = 0.6D0/XR
C
      K = 0
      DO 10 I = 1,NNO
         DO 20 IDIM =1,2
            K = K+1
            ULOC(IDIM,I) = ZR(IVITE+K-1)
   20    CONTINUE
   10 CONTINUE
C
      DO 101 KP=1,NPG
        UL(1,KP) = 0.D0
        UL(2,KP) = 0.D0
        K=(KP-1)*NNO
        CALL DFDM2D ( NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS )
        R      = 0.D0
        TPG    = 0.D0
        TPG0   = 0.D0
        DTPGDX(KP) = 0.D0
        DTPGDY(KP) = 0.D0
C
        DO 102 I=1,NNO
          R         = R        + ZR(IGEOM+2*(I-1)) *ZR(IVF+K+I-1)
          TPG       = TPG      + ZR(ITEMPI+I-1)    *ZR(IVF+K+I-1)
          TPG0      = TPG0     + ZR(ITEMP +I-1)    *ZR(IVF+K+I-1)
          UL(1,KP)  = UL(1,KP) + ULOC(1,I)         *ZR(IVF+K+I-1)
          UL(2,KP)  = UL(2,KP) + ULOC(2,I)         *ZR(IVF+K+I-1)
          DTPGDX(KP)= DTPGDX(KP) + ZR(ITEMPI+I-1)  *DFDX(I)
          DTPGDY(KP)= DTPGDY(KP) + ZR(ITEMPI+I-1)  *DFDY(I)
102     CONTINUE
C
        IF ( LTEATT(' ','AXIS','OUI') ) POIDS = POIDS*R
        CALL RCFODE(IFON(2), TPG , XK1, XKPT)
        CALL RCFODE(IFON(2), TPG0, XK0, XKPT)
        PN   = ZR(ILAGRM + KP - 1)
        CALL RCFODI(IFON(1), PN, BETAA, DBETA)
        PNP1 = PN + ((TPG - BETAA)*RR)
        ZR(ILAGRP + KP - 1) = PNP1
        VECT(KP)  = PNP1
        JACOB(KP) = POIDS
        XKPTT(KP) = XK1 - XK0
C
 101    CONTINUE
        CALL PROJET(2, NPG, NNO, VECT, RES)
C
        DO 110 KP = 1,NPG
          K = (KP -1)*NNO
          CALL DFDM2D ( NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS )
          DBPGDX(KP) = 0.D0
          DBPGDY(KP) = 0.D0
          DUPGDX(KP) = 0.D0
          DUPGDY(KP) = 0.D0
C
          DO 120 I = 1, NNO
            DUPGDX(KP) = DUPGDX(KP) + RES(I)*DFDX(I)
            DUPGDY(KP) = DUPGDY(KP) + RES(I)*DFDY(I)
            TPN        = RES(I)
            CALL RCFODI(IFON(1), TPN, BETAI, RBID)
            DBPGDX(KP) = DBPGDX(KP) + BETAI*DFDX(I)
            DBPGDY(KP) = DBPGDY(KP) + BETAI*DFDY(I)
 120      CONTINUE
C
 110    CONTINUE
C
         DO 103 KP = 1,NPG
         K=(KP-1)*NNO
         CALL DFDM2D (NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS )
C
         DO 104 I = 1, NNO
        ZR(IVERES+I-1) = ZR(IVERES+I-1) + JACOB(KP)*ZR(IVF+K+I-1)*
     &     ( RR  * (UL(1,KP)*DBPGDX(KP) + UL(2,KP)*DBPGDY(KP) )
     &           - (UL(1,KP)*DUPGDX(KP) + UL(2,KP)*DUPGDY(KP) )  )
     &           + JACOB(KP)*XKPTT(KP)*
     &             (DFDX(I)*DTPGDX(KP)+DFDY(I)*DTPGDY(KP))
C
 104    CONTINUE
C
 103    CONTINUE
C
C FIN ------------------------------------------------------------------
      END
