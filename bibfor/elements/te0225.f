      SUBROUTINE TE0225(OPTION,NOMTE)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*16 OPTION,NOMTE
C ......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/01/2013   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
C                          COQUE 1D
C                          OPTION : 'CHAR_MECA_TEMP_R'
C                          ELEMENT: MECXSE3,METCSE3,METDSE3
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................

      INTEGER I,IP,K,KP,IGEOM,ICACO,IVECTT,IMATE
      INTEGER IVF,IDFDK,NNO,NPG,JCOOPG,J,NBRES,JDFD2
C-----------------------------------------------------------------------
      INTEGER IPOIDS ,IRET1 ,IRET2 ,IRET3 ,IRET4 ,JGANO ,NDIM
      INTEGER NNOS
      REAL*8 TREF
C-----------------------------------------------------------------------
      PARAMETER (NBRES=3)
      CHARACTER*16 PHENOM
      CHARACTER*8 NOMRES(NBRES)
      CHARACTER*4 FAMI
      INTEGER ICODRE(NBRES)
      REAL*8 VALRES(NBRES),DFDX(3),R,COUR,JAC,COSA,SINA
      REAL*8 TPG1,TPG2,TPG3,TPG,ZERO,UN,DEUX,X3
      REAL*8 H,EPSTHE,NU,COEF,AXIS

      DATA ZERO,UN,DEUX/0.D0,1.D0,2.D0/
C     ------------------------------------------------------------------
      FAMI = 'RIGI'
      CALL ELREF5(' ',FAMI,NDIM,NNO,NNOS,NPG,IPOIDS,JCOOPG,IVF,IDFDK,
     &            JDFD2,JGANO)

      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PCACOQU','L',ICACO)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PVECTUR','E',IVECTT)
C     TEMPERATURE DE REFERENCE
      CALL RCVARC(' ','TEMP','REF',FAMI,1,1,TREF,IRET1)

C --- RECUPERATION DE LA NATURE DU MATERIAU DANS PHENOM
C     -------------------------------------------------
      CALL RCCOMA(ZI(IMATE),'ELAS',1,PHENOM,ICODRE)

      IF (PHENOM.EQ.'ELAS') THEN

C ==== CALCUL ISOTROPE HOMOGENE =====

        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
        NOMRES(3) = 'ALPHA'

        H = ZR(ICACO)
        AXIS = ZERO
        IF (NOMTE.EQ.'MECXSE3 ') AXIS = UN

C     ** BOUCLE CONCERNANT LES POINTS DE GAUSS **************

        DO 40 KP = 1,NPG
          K = (KP-1)*NNO
          CALL DFDM1D(NNO,ZR(IPOIDS+KP-1),ZR(IDFDK+K),ZR(IGEOM),DFDX,
     &                COUR,JAC,COSA,SINA)
          R = ZERO
          TPG = ZERO
          CALL RCVARC(' ','TEMP','+',FAMI,KP,1,TPG2,IRET2)
          CALL RCVARC(' ','TEMP','+',FAMI,KP,2,TPG1,IRET3)
          CALL RCVARC(' ','TEMP','+',FAMI,KP,3,TPG3,IRET4)
          DO 10 I = 1,NNO
            R = R + ZR(IGEOM+2*I-2)*ZR(IVF+K+I-1)
   10     CONTINUE
          IF (NOMTE.EQ.'MECXSE3 ') JAC = JAC*R

C---- UTILISATION DE 4 POINTS DE GAUSS DANS L'EPAISSEUR
C---- COMME POUR LA LONGUEUR

          DO 30 IP = 1,NPG
            X3 = ZR(JCOOPG+IP-1)
            TPG = TPG1* (UN-X3**2) + X3* (TPG3* (UN+X3)-TPG2* (UN-X3))/
     &            DEUX
            CALL RCVALB('RIGI',1,1,'+',ZI(IMATE),' ','ELAS',
     &                 1,'TEMP',TPG,2,NOMRES,
     &                 VALRES, ICODRE,1)
            CALL RCVALB('RIGI',1,1,'+',ZI(IMATE),' ','ELAS',
     &                  1,'TEMP',TPG,1,NOMRES(3),
     &                  VALRES(3),ICODRE(3),0)
            IF (((IRET1+IRET2+IRET3+IRET4).GE.1).AND.
     &              (ICODRE(3).EQ.0)) THEN
              CALL U2MESS('F','CALCULEL_15')
            ELSEIF (ICODRE(3).NE.0) THEN
              EPSTHE = 0.D0
            ELSE
               EPSTHE = (TPG-TREF)*VALRES(3)
            ENDIF
            NU = VALRES(2)
            COEF = VALRES(1)*JAC*EPSTHE*ZR(IPOIDS+IP-1)* (H/DEUX)
            IF (NOMTE.NE.'METCSE3 ') COEF = COEF/ (UN-NU)

            DO 20 I = 1,NNO
              J = 3* (I-1)
              ZR(IVECTT+J) = ZR(IVECTT+J) +
     &                       COEF* (AXIS*ZR(IVF+K+I-1)/R-DFDX(I)*SINA)
              ZR(IVECTT+J+1) = ZR(IVECTT+J+1) + COEF*DFDX(I)*COSA
              ZR(IVECTT+J+2) = ZR(IVECTT+J+2) -
     &                         COEF*X3*H/DEUX* (AXIS*ZR(IVF+K+I-1)*SINA/
     &                         R-DFDX(I))
   20       CONTINUE
   30     CONTINUE
   40   CONTINUE
      ELSE
C  ==== CALCUL ANISOTROPE  =====
        CALL U2MESS('F','ELEMENTS3_49')
      END IF
      END
