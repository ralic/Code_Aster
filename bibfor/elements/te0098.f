      SUBROUTINE TE0098(OPTION,NOMTE)
      IMPLICIT   NONE
      CHARACTER*16 OPTION,NOMTE
C ......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 24/10/2011   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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

C     FONCTION REALISEE:  CALCUL DES CHAMELEM AUX NOEUDS A PARTIR DES
C     VALEURS AUX POINTS DE GAUSS ( SIEF_ELNO VARI_ELNO )
C     ELEMENTS 2D ET 2D AXISYMETRIQUE

C IN  OPTION : OPTION DE CALCUL
C IN  NOMTE  : NOM DU TYPE ELEMENT
C ......................................................................

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

      INTEGER ICHG,ICHN,IPOIDS,IVF,JTAB(7),JTAB1(7),JTAB2(7),LGPG1,LGPG2
      INTEGER NNO,NPG,NCMP,NNOS,IDFDE,JGANO,NDIM,IRET
C     ------------------------------------------------------------------

      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)

C
      IF (OPTION.EQ.'SIEF_ELNO  ') THEN
C     ---------------------------------------
C        NCMP = 4 + NCMP2
C        CALL JEVECH('PCONTRR','L',ICHG)
        CALL TECACH('OOO','PCONTRR',3,JTAB,IRET)
        NCMP=JTAB(2)/JTAB(3)
C       POUR LES ELEMENTS SOUS-INTEGRES LE NOMBRE
C       DE COMPOSANTE EST 4
        IF (NOMTE(5:7).EQ.'QS4') THEN
           NCMP=4
        ENDIF
        ICHG=JTAB(1)
        CALL JEVECH('PSIEFNOR','E',ICHN)

      ELSE IF (OPTION.EQ.'VARI_ELNO  ') THEN
C     ---------------------------------------

        CALL JEVECH('PVARIGR','L',ICHG)
        CALL JEVECH('PVARINR','E',ICHN)

        CALL TECACH('OON','PVARIGR',7,JTAB1,IRET)
        CALL TECACH('OON','PVARINR',7,JTAB2,IRET)
        LGPG1= MAX(JTAB1(6),1)*JTAB1(7)
        LGPG2= MAX(JTAB2(6),1)*JTAB2(7)

        CALL ASSERT(LGPG1.EQ.LGPG2)

        NCMP = LGPG2

      END IF
C
C --- CHAMELEM(NOEUD) = P * CHAMELEM(GAUSS)
C
      CALL PPGAN2 ( JGANO, 1, NCMP, ZR(ICHG), ZR(ICHN))

      END
