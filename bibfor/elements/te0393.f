      SUBROUTINE TE0393(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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

      IMPLICIT NONE

      INCLUDE 'jeveux.h'
      CHARACTER*16 OPTION,NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES FORCES NODALES DE MECA_POU_D_T_GD
C                          OPTION : 'FORC_NODA' OU 'REFE_FORC_NODA'

C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................

      CHARACTER*8 ELREFE
      REAL*8 EN(3,2),ENPRIM(3,2),FINT(6,3),Y0(3),X00(3,3),X0K(3,3),
     &       QIK(3,3),X0PG(3),QIG(3),ROT(3,3),ROT0(3,3),ROTABS(3,3),
     &       GN(3),GM(3),PN(3),PM(3),ZERO,UN,UNSURJ,PJACOB,AJACOB

      REAL*8  FORREF,MOMREF
      INTEGER NNO,NC,INO,I,NDIM,NNOS,NPG,IPOIDS,IVF,IDFDK,JGANO,KP
      INTEGER NE,IC,KC,K0,K1,K2,ICO
      INTEGER IVECTU,IDEPM,IGEOM,IDEPDE,ISIGMA,LORIEN,JEFINT
      INTEGER IFINT

      PARAMETER (ZERO=0.0D0,UN=1.0D0)
C ----------------------------------------------------------------------


      IF(OPTION.EQ.'REFE_FORC_NODA')THEN
         NNO = 2
         NC  = 6
         CALL TEREFE('MOMENT_REFE','MECA_POUTRE',MOMREF)
         CALL TEREFE('EFFORT_REFE','MECA_POUTRE',FORREF)
         CALL JEVECH ( 'PVECTUR','E',IVECTU)
         DO 201 INO=1,NNO
            DO 203  I=1,3
               ZR(IVECTU+(INO-1)*NC+I-1)=FORREF
203         CONTINUE
            DO 202 I=4,NC
               ZR(IVECTU+(INO-1)*NC+I-1)=MOMREF
202         CONTINUE
201      CONTINUE

      ELSEIF(OPTION.EQ.'FORC_NODA')THEN
         CALL ELREF1(ELREFE)

C        PARAMETRES EN ENTREE
         CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,
     &               IVF,IDFDK,JGANO)

         ICO = 0
         DO 20 KP = 1,NPG
            DO 10 NE = 1,NNO
               ICO = ICO + 1
               EN(NE,KP) = ZR(IVF-1+ICO)
               ENPRIM(NE,KP) = ZR(IDFDK-1+ICO)
10          CONTINUE
20       CONTINUE

C        CALL JEVECH('PTEMPPR','L',ITEMPR)
         CALL JEVECH('PGEOMER','L',IGEOM)
         CALL JEVECH('PDEPLMR','L',IDEPM)
         CALL JEVECH('PDEPLPR','L',IDEPDE)
         CALL JEVECH('PCONTMR','L',ISIGMA)

C        --- RECUPERATION DES ORIENTATIONS INITIALES Y0(1), Y0(2), Y0(3)
         CALL JEVECH('PCAORIE','L',LORIEN)
         Y0(1) = ZR(LORIEN)
         Y0(2) = ZR(LORIEN+1)
         Y0(3) = ZR(LORIEN+2)

C        PARAMETRES EN SORTIE
         CALL JEVECH('PVECTUR','E',JEFINT)
         DO 40 NE = 1,NNO
            DO 30 KC = 1,6
               FINT(KC,NE) = ZERO
30          CONTINUE
40       CONTINUE

C       DO 21 NE=1,NNO
C         TEMPN(NE) = ZR(ITEMPR-1+NE)
C21      CONTINUE

         K0 = IGEOM - 1
         K1 = IDEPM - 1
         K2 = IDEPDE - 1

         DO 70 NE = 1,NNO
            DO 50 KC = 1,3
               K0 = K0 + 1
               K1 = K1 + 1
               K2 = K2 + 1
               X00(KC,NE) = ZR(K0)
               X0K(KC,NE) = ZR(K0) + ZR(K1) + ZR(K2)
50          CONTINUE
            DO 60 KC = 1,3
               K1 = K1 + 1
               K2 = K2 + 1
               QIK(KC,NE) = ZR(K1) + ZR(K2)
60          CONTINUE
70       CONTINUE

C        BOUCLE SUR LES POINTS DE GAUSS
         DO 120 KP = 1,NPG
            CALL GDJRG0(KP,NNO,ENPRIM,X00,Y0,AJACOB,ROT0)
            PJACOB = ZR(IPOIDS-1+KP)*AJACOB

            DO 80 IC = 1,3
               X0PG(IC) = ZERO
               QIG(IC) = ZERO
80          CONTINUE
C           TEMPG = ZERO
            UNSURJ = UN/AJACOB
            DO 100 IC = 1,3
               DO 90 NE = 1,NNO
                  X0PG(IC) = X0PG(IC) + UNSURJ*ENPRIM(NE,KP)*X0K(IC,NE)
                  QIG(IC) = QIG(IC) + EN(NE,KP)*QIK(IC,NE)
90             CONTINUE
100         CONTINUE
C         DO 45 NE=1,NNO
C          TEMPG = TEMPG + EN(NE,KP)*TEMPN(NE)
C45       CONTINUE

            CALL MAROTA(QIG,ROT)
            CALL PROMAT(ROT,3,3,3,ROT0,3,3,3,ROTABS)
            DO 110 IC = 1,3
               GN(IC) = ZR(ISIGMA-1+6* (KP-1)+IC)
               GM(IC) = ZR(ISIGMA+2+6* (KP-1)+IC)
110         CONTINUE
            CALL PROMAT(ROTABS,3,3,3,GN,3,3,1,PN)
            CALL PROMAT(ROTABS,3,3,3,GM,3,3,1,PM)

            CALL GDFINT(KP,NNO,AJACOB,PJACOB,EN,ENPRIM,X0PG,PN,PM,FINT)

120      CONTINUE
C        FIN DE BOUCLE SUR LES POINTS DE GAUSS
         IFINT = JEFINT - 1
         DO 140 NE = 1,NNO
            DO 130 KC = 1,6
               IFINT = IFINT + 1
               ZR(IFINT) = FINT(KC,NE)
130         CONTINUE
140      CONTINUE
      ENDIF
C
      END
