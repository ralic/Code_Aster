      SUBROUTINE PROJTR (NUTYP,NDIM,NBNO,PROJ,COORDA,COORDB,COORDC,
     &                   COORDP,NORM,COORDM,COEF,OLDJEU,JEU,
     &                   TANG,PRONOR,TANGDF,VLISSA)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/05/2002   AUTEUR PABHHHH N.TARDIEU 
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
C
      IMPLICIT NONE
C
      INTEGER      NUTYP,NBNO,PROJ,PRONOR,TANGDF,NDIM
      REAL*8       COORDA(3),COORDB(3),COORDC(3),COORDP(3),NORM(3)
      REAL*8       COORDM(3),COEF(NBNO),OLDJEU,JEU,VLISSA(9)
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : PROJEC / PROJQU
C ----------------------------------------------------------------------
C
C "PROJECTION" D'UN NOEUD ESCLAVE P SUR UN TRIANGLE MAITRE SUPPOSE PLAN.
C ON UTILISE LA NORMALE AU TRIANGLE.
C
C IN  NUTYP  : TYPE DE MAILLE ( TRIA3 , TRIA6 )
C IN  NDIM   : DIMENSION DU PB
C IN  NBNO   : NOMBRE DE NOEUDS (3 OU 6)
C IN  PROJ   : PROJECTION LINEAIRE (1) OU QUADRATIQUE (2) SUR LA MAILLE
C              OU PAS DE NOUVELLE PROJECTION (0)
C IN  COORDA : COORDONNEES DU SOMMET A DU TRIANGLE
C IN  COORDB : COORDONNEES DU SOMMET B DU TRIANGLE
C IN  COORDC : COORDONNEES DU SOMMET C DU TRIANGLE
C IN  COORDP : COORDONNEES DU NOEUD ESCLAVE P
C OUT NORM   : NORMALE ENTRANTE A LA MAILLE MAITRE
C OUT COORDM : COORDONNEES DE LA "PROJECTION" M
C OUT COEF   : VALEURS EN M DES FONCTIONS DE FORME ASSOCIEES
C              AUX NOEUDS MAITRES
C OUT OLDJEU : JEU AVANT CORRECTION DES PROJECTIONS TOMBANT HORS DE
C              LA MAILLE MAITRE
C OUT JEU    : JEU DANS LA DIRECTION (NORM) DE LA NORMALE ENTRANTE
C              A LA MAILLE MAITRE (PM.NORM)
C
C ----------------------------------------------------------------------
C
      INTEGER K, NTTRI3, NTTRI6
      REAL*8  AP(3),AB(3),AC(3),BC(3),ABAC(3),LONG,SIGNE,COEFD
      REAL*8  DENOM,KSI1,KSI2,KSI3,R8DOT,W(3),AM(3),COORA2(3)
      REAL*8  COORB2(3),COORC2(3),COORM2(3),B2A2(3),C2A2(3)
      REAL*8  LAB,LAC,LBC,ABSAC,ACSAB,ABSBC,ACSBC,LAMBDA,LAMBDB
      REAL*8  TANG(6),COEFA,COEFB,COEFC,COEFF,XNORM(3),LAMBDC
      REAL*8  RBID,R3BID(3),R6BID(6),R1,ALPHA,BETA,GAMMA
      REAL*8  NORME,R8PI,INORM(3)
      CHARACTER*32   JEXNOM
C
C ----------------------------------------------------------------------
C
      IF (PROJ.EQ.0) THEN
        CALL UTMESS ('F','PROJTR_00',
     &               'IL FAUT REACTUALISER LA PROJECTION')
      END IF
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TRIA3') , NTTRI3 )
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TRIA6') , NTTRI6 )
C
C ======================================================================
C          PROJECTION SUR LE TRIA3 OU LE TRIA6 SUPPOSE PLAN
C ======================================================================
C
      IF ((NUTYP.EQ.NTTRI3).OR.(PROJ.EQ.1)) THEN
C
C --- SI PAS DE LISSAGE DES NORMALES
C
         IF(PRONOR.EQ.0.OR.PRONOR.EQ.2) THEN
C
C --- CALCUL DE AP ET DES COTES DU TRIANGLE ABC
C
            LAB = 0.D0
            LAC = 0.D0
            LBC = 0.D0
            DO 10 K = 1,3
               AP(K) = COORDP(K) - COORDA(K)
               AB(K) = COORDB(K) - COORDA(K)
               AC(K) = COORDC(K) - COORDA(K)
               BC(K) = COORDC(K) - COORDB(K)
               LAB = LAB + AB(K)*AB(K)
               LAC = LAC + AC(K)*AC(K)
               LBC = LBC + BC(K)*BC(K)
 10         CONTINUE
C
C --- CALCUL DES COORDONNEES PARAMETRIQUES KSI1 ET KSI2 DE M DANS ABC
C
            CALL PROVEC (AB,AC,ABAC)
            CALL R8COPY (3,ABAC,1,NORM,1)
C
            DENOM = R8DOT (3,ABAC,1,NORM,1)
C
            IF (DENOM.EQ.0) THEN
               CALL UTMESS ('F','PROJTR_01','UNE MAILLE MAITRE A UNE '
     &                      //'SURFACE NULLE')
            END IF
C
            CALL PROVEC (AP,NORM,W)
            KSI1 = - R8DOT (3,W,1,AC,1) / DENOM
            KSI2 =   R8DOT (3,W,1,AB,1) / DENOM
            KSI3 = 1.D0 - KSI1 - KSI2
C
            IF ((KSI1.LT.0.D0).OR.(KSI2.LT.0.D0).OR.
     &           (KSI3.LT.0.D0)) THEN
               ABSAC = R8DOT (3,AB,1,AC,1) / LAC
               ACSAB = R8DOT (3,AB,1,AC,1) / LAB
               ABSBC = R8DOT (3,AB,1,BC,1) / LBC
               ACSBC = R8DOT (3,AC,1,BC,1) / LBC
               CALL AJUSTT (ABSAC,ACSAB,ABSBC,ACSBC,KSI1,KSI2)
            END IF
C
C --- CALCUL DES COORDONNEES CARTESIENNES DE M ("PROJECTION" DE P)
C
            DO 30 K = 1,3
               COORDM(K) = COORDA(K) + KSI1*AB(K) + KSI2*AC(K)
 30         CONTINUE
C
C --- CALCUL DES VALEURS DES FONCTIONS DE FORME DES NOEUDS EN M
C
            LAMBDA  = 1.D0 - KSI1 - KSI2
            LAMBDB  = KSI1
            LAMBDC  = KSI2
            COEF(1) = - LAMBDA
            COEF(2) = - LAMBDB
            COEF(3) = - LAMBDC
C            IF (NUTYP.EQ.NTTRI6) THEN
C               COEF(1) = - LAMBDA * (2*LAMBDA-1)
C               COEF(2) = - LAMBDB * (2*LAMBDB-1)
C               COEF(3) = - LAMBDC * (2*LAMBDC-1)
C               COEF(4) = - 4 * LAMBDA * LAMBDB
C               COEF(5) = - 4 * LAMBDB * LAMBDC
C               COEF(6) = - 4 * LAMBDC * LAMBDA
C            END IF
C
C --- CALCUL DU JEU ET DE LA DIRECTION DE PROJECTION (UNITAIRE)
C
            DO 50 K = 1,3
               NORM(K) = COORDM(K) - COORDP(K)
 50         CONTINUE
            OLDJEU = SQRT(R8DOT(3,NORM,1,NORM,1))
            IF (PRONOR.EQ.0) THEN
               LONG = ABAC(1)**2 + ABAC(2)**2 + ABAC(3)**2
               NORM(1) = - ABAC(1) / SQRT(LONG)
               NORM(2) = - ABAC(2) / SQRT(LONG)
               NORM(3) = - ABAC(3) / SQRT(LONG)
            ELSE
               NORM(1) = COEF(1)*VLISSA(1)  + COEF(2)*VLISSA(4)  +
     &                   COEF(3)*VLISSA(7)
               NORM(2) = COEF(1)*VLISSA(2)  + COEF(2)*VLISSA(5)  +
     &                   COEF(3)*VLISSA(8)
               NORM(3) = COEF(1)*VLISSA(3)  + COEF(2)*VLISSA(6)  +
     &                   COEF(3)*VLISSA(9)
               CALL NORMEV(NORM,NORME)
            ENDIF
            CALL CATANG(3,NORM,TANG,TANGDF)
            JEU = (COORDM(1)-COORDP(1))*NORM(1) +
     &            (COORDM(2)-COORDP(2))*NORM(2) +
     &            (COORDM(3)-COORDP(3))*NORM(3)
C
         ELSE
            LAB = 0.D0
            LAC = 0.D0
            LBC = 0.D0
            DO 11 K = 1,3
               AB(K) = COORDB(K) - COORDA(K)
               AC(K) = COORDC(K) - COORDA(K)
               BC(K) = COORDC(K) - COORDB(K)
               LAB = LAB + AB(K)*AB(K)
               LAC = LAC + AC(K)*AC(K)
               LBC = LBC + BC(K)*BC(K)
 11         CONTINUE
C
C --- CALCUL DES COORDONNEES PARAMETRIQUES KSI1 ET KSI2 DE M DANS ABC
C
            CALL PROVEC (AB,AC,ABAC)
            LONG = ABAC(1)**2 + ABAC(2)**2 + ABAC(3)**2
C
C --- ON STOCKE DANS XNORM LA NORMALE SORTANTE A LA MAILLE MAITRE
C
            XNORM(1) = ABAC(1) / SQRT(LONG)
            XNORM(2) = ABAC(2) / SQRT(LONG)
            XNORM(3) = ABAC(3) / SQRT(LONG)
C
C --- ON STOCKE DANS INORM LA NORMALE AU NOEUD ESCLAVE
C
            INORM(1) = NORM(1)
            INORM(2) = NORM(2)
            INORM(3) = NORM(3)
C
C --- ON STOCKE DANS NORM LA MOYENNE DES NORMALES AU NOEUD ESCLAVE ET
C --- A LA MAILLE MAITRE
C
            NORM(1) = NORM(1)-XNORM(1)
            NORM(2) = NORM(2)-XNORM(2)
            NORM(3) = NORM(3)-XNORM(3)
            CALL NORMEV(NORM,NORME)
            CALL CATANG(3,NORM,TANG,TANGDF)
            COEFA = XNORM(1)
            COEFB = XNORM(2)
            COEFC = XNORM(3)
            COEFD = -(COEFA*COORDA(1)+COEFB*COORDA(2)+
     &                COEFC*COORDA(3))
            COEFF = COEFA*NORM(1)+COEFB*NORM(2)+COEFC*NORM(3)
            IF(COEFF.NE.0.D0) THEN
              COEFF = -(COEFA*COORDP(1)+COEFB*COORDP(2)+
     &                  COEFC*COORDP(3)+COEFD)/COEFF
            ELSE
              CALL UTMESS ('F','PROJTR_02','LE VECTEUR NORMAL EST '
     &                     //'COLINEAIRE AU PLAN DE PROJECTION')
            ENDIF
            COORDM(1) = COORDP(1)+COEFF*NORM(1)
            COORDM(2) = COORDP(2)+COEFF*NORM(2)
            COORDM(3) = COORDP(3)+COEFF*NORM(3)
            IF(NORM(2).EQ.0.D0) THEN
               ALPHA = 0.D0
               IF(NORM(3).EQ.0.D0) THEN
                  SIGNE = NORM(1)
                  IF(SIGNE.GT.0) THEN
                     BETA  =  R8PI()/2
                  ELSE
                     BETA  = -R8PI()/2
                  ENDIF
               ENDIF
            ELSE
               IF(NORM(1).EQ.0.D0) THEN
                  BETA = 0.D0
                  IF(NORM(3).EQ.0.D0) THEN
                     SIGNE = NORM(2)
                     IF(SIGNE.GT.0) THEN
                        ALPHA  = -R8PI()/2
                     ELSE
                        ALPHA  =  R8PI()/2
                     ENDIF
                  ENDIF
               ELSE
                  IF(NORM(1).NE.0.D0.AND.NORM(2).NE.0.D0) THEN
                     R1    = SQRT(NORM(2)*NORM(2)+NORM(3)*NORM(3))
                     SIGNE = NORM(2)*NORM(3)
                     IF(SIGNE.GE.0.D0) THEN
                        ALPHA =   ATAN2(NORM(3),NORM(2))
                     ELSE
                        ALPHA = - ATAN2(NORM(3),NORM(2))
                     ENDIF
                     SIGNE = - NORM(1)
                     IF(SIGNE.GE.0.D0) THEN
                        BETA =   ATAN2(NORM(1),R1)
                     ELSE
                        BETA = - ATAN2(NORM(1),R1)
                     ENDIF
                  ENDIF
              ENDIF
         ENDIF
         IF(NORM(1).NE.0.D0.OR.NORM(2).NE.0.D0) THEN
           GAMMA = 0.D0
           CALL ROT3D(COORDA,SIN(GAMMA),COS(GAMMA),SIN(BETA),
     &                COS(BETA),SIN(ALPHA),COS(ALPHA),COORA2)
           CALL ROT3D(COORDB,SIN(GAMMA),COS(GAMMA),SIN(BETA),
     &                COS(BETA),SIN(ALPHA),COS(ALPHA),COORB2)
           CALL ROT3D(COORDC,SIN(GAMMA),COS(GAMMA),SIN(BETA),
     &                COS(BETA),SIN(ALPHA),COS(ALPHA),COORC2)
           CALL ROT3D(COORDM,SIN(GAMMA),COS(GAMMA),SIN(BETA),
     &                COS(BETA),SIN(ALPHA),COS(ALPHA),COORM2)
           C2A2(1) = COORA2(1) - COORC2(1)
           C2A2(2) = COORA2(2) - COORC2(2)
           C2A2(3) = 0.D0
           B2A2(1) = COORA2(1) - COORB2(1)
           B2A2(2) = COORA2(2) - COORB2(2)
           B2A2(3) = 0.D0
         ELSE
           C2A2(1) = -AC(1)
           C2A2(2) = -AC(2)
           C2A2(3) = 0.D0
           B2A2(1) = -AB(1)
           B2A2(2) = -AB(2)
           B2A2(3) = 0.D0
         ENDIF
         CALL PROJLI(COORA2,COORB2,COORM2,C2A2,R3BID,
     &               KSI1,RBID,RBID,R6BID,RBID,1,TANGDF,NDIM)
         CALL PROJLI(COORA2,COORC2,COORM2,B2A2,R3BID,
     &               KSI2,RBID,RBID,R6BID,RBID,1,TANGDF,NDIM)
         KSI3 = 1.D0 - KSI1 - KSI2
C
         IF ((KSI1.LT.0.D0).OR.(KSI2.LT.0.D0).OR.
     &       (KSI3.LT.0.D0)) THEN
            ABSAC = R8DOT (3,AB,1,AC,1) / LAC
            ACSAB = R8DOT (3,AB,1,AC,1) / LAB
            ABSBC = R8DOT (3,AB,1,BC,1) / LBC
            ACSBC = R8DOT (3,AC,1,BC,1) / LBC
            CALL AJUSTT (ABSAC,ACSAB,ABSBC,ACSBC,KSI1,KSI2)
         END IF
C
C --- CALCUL DES VALEURS DES FONCTIONS DE FORME DES NOEUDS EN M
C
         LAMBDA  = 1.D0 - KSI1 - KSI2
         LAMBDB  = KSI1
         LAMBDC  = KSI2
         COEF(1) = - LAMBDA
         COEF(2) = - LAMBDB
         COEF(3) = - LAMBDC
C         IF (NUTYP.EQ.NTTRI6) THEN
C            COEF(1) = - LAMBDA * (2*LAMBDA-1)
C            COEF(2) = - LAMBDB * (2*LAMBDB-1)
C            COEF(3) = - LAMBDC * (2*LAMBDC-1)
C            COEF(4) = - 4 * LAMBDA * LAMBDB
C            COEF(5) = - 4 * LAMBDB * LAMBDC
C            COEF(6) = - 4 * LAMBDC * LAMBDA
C         END IF
C
C --- CALCUL DU JEU ET DE LA DIRECTION DE PROJECTION (UNITAIRE)
C
         DO 51 K = 1,3
            XNORM(K) = COORDM(K) - COORDP(K)
 51      CONTINUE
         OLDJEU = SQRT(R8DOT(3,XNORM,1,XNORM,1))
C
         IF (PRONOR.EQ.3) THEN
             NORM(1) = COEF(1)*VLISSA(1)  + COEF(2)*VLISSA(4)  +
     &                 COEF(3)*VLISSA(7)
             NORM(2) = COEF(1)*VLISSA(2)  + COEF(2)*VLISSA(5)  +
     &                 COEF(3)*VLISSA(8)
             NORM(3) = COEF(1)*VLISSA(3)  + COEF(2)*VLISSA(6)  +
     &                 COEF(3)*VLISSA(9)
             CALL NORMEV(NORM,NORME)
             NORM(1) = NORM(1) + INORM(1)
             NORM(2) = NORM(2) + INORM(2)
             NORM(3) = NORM(3) + INORM(3)
             CALL NORMEV(NORM,NORME)
         ENDIF
         JEU = (COORDM(1)-COORDP(1))*NORM(1) +
     &         (COORDM(2)-COORDP(2))*NORM(2) +
     &         (COORDM(3)-COORDP(3))*NORM(3)
      ENDIF
      ENDIF
      
C
C ======================================================================
C            PROJECTION QUADRATIQUE SUR LE TRIA6 (PROJ = 2)
C ======================================================================
C
      IF ((NUTYP.EQ.NTTRI6).AND.(PROJ.EQ.2)) THEN
        CALL UTMESS ('F','PROJTR_02',
     &               'LA PROJECTION QUADRATIQUE POUR LES TRIANGLES '
     &               //'N''EST PAS DISPONIBLE')
      END IF
C
C ----------------------------------------------------------------------
C
      END
