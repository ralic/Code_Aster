      SUBROUTINE RCTRAC(JMAT,NOMRC,NOMCL,TEMP,JPROL,JVALE,NBVALE,E)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/12/2008   AUTEUR PROIX J-M.PROIX 
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
C ----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            IMATE, JPROL, JVALE, NBVALE, JMAT, NBMAT
      REAL*8             TEMP, E
      CHARACTER*(*)      NOMRC,NOMCL
C ----------------------------------------------------------------------
C     DETERMINATION DU MODULE DE YOUNG ET DE LA FONCTION D'ECROUISSAGE
C     A PARTIR DE LA COURBE DE TRACTION D'UN MATERIAU DONNE

C IN  IMATE  : ADRESSE DU MATERIAU CODE
C IN  NOMRC  : NOM DE LA RELATION DE COMPORTEMENT
C              NOMRC = 'TRACTION' OU 'META_TRACTION'
C IN  NOMCL  : NOM DU MOT CLE DE NOMRC
C              SI NOMRC = 'TRACTION' NOMCL = 'SIGM'
C              SI NOMRC = 'META_TRACTION'
C               NOMCL = 'SIGM_F' OU 'SIGM_B' OU 'SIGM_M' OU 'SIGM_A'
C IN  TEMP   : TEMPERATURE AU POINT DE GAUSS CONSIDERE
C IN  COMP   :
C IN  MOT    :
C OUT JPROL  : ADRESSE DE L'OBJET .PROL DE LA S.D. FONCTION R(P)
C OUT JVALE  : ADRESSE DE L'OBJET .VALE DE LA S.D. FONCTION R(P)
C OUT NBVALE : NOMBRE DE VALEURS DE LA FONCTION R(P)
C OUT E      : MODULE DE YOUNG
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      CHARACTER*32       JEXNUM , JEXNOM
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------

      LOGICAL       PROCON, LTRAC
      INTEGER       ICOMP, IPI, IDF, NBF, IVALK, IK, IPIF, IPIFC, JPRO
      INTEGER       JVALF1, NBVF1, K, K1, K2, NAR
      INTEGER       JVALN, NBVN, I, J, JVALF2, NBVF2
      REAL*8        COEF,TOLE,R8PREM,T1,T2,E1,E2,Z1,Z2,ZP1,ZP2
      REAL*8        RPRIM1, RP1, RPRIM2, RP2
      CHARACTER*8   NOMCL2
      CHARACTER*24 VALK(2)
      CHARACTER*1   PRO1, PRO2
C ----------------------------------------------------------------------
C PARAMETER ASSOCIE AU MATERIAU CODE
      INTEGER       LMAT,   LFCT, LSUP
      PARAMETER    (LMAT=7, LFCT=9, LSUP=2)
C DEB ------------------------------------------------------------------
C      TOLE=R8PREM() TROP PETIT
      TOLE=1.D-6

      NBMAT=ZI(JMAT)
C     UTILISABLE SEULEMENT AVEC UN MATERIAU PAR MAILLE
      CALL ASSERT(NBMAT.EQ.1)
      IMATE = JMAT+ZI(JMAT+NBMAT+1)


C - COURBE DE TRACTION SANS METALLURGIE

      NOMCL2 = NOMCL
      IF(NOMRC(1:8).EQ.'TRACTION') THEN
       DO 10 ICOMP=1,ZI(IMATE+1)
        IF ( 'TRACTION' .EQ. ZK16(ZI(IMATE)+ICOMP-1)(1:8) ) THEN
         IPI = ZI(IMATE+2+ICOMP-1)
         GOTO 11
        ENDIF
 10    CONTINUE
       CALL U2MESS('F','MODELISA6_80')
 11    CONTINUE
       IDF   = ZI(IPI)+ZI(IPI+1)
       NBF   = ZI(IPI+2)
       IVALK = ZI(IPI+3)
       DO 160 IK = 1,NBF
        IF ('SIGM    ' .EQ. ZK8(IVALK+IDF+IK-1)) THEN
         IPIF = IPI+LMAT-1+LFCT*(IK-1)
         GOTO 170
        ENDIF
 160   CONTINUE
       CALL U2MESS('F','MODELISA6_81')
 170   CONTINUE
      ENDIF

C - COURBE DE TRACTION AVEC METALLURGIE

      IF(NOMRC(1:13).EQ.'META_TRACTION') THEN
       DO 12 ICOMP=1,ZI(IMATE+1)
        IF ('META_TRACTION' .EQ. ZK16(ZI(IMATE)+ICOMP-1)(1:13) ) THEN
         IPI = ZI(IMATE+2+ICOMP-1)
         GOTO 13
        ENDIF
 12    CONTINUE
       CALL U2MESS('F','MODELISA6_82')
 13    CONTINUE

       IDF   = ZI(IPI)+ZI(IPI+1)
       NBF   = ZI(IPI+2)
       IVALK = ZI(IPI+3)

       IF (NOMCL(1:7) .EQ.'SIGM_F1') THEN
        DO 161 IK = 1,NBF
         IF ('SIGM_F1  ' .EQ. ZK8(IVALK+IDF+IK-1)) THEN
          IPIF = IPI+LMAT-1+LFCT*(IK-1)+LSUP*(IK-1)

          GOTO 171
         ENDIF
 161    CONTINUE
        CALL U2MESS('F','MODELISA6_83')
 171    CONTINUE
       ENDIF

       IF (NOMCL(1:7) .EQ.'SIGM_F2') THEN
        DO 162 IK = 1,NBF
         IF ('SIGM_F2  ' .EQ. ZK8(IVALK+IDF+IK-1)) THEN
          IPIF = IPI+LMAT-1+LFCT*(IK-1)+LSUP*(IK-1)
          GOTO 172
         ENDIF
 162    CONTINUE
        CALL U2MESS('F','MODELISA6_84')
 172    CONTINUE
       ENDIF

       IF (NOMCL(1:7) .EQ.'SIGM_F3') THEN
        DO 163 IK = 1,NBF
         IF ('SIGM_F3  ' .EQ. ZK8(IVALK+IDF+IK-1)) THEN
          IPIF = IPI+LMAT-1+LFCT*(IK-1)+LSUP*(IK-1)
          GOTO 173
         ENDIF
 163    CONTINUE
        CALL U2MESS('F','MODELISA6_85')
 173    CONTINUE
       ENDIF

       IF (NOMCL(1:7) .EQ.'SIGM_F4') THEN
        DO 164 IK = 1,NBF
         IF ('SIGM_F4  ' .EQ. ZK8(IVALK+IDF+IK-1)) THEN
          IPIF = IPI+LMAT-1+LFCT*(IK-1)+LSUP*(IK-1)
          GOTO 174
         ENDIF
 164    CONTINUE
        CALL U2MESS('F','MODELISA6_86')
 174    CONTINUE
       ENDIF

        IF (NOMCL(1:6) .EQ.'SIGM_C') THEN
        DO 165 IK = 1,NBF
         IF ('SIGM_C  ' .EQ. ZK8(IVALK+IDF+IK-1)) THEN
          IPIF = IPI+LMAT-1+LFCT*(IK-1)+LSUP*(IK-1)
          GOTO 175
         ENDIF
 165    CONTINUE
        CALL U2MESS('F','MODELISA6_87')
 175    CONTINUE
       ENDIF
      ENDIF

      IPIFC  = ZI(IPIF+6)
      JPROL  = ZI(IPIFC)
      JVALE  = ZI(IPIFC+1)

C
C LES POINTEURS JPROL ET JVALE SUR ZONE DE TRAVAIL RDPE
C ETENDU SONT MODIFIES PAR RCTRAC

      JPRO   = ZI(IPIF+1)

      IF (ZK24(JPRO)(1:1).EQ.'C') THEN
C
C ----- FONCTION CONSTANTE - IMPOSSIBLE
C
        CALL U2MESK('F','MODELISA6_88',1,NOMCL2)
      ELSE IF (ZK24(JPRO)(1:1).EQ.'F') THEN
C
C ----- FONCTION D'UNE SEULE VARIABLE
C
        JVALF1= ZI(IPIF+2)
        NBVF1 = ZI(IPIF)
        NBVALE  = NBVF1

        DO 50 K=1,NBVF1
          ZR(JVALE+K-1)       = ZR(JVALF1+K-1)
          ZR(JVALE+K-1+NBVF1) = ZR(JVALF1+K-1+NBVF1)
 50     CONTINUE
        ZK24(JPROL+4) = ZK24(JPRO+4)
        ZK24(JPROL+5) = ZK24(JPRO+5)

      ELSE IF (ZK24(JPRO)(1:1).EQ.'N') THEN
C
C ----- NAPPE : FONCTION DE DEUX VARIABLES, DETERMINATION DE I TEL QUE
C ----- ZR(JVALN+I) < TEMP < ZR(JVALN+I+1)
C
        JVALN = ZI(IPIF+4)
        NBVN  = ZI(IPIF+5)
        ZK24(JPROL+5) = ZK24(JPRO+5)
        PROCON = .FALSE.
        IF (TEMP .LT. ZR(JVALN)) THEN
          IF (ZK24(JPRO+4)(1:1) .EQ. 'C') THEN
            I=1
            PROCON = .TRUE.
          ELSE IF (ZK24(JPRO+4)(1:1) .EQ. 'L') THEN
            I=1
          ELSE IF (ZK24(JPRO+4)(1:1) .EQ. 'E') THEN
         CALL U2MESK('F','MODELISA6_89',1,NOMCL2)
          END IF
        ELSE IF (TEMP .GT. ZR(JVALN+NBVN-1)) THEN
          IF (ZK24(JPRO+4)(2:2) .EQ. 'C') THEN
            I=NBVN
            PROCON = .TRUE.
          ELSE IF (ZK24(JPRO+4)(2:2) .EQ. 'L') THEN
            I=NBVN-1
          ELSE IF (ZK24(JPRO+4)(2:2) .EQ. 'E') THEN
            CALL U2MESK('F','MODELISA6_90',1,NOMCL2)
          END IF          
        ELSE
          DO 1 J=1,NBVN-1
C          IF (ZR(JVALN+J-1) .LE.TEMP .AND. TEMP .LE. ZR(JVALN+J)) THEN
C          CAS DE STRICTE EGALITE. PAS D'INTERPOLATION
           T1 = ZR(JVALN+J-1)
           T2 = ZR(JVALN+J)
           IF (ABS(T1-TEMP).LE.(TOLE*ABS(T1))) THEN
              I=J
              PROCON=.TRUE.
              GOTO 2
           ENDIF
           IF (ABS(T2-TEMP).LE.(TOLE*ABS(T2))) THEN
              I=J+1
              PROCON=.TRUE.
              GOTO 2
           ENDIF
           IF (T1 .LT.TEMP .AND. TEMP .LT. T2) THEN
              I=J
              GOTO 2
           END IF
    1     CONTINUE
    2     CONTINUE
        END IF
C
C ----- INTERPOLATION ENTRE I ET I+1
C
        JVALF1= ZI(IPIF+2)+ZI(ZI(IPIF+3)+I-1)-1
        NBVF1 = ZI(ZI(IPIF+3)+I)-ZI(ZI(IPIF+3)+I-1)
        NBVF1 = NBVF1/2
        IF ( PROCON ) THEN
C
C ------- SI LE PROLONGEMENT EST CONSTANT, ON SE RAMENE AU CAS FONCTION
C
          DO 100 K=1,NBVF1
            ZR(JVALE+K-1)       = ZR(JVALF1+K-1)
            ZR(JVALE+K-1+NBVF1) = ZR(JVALF1+K-1+NBVF1)
 100      CONTINUE
          ZK24(JPROL+4) = ZK24(JPRO+6+2*I)
          NBVF2 = NBVF1
          NBVALE  = NBVF1
        ELSE
C
C ------- INTERPOLATION POUR LA FONCTION ENTRE I ET I+1
C
          ZK24(JPROL+4)(1:2) = 'CC'
          IF ( ZK24(JPRO+6+2*I  )(1:1) .EQ. 'E' .OR.
     &         ZK24(JPRO+6+2*I+2)(1:1) .EQ. 'E' ) THEN
            ZK24(JPROL+4)(1:1) = 'E'
          ELSE IF ( ZK24(JPRO+6+2*I  )(1:1) .EQ. 'L' .OR.
     &              ZK24(JPRO+6+2*I+2)(1:1) .EQ. 'L' ) THEN
            ZK24(JPROL+4)(1:1) = 'L'
          ENDIF
          IF ( ZK24(JPRO+6+2*I  )(2:2) .EQ. 'E' .OR.
     &         ZK24(JPRO+6+2*I+2)(2:2) .EQ. 'E' ) THEN
            ZK24(JPROL+4)(2:2) = 'E'
          ELSE IF ( ZK24(JPRO+6+2*I  )(2:2) .EQ. 'L' .OR.
     &              ZK24(JPRO+6+2*I+2)(2:2) .EQ. 'L' ) THEN
            ZK24(JPROL+4)(2:2) = 'L'
          ENDIF
          JVALF2= ZI(IPIF+2)+ZI(ZI(IPIF+3)+I)-1
          NBVF2 = ZI(ZI(IPIF+3)+I+1)-ZI(ZI(IPIF+3)+I)
          NBVF2 = NBVF2/2
C
C ------- INTERPOLATION ENTRE LES COURBES R(P,T1) ET R(P,T2)
C ----------------------------------------------------------
C
C
C ------- INITIALISATION DES VARIABLES  ET INTERPOLATION POUR P=0
C
          NBVALE = NBVF1 + NBVF2 - 1
          COEF = (TEMP-ZR(JVALN+I-1))/(ZR(JVALN+I)-ZR(JVALN+I-1))
          LTRAC = .FALSE.
          IF (NOMRC(1:8).EQ.'TRACTION') THEN
            E1 = ZR(JVALF1+NBVF1)/ZR(JVALF1)
            E2 = ZR(JVALF2+NBVF2)/ZR(JVALF2)
            E = E1 + COEF*(E2-E1)
            LTRAC = .TRUE.
            ZR(JVALE) = 0.D0
            K1 = 1
            K2 = 1
          ELSE
            NBVALE = NBVALE+1
            Z1 = ZR(JVALF1)
            Z2 = ZR(JVALF2)
            IF (ABS(Z2-Z1).LE.(TOLE*Z1)) THEN
               ZR(JVALE) = Z1
               K1=1
               K2=1
            ELSEIF (Z2.GT.Z1) THEN
               ZR(JVALE)=Z1
               K1=1
               K2=0
            ELSE
               ZR(JVALE)=Z2
               K1=0
               K2=1
            ENDIF
          ENDIF

          ZR(JVALE+NBVALE) = ZR(JVALF1+NBVF1) +
     &                       COEF*(ZR(JVALF2+NBVF2)-ZR(JVALF1+NBVF1))

          PRO1 = ZK24(JPRO+6+2*I)(2:2)
          PRO2 = ZK24(JPRO+6+2*I+2)(2:2)
          NAR = 0
C
C ------- DEBUT DE LA BOUCLE D'INTERPOLATION
C ------- LA LONGUEUR DE CETTE BOUCLE VAUT NBVF1+NBVF2-2
C
C ------- DANS LE <<CAS COURANT>>:
C ------- ON PROGRESSE POINT PAR POINT PAR ABSCISSES CROISSANTES,
C ------- AVEC 2 COMPTEURS K1 ET K2 (UN PAR COURBE)
C ------- LE CPTEUR NAR (NBRE A RETIRER) EST INCREMENTE DE 1 A CHAQUE
C ------- FOIS QUE 2 POINTS DES COURBES 1 ET 2 ONT LA MEME ABSCISSE
C
          DO 200 K = 1,NBVALE-1
            IF ((K1.LT.NBVF1).OR.(K2.LT.NBVF2)) THEN
C
C ----------- CAS OU ON ARRIVE AU BOUT DE LA COURBE 1 :
C -----------   PROLONGEMENT EXCLUS => ON CALCULE NAR ET ON ARRETE
C -----------   SINON (L OU C) =>ON FAIT L'INTERPOLATION A L'ABSCISSE
C -----------                    Z2 ET ON INCREMENTE K2
C
              IF (K1.GE.NBVF1) THEN
                IF (PRO1.EQ.'E') THEN
                  NAR = NBVALE - K
                  GOTO 201
                ELSE IF (PRO1.EQ.'L') THEN
                  RPRIM1=(ZR(JVALF1+NBVF1+K1-1)-ZR(JVALF1+NBVF1+K1-2))
     &                   /(Z1-ZP1)
                ELSE
                  RPRIM1 = 0.D0
                ENDIF
                Z2 = ZR(JVALF2+K2)
                IF (LTRAC) Z2 = Z2 - ZR(JVALF2+NBVF2+K2)/E2
                ZR(JVALE+K) = Z2
                RP1 = ZR(JVALF1+NBVF1+K1-1)+RPRIM1*(Z2-Z1)
                RP2 = ZR(JVALF2+NBVF2+K2)
                ZR(JVALE+NBVALE+K) = RP1+COEF*(RP2-RP1)
                K2 = K2 + 1
                GOTO 199
              ELSE
                ZP1 = ZR(JVALF1+K1-1)
                IF (LTRAC) ZP1 = ZP1 - ZR(JVALF1+NBVF1+K1-1)/E1
                Z1 = ZR(JVALF1+K1)
                IF (LTRAC) Z1 = Z1 - ZR(JVALF1+NBVF1+K1)/E1
              ENDIF
C
C ----------- CAS OU ON ARRIVE AU BOUT DE LA COURBE 2 : IDEM
C
              IF (K2.GE.NBVF2) THEN
                IF (PRO2.EQ.'E') THEN
                  NAR = NBVALE - K
                  GOTO 201
                ELSE IF (PRO2.EQ.'L') THEN
                  RPRIM2=(ZR(JVALF2+NBVF2+K2-1)-ZR(JVALF2+NBVF2+K2-2))
     &                   /(Z2-ZP2)
                ELSE
                  RPRIM2 = 0.D0
                ENDIF
                Z1 = ZR(JVALF1+K1)
                IF (LTRAC) Z1 = Z1 - ZR(JVALF1+NBVF1+K1)/E1
                ZR(JVALE+K) = Z1
                RP1 = ZR(JVALF1+NBVF1+K1)
                RP2 = ZR(JVALF2+NBVF2+K2-1)+RPRIM2*(Z1-Z2)
                ZR(JVALE+NBVALE+K) = RP1+COEF*(RP2-RP1)
                K1 = K1 + 1
                GOTO 199
              ELSE
                ZP2 = ZR(JVALF2+K2-1)
                IF (LTRAC) ZP2 = ZP2 - ZR(JVALF2+NBVF2+K2-1)/E2
                Z2 = ZR(JVALF2+K2)
                IF (LTRAC) Z2 = Z2 - ZR(JVALF2+NBVF2+K2)/E2
              ENDIF
C
C ----------- <<CAS COURANT>> : CF COMMENTAIRE AU DEBUT DE LA BOUCLE
C
              IF (ABS(Z2-Z1).LE.(TOLE*Z1)) THEN
                ZR(JVALE+K) = Z1
                RP1 = ZR(JVALF1+NBVF1+K1)
                RP2 = ZR(JVALF2+NBVF2+K2)
                NAR = NAR + 1
                K1 = K1 + 1
                K2 = K2 + 1
              ELSE
                IF (Z2.GT.Z1) THEN
                  ZR(JVALE+K) = Z1
                  RP1 = ZR(JVALF1+NBVF1+K1)
                  RPRIM2=(ZR(JVALF2+NBVF2+K2)-ZR(JVALF2+NBVF2+K2-1))
     &                   /(Z2-ZP2)
                  RP2 = ZR(JVALF2+NBVF2+K2-1)+RPRIM2*(Z1-ZP2)
                  K1 = K1 + 1
                ELSE
                  ZR(JVALE+K) = Z2
                  RP2 = ZR(JVALF2+NBVF2+K2)
                  RPRIM1=(ZR(JVALF1+NBVF1+K1)-ZR(JVALF1+NBVF1+K1-1))
     &                   /(Z1-ZP1)
                  RP1 = ZR(JVALF1+NBVF1+K1-1)+RPRIM1*(Z2-ZP1)
                  K2 = K2 + 1
                ENDIF
              ENDIF
              ZR(JVALE+NBVALE+K) = RP1+COEF*(RP2-RP1)
            ENDIF
 199        CONTINUE
 200      CONTINUE
C
C ------- FIN DE LA BOUCLE D'INTERPOLATION
C
 201      CONTINUE
C
C ------- CORRECTION DE NBVALE ET
C ------- DECALAGE DES ORDONNEES DE NAR VERS LA GAUCHE
C
          IF (NAR.GT.0) THEN
            NBVALE = NBVALE - NAR
            DO 210 K = 1,NBVALE
              ZR(JVALE+NBVALE+K-1) = ZR(JVALE+NBVALE+NAR+K-1)
 210        CONTINUE
          ENDIF
        ENDIF
      ELSE
         VALK(1) = ZK24(JPRO)
         VALK(2) = NOMCL2
         CALL U2MESK('F','MODELISA6_91', 2 ,VALK)
      ENDIF
C
C --- CONSTRUCTION DE LA COURBE R(P) POUR TRACTION
C --- DANS LE CAS DE LA FONCTION D'UNE SEULE VARIABLE
C --- PAS LA PEINE EN METALLURGIE CAR ON DONNE DIRECTEMENT
C --- LA COURBE R(P) OU PLUS EXACTEMENT LA COURBE R(R)
C
      IF(NOMRC(1:8).EQ.'TRACTION') THEN
        IF (ZK24(JPRO)(1:1).EQ.'N') THEN
          IF (.NOT.(PROCON)) GOTO 310
        ELSE
          IF (ZK24(JPRO)(1:1).NE.'F') GOTO 310
        ENDIF
        E = ZR(JVALE+NBVALE)/ZR(JVALE)
        ZR(JVALE) = 0.D0
        DO 300 K = 1,NBVALE-1
          ZR(JVALE+K) = ZR(JVALE+K) - ZR(JVALE+NBVALE+K)/E
 300    CONTINUE
 310    CONTINUE
      ENDIF

      END
