      SUBROUTINE SSVARO(L,SENS,MATRIX,TYPNOE,NOMACR,IADM1,IADM2)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SOUSTRUC  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
      IMPLICIT NONE
C
C     ARGUMENTS:
C     ----------
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNOM
      CHARACTER*8 NOMACR
      CHARACTER*(*) SENS
      LOGICAL MATRIX
      CHARACTER*4 TYPNOE
      INTEGER IADM1,IADM2
      REAL*8 L(6,6)
      CHARACTER*8 KBID
C ----------------------------------------------------------------------
C     BUT:
C         "TOURNER" LA MATRICE ELEMENTAIRE (OU LE VECTEUR ELEMENTAIRE)
C          D'UN MACR_ELEM_STAT
C          A L'AIDE DE LA MATRICE DE CHANGEMENT DE REPERE L.
C
C     PRECAUTION:
C         CETTE ROUTINE EST ECRITE POUR LA GRANDEUR "DEPL_R".
C         LES 4 SEULS CAS DE FIGURE PREVUS SONT :
C          1) NOEUDS "2D"                DX, DY
C          2) NOEUDS "3D"                DX, DY, DZ
C          3) NOEUDS "POUTRE/COQUE 3D"   DX, DY, DZ, DRX, DRY, DRZ
C          4) NOEUDS "COQUE AXIS"        DX, DY, DRZ
C          LES AUTRES COMPOSANTES : LAGR, PRES, PHI, DDZDN, ...
C          NE SONT PAS "TOURNEES".
C
C     IN: NOMACR : NOM DU MACR_ELEM_STAT
C           L    : MATRICE DE CHANGEMENT DE REPERE (LOCAL -> GLOBAL)
C           SENS : 'LG' : LOCAL  -> GLOBAL
C                  'GL' : GLOBAL -> LOCAL
C          MATRIX: .TRUE. : MATRICE SYMETRIQUE
C                  .FALSE.: VECTEUR
C          TYPNOE N'EST UTILISE QUE SI MATRIX=.FALSE.
C          TYPNOE: 'EXTE' : UNIQUEMENT LES NOEUDS EXTERNES
C                  'TOUS' : TOUS LES NOEUDS (INTERNES ET EXTERNES)
C
C          SI "MATRIX"     , ON TOURNE K_EE
C          SI "NOT.MATRIX" , ON TOURNE F_I ET F_E
C
C
C          NOMACR: NOM DU MACR_ELEM_STAT.
C          IADM1 : ADRESSE DANS ZR DE LA ZONE MEMOIRE INITIALE (M1)
C          IADM2 : ADRESSE DANS ZR DU "RESULTAT" TOURNE (M2)
C
C          ATTENTION : DANS LE CAS .NOT.MATRIX :
C                      LES 2 ZONES MEMOIRES ZR(IADM1) ET ZR(IADM2)
C                      DOIVENT ETRE DIMENSIONNEES A NDDLI+NDDLE
C                      (POUR "TOUS" ET AUSSI POUR "EXTE" !!)
C
C          ATTENTION : LES 2 ADRESSES IADM1 ET IADM2 DOIVENT ETRE
C                      DIFFERENTES : PAS DE CALCUL "EN PLACE" POSSIBLE.
C
C
C     OUT:
C          LE VECTEUR A L'ADRESSE IADM2 EST CALCULE.
C-----------------------------------------------------------------------
C     VARIABLES LOCALES:
C     ------------------
      INTEGER LONG
      CHARACTER*2 SENS2
      INTEGER I ,I1 ,IACAGD ,IADEEQ ,IADESM ,IAIINO ,ICMP
      INTEGER ICMPP ,ICUMUL ,IED ,IEQ ,IEQDEB ,IEQP ,IER
      INTEGER II ,INO ,IRET ,J ,J1 ,JJ ,K
      INTEGER N1 ,NBNO ,NDDLE ,NDDLI ,NDDLT ,NULAG ,NUNO
      INTEGER NUNOLD ,NUNOP
      INTEGER DI,DJ,DMI,DMJ,V1,V2,M1,M2,M1T,INDIK8
      REAL*8 P1(10,10),P2(10,10),L2(6,6),LI(10,10),LJ(10,10)
C-----------------------------------------------------------------------

C     P1 ET P2 MATRICES DE TRAVAIL.
C
C     FONCTIONS FORMULES:
C     -------------------
      M1 (I,J) = IADM1-1+(J-1)*J/2+I
      M2 (I,J) = IADM2-1+(J-1)*J/2+I
      M1T(I,J) = IADM1-1+(I-1)*I/2+J
      V1 (I)   = IADM1-1+I
      V2 (I)   = IADM2-1+I
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
      SENS2=SENS
C
C
C     -1 VERIFICATION DE LA GRANDEUR "DEPLACEMENT" ET CALCUL DE NULAG
C     -------------------------------------------------------------
      CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP','DEPL_R'),'L',IACAGD)
      IER=0
      IF(INDIK8(ZK8(IACAGD),'DX',1,1).NE.1) IER=IER+1
      IF(INDIK8(ZK8(IACAGD),'DY',1,2).NE.2) IER=IER+1
      IF(INDIK8(ZK8(IACAGD),'DZ',1,3).NE.3) IER=IER+1
      IF(INDIK8(ZK8(IACAGD),'DRX',1,4).NE.4) IER=IER+1
      IF(INDIK8(ZK8(IACAGD),'DRY',1,5).NE.5) IER=IER+1
      IF(INDIK8(ZK8(IACAGD),'DRZ',1,6).NE.6) IER=IER+1
      IF (IER.GT.0) THEN
        CALL U2MESS('F','SOUSTRUC_73')
      END IF
      CALL DISMOI('F','NU_CMP_LAGR','DEPL_R','GRANDEUR',NULAG,KBID,IED)
      IF (NULAG.EQ.0) THEN
        CALL U2MESS('F','SOUSTRUC_74')
      END IF
C
C
C     -2 CALCUL DE L2 = L OU LT :
C     ------------------------
      IF (SENS2.EQ.'LG') THEN
        DO 77, I=1,6
        DO 77, J=1,6
          L2(I,J)=L(I,J)
 77     CONTINUE
      ELSE IF (SENS2.EQ.'GL') THEN
        DO 88, I=1,6
        DO 88, J=1,6
          L2(I,J)=L(J,I)
 88     CONTINUE
      ELSE
        CALL U2MESK('F','SOUSTRUC_75',1,SENS2)
      END IF
C
C
C     -3 2CALCUL DE NDDLI, NDDLE, NDDLT ET LONG :
C     --------------------------------
      CALL JEVEUO(NOMACR//'.DESM','L',IADESM)
      CALL JEVEUO(NOMACR//'      .NUME.DEEQ','L',IADEEQ)
      NDDLE=ZI(IADESM-1+4)
      NDDLI=ZI(IADESM-1+5)
      NDDLT= NDDLI+NDDLE
      IF (MATRIX) THEN
        LONG= NDDLE*(NDDLE+1)/2
        IEQDEB=NDDLI+1
      ELSE
        CALL ASSERT((TYPNOE.EQ.'TOUS').OR.(TYPNOE.EQ.'EXTE'))
        IF (TYPNOE.EQ.'TOUS') THEN
          LONG= NDDLT
          IEQDEB=1
        ELSE IF (TYPNOE.EQ.'EXTE') THEN
          LONG= NDDLE
          IEQDEB=NDDLI+1
        END IF
      END IF
C
C
C     -4 RECUPERATION DE L'OBJET .IINO :
C     -------------------------------
C     .IINO(1) = NUMERO D'EQUATION DU 1ER CMP DU NOEUD 1
C     .IINO(2) = NOMBRE D'EQUATIONS DU NOEUD 1 (DI(1))
C     .IINO(3) = NOMBRE D'EQUATIONS CHANGEES PAR LA ROTATION (DMI(1))
C       (CES EQUATIONS SONT SENSEES SE SUIVRE ET ETRE PLACEES EN TETE)
C       VALEURS POSSIBLES : 2, 3, OU 6
C     .IINO(3+1) = NUMERO D'EQUATION DU 1ER CMP DU NOEUD 2
C     .IINO(3+2) = NOMBRE D'EQUATIONS DU NOEUD 2  (DI(2))
C     ....
C
      CALL JEEXIN('&&SSVARO.IINO',IRET)
      IF (IRET.GT.0) THEN
        CALL JELIRA('&&SSVARO.IINO','LONMAX',N1,KBID)
        IF (N1.LT.NDDLT) THEN
          CALL JEDETR('&&SSVARO.IINO')
          CALL WKVECT('&&SSVARO.IINO','V V I',3*NDDLT,IAIINO)
        ELSE
          CALL JEVEUO('&&SSVARO.IINO','E',IAIINO)
        END IF
      ELSE
        CALL WKVECT('&&SSVARO.IINO','V V I',3*NDDLT,IAIINO)
      END IF
C
C
C     -5 CALCUL DE .IINO :
C     --------------------
      NUNOLD=0
      INO=0
      DO 2, IEQ=IEQDEB,NDDLT
        NUNO =ZI(IADEEQ-1+2*(IEQ-1)+1)
        ICMP=ZI(IADEEQ-1+2*(IEQ-1)+2)
        IF ((ICMP.LE.0).OR.(ICMP.EQ.NULAG)) THEN
C         --NOEUD DE LAGRANGE:
          INO= INO+1
          ZI(IAIINO-1+3*(INO-1)+1)=IEQ-(IEQDEB-1)
          ZI(IAIINO-1+3*(INO-1)+2)=1
          ZI(IAIINO-1+3*(INO-1)+3)=0
          GO TO 2
        END IF
        IF (NUNO.EQ.NUNOLD) GO TO 2
        NUNOLD=NUNO
        INO= INO+1
        ZI(IAIINO-1+3*(INO-1)+1)=IEQ-(IEQDEB-1)
        IF (ICMP.EQ.1) THEN
          ICUMUL=1
C       --ICUMUL COMPTE LA SOMME DES COMPOSANTES PRESENTES SUR LE NOEUD
        ELSE
          CALL U2MESS('F','SOUSTRUC_76')
        END IF
C
        DMI=0
        DO 21, IEQP=IEQ+1,NDDLT
          NUNOP =ZI(IADEEQ-1+2*(IEQP-1)+1)
          IF (NUNOP.NE.NUNO) GO TO 22
          ICMPP=ZI(IADEEQ-1+2*(IEQP-1)+2)
          ICUMUL=ICUMUL+ICMPP
C
C         -- CAS "2D"
C             3 = DX + DY
          IF (ICMPP.EQ.2) THEN
            DMI=2
            IF (ICUMUL.NE.3) CALL U2MESS('F','SOUSTRUC_77')
          END IF
C
C         -- CAS "3D"
C             6 = DX + DY + DZ
          IF (ICMPP.EQ.3) THEN
            DMI=3
            IF (ICUMUL.NE.6) CALL U2MESS('F','SOUSTRUC_77')
          END IF
C
C         -- CAS "POUTRE/COQUE 3D"
C             21 = DX + DY + DZ + DRX + DRY + DRZ
          IF (ICMPP.EQ.6) THEN
            IF (ICUMUL.EQ.21) THEN
              DMI=6
C
C           -- CAS "POUTRE/COQUE AXIS"
C              9 = DX + DY + DRZ
            ELSE IF (ICUMUL.EQ.9) THEN
              DMI=2
            ELSE
              CALL U2MESS('F','SOUSTRUC_77')
            END IF
          END IF
 21     CONTINUE
C
 22     CONTINUE
        DI= IEQP-IEQ
        IF (DI.GT.10) CALL ASSERT(.FALSE.)
        ZI(IAIINO-1+3*(INO-1)+2)= DI
        ZI(IAIINO-1+3*(INO-1)+3)= DMI
 2    CONTINUE
      NBNO=INO
C
C
C     -6 RECOPIE DE M1 DANS M2 (POUR LES TERMES QUI NE TOURNENT PAS )
C        ("LAGR","PRES","DDNDZ","PHI",...  )
C     ----------------------------------------------------------------
      IF (MATRIX) THEN
        DO 31, K=1,LONG
          ZR(IADM2-1+K)=ZR(IADM1-1+K)
 31     CONTINUE
      ELSE
        DO 32, K=IEQDEB,NDDLT
          ZR(IADM2-1+K)=ZR(IADM1-1+K)
 32     CONTINUE
      END IF
C
C
C     -7 CALCUL DES TERMES QUI TOURNENT (POUR UNE MATRICE) :
C     ------------------------------------------------------
      IF (MATRIX) THEN
C
C     -7-1 CALCUL DE M2(I,J)= LIT*M1(I,J)*LJ:
C        (I ET J SONT DES NOEUDS !)
C     -----------------------------------------
      DO 1, J=1,NBNO
        J1= ZI(IAIINO-1+3*(J-1)+1)
        DJ= ZI(IAIINO-1+3*(J-1)+2)
        DMJ= ZI(IAIINO-1+3*(J-1)+3)
C
C       -7-2 CALCUL DE LJ :
C       -----------------
        DO 11, II=1,DJ
        DO 11, JJ=1,DJ
          LJ(II,JJ)=0.0D0
 11     CONTINUE
        DO 12, JJ=1,DJ
          LJ(JJ,JJ)=1.0D0
 12     CONTINUE
        DO 13, II=1,DMJ
        DO 13, JJ=1,DMJ
          LJ(II,JJ)=L2(II,JJ)
 13     CONTINUE
C
        DO 14, I=1,J
          I1= ZI(IAIINO-1+3*(I-1)+1)
          DI= ZI(IAIINO-1+3*(I-1)+2)
          DMI= ZI(IAIINO-1+3*(I-1)+3)
C
C         -7-3 CALCUL DE LI :
C         -----------------
          DO 141, II=1,DI
          DO 141, JJ=1,DI
            LI(II,JJ)=0.0D0
 141      CONTINUE
          DO 142, JJ=1,DI
            LI(JJ,JJ)=1.0D0
 142      CONTINUE
          DO 143, II=1,DMI
          DO 143, JJ=1,DMI
            LI(II,JJ)=L2(II,JJ)
 143      CONTINUE
C
C         -7-4 RECOPIE DE M1(I,J) DANS P1:
C         ------------------------------
          IF (I.LT.J) THEN
            DO 144, JJ=1,DJ
              DO 1441, II=1,DI
                P1(II,JJ)=ZR(M1(I1-1+II,J1-1+JJ))
 1441         CONTINUE
 144        CONTINUE
          ELSE
            DO 145, JJ=1,DJ
              DO 1451, II=1,JJ
                P1(II,JJ)=ZR(M1(I1-1+II,J1-1+JJ))
 1451         CONTINUE
              DO 1452, II=JJ+1,DI
                P1(II,JJ)=ZR(M1T(I1-1+II,J1-1+JJ))
 1452         CONTINUE
 145        CONTINUE
          END IF
C
C         -7-5 P2=P1*L2(J):
C         --------------
          DO 146, II=1,DI
            DO 1461, JJ=1,DJ
              P2(II,JJ)=0.0D0
              DO 1462, K=1,DJ
                P2(II,JJ)=P2(II,JJ)+P1(II,K)*LJ(K,JJ)
 1462         CONTINUE
 1461       CONTINUE
 146      CONTINUE
C
C         -7-6 P1=LT2(I)*P2:
C         ---------------
          DO 147, II=1,DI
            DO 1471, JJ=1,DJ
              P1(II,JJ)=0.0D0
              DO 1472, K=1,DI
                P1(II,JJ)=P1(II,JJ)+LI(K,II)*P2(K,JJ)
 1472         CONTINUE
 1471       CONTINUE
 147      CONTINUE
C
C         -7-7 RECOPIE DE P1 DANS M2(I,J):
C         ------------------------------
          IF (I.LT.J) THEN
            DO 148, JJ=1,DJ
              DO 1481, II=1,DI
                ZR(M2(I1-1+II,J1-1+JJ))=P1(II,JJ)
 1481         CONTINUE
 148        CONTINUE
          ELSE
            DO 149, JJ=1,DJ
              DO 1491, II=1,JJ
                ZR(M2(I1-1+II,J1-1+JJ))=P1(II,JJ)
 1491         CONTINUE
 149        CONTINUE
          END IF
C
C
 14       CONTINUE
 1      CONTINUE
      END IF
C
C
C
C     -8 CALCUL DES TERMES QUI TOURNENT (POUR UN VECTEUR) :
C     ------------------------------------------------------
C
      IF (.NOT.MATRIX) THEN
C
C     -- CALCUL DE V2(I)= L2T(I)*V1(I)
C        (I EST UN NOEUD !)
C     -----------------------------------------
        DO 5, I=1,NBNO
          I1= ZI(IAIINO-1+3*(I-1)+1) + (IEQDEB-1)
          DMI= ZI(IAIINO-1+3*(I-1)+3)
          DO 51, II=1,DMI
            ZR(V2(I1-1+II))=0.0D0
            DO 511, K=1,DMI
              ZR(V2(I1-1+II))=ZR(V2(I1-1+II))+L2(K,II)*ZR(V1(I1-1+K))
 511        CONTINUE
 51       CONTINUE
 5      CONTINUE
      END IF
C
C
C
      CALL JEDEMA()
      END
