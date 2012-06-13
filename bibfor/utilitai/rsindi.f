      SUBROUTINE RSINDI(TYSCA,IAOBJ,PAOBJ,JORDR,IVAL,RVAL,KVAL,CVAL,
     &                  EPSI,CRIT,NBORDR,NBTROU,NUTROU,NDIM)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'jeveux.h'
      INTEGER NBORDR,NBTROU,NUTROU(*),NDIM,IVAL,PAOBJ
      REAL*8 RVAL,EPSI
      CHARACTER*4 TYSCA
      CHARACTER*(*) KVAL,CRIT
      COMPLEX*16 CVAL
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C RESPONSABLE VABHHTS J.PELLET
C      TROUVER DANS LA TABLE ZX(IAOBJ-1+I),I=1,NBORDR LE SCALAIRE
C      IVAL,RVAL,CVAL...
C               AVEC LA PRECISION  / RELATIVE EPSI
C                                  / ABSOLUE  EPSI
C      LE TEST FAIT EST : ABS(V-VR).LE.ABS(EPSI*VR) EN RELATIF
C                    OU   ABS(V-VR).LE.ABS(EPSI)    EN ABSOLU
C ----------------------------------------------------------------------
C IN  : TYSCA  : R8 OU C16 OU I8 OU K8,  K16, K24, K32, K80
C IN  : IAOBJ  : ADRESSE DE LA TABLE DANS ZI, ZR OU ZC
C IN  : PAOBJ  : "PAS" dU parcours de l'objet :
C                 ZX(IAOBJ),ZX(IAOBJ+1*PAOBJ),,ZX(IAOBJ+2*PAOBJ), ...
C IN  : JORDR  : ADRESSE DU .ORDR DU RESULTAT
C IN  : IVAL   : VALEUR CHECHEE SI ENTIERE.
C IN  : RVAL   : VALEUR CHECHEE SI REELLE.
C IN  : KVAL   : VALEUR CHECHEE SI CARACTERE.
C IN  : CVAL   : VALEUR CHECHEE SI COMPLEXE.
C IN  : CRIT   : CRITERE : 'RELATIF' OU 'ABSOLU'
C IN  : EPSI   : PRECISION VOULUE.
C IN  : NBORDR : DIMENSION MAXI DE LA TABLE DE RECHERCHE.
C IN  : NDIM   : DIMENSION MAXI DE LA LISTE A REMPLIR.
C OUT : NBTROU : NOMBRE DE VALEURS CONVENABLES.
C OUT : NUTROU : LISTE DES INDICES DES VALEURS CONVENABLES.
C                   SI NBTROU EST > NDIM , ON SIGNALE L'ERREUR EN
C                   RENDANT NBTROU = - NBTROU
C ----------------------------------------------------------------------
      CHARACTER*8 CRIT2
      LOGICAL DEPASS,TROUVE
C     ------------------------------------------------------------------

      CRIT2 = CRIT
      NBTROU = 0
      DEPASS = .FALSE.
      IF (TYSCA(1:1).EQ.'R') THEN
        DO 10 I = 1,NBORDR
          IF (CRIT2(1:4).EQ.'RELA') THEN
            IF (ABS(ZR(IAOBJ+(I-1)*PAOBJ)-RVAL).LE.ABS(EPSI*RVAL)) THEN
              TROUVE = .TRUE.
            ELSE
              TROUVE = .FALSE.
            END IF
          ELSE IF (CRIT2(1:4).EQ.'ABSO') THEN
            IF (ABS(ZR(IAOBJ+(I-1)*PAOBJ)-RVAL).LE.ABS(EPSI)) THEN
              TROUVE = .TRUE.
            ELSE
              TROUVE = .FALSE.
            END IF
          ELSE
            CALL U2MESK('F','ALGORITH3_42',1,CRIT2)
          END IF
          IF (TROUVE) THEN
            NBTROU = NBTROU + 1
            IF (NBTROU.LE.NDIM) THEN
              NUTROU(NBTROU) = ZI(JORDR+I-1)
            ELSE
              DEPASS = .TRUE.
            END IF
          END IF
   10   CONTINUE
      ELSE IF (TYSCA(1:1).EQ.'I') THEN
        DO 20 I = 1,NBORDR
          IF (ZI(IAOBJ+(I-1)*PAOBJ).EQ.IVAL) THEN
            NBTROU = NBTROU + 1
            IF (NBTROU.LE.NDIM) THEN
              NUTROU(NBTROU) = ZI(JORDR+I-1)
            ELSE
              DEPASS = .TRUE.
            END IF
          END IF
   20   CONTINUE
      ELSE IF (TYSCA.EQ.'K8  ') THEN
        DO 30 I = 1,NBORDR
          IF (ZK8(IAOBJ+(I-1)*PAOBJ).EQ.KVAL) THEN
            NBTROU = NBTROU + 1
            IF (NBTROU.LE.NDIM) THEN
              NUTROU(NBTROU) = ZI(JORDR+I-1)
            ELSE
              DEPASS = .TRUE.
            END IF
          END IF
   30   CONTINUE
      ELSE IF (TYSCA.EQ.'K16 ') THEN
        DO 40 I = 1,NBORDR
          IF (ZK16(IAOBJ+(I-1)*PAOBJ).EQ.KVAL) THEN
            NBTROU = NBTROU + 1
            IF (NBTROU.LE.NDIM) THEN
              NUTROU(NBTROU) = ZI(JORDR+I-1)
            ELSE
              DEPASS = .TRUE.
            END IF
          END IF
   40   CONTINUE
      ELSE IF (TYSCA.EQ.'K24 ') THEN
        DO 50 I = 1,NBORDR
          IF (ZK24(IAOBJ+(I-1)*PAOBJ).EQ.KVAL) THEN
            NBTROU = NBTROU + 1
            IF (NBTROU.LE.NDIM) THEN
              NUTROU(NBTROU) = ZI(JORDR+I-1)
            ELSE
              DEPASS = .TRUE.
            END IF
          END IF
   50   CONTINUE
      ELSE IF (TYSCA.EQ.'K32 ') THEN
        DO 60 I = 1,NBORDR
          IF (ZK32(IAOBJ+(I-1)*PAOBJ).EQ.KVAL) THEN
            NBTROU = NBTROU + 1
            IF (NBTROU.LE.NDIM) THEN
              NUTROU(NBTROU) = ZI(JORDR+I-1)
            ELSE
              DEPASS = .TRUE.
            END IF
          END IF
   60   CONTINUE
      ELSE IF (TYSCA.EQ.'K80 ') THEN
        DO 70 I = 1,NBORDR
          IF (ZK80(IAOBJ+(I-1)*PAOBJ).EQ.KVAL) THEN
            NBTROU = NBTROU + 1
            IF (NBTROU.LE.NDIM) THEN
              NUTROU(NBTROU) = ZI(JORDR+I-1)
            ELSE
              DEPASS = .TRUE.
            END IF
          END IF
   70   CONTINUE
      ELSE IF (TYSCA(1:1).EQ.'C') THEN
        DO 80 I = 1,NBORDR
          IF (CRIT2(1:4).EQ.'RELA') THEN
            IF (ABS(ZC(IAOBJ+(I-1)*PAOBJ)-CVAL).LE.ABS(EPSI*CVAL)) THEN
              TROUVE = .TRUE.
            ELSE
              TROUVE = .FALSE.
            END IF
          ELSE IF (CRIT2(1:4).EQ.'ABSO') THEN
            IF (ABS(ZC(IAOBJ+(I-1)*PAOBJ)-CVAL).LE.ABS(EPSI)) THEN
              TROUVE = .TRUE.
            ELSE
              TROUVE = .FALSE.
            END IF
          ELSE
            CALL U2MESK('F','ALGORITH3_42',1,CRIT2)
          END IF
          IF (TROUVE) THEN
            NBTROU = NBTROU + 1
            IF (NBTROU.LE.NDIM) THEN
              NUTROU(NBTROU) = ZI(JORDR+I-1)
            ELSE
              DEPASS = .TRUE.
            END IF
          END IF
   80   CONTINUE
      ELSE
        CALL U2MESK('F','UTILITAI4_33',1,TYSCA)
      END IF


      IF (DEPASS) NBTROU = -NBTROU

      END
