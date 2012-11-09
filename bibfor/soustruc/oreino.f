      SUBROUTINE OREINO ( NOMA, LNOEUD, NBNO, NORI, NEXT, COOR,
     &                    CRIT, PREC,IERA, IER )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM
      INTEGER             LNOEUD(*), NBNO, NORI, NEXT, IER,IERA
      REAL*8              COOR(*), PREC
      CHARACTER*8         NOMA
      CHARACTER*(*)       CRIT
C-----------------------------------------------------------------------
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
C-----------------------------------------------------------------------
C     BUT : CLASSER DES NUMEROS DE NOEUDS SELON LEUR PROJECTION
C           SUR UN SEGMENT
C-----------------------------------------------------------------------
C     I/O : LNOEUD: NUMEROS DES NOEUDS
C     IN  : NBNO  : NOMBRE DE NOEUDS
C     IN  : NORI  : NUMERO DU NOEUD ORIGINE
C     IN  : NEXT  : NUMERO DU NOEUD EXTREMITE
C     IN  : COOR  : COORDONNEES DES NOEUDS
C     IN  : CRIT  : CRITERE
C     IN  : PREC  : PRECISION
C     IN  : IER   : CODE RETOUR,  = 0  OK
C-----------------------------------------------------------------------
      INTEGER       I, J, K, N, IDIS, INOE, INOD
      REAL*8        XA, YA, ZA, XB, YB, ZB, XAB, YAB, ZAB, AB2, XM, YM,
     &              ZM, XAM, YAM, ZAM, C, C2, XV, YV, ZV, V2, R8B,
     &              ECART,R8PREM,VALR
      CHARACTER*8   NOMN
      CHARACTER*24  NOMNOE
      CHARACTER*24 VALK(2)
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
      NOMNOE  = NOMA//'.NOMNOE         '
C
      IER = 0
C
      XA = COOR(3*(NORI-1)+1)
      YA = COOR(3*(NORI-1)+2)
      ZA = COOR(3*(NORI-1)+3)
C
      XB = COOR(3*(NEXT-1)+1)
      YB = COOR(3*(NEXT-1)+2)
      ZB = COOR(3*(NEXT-1)+3)
C
      XAB = XB-XA
      YAB = YB-YA
      ZAB = ZB-ZA
      AB2 = XAB**2 + YAB**2 + ZAB**2
      IF ( AB2 .EQ. 0.0D0 ) THEN
        CALL U2MESS('A','SOUSTRUC_20')
        IER = IER + 1
        GOTO 9999
      ENDIF
C
      CALL WKVECT ( '&&OREINO.BARY', 'V V R', NBNO, IDIS )
C
C     --- CALCUL DE LA CORDONNEE BARYCENTRIQUE ---
C
      DO 100 INOE = 1,NBNO
         INOD = LNOEUD(INOE)
         XM  = COOR(3*(INOD-1)+1)
         YM  = COOR(3*(INOD-1)+2)
         ZM  = COOR(3*(INOD-1)+3)
         XAM = XM-XA
         YAM = YM-YA
         ZAM = ZM-ZA
         C = (XAM*XAB+YAM*YAB+ZAM*ZAB) / AB2
         C2 = XAM**2 + YAM**2 + ZAM**2
         XV = XAM - C*XAB
         YV = YAM - C*YAB
         ZV = ZAM - C*ZAB
         V2 = XV**2 + YV**2 + ZV**2
C        --- VERIFICATION QUE LA DISTANCE A L'AXE
C                         NE DEPASSE PAS LA TOLERANCE ---
         IF ( CRIT(1:4) .EQ. 'ABSO' ) THEN
            R8B = V2
         ELSEIF ( CRIT(1:4) .EQ. 'RELA' ) THEN
            R8B = V2 / AB2
         ELSE
            CALL U2MESS('A','SOUSTRUC_21')
            IER = IER + 1
            GOTO 9999
         ENDIF
         R8B = SQRT( R8B )
         IF ( R8B .GT. PREC ) THEN
            V2 = SQRT( V2 )
            CALL JENUNO( JEXNUM(NOMNOE,INOD),NOMN)
            IF (IERA.EQ.0) THEN
              CALL U2MESG('A','SOUSTRUC_22',1,NOMN,0,0,1,V2)
              IERA = IERA+1
            ELSE
              CALL U2MESG('I','SOUSTRUC_22',1,NOMN,0,0,1,V2)
            ENDIF
            IER = IER + 1
         ENDIF
C        --- VERIFICATION QUE LA PROJECTION EST BIEN
C                         SITUEE ENTRE LES POINTS A ET B ---
         ECART = (C2-AB2)/AB2
         IF ( C.LT.0.0D0 .OR. C2.GT.AB2 ) THEN
            IF (ECART.GT.R8PREM()) THEN
               CALL JENUNO( JEXNUM(NOMNOE,INOD),NOMN)
               VALK (1) = NOMN
               VALK (2) = NOMN
               VALR = C
               CALL U2MESG('A', 'SOUSTRUC_86',2,VALK,0,0,1,VALR)
               IER = IER + 1
            ENDIF
         ENDIF
         ZR(IDIS-1+INOE) = C
  100 CONTINUE
C
C     --- TRI PAR BUBBLE SORT ---
C
      DO 300 K = 1,NBNO-1
         DO 200 I = NBNO-1,K,-1
            J = I+1
            IF (ZR(IDIS-1+I).GT.ZR(IDIS-1+J)) THEN
                          C=ZR(IDIS-1+J)
               ZR(IDIS-1+J)=ZR(IDIS-1+I)
               ZR(IDIS-1+I)=C
                       N=LNOEUD(J)
               LNOEUD(J)=LNOEUD(I)
               LNOEUD(I)=N
            ENDIF
  200    CONTINUE
  300 CONTINUE
C
C     --- VERIFICATION QUE DEUX NOEUDS CONSECUTIFS
C                          N'ONT PAS LA MEME PROJECTION ---
      DO 400 INOE = 1,NBNO-1
         IF (ZR(IDIS-1+INOE).EQ.ZR(IDIS-1+INOE+1)) THEN
            CALL U2MESS('A','SOUSTRUC_23')
            IER = IER + 1
         ENDIF
  400 CONTINUE
C
 9999 CONTINUE
C
      CALL JEDETR('&&OREINO.BARY')
C
      CALL JEDEMA()
      END
