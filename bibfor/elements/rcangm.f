      SUBROUTINE RCANGM ( NDIM,COOR, ANGMAS )
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER  NDIM
      REAL*8   ANGMAS(7),COOR(3)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C ......................................................................
C    - ORIENTATION DU MASSIF
C
C   IN      NDIM    I      : DIMENSION DU PROBLEME
C   IN      COOR    R        COORDONNEE DU POINT
C                            (CAS CYLINDRIQUE)
C   OUT     ANGMAS  R      : ANGLE NAUTIQUE ( OU EULERIEN )
C ......................................................................
      INTEGER  ICAMAS, IRET, I
      REAL*8   R8NNEM, R8DGRD,P(3,3),XG(3),YG(3),ORIG(3),DIRE(3)
      REAL*8   ALPHA,BETA
C     ------------------------------------------------------------------

      CALL TECACH ( 'NNO', 'PCAMASS', 1, ICAMAS, IRET )
      CALL R8INIR ( 7, 0.D0, ANGMAS ,1 )

      IF (IRET.EQ.0) THEN
         CALL R8INIR ( 7, 0.D0, ANGMAS ,1 )
         IF (ZR(ICAMAS).GT.0.D0) THEN
            ANGMAS(1) = ZR(ICAMAS+1)*R8DGRD()
            IF ( NDIM .EQ. 3 ) THEN
               ANGMAS(2) = ZR(ICAMAS+2)*R8DGRD()
               ANGMAS(3) = ZR(ICAMAS+3)*R8DGRD()
               ANGMAS(4) = 1.D0
            ENDIF
C           ECRITURE DES ANGLES D'EULER A LA FIN LE CAS ECHEANT
            IF (ABS(ZR(ICAMAS)-2.D0).LT.1.D-3) THEN
               IF ( NDIM .EQ. 3 ) THEN
                  ANGMAS(5) = ZR(ICAMAS+4)*R8DGRD()
                  ANGMAS(6) = ZR(ICAMAS+5)*R8DGRD()
                  ANGMAS(7) = ZR(ICAMAS+6)*R8DGRD()
               ELSE
                  ANGMAS(5) = ZR(ICAMAS+1)*R8DGRD()
               ENDIF
               ANGMAS(4) = 2.D0
            ENDIF

         ELSE IF ( ABS(ZR(ICAMAS)+1.D0).LT.1.D-3) THEN

C ON TRANSFORME LA DONNEE DU REPERE CYLINDRIQUE EN ANGLE NAUTIQUE
C (EN 3D, EN 2D ON MET A 0)

           IF (NDIM.EQ.3) THEN
             ALPHA=ZR(ICAMAS+1)*R8DGRD()
             BETA =ZR(ICAMAS+2)*R8DGRD()
             DIRE(1) = COS(ALPHA)*COS(BETA)
             DIRE(2) = SIN(ALPHA)*COS(BETA)
             DIRE(3) = -SIN(BETA)
             ORIG(1)=ZR(ICAMAS+4)
             ORIG(2)=ZR(ICAMAS+5)
             ORIG(3)=ZR(ICAMAS+6)
             CALL UTRCYL(COOR,DIRE,ORIG,P)
             DO 1 I=1,3
               XG(I)=P(1,I)
               YG(I)=P(2,I)
    1        CONTINUE
             CALL ANGVXY(XG, YG, ANGMAS)
           ELSE
             CALL U2MESS('F','ELEMENTS2_38')
             CALL R8INIR ( 7, 0.D0, ANGMAS ,1 )
           ENDIF
         ENDIF
C
      ELSEIF (IRET.EQ.1) THEN
         CALL R8INIR ( 7, R8NNEM(), ANGMAS ,1 )
C
      ELSEIF (IRET.EQ.2) THEN
         CALL R8INIR ( 7, 0.D0, ANGMAS ,1 )
C
      ENDIF
C
      END
