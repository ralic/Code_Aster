      SUBROUTINE BARTET ( I1, I2, COOR, POIN )
      IMPLICIT   NONE
      INTEGER             I1, I2, POIN(*)
      REAL*8              COOR(*)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/09/2007   AUTEUR DURAND C.DURAND 
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
C     BARSOUM : TRAITEMENT DES MAILLES "TETRA10"
C-----------------------------------------------------------------------
C
      INTEGER   I, N1, N2, N3
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
C                       TRAITEMENT DES "POI1"
C     ------------------------------------------------------------------
      IF ( I1.EQ.1 .AND. I2.EQ.0 ) THEN
         DO 110 I = 1 , 3
            IF ( I .EQ. 1 ) THEN
               N1 = 1
               N2 = 2
               N3 = 5
            ELSEIF ( I .EQ. 2 ) THEN
               N1 = 1
               N2 = 3
               N3 = 7
            ELSEIF ( I .EQ. 3 ) THEN
               N1 = 1
               N2 = 4
               N3 = 8
            ENDIF
            CALL BARSO1 ( N1, N2, N3, COOR, POIN )
 110     CONTINUE
      ELSEIF ( I1.EQ.2 .AND. I2.EQ.0 ) THEN
         DO 120 I = 1 , 3
            IF ( I .EQ. 1 ) THEN
               N1 = 2
               N2 = 1
               N3 = 5
            ELSEIF ( I .EQ. 2 ) THEN
               N1 = 2
               N2 = 3
               N3 = 6
            ELSEIF ( I .EQ. 3 ) THEN
               N1 = 2
               N2 = 4
               N3 = 9
            ENDIF
            CALL BARSO1 ( N1, N2, N3, COOR, POIN )
 120     CONTINUE
      ELSEIF ( I1.EQ.3 .AND. I2.EQ.0 ) THEN
         DO 130 I = 1 , 3
            IF ( I .EQ. 1 ) THEN
               N1 = 3
               N2 = 1
               N3 = 7
            ELSEIF ( I .EQ. 2 ) THEN
               N1 = 3
               N2 = 2
               N3 = 6
            ELSEIF ( I .EQ. 3 ) THEN
               N1 = 3
               N2 = 4
               N3 = 10
            ENDIF
            CALL BARSO1 ( N1, N2, N3, COOR, POIN )
 130     CONTINUE
      ELSEIF ( I1.EQ.4 .AND. I2.EQ.0 ) THEN
         DO 140 I = 1 , 3
            IF ( I .EQ. 1 ) THEN
               N1 = 4
               N2 = 1
               N3 = 8
            ELSEIF ( I .EQ. 2 ) THEN
               N1 = 4
               N2 = 2
               N3 = 9
            ELSEIF ( I .EQ. 3 ) THEN
               N1 = 4
               N2 = 3
               N3 = 10
            ENDIF
            CALL BARSO1 ( N1, N2, N3, COOR, POIN )
 140     CONTINUE
C
C     ------------------------------------------------------------------
C                       TRAITEMENT DES "SEG3"
C     ------------------------------------------------------------------
      ELSEIF ( I1+I2 .EQ. 3 ) THEN
         DO 210 I = 1 , 4
            IF ( I .EQ. 1 ) THEN
               N1 = 2
               N2 = 4
               N3 = 9
            ELSEIF ( I .EQ. 2 ) THEN
               N1 = 2
               N2 = 3
               N3 = 6
            ELSEIF ( I .EQ. 3 ) THEN
               N1 = 1
               N2 = 4
               N3 = 8
            ELSEIF ( I .EQ. 4 ) THEN
               N1 = 1
               N2 = 3
               N3 = 7
            ENDIF
            CALL BARSO1 ( N1, N2, N3, COOR, POIN )
 210     CONTINUE
C
      ELSEIF ( I1+I2 .EQ. 4 ) THEN
         DO 220 I = 1 , 4
            IF ( I .EQ. 1 ) THEN
               N1 = 3
               N2 = 4
               N3 = 10
            ELSEIF ( I .EQ. 2 ) THEN
               N1 = 3
               N2 = 2
               N3 = 6
            ELSEIF ( I .EQ. 3 ) THEN
               N1 = 1
               N2 = 4
               N3 = 8
            ELSEIF ( I .EQ. 4 ) THEN
               N1 = 1
               N2 = 2
               N3 = 5
            ENDIF
            CALL BARSO1 ( N1, N2, N3, COOR, POIN )
 220     CONTINUE
C
      ELSEIF ( (I1+I2.EQ.5) .AND. (I1.EQ.2 .OR. I2.EQ.2) ) THEN
         DO 230 I = 1 , 4
            IF ( I .EQ. 1 ) THEN
               N1 = 3
               N2 = 4
               N3 = 10
            ELSEIF ( I .EQ. 2 ) THEN
               N1 = 3
               N2 = 1
               N3 = 7
            ELSEIF ( I .EQ. 3 ) THEN
               N1 = 2
               N2 = 4
               N3 = 9
            ELSEIF ( I .EQ. 4 ) THEN
               N1 = 2
               N2 = 1
               N3 = 5
            ENDIF
            CALL BARSO1 ( N1, N2, N3, COOR, POIN )
 230     CONTINUE
C
      ELSEIF ( (I1+I2.EQ.5) .AND. (I1.EQ.4 .OR. I2.EQ.4) ) THEN
         DO 240 I = 1 , 4
            IF ( I .EQ. 1 ) THEN
               N1 = 4
               N2 = 2
               N3 = 9
            ELSEIF ( I .EQ. 2 ) THEN
               N1 = 4
               N2 = 3
               N3 = 10
            ELSEIF ( I .EQ. 3 ) THEN
               N1 = 1
               N2 = 2
               N3 = 5
            ELSEIF ( I .EQ. 4 ) THEN
               N1 = 1
               N2 = 3
               N3 = 7
            ENDIF
            CALL BARSO1 ( N1, N2, N3, COOR, POIN )
 240     CONTINUE
C
      ELSEIF ( I1+I2 .EQ. 6 ) THEN
         DO 250 I = 1 , 4
            IF ( I .EQ. 1 ) THEN
               N1 = 4
               N2 = 1
               N3 = 8
            ELSEIF ( I .EQ. 2 ) THEN
               N1 = 4
               N2 = 3
               N3 = 10
            ELSEIF ( I .EQ. 3 ) THEN
               N1 = 2
               N2 = 1
               N3 = 5
            ELSEIF ( I .EQ. 4 ) THEN
               N1 = 2
               N2 = 3
               N3 = 6
            ENDIF
            CALL BARSO1 ( N1, N2, N3, COOR, POIN )
 250     CONTINUE
C
      ELSEIF ( I1+I2 .EQ. 7 ) THEN
         DO 160 I = 1 , 4
            IF ( I .EQ. 1 ) THEN
               N1 = 4
               N2 = 1
               N3 = 8
            ELSEIF ( I .EQ. 2 ) THEN
               N1 = 4
               N2 = 2
               N3 = 9
            ELSEIF ( I .EQ. 3 ) THEN
               N1 = 3
               N2 = 1
               N3 = 7
            ELSEIF ( I .EQ. 4 ) THEN
               N1 = 3
               N2 = 2
               N3 = 6
            ENDIF
            CALL BARSO1 ( N1, N2, N3, COOR, POIN )
 160     CONTINUE
C
      ELSE
         CALL U2MESK('F','ALGORITH_36',1,'TETRA')
C
      ENDIF
C
      END
