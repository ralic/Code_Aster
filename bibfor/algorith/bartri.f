      SUBROUTINE BARTRI ( I1, I2, COOR, POIN )
      IMPLICIT   NONE
      INTEGER             I1, I2, POIN(*)
      REAL*8              COOR(*)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C     BARSOUM : TRAITEMENT DES MAILLES "TRIA6" ET "TRIA7"
C-----------------------------------------------------------------------
C
      INTEGER   I, N1, N2, N3
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
C                       TRAITEMENT DES "POI1"
C     ------------------------------------------------------------------
      IF ( I1.EQ.1 .AND. I2.EQ.0 ) THEN
         DO 110 I = 1 , 2
            IF ( I .EQ. 1 ) THEN
               N1 = 1
               N2 = 2
               N3 = 4
            ELSE
               N1 = 1
               N2 = 3
               N3 = 6
            ENDIF
            CALL BARSO1 ( N1, N2, N3, COOR, POIN )
 110     CONTINUE
      ELSEIF ( I1.EQ.2  .AND. I2.EQ.0 ) THEN
         DO 120 I = 1 , 2
            IF ( I .EQ. 1 ) THEN
               N1 = 2
               N2 = 1
               N3 = 4
            ELSE
               N1 = 2
               N2 = 3
               N3 = 5
            ENDIF
            CALL BARSO1 ( N1, N2, N3, COOR, POIN )
 120     CONTINUE
      ELSEIF ( I1.EQ.3  .AND. I2.EQ.0 ) THEN
         DO 130 I = 1 , 2
            IF ( I .EQ. 1 ) THEN
               N1 = 3
               N2 = 1
               N3 = 6
            ELSE
               N1 = 3
               N2 = 2
               N3 = 5
            ENDIF
            CALL BARSO1 ( N1, N2, N3, COOR, POIN )
 130     CONTINUE
C
C     ------------------------------------------------------------------
C                       TRAITEMENT DES "SEG3"
C     ------------------------------------------------------------------
      ELSEIF ( I1+I2 .EQ. 3 ) THEN
         DO 210 I = 1 , 2
            IF ( I .EQ. 1 ) THEN
               N1 = 2
               N2 = 3
               N3 = 5
            ELSE
               N1 = 1
               N2 = 3
               N3 = 6
            ENDIF
            CALL BARSO1 ( N1, N2, N3, COOR, POIN )
 210     CONTINUE
C
      ELSEIF ( I1+I2 .EQ. 5 ) THEN
         DO 220 I = 1 , 2
            IF ( I .EQ. 1 ) THEN
               N1 = 3
               N2 = 1
               N3 = 6
            ELSE
               N1 = 2
               N2 = 1
               N3 = 4
            ENDIF
            CALL BARSO1 ( N1, N2, N3, COOR, POIN )
 220     CONTINUE
C
      ELSEIF ( I1+I2 .EQ. 4 ) THEN
         DO 230 I = 1 , 2
            IF ( I .EQ. 1 ) THEN
               N1 = 3
               N2 = 2
               N3 = 5
            ELSE
               N1 = 1
               N2 = 2
               N3 = 4
            ENDIF
            CALL BARSO1 ( N1, N2, N3, COOR, POIN )
 230     CONTINUE
C
      ELSE
         CALL U2MESS('F','ALGORITH_41')
C
      ENDIF
C
      END
