      SUBROUTINE ORIEMA ( NOMAIL, TPMAIL, NBNMAI, LNMAIL,
     &                    TYP3D, NBNM3D, LNM3D,
     &                    NDIM, COOR, REORIE, NORIEN, IFM, NIV )
      IMPLICIT   NONE
      INTEGER             NBNMAI, LNMAIL(*), NBNM3D, LNM3D(*), NDIM,
     &                    NORIEN, IFM, NIV
      REAL*8              COOR(*)
      LOGICAL             REORIE
      CHARACTER*8         NOMAIL, TPMAIL, TYP3D
      CHARACTER*24 VALK(2)
C.======================================================================
C MODIF MODELISA  DATE 13/12/2006   AUTEUR PELLET J.PELLET 
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C ORIEMA  --  ORIENTATION DE LA MAILLE DE NOM NOMAIL
C             DE TELLE MANIERE A CE QUE LA NORMALE A CETTE MAILLE
C             SOIT EXTERIEURE AU VOLUME.
C             CETTE MAILLE EST UNE MAILLE DE PEAU :
C               .EN 2D C'EST UNE MAILLE DE TYPE SEG2 OU SEG3
C               .EN 3D C'EST UNE MAILLE DE TYPE TRIA3, TRIA6,
C                                               QUAD4, QUAD8 OU QUAD9
C
C IN  : REORIE : = .FALSE.  ON VERIFIE L'ORIENTATION
C                = .TRUE.   ON REORIENTE
C OUT : NORIEN : = 0  LA MAILLE EST BIEN ORIENTEE
C                = 1  LA MAILLE EST A REORIENTER
C
C.========================= DEBUT DES DECLARATIONS ====================
C
      INTEGER       NBNOMX
      PARAMETER    (NBNOMX = 27)
      INTEGER       I, IC, N1, N2, N3, NBNSM, NBNS3D, INO, NBNOE
      INTEGER       LISNOE(NBNOMX)
      REAL*8        COON1(3),COON2(3),COON3(3), N1N2(3),N1N3(3), DDOT
      REAL*8        N(3), NORME, N1G(3), XG3D(3), XGM(3), XGN, ZERO
C
C ========================= DEBUT DU CODE EXECUTABLE ==================
C
      NORIEN = 0
      ZERO   = 0.D0
C
      DO 10 I = 1, NBNMAI
        LISNOE(I) = LNMAIL(I)
  10  CONTINUE
      DO 12 I = 1, 3
         N(I)    = ZERO
         N1N2(I) = ZERO
         N1N3(I) = ZERO
         XGM(I)  = ZERO
         XG3D(I) = ZERO
 12   CONTINUE
C
C --- NUMERO DES 2 (3 EN 3D) PREMIERS NOEUDS DE LA MAILLE :
C     ---------------------------------------------------
      N1 = LISNOE(1)
      N2 = LISNOE(2)
      IF (NDIM.EQ.3)  N3 = LISNOE(3)
C
      DO 20 IC = 1 , 3
         COON1(IC) = COOR(3*(N1-1)+IC)
         COON2(IC) = COOR(3*(N2-1)+IC)
 20   CONTINUE
      CALL VDIFF ( 3, COON2, COON1, N1N2 )
C
      IF (NDIM.EQ.2) THEN
         N(1) =  N1N2(2)
         N(2) = -N1N2(1)
      ELSEIF (NDIM.EQ.3) THEN
         DO 22 IC=1,3
            COON3(IC) = COOR(3*(N3-1)+IC)
 22      CONTINUE
         CALL VDIFF ( 3, COON3, COON1, N1N3 )
         CALL PROVEC ( N1N2, N1N3, N )
      ENDIF
      CALL NORMEV ( N, NORME )
C
      IF (TPMAIL(1:4).EQ.'QUAD') THEN
         NBNSM = 4
      ELSEIF (TPMAIL(1:4).EQ.'TRIA') THEN
         NBNSM = 3
      ELSEIF (TPMAIL(1:3).EQ.'SEG') THEN
         IF (NDIM.EQ.3) GOTO 9999
         NBNSM = 2
      ELSE
          VALK(1) = NOMAIL
          VALK(2) = TPMAIL
          CALL U2MESK('F','MODELISA5_94', 2 ,VALK)
      ENDIF
C
C --- CENTRE DE GRAVITE DE LA MAILLE DE PEAU
C
      DO 30 I = 1 , NBNSM
         INO = LISNOE(I)
         XGM(1) = XGM(1) + COOR(3*(INO-1)+1)
         XGM(2) = XGM(2) + COOR(3*(INO-1)+2)
         XGM(3) = XGM(3) + COOR(3*(INO-1)+3)
 30   CONTINUE
      XGM(1) = XGM(1) / NBNSM
      XGM(2) = XGM(2) / NBNSM
      XGM(3) = XGM(3) / NBNSM
C
      IF (TYP3D(1:4).EQ.'HEXA') THEN
         NBNS3D=8
      ELSEIF (TYP3D(1:4).EQ.'PENT') THEN
         NBNS3D=6
      ELSEIF (TYP3D(1:4).EQ.'PYRA') THEN
         NBNS3D=5
      ELSEIF (TYP3D(1:4).EQ.'TETR') THEN
         NBNS3D=4
      ELSEIF (TYP3D(1:4).EQ.'QUAD') THEN
         NBNS3D=4
      ELSEIF (TYP3D(1:4).EQ.'TRIA') THEN
         NBNS3D=3
      ENDIF
C
C --- DETERMINATION DU CENTRE DE GRAVITE DE LA MAILLE 3D
C
      DO 40 I = 1 , NBNS3D
         INO = LNM3D(I)
         XG3D(1)  = XG3D(1) + COOR(3*(INO-1)+1)
         XG3D(2)  = XG3D(2) + COOR(3*(INO-1)+2)
         XG3D(3)  = XG3D(3) + COOR(3*(INO-1)+3)
 40   CONTINUE
      XG3D(1) = XG3D(1) / NBNS3D
      XG3D(2) = XG3D(2) / NBNS3D
      XG3D(3) = XG3D(3) / NBNS3D

      CALL VDIFF  ( 3, XG3D, XGM, N1G )
      CALL NORMEV ( N1G, NORME )
      XGN = DDOT ( 3, N1G, 1, N, 1 )
C
C --- SI XGN > 0, LA NORMALE A LA MAILLE DE PEAU
C --- EST DIRIGEE VERS L'INTERIEUR DU VOLUME, IL FAUT
C --- LA REORIENTER :
C     -------------
      IF ( XGN .GT. ZERO ) THEN
         NORIEN = NORIEN + 1
         IF ( .NOT. REORIE ) GOTO 9999
         IF ( TPMAIL(1:5).EQ.'QUAD9' .OR.
     &        TPMAIL(1:5).EQ.'TRIA7' ) THEN
            NBNOE = NBNMAI - 1
         ELSE
            NBNOE = NBNMAI
         ENDIF
         INO = 0
         DO 50 I = NBNSM, 1 , -1
            INO = INO+1
            LNMAIL(I) = LISNOE(INO)
 50      CONTINUE
         IF (NBNSM.NE.NBNOE) THEN
            INO = 0
            DO 52 I = NBNOE-1, NBNSM+1 , -1
               INO = INO+1
               LNMAIL(I) = LISNOE(INO+NBNSM)
 52         CONTINUE
            LNMAIL(NBNOE) = LISNOE(NBNOE)
         ENDIF
         IF (NIV.EQ.2) THEN
            WRITE(IFM,*) ' REORIENTATION MAILLE ',NOMAIL,
     &                   ' NOEUDS ',(LNMAIL(I),I=1,NBNMAI),
     &                         ' PRODUIT SCALAIRE ',XGN
         ENDIF
      ENDIF
C
 9999 CONTINUE
C
      END
