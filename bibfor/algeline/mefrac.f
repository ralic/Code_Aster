      SUBROUTINE MEFRAC(MAILLA,NBGRMX,NOMRAC,NBGRMA,NOMCYL)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER      NBGRMX,NBGRMA
      CHARACTER*8  NOMCYL(*),MAILLA,NOMRAC
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C     RECHERCHE DES GROUPES DE MAILLES PRESENTANT UNE RACINE COMMUNE
C     SOUS LA FORME *RACINE*, OU *RACINE, OU RACINE*.
C     OPERATEUR APPELANT : OP0144 , FLUST3
C ----------------------------------------------------------------------
C     OPTION DE CALCUL   : CALC_FLUI_STRU , CALCUL DES PARAMETRES DE
C     COUPLAGE FLUIDE-STRUCTURE POUR UNE CONFIGURATION DE TYPE "FAISCEAU
C     DE TUBES SOUS ECOULEMENT AXIAL"
C ----------------------------------------------------------------------
C IN  : MAILLA : NOM DU MAILLAGE
C IN  : NBGRMX : NOMBRE DE GROUPES DE MAILLES DU MAILLAGE
C IN  : NOMRAC : NOM DE LA RACINE COMMUNE
C OUT : NBGRMA : NOMBRE DE GROUPES DE MAILLES AVEC LA RACINE COMMUNE
C                NOMRAC
C OUT : NOMCYL : NOMS DES GROUPES DE MAILLES AVEC LA RACINE COMMUNE
C                NOMRAC
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     ------------------------------------------------------------------
      CHARACTER*8  NOMGRI
      CHARACTER*1  K1BID
C     ------------------------------------------------------------------
C
C
      CALL JEMARQ()
C
      NBGRMA = 0
      IPRE = 0
      ISUF = 0
      ICAR = 0
      IF(NOMRAC(1:1).EQ.'*') THEN
         IPRE = 1
      ENDIF
      DO 10 I = 2,8
         IF(NOMRAC(I:I).EQ.'*') THEN
            ISUF = 1
            ICAR = I - 1 - IPRE
            GOTO 20
         ELSE IF(NOMRAC(I:I).EQ.' ') THEN
            ICAR = I - 1 - IPRE
            GOTO 20
         ENDIF
  10  CONTINUE
  20  CONTINUE
C
      IF(ISUF.EQ.0.AND.IPRE.EQ.0) THEN
         CALL U2MESK('F','ALGELINE_86',1,NOMRAC)
      ENDIF
C
      IF(IPRE.EQ.0) THEN
         DO 40 I = 1,NBGRMX
            CALL JENUNO(JEXNUM(MAILLA//'.GROUPEMA',I),NOMGRI)
            IF(NOMRAC(1:ICAR).EQ.NOMGRI(1:ICAR)) THEN
               NBGRMA = NBGRMA + 1
               NOMCYL(NBGRMA) = NOMGRI
            ENDIF
  40     CONTINUE
      ELSE IF(IPRE.EQ.1.AND.ISUF.EQ.0) THEN
         DO 70 I = 1,NBGRMX
            CALL JENUNO(JEXNUM(MAILLA//'.GROUPEMA',I),NOMGRI)
            DO 50 J = 2,8-ICAR
               IF(NOMRAC(2:2).EQ.NOMGRI(J:J)) THEN
                  IF(NOMRAC(2:ICAR+1).EQ.NOMGRI(J:(J+ICAR-1)).AND.
     &               NOMGRI((J+ICAR):(J+ICAR)).EQ.' ') THEN
                     NBGRMA = NBGRMA + 1
                     NOMCYL(NBGRMA) = NOMGRI
                     GOTO 60
                  ENDIF
               ENDIF
  50        CONTINUE
            J = 8-ICAR+1
            IF(NOMRAC(2:2).EQ.NOMGRI(J:J)) THEN
               IF(NOMRAC(2:ICAR+1).EQ.NOMGRI(J:(J+ICAR-1))) THEN
                  NBGRMA = NBGRMA + 1
                  NOMCYL(NBGRMA) = NOMGRI
               ENDIF
            ENDIF
  60        CONTINUE
  70     CONTINUE
      ELSE IF(IPRE.EQ.1.AND.ISUF.NE.0) THEN
         DO 100 I = 1,NBGRMX
         CALL JENUNO(JEXNUM(MAILLA//'.GROUPEMA',I),NOMGRI)
            DO 80 J = 1,8-ICAR
               IF(NOMRAC(2:2).EQ.NOMGRI(J:J)) THEN
                 IF(NOMRAC(2:ICAR+1).EQ.NOMGRI(J:(J+ICAR-1))) THEN
                    NBGRMA = NBGRMA + 1
                    NOMCYL(NBGRMA) = NOMGRI
                    GOTO 90
                 ENDIF
              ENDIF
  80        CONTINUE
  90        CONTINUE
 100     CONTINUE
      ENDIF
      IF(NBGRMA.EQ.0) THEN
         CALL U2MESS('F','ALGELINE_87')
      ENDIF
C
      IFM = IUNIFI('MESSAGE')
      WRITE (IFM,*) '==============================================='
     &   ,'================================='
      WRITE (IFM,*) '           GROUPES DE MAILLES SELECTIONNES '
     &   ,'POUR LA RACINE COMMUNE'
      WRITE (IFM,*) '==============================================='
     &   ,'================================='
      NT = INT(NBGRMA/8)
      DO 200 I = 1,NT
         NDEB = NT*(I-1)+1
         NFIN = NT*(I-1)+8
         WRITE (IFM,6001) (NOMCYL(J), J=NDEB,NFIN)
 200  CONTINUE
      IF((NT*8).LT.NBGRMA) THEN
         NDEB = NT*8+1
         NFIN = NBGRMA
         WRITE (IFM,6001) (NOMCYL(J), J=NDEB,NFIN)
      ENDIF
      WRITE (IFM,*) '==============================================='
     &   ,'================================='
C
 6001 FORMAT (1X,6(2X,A8))
C
C
      CALL JEDEMA()
      END
