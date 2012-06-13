      SUBROUTINE SINGUM (NOMAIL,NDIM,NNOEM,NELEM,ITYPE,XY)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER NDIM,NNOEM,NELEM,ITYPE(NELEM)
      REAL*8  XY(3,NNOEM)
      CHARACTER*8  NOMAIL
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C TOLE CRP_20 CRS_1404
C
C     BUT:
C         CREATION D'OBJETS TEMPORAIRES NECESSAIRES POUR LE CALCUL
C         OPTION : 'SING_ELEM'
C
C
C     ARGUMENTS:
C     ----------
C
C      ENTREE :
C-------------
C IN   NOMAIL       : NOM UTILISATEUR DU MAILLAGE
C IN   NDIM     : DIMENSION DU PROBLEME
C IN   NNOEM        : NOMBRE DE NOEUDS DU MAILLAGE
C IN   NELEM        : NOMBRE D ELEMENTS FINIS DU MAILLAGE
C IN   ITYPE(NELEM) : NUMERO DU TYPE D'ELEMENT FINI
C IN   XY(3,NNOEM)  : XYONNEES DES NOEUDS
C
C      SORTIE :
C-------------
C OUT STOCKES DANS DES OBJETS TEMPORAIRES '&&SINGUM.XXXX'
C 1) '&&SINGUM.DIME' (DIM=3) CONTIENT
C     NBRE MAX DE NOEUDS SOMMETS CONNECTES AUX EFS UTILES (NSOMMX)
C     NBRE MAX D EFS UTILES CONNECTES AUX NOEUDS SOMMETS (NELCOM)
C     EN 2D UTILE = QUAD OU TRIA
C     EN 3D UTILE = TETRA OU HEXA
C     ORDRE DES EF (1 SI LINEAIRE ET 2 SI QUADRATIQUE)
C 2) '&&SINGUM.MESU' (DIM=NELEM) CONTIENT L AIRE OU LE VOLUME DES EFS
C 3) '&&SINGUM.CONN' (DIM=NELEM*(NSOMMX+2)) CONTIENT
C     1ERE VALEUR = NBRE DE NOEUDS SOMMETS CONNECTES A L EF N°X
C     2EME VALEUR = 1 SI EF UTILE 0 SINON
C     CONNECTIVITE  EF N°X=>N° DE NOEUDS SOMMETS CONNECTES A X
C 4) '&&SINGUM.CINV' (DIM=NNOEM*(NELCOM+2)) CONTIENT
C     1ERE VALEUR = NBRE D EFS UTILES CONNECTES AU NOEUD N°X
C     2EME VALEUR = 0 NOEUD MILIEU OU NON CONNECTE A UN EF UTILE
C                   1 NOEUD SOMMET A L INTERIEUR + LIE A UN EF UTILE
C                   2 NOEUD SOMMET BORD + LIE A UN EF UTILE
C     CONNECTIVITE  NOEUD N°X=>N° DES EF UTILE CONNECTES A X
C
C ......................................................................
C
C
C
C
      INTEGER JCONN1,JCONN2,JCINV1,JCINV2
      INTEGER JDIME,JMESU,JCONN,JCINV,ADRESS
      INTEGER INNO,INEL,JEL,NUEF,NUNO,I
      INTEGER IFAC,ISUR
      INTEGER NFAC,NSUR,NBPT
      INTEGER NSOMMX,NELCOM,NBRE,NBEF(NNOEM)
      INTEGER ORDRE
      INTEGER N1,N2,N3,N4,N5,N6,N7,N8
      INTEGER PT1(24),PT2(24)
      INTEGER NOMILI(NNOEM)
      CHARACTER*8 TYPEMA(NELEM)
      CHARACTER*24 CINV
      CHARACTER*24 CHDIME,CHMESU,CHCONN,CHCINV
      REAL*8   AIRE,VOLUME
      LOGICAL  TEST

      CALL JEMARQ()
C
C 1 - ADRESSES DE CONNECTIVITE EF=>NOEUDS CONNECTES
C                           ET NOEUD=>EF CONNECTES

      CALL JEVEUO(NOMAIL//'.CONNEX','L',JCONN1)
      CALL JEVEUO(JEXATR(NOMAIL//'.CONNEX','LONCUM'),'L',JCONN2)

      CINV = '&&SINGU.CONNECINVERSE   '
      CALL CNCINV(NOMAIL,0,0,'V',CINV)
      CALL JEVEUO(CINV,'L',JCINV1)
      CALL JEVEUO(JEXATR(CINV,'LONCUM'),'L',JCINV2)
C
C 2 - INITIALISATION DE NOMILI(NNOEM)
C     TOUS LES NOEUDS SONT SOMMETS A L INTERIEUR

      DO 10 INNO=1,NNOEM
        NOMILI(INNO)=1
 10   CONTINUE
C
C 3 - ON REMPLIT LES OBJETS '&&SINGUM.CONN' ET '&&SINGUM.MESU'
C     '&&SINGUE.CONN' (DIM=NELEM*(NSOMMX+2)) CONTIENT
C       1ERE VALEUR = NBRE DE NOEUDS SOMMETS CONNECTES A L EF N°X
C       2EME VALEUR = 1 SI EF UTILE 0 SINON
C       CONNECTIVITE  EF N°X=>N° DE NOEUDS SOMMETS CONNECTES A X
C    '&&SINGUM.MESU' : AIRE OU VOLUME DES EFS
C    ORDRE : ORDRE DES EF
C    DETECTION DES NOEUDS MILIEUX + NOEUDS BORDS

      NSOMMX=4
      CHMESU='&&SINGUM.MESU           '
      CALL WKVECT(CHMESU,'V V R',NELEM,JMESU)
      CHCONN='&&SINGUM.CONN           '
      CALL WKVECT(CHCONN,'V V I',NELEM*(NSOMMX+2),JCONN)

      ORDRE=0
      DO 20 INEL=1,NELEM
        CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYPE(INEL)),TYPEMA(INEL))
        IF (TYPEMA(INEL)(1:4).EQ. 'HEXA' .OR.
     &     TYPEMA(INEL)(1:5).EQ. 'PENTA'.OR.
     &     TYPEMA(INEL)(1:5).EQ. 'PYRAM') THEN
          CALL U2MESS('F','CALCULEL3_98')
        ENDIF
C
C OBJETS '&&SINGUM.CONN'
C
        ADRESS=JCONN+(NSOMMX+2)*(INEL-1)
        IF (TYPEMA(INEL)(1:4).EQ.'POI1' ) ZI(ADRESS+1-1)=1
        IF (TYPEMA(INEL)(1:3).EQ.'SEG'  ) ZI(ADRESS+1-1)=2
        IF (TYPEMA(INEL)(1:4).EQ.'TRIA' ) ZI(ADRESS+1-1)=3
        IF (TYPEMA(INEL)(1:4).EQ.'QUAD' ) ZI(ADRESS+1-1)=4
        IF (TYPEMA(INEL)(1:5).EQ.'TETRA') ZI(ADRESS+1-1)=4
        IF (TYPEMA(INEL)(1:4).EQ.'HEXA' ) ZI(ADRESS+1-1)=8
        ZI(ADRESS+2-1)=0
        IF (NDIM.EQ.2) THEN
          IF (TYPEMA(INEL)(1:4).EQ.'TRIA'.OR.
     &       TYPEMA(INEL)(1:4).EQ.'QUAD' ) THEN
            ZI(ADRESS+2-1)=1
          ENDIF
        ELSE
          IF (TYPEMA(INEL)(1:5).EQ.'TETRA'.OR.
     &       TYPEMA(INEL)(1:4).EQ.'HEXA' ) THEN
            ZI(ADRESS+2-1)=1
          ENDIF
        ENDIF
        DO 30 INNO=1,ZI(ADRESS+1-1)
          NUNO=ZI(JCONN1-1+ZI(JCONN2+INEL-1)+INNO-1)
          ZI(ADRESS+INNO+2-1)=NUNO
 30     CONTINUE
C
C OBJET '&&SINGUM.MESU' + RECHERCHE NOEUD BORD
C
        ZR(JMESU+INEL-1)=0.D0
        IF (NDIM.EQ.2) THEN
          IF (TYPEMA(INEL)(1:4).EQ.'TRIA') THEN
            N1=ZI(JCONN1-1+ZI(JCONN2+INEL-1)+1-1)
            N2=ZI(JCONN1-1+ZI(JCONN2+INEL-1)+2-1)
            N3=ZI(JCONN1-1+ZI(JCONN2+INEL-1)+3-1)
            AIRE=(XY(1,N2)-XY(1,N1))*(XY(2,N3)-XY(2,N1))
     &         -(XY(2,N2)-XY(2,N1))*(XY(1,N3)-XY(1,N1))
            ZR(JMESU+INEL-1)=ABS(AIRE)/2.D0
            NFAC=3
            PT1(1)=N1
            PT1(2)=N2
            PT1(3)=N3
            PT1(4)=N1
          ENDIF
          IF (TYPEMA(INEL)(1:4).EQ.'QUAD') THEN
            N1=ZI(JCONN1-1+ZI(JCONN2+INEL-1)+1-1)
            N2=ZI(JCONN1-1+ZI(JCONN2+INEL-1)+2-1)
            N3=ZI(JCONN1-1+ZI(JCONN2+INEL-1)+3-1)
            N4=ZI(JCONN1-1+ZI(JCONN2+INEL-1)+4-1)
            AIRE =((XY(1,N2)-XY(1,N1))*(XY(2,N4)-XY(2,N1))-
     &            (XY(2,N2)-XY(2,N1))*(XY(1,N4)-XY(1,N1)))
     &          +((XY(1,N4)-XY(1,N3))*(XY(2,N2)-XY(2,N3))-
     &            (XY(2,N4)-XY(2,N3))*(XY(1,N2)-XY(1,N3)))
            ZR(JMESU+INEL-1)=ABS(AIRE)/2.D0
            NFAC=4
            PT1(1)=N1
            PT1(2)=N2
            PT1(3)=N3
            PT1(4)=N4
            PT1(5)=N1
          ENDIF
        ELSE
          IF (TYPEMA(INEL)(1:5).EQ.'TETRA') THEN
            N1=ZI(JCONN1-1+ZI(JCONN2+INEL-1)+1-1)
            N2=ZI(JCONN1-1+ZI(JCONN2+INEL-1)+2-1)
            N3=ZI(JCONN1-1+ZI(JCONN2+INEL-1)+3-1)
            N4=ZI(JCONN1-1+ZI(JCONN2+INEL-1)+4-1)
            VOLUME =(XY(1,N1)-XY(1,N2))*
     &       ((XY(2,N1)-XY(2,N3))*(XY(3,N1)-XY(3,N4))
     &       -(XY(2,N1)-XY(2,N4))*(XY(3,N1)-XY(3,N3)))
            VOLUME = VOLUME-(XY(1,N1)-XY(1,N3))*
     &       ((XY(2,N1)-XY(2,N2))*(XY(3,N1)-XY(3,N4))
     &       -(XY(2,N1)-XY(2,N4))*(XY(3,N1)-XY(3,N2)))
            VOLUME = VOLUME+(XY(1,N1)-XY(1,N4))*
     &       ((XY(2,N1)-XY(2,N2))*(XY(3,N1)-XY(3,N3)) -
     &        (XY(2,N1)-XY(2,N3))*(XY(3,N1)-XY(3,N2)) )
            ZR(JMESU+INEL-1)=ABS(VOLUME) / 6.D0
            NFAC=4
            NBPT=3
            PT1(1)=N1
            PT1(2)=N2
            PT1(3)=N3
            PT1(4)=N1
            PT1(5)=N3
            PT1(6)=N4
            PT1(7)=N2
            PT1(8)=N3
            PT1(9)=N4
            PT1(10)=N1
            PT1(11)=N2
            PT1(12)=N4
          ENDIF
          IF (TYPEMA(INEL)(1:4).EQ.'HEXA') THEN
            N1=ZI(JCONN1-1+ZI(JCONN2+INEL-1)+1-1)
            N2=ZI(JCONN1-1+ZI(JCONN2+INEL-1)+2-1)
            N3=ZI(JCONN1-1+ZI(JCONN2+INEL-1)+3-1)
            N4=ZI(JCONN1-1+ZI(JCONN2+INEL-1)+4-1)
            N5=ZI(JCONN1-1+ZI(JCONN2+INEL-1)+5-1)
            N6=ZI(JCONN1-1+ZI(JCONN2+INEL-1)+6-1)
            N7=ZI(JCONN1-1+ZI(JCONN2+INEL-1)+7-1)
            N8=ZI(JCONN1-1+ZI(JCONN2+INEL-1)+8-1)
            NFAC=6
            NBPT=4
            PT1(1)=N1
            PT1(2)=N2
            PT1(3)=N3
            PT1(4)=N4
            PT1(5)=N5
            PT1(6)=N6
            PT1(7)=N7
            PT1(8)=N8
            PT1(9)=N1
            PT1(10)=N2
            PT1(11)=N6
            PT1(12)=N5
            PT1(13)=N4
            PT1(14)=N3
            PT1(15)=N7
            PT1(16)=N8
            PT1(17)=N2
            PT1(18)=N3
            PT1(19)=N7
            PT1(20)=N6
            PT1(21)=N1
            PT1(22)=N4
            PT1(23)=N8
            PT1(24)=N5
          ENDIF
        ENDIF

        IF (ZI(ADRESS+2-1).NE.1) GOTO 70
        DO 40 IFAC=1,NFAC
          IF (NDIM.EQ.2) THEN
            N1=PT1(IFAC)
            N2=PT1(IFAC+1)
          ELSE
            N1=PT1(NBPT*(IFAC-1)+1)
            N2=PT1(NBPT*(IFAC-1)+2)
            N3=PT1(NBPT*(IFAC-1)+3)
            IF (TYPEMA(INEL)(1:4).EQ.'HEXA') N4=PT1(NBPT*(IFAC-1)+4)
          ENDIF
          DO 50 JEL=1,NELEM
            IF (JEL.EQ.INEL) GOTO 50
            CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYPE(JEL)),TYPEMA(JEL))
            IF (NDIM.EQ.2) THEN
              IF (TYPEMA(JEL)(1:4).EQ.'TRIA') THEN
                NSUR=3
                PT2(1)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+1-1)
                PT2(2)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+2-1)
                PT2(3)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+3-1)
                PT2(4)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+1-1)
              ELSE IF (TYPEMA(JEL)(1:4).EQ.'QUAD') THEN
                NSUR=4
                PT2(1)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+1-1)
                PT2(2)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+2-1)
                PT2(3)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+3-1)
                PT2(4)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+4-1)
                PT2(5)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+1-1)
              ELSE
                GOTO 50
              ENDIF
            ELSE
              IF (TYPEMA(JEL)(1:5).EQ.'TETRA') THEN
                NSUR=4
                PT2(1)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+1-1)
                PT2(2)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+2-1)
                PT2(3)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+3-1)
                PT2(4)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+1-1)
                PT2(5)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+3-1)
                PT2(6)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+4-1)
                PT2(7)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+2-1)
                PT2(8)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+3-1)
                PT2(9)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+4-1)
                PT2(10)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+1-1)
                PT2(11)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+2-1)
                PT2(12)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+4-1)
              ELSE IF (TYPEMA(JEL)(1:4).EQ.'HEXA') THEN
                NSUR=6
                PT2(1)= ZI(JCONN1-1+ZI(JCONN2+JEL-1)+1-1)
                PT2(2)= ZI(JCONN1-1+ZI(JCONN2+JEL-1)+2-1)
                PT2(3)= ZI(JCONN1-1+ZI(JCONN2+JEL-1)+3-1)
                PT2(4)= ZI(JCONN1-1+ZI(JCONN2+JEL-1)+4-1)
                PT2(5)= ZI(JCONN1-1+ZI(JCONN2+JEL-1)+5-1)
                PT2(6)= ZI(JCONN1-1+ZI(JCONN2+JEL-1)+6-1)
                PT2(7)= ZI(JCONN1-1+ZI(JCONN2+JEL-1)+7-1)
                PT2(8)= ZI(JCONN1-1+ZI(JCONN2+JEL-1)+8-1)
                PT2(9)= ZI(JCONN1-1+ZI(JCONN2+JEL-1)+1-1)
                PT2(10)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+2-1)
                PT2(11)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+6-1)
                PT2(12)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+5-1)
                PT2(13)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+4-1)
                PT2(14)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+3-1)
                PT2(15)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+7-1)
                PT2(16)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+8-1)
                PT2(17)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+2-1)
                PT2(18)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+3-1)
                PT2(19)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+7-1)
                PT2(20)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+6-1)
                PT2(21)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+1-1)
                PT2(22)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+4-1)
                PT2(23)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+8-1)
                PT2(24)=ZI(JCONN1-1+ZI(JCONN2+JEL-1)+5-1)
              ELSE
                GOTO 50
              ENDIF
            ENDIF
            DO 60 ISUR=1,NSUR
              IF (NDIM.EQ.2) THEN
                TEST=(N1.EQ.PT2(ISUR).OR.N2.EQ.PT2(ISUR)).AND.
     &              (N1.EQ.PT2(ISUR+1).OR.N2.EQ.PT2(ISUR+1))
              ELSE
                IF (TYPEMA(INEL)(1:5).EQ.'TETRA') THEN
                  TEST=( N1.EQ.PT2(3*(ISUR-1)+1)  .OR.
     &                  N2.EQ.PT2(3*(ISUR-1)+1)  .OR.
     &                  N3.EQ.PT2(3*(ISUR-1)+1))  .AND.
     &                ( N1.EQ.PT2(3*(ISUR-1)+2)  .OR.
     &                  N2.EQ.PT2(3*(ISUR-1)+2)  .OR.
     &                  N3.EQ.PT2(3*(ISUR-1)+2))  .AND.
     &                ( N1.EQ.PT2(3*(ISUR-1)+3)  .OR.
     &                  N2.EQ.PT2(3*(ISUR-1)+3)  .OR.
     &                  N3.EQ.PT2(3*(ISUR-1)+3))
                ENDIF
                IF (TYPEMA(INEL)(1:4).EQ.'HEXA') THEN
                  TEST=( N1.EQ.PT2(4*(ISUR-1)+1) .OR.
     &                  N2.EQ.PT2(4*(ISUR-1)+1) .OR.
     &                  N3.EQ.PT2(4*(ISUR-1)+1) .OR.
     &                  N4.EQ.PT2(4*(ISUR-1)+1)) .AND.
     &                ( N1.EQ.PT2(4*(ISUR-1)+2) .OR.
     &                  N2.EQ.PT2(4*(ISUR-1)+2) .OR.
     &                  N3.EQ.PT2(4*(ISUR-1)+2) .OR.
     &                  N4.EQ.PT2(4*(ISUR-1)+2)) .AND.
     &                ( N1.EQ.PT2(4*(ISUR-1)+3) .OR.
     &                  N2.EQ.PT2(4*(ISUR-1)+3) .OR.
     &                  N3.EQ.PT2(4*(ISUR-1)+3 ) .OR.
     &                  N4.EQ.PT2(4*(ISUR-1)+3)) .AND.
     &                ( N1.EQ.PT2(4*(ISUR-1)+4) .OR.
     &                  N2.EQ.PT2(4*(ISUR-1)+4) .OR.
     &                  N3.EQ.PT2(4*(ISUR-1)+4) .OR.
     &                  N4.EQ.PT2(4*(ISUR-1)+4))
                ENDIF
              ENDIF
              IF (TEST) GOTO 40
 60         CONTINUE
 50       CONTINUE
          NOMILI(N1)=2
          NOMILI(N2)=2
          IF (NDIM.EQ.3) THEN
            NOMILI(N3)=2
            IF (TYPEMA(INEL)(1:4).EQ.'HEXA') NOMILI(N4)=2
          ENDIF
 40     CONTINUE

 70     CONTINUE
C
C ORDRE
C
        IF (ORDRE.EQ.0) THEN
          IF (TYPEMA(INEL)(1:5).EQ.'TRIA3' .OR.
     &       TYPEMA(INEL)(1:5).EQ.'QUAD4' .OR.
     &       TYPEMA(INEL)(1:6).EQ.'TETRA4'.OR.
     &       TYPEMA(INEL)(1:5).EQ.'HEXA8') ORDRE=1
          IF (TYPEMA(INEL)(1:5).EQ.'TRIA6'  .OR.
     &       TYPEMA(INEL)(1:5).EQ.'QUAD8'  .OR.
     &       TYPEMA(INEL)(1:5).EQ.'QUAD9'  .OR.
     &       TYPEMA(INEL)(1:7).EQ.'TETRA10'.OR.
     &       TYPEMA(INEL)(1:6).EQ.'HEXA20' .OR.
     &       TYPEMA(INEL)(1:6).EQ.'HEXA27' ) ORDRE=2
        ENDIF
C
C NOEUD MILIEU
C
        IF(TYPEMA(INEL)(1:4).EQ.'SEG3') THEN
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+3-1))=0
        ENDIF
        IF(TYPEMA(INEL)(1:5).EQ.'TRIA6') THEN
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+4-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+5-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+6-1))=0
        ENDIF
        IF(TYPEMA(INEL)(1:5).EQ.'QUAD8') THEN
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+5-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+6-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+7-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+8-1))=0
        ENDIF
        IF(TYPEMA(INEL)(1:5).EQ.'QUAD9') THEN
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+5-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+6-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+7-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+8-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+9-1))=0
        ENDIF
        IF(TYPEMA(INEL)(1:7).EQ.'TETRA10') THEN
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+5-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+6-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+7-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+8-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+9-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+10-1))=0
        ENDIF
        IF(TYPEMA(INEL)(1:6).EQ.'HEXA20') THEN
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+9-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+10-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+11-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+12-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+13-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+14-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+15-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+16-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+17-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+18-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+19-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+20-1))=0
        ENDIF
        IF(TYPEMA(INEL)(1:6).EQ.'HEXA27') THEN
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+9-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+10-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+11-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+12-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+13-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+14-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+15-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+16-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+17-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+18-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+19-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+20-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+21-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+22-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+23-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+24-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+25-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+26-1))=0
          NOMILI(ZI(JCONN1-1+ZI(JCONN2+INEL-1)+27-1))=0
        ENDIF

 20   CONTINUE
C
C 5 - CALCUL DE NELCOM ET NBEF(INNO)
C NELCOM     : NBRE MAX D EFS UTILES CONNECTES AUX NOEUDS SOMMETS
C NBRE       : NBRE D EF CONNECTES AU NOEUD INNO
C NBEF(INNO) : NBRE D EFS UTILES CONNECTES AU NOEUD INNO SOMMET

      NELCOM=0
      DO 80 INNO = 1,NNOEM
        NBEF(INNO)=0
        IF (NOMILI(INNO).NE.0) THEN
          NBRE=ZI(JCINV2+INNO)-ZI(JCINV2+INNO-1)
          DO 90 INEL=1,NBRE
            NUEF = ZI(JCINV1-1+ZI(JCINV2+INNO-1)+INEL-1)
            IF (NDIM.EQ.2) THEN
              IF (TYPEMA(NUEF)(1:4).EQ.'TRIA'.OR.
     &           TYPEMA(NUEF)(1:4).EQ.'QUAD') THEN
                NBEF(INNO)=NBEF(INNO)+1
              ENDIF
            ELSE
              IF (TYPEMA(NUEF)(1:5).EQ.'TETRA'.OR.
     &           TYPEMA(NUEF)(1:4).EQ.'HEXA') THEN
                NBEF(INNO)=NBEF(INNO)+1
              ENDIF
            ENDIF
 90       CONTINUE
          NELCOM=MAX(NELCOM,NBEF(INNO))
        ENDIF
 80   CONTINUE
C
C 5 - OBJET '&&SINGUM.DIME'
C
      CHDIME='&&SINGUM.DIME           '
      CALL WKVECT(CHDIME,'V V I',3,JDIME)
      ZI(JDIME+1-1)=NSOMMX
      ZI(JDIME+2-1)=NELCOM
      ZI(JDIME+3-1)=ORDRE

C 6 - OBJET '&&SINGUM.CINV'
C     1ERE VALEUR = NBRE D EFS UTILES CONNECTES AU NOEUD N°X
C     2EME VALEUR = 0 NOEUD MILIEU OU NON CONNECTE A UN EF UTILE
C                   1 NOEUD SOMMET A L INTERIEUR + LIE A UN EF UTILE
C                   2 NOEUD SOMMET BORD + LIE A UN EF UTILE
C     CONNECTIVITE  NOEUD N°X=>N° DES EF CONNECTES A X

      CHCINV='&&SINGUM.CINV           '
      CALL WKVECT(CHCINV,'V V I',NNOEM*(NELCOM+2),JCINV)

      DO 100 INNO=1,NNOEM
        NBRE=ZI(JCINV2+INNO)-ZI(JCINV2+INNO-1)
        ADRESS=JCINV+(NELCOM+2)*(INNO-1)
        ZI(ADRESS+1-1)=NBEF(INNO)
        ZI(ADRESS+2-1)=0
        I=1
        DO 110 INEL=1,NBRE
          NUEF = ZI(JCINV1-1+ZI(JCINV2+INNO-1)+INEL-1)
          IF (NDIM.EQ.2) THEN
            IF (TYPEMA(NUEF)(1:4).EQ.'TRIA'.OR.
     &         TYPEMA(NUEF)(1:4).EQ.'QUAD') THEN
              IF (NOMILI(INNO).NE.0) ZI(ADRESS+2-1)=NOMILI(INNO)
              ZI(ADRESS+I+2-1)=NUEF
              I=I+1
            ENDIF
          ELSE
            IF (TYPEMA(NUEF)(1:5).EQ.'TETRA'.OR.
     &         TYPEMA(NUEF)(1:4).EQ.'HEXA') THEN
              IF (NOMILI(INNO).NE.0) ZI(ADRESS+2-1)=NOMILI(INNO)
              ZI(ADRESS+I+2-1)=NUEF
              I=I+1
            ENDIF
          ENDIF
 110    CONTINUE
 100  CONTINUE

      DO 120 INNO=1,NNOEM
        ADRESS=JCINV+(NELCOM+2)*(INNO-1)
 120  CONTINUE

      CALL JEDETR(CINV)
      CALL JEDEMA()

      END
