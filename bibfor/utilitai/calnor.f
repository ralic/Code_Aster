      SUBROUTINE CALNOR ( CHDIM, GEOM,
     &                    IARE, NNOS, NNOA, ORIEN,
     &                    NNO, NPG, NOE, IFA, TYMVOL, IDFDE,
     &                    JAC, NX, NY, NZ,
     &                    TX, TY, HF )
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 31/01/2012   AUTEUR REZETTE C.REZETTE 
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
C RESPONSABLE DELMAS J.DELMAS
C
C     BUT:
C         CALCUL DE LA NORMALE SORTANTE D'UN ELEMENT LINEAIRE
C         OU QUADRATIQUE.
C
C     ARGUMENTS:
C     ----------
C
C      ENTREE :
C-------------
C IN   CHDIM  : DIMENSION DE L'ESPACE : '2D' OU '3D'
C IN   GEOM   : LES COORDONNEES
C
C POUR DU 2D :
C ON CHERCHE LA NORMALE SORTANTE A L'ARETE D'UNE FACE QUAD OU TRIA.
C PAR CONVENTION, L'ARETE NUMERO I EST SITUE ENTRE LES SOMMETS I
C ET I+1 DE LA FACE (MODULO LE DERNIER QUI REPART A 1)
C EXEMPLE POUR UN QUAD :
C                           ARETE 1
C                1 o---------------------o 2
C                  .                     .
C          ARETE 4 .                     . ARETE 2
C                  .                     .
C                4 o---------------------o 3
C                           ARETE 3
C
C
C IN   IARE   : NUMERO DE L'ARETE A EXAMINER
C IN   NNOS   : NOMBRE DE SOMMETS DE LA FACE (3 OU 4)
C IN   NNOA   : NOMBRE DE NOEUDS PAR ARETE (2 OU 3)
C IN   ORIEN  : ORIENTATION DE LA MAILLE PAR RAPPORT A ELREF
C                1 SI IDEM ELEMENT DE REFERENCE
C               -1 SI DIFFERENT
C
C POUR DU 3D :
C ON CHERCHE LA NORMALE SORTANTE A LA FACE QUAD OU TRIA D'UNE
C MAILLE VOLUMIQUE.
C IN   NNO    : NOMBRE DE NOEUDS DE LA FACE (3 OU 4)
C IN   NPG    : NOMBRE DE POINTS DE GAUSS DE L'ELEMENT FACE.
C IN   NOE    : LISTE DES NUMEROS DES NOEUDS PAR FACE (VOIR TE0003)
C IN   IFA    : NUMERO DE LA FACE (POUR DU 3D)
C IN   TYMVOL : TYPE DE LA MAILLE VOLUMIQUE
C               1 : HEXAEDRE      2 : PENTAEDRE
C               3 : TETRAEDRE     4 : PYRAMIDE
C IN   IDFDE  : ADRESSE DES DERIVEES DES FONCTIONS DE FORME SUR LA FACE
C
C      SORTIE :
C-------------
C POUR DU 2D :
C OUT  JAC    : VECTEUR DES JACOBIENS DE LA TRANSFORMATION AUX NOEUDS
C OUT  NX     : VECTEUR DES ABSCISSES DES NORMALES AUX NOEUDS
C OUT  NY     : VECTEUR DES ORDONNEES DES NORMALES AUX NOEUDS
C OUT  TX     : VECTEUR DES ABSCISSES DES TANGENTES AUX NOEUDS
C OUT  TY     : VECTEUR DES ORDONNEES DES TANGENTES AUX NOEUDS
C OUT  HF     : EN 2D : LONGUEUR DE L'ARETE
C POUR DU 3D :
C OUT  JAC    : VECTEUR DES JACOBIENS DE LA TRANSFORMATION AUX NOEUDS
C OUT  NX     : VECTEUR DES ABSCISSES DES NORMALES AUX POINTS DE GAUSS
C OUT  NY     : VECTEUR DES ORDONNEES DES NORMALES AUX POINTS DE GAUSS
C OUT  NZ     : VECTEUR DES COTES DES NORMALES AUX POINTS DE GAUSS
C
C ......................................................................
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      CHARACTER*2 CHDIM
C
      INTEGER IARE, NNOS, NNOA
      REAL*8  ORIEN,GEOM(*)
C
      INTEGER NNO, NPG, NOE(9,6,4)
      INTEGER IFA, TYMVOL, IDFDE
C
      REAL*8 JAC(9), NX(9), NY(9), NZ(9)
      REAL*8 TX(3), TY(3), HF 
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------

      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)

C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C DECLARATION VARIABLES LOCALES
C

      INTEGER I, J, INO, JNO, MNO, IPG,IDEC, JDEC, KDEC
      INTEGER LENOEU, NUMNO, NNOS2

      REAL*8 X1, Y1, X2, Y2, X3, Y3, NORME
      REAL*8 X(9), Y(9), Z(9),SX(9,9), SY(9,9), SZ(9,9)
      LOGICAL  LTEATT, LAXI

C ----------------------------------------------------------------------
C
C ----- METHODE :
C
C       M(U,V) SURFACE PARAMETREE
C       NORMALE A LA SURFACE DONNEE PAR
C       N(U,V)=DM(U,V)/DU (VECTORIEL) DM(U,V)/DV
C
C       EN 2D LE PRODUIT VECTORIEL ET LA DERIVEE DES FONCTIONS DE FORME
C       EST CALCULE DE MANIERE IMPLICITE.
C       EN 3D LE PRODUIT VECTORIEL EST CALCULE EXPLICITEMENT ET LA
C       DERIVEE DES FONCTIONS DE FORME EST RECUPERE DANS LA ROUTINE
C       CHAPEAU.
C       LES ELEMENTS DE BARSOUM SUBISSENT UN TRAITEMENT CAR LE JACOBIEN
C       EST NUL EN POINTE DE FISSURE.
C
C              X1          X3          X2
C               O-----------O-----------O
C              IARE      MNO         JNO
C
C         POINTS  1 --> IARE PREMIER POINT DE L'ARETE COURANTE
C                 2 --> JNO DEUXIEME POINT  DE L'ARETE COURANTE
C                 3 --> MNO NOEUD MILIEU S'IL EXISTE
C
C====
C 1. --- CAS DU 2D -----------------------------------------------------
C====
C
      IF ( CHDIM.EQ.'2D' ) THEN
C
C 1.1. ==> PREALABLE
C
        IF ( LTEATT(' ','AXIS','OUI') ) THEN
          LAXI = .TRUE.
        ELSE
          LAXI = .FALSE.
        ENDIF
C
C 1.2. ==> COORDONNEES DES 3 NOEUDS
C
        IF ( IARE.EQ.NNOS ) THEN
          JNO=1
        ELSE
          JNO=IARE+1
        ENDIF
C
        X1=GEOM(2*IARE-1)
        Y1=GEOM(2*IARE)
        X2=GEOM(2*JNO-1)
        Y2=GEOM(2*JNO)
C
C 1.3. ==> LONGUEUR DE L'ARETE
C
        HF=SQRT((X1-X2)**2+(Y1-Y2)**2)
C
        IF ( NNOA.EQ.3 ) THEN
          MNO = NNOS + IARE
          X3 = GEOM(2*MNO-1)
          Y3 = GEOM(2*MNO)
C
C 1.2.1. ==> TRAITEMENT ELEMENTS DE BARSOUM
C
C ----- LA NORMALE EST CALCULEE EN UTILISANT LE POINT MILIEU
          NORME=SQRT((X3-X1)**2+(Y3-Y1)**2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
C
          IF ( (NORME.LT.0.4D0) .OR. (NORME.GT.0.6D0) ) THEN
C
            X3 = (X1+X2)*0.5D0
            Y3 = (Y1+Y2)*0.5D0
C
          ENDIF
C
C 1.2.2. ==> TRAITEMENT AUTRES ELEMENTS
C
        ELSE
          X3 = (X1+X2)*0.5D0
          Y3 = (Y1+Y2)*0.5D0
        ENDIF
C
C 1.3. ==> CALCUL NORMALE SORTANTE, TANGENTE ET JACOBIEN PREMIER POINT
C
        X(1) = -( (Y2-Y1)*0.5D0 - (Y1+Y2-2.D0*Y3) )
        Y(1) =    (X2-X1)*0.5D0 - (X1+X2-2.D0*X3)
C
        JAC(1) = SQRT(X(1)**2+Y(1)**2)
C
        IF ( LAXI ) THEN
          JAC(1)=JAC(1)*X1
        ENDIF
C
        NX(1) = -(X(1)*ORIEN)/(SQRT(X(1)**2+Y(1)**2))
        NY(1) = -(Y(1)*ORIEN)/(SQRT(X(1)**2+Y(1)**2))
C
C 1.4. ==> CALCUL NORMALE SORTANTE, TANGENTE ET JACOBIEN DEUXIEME POINT
C
        X(2) = -( (Y2-Y1)*0.5D0 + (Y1+Y2-2.D0*Y3) )
        Y(2) =    (X2-X1)*0.5D0 + (X1+X2-2.D0*X3)
C
        JAC(2) = SQRT(X(2)**2+Y(2)**2)
C
        IF ( LAXI ) THEN
          JAC(2) = JAC(2)*X2
        ENDIF
C
        NX(2) = -(X(2)*ORIEN)/(SQRT(X(2)**2+Y(2)**2))
        NY(2) = -(Y(2)*ORIEN)/(SQRT(X(2)**2+Y(2)**2))
C
C 1.5. ==> CALCUL NORMALE SORTANTE, TANGENTE ET JACOBIEN TROISIEME POINT
C
        IF (NNOA.EQ.3) THEN
C
          JAC(3) = HF*0.5D0
C
          IF ( LAXI ) THEN
            JAC(3) = JAC(3)*(X2+X1)*0.5D0
          ENDIF
C
          NX(3) = -ORIEN*(Y1-Y2)/HF
          NY(3) = -ORIEN*(X2-X1)/HF
C
        ENDIF
C
C 1.6. ==> CALCUL DES TANGENTES
C
        DO 16 , I = 1 , NNOA
          TX(I) =  NY(I)
          TY(I) = -NX(I)
   16   CONTINUE
C
C====
C 2. -- CAS DU 3D ------------------------------------------------------
C====
C
      ELSE IF ( CHDIM.EQ.'3D' ) THEN
C
C 2.1. ==> COORDONNEES DES NOEUDS
C
        DO 21 , NUMNO=1,NNO
          LENOEU=NOE(NUMNO,IFA,TYMVOL)
          X(NUMNO)=GEOM(3*LENOEU-2)
          Y(NUMNO)=GEOM(3*LENOEU-1)
          Z(NUMNO)=GEOM(3*LENOEU)
  21    CONTINUE
C
C 2.2. ==> TRAITEMENT ELEMENTS DE BARSOUM
C
C ----- LA NORMALE EST CALCULEE EN UTILISANT LE POINT MILIEU
        IF ((NNO.EQ.6).OR.(NNO.EQ.8)) THEN
C
          NNOS2=NNO/2

          DO 22 , INO=1,NNOS2
C
            IF (INO.EQ.NNOS2) THEN
              JNO=1
            ELSE
             JNO=INO+1
            ENDIF
            MNO=NNOS2+INO
C
            NORME=SQRT( (X(MNO) - X(INO) )**2
     &                + (Y(MNO) - Y(INO) )**2
     &                + (Z(MNO) - Z(INO) )**2 )
     &           /SQRT( (X(JNO) - X(INO) )**2
     &                + (Y(JNO) - Y(INO) )**2
     &                + (Z(JNO) - Z(INO) )**2 )
C
            IF ((NORME.LT.0.4D0).OR.(NORME.GT.0.6D0)) THEN
              X(MNO)=(X(INO)+X(JNO))*0.5D0
              Y(MNO)=(Y(INO)+Y(JNO))*0.5D0
              Z(MNO)=(Z(INO)+Z(JNO))*0.5D0
            ENDIF
C
   22     CONTINUE
        ENDIF
C
C 2.3. ==> CALCUL DU PRODUIT VECTORIEL OMI OMJ

        DO 23 , I=1,NNO
C
          DO 231 , J=1,NNO
C
            SX(I,J)=Y(I)*Z(J)-Z(I)*Y(J)
            SY(I,J)=Z(I)*X(J)-X(I)*Z(J)
            SZ(I,J)=X(I)*Y(J)-Y(I)*X(J)
C
  231     CONTINUE
C
   23   CONTINUE
C
C 2.4. ==> SOMMATION DES DERIVEES
C
        DO 24 , IPG=1,NPG
C
          NX(IPG) = 0.D0
          NY(IPG) = 0.D0
          NZ(IPG) = 0.D0
C
          KDEC=2*(IPG-1)*NNO
C
          DO 241 , I=1,NNO
            IDEC=2*(I-1)
C
            DO 242 , J=1,NNO
              JDEC=2*(J-1)
              NX(IPG)=NX(IPG)-ZR(IDFDE+KDEC+IDEC)
     &                *ZR(IDFDE+KDEC+JDEC+1)*SX(I,J)
              NY(IPG)=NY(IPG)-ZR(IDFDE+KDEC+IDEC)
     &                *ZR(IDFDE+KDEC+JDEC+1)*SY(I,J)
              NZ(IPG)=NZ(IPG)-ZR(IDFDE+KDEC+IDEC)
     &                *ZR(IDFDE+KDEC+JDEC+1)*SZ(I,J)
  242       CONTINUE
C
  241     CONTINUE
C
C        CALCUL DU JACOBIEN
C
          JAC(IPG)=SQRT(NX(IPG)**2+NY(IPG)**2+NZ(IPG)**2)
          NX(IPG)=NX(IPG)/JAC(IPG)
          NY(IPG)=NY(IPG)/JAC(IPG)
          NZ(IPG)=NZ(IPG)/JAC(IPG)
C
   24    CONTINUE
C
C ----- PROBLEME -------------------------------------------------------
C
      ELSE
C
        CALL U2MESK('F','UTILITAI_9',1,CHDIM)
C
      ENDIF
C
C ----------------------------------------------------------------------
C
      END
