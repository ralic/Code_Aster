      SUBROUTINE CALNOR ( DIM, INO, NNO, NPG, NBS, NBNA, NOE,
     &                    IGEOM, IDFDE,
     &                    IFA, ITYP, ORIEN, HF,
     &                    JAC, NX, NY, NZ, TX, TY )
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 03/07/2006   AUTEUR MEUNIER S.MEUNIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C     BUT:
C         CALCUL DE LA NORMALE SORTANTE D'UN SEGMENT LINEAIRE 
C         OU QUADRATIQUE.
C
C
C     ARGUMENTS:
C     ----------
C
C      ENTREE :
C-------------
C IN   DIM    : DIMENSION DE L'ESPACE : '2D' OU '3D'
C IN   INO    : NUMERO DU PREMIER POINT DE L'ARETE ET DE L'ARETE
C IN   NNO    : NOMBRE DE NOEUDS DE L'ELEMENT
C IN   NPG    : NOMBRE DE POINTS DE GAUSS DE L'ELEMENT (POUR DU 3D)
C IN   NBS    : NOMBRE DE SOMMETS DE L'ELEMENT
C IN   NBNA   : NOMBRE DE NOEUDS PAR ARETE
C IN   NOE    : LISTE DES NOEUDS PAR FACE (POUR DU 3D)
C IN   IGEOM  : ADRESSE DES COORDONNEES
C IN   IDFDE  : ADRESSE DES DERIVEES DES FONCTIONS DE FORME (POUR DU 3D)
C IN   IFA    : NUMERO DE LA FACE (POUR DU 3D)
C IN   ITYPE  : TYPE DE LA FACE (POUR DU 3D)
C IN   ORIEN  : ORIENTATION DE LA MAILLE PAR RAPPORT A ELREF
C                1 SI IDEM ELEMENT DE REFERENCE
C               -1 SI DIFFERENT
C IN   HF     : LONGUEUR DE L'ARETE
C
C      SORTIE :
C-------------
C OUT  JAC    : VECTEUR DES JACOBIENS DE LA TRANSFORMATION AUX NOEUDS
C OUT  NX     : VECTEUR DES ABSCISSES DES NORMALES 
C                   * EN 2D : AUX NOEUDS 
C                   * EN 3D : AUX POINTS DE GAUSS 
C OUT  NY     : VECTEUR DES ORDONNEES DES NORMALES
C                   * EN 2D : AUX NOEUDS 
C                   * EN 3D : AUX POINTS DE GAUSS
C OUT  NZ     : VECTEUR DES COTES DES NORMALES
C                   * EN 2D : AUX NOEUDS 
C                   * EN 3D : AUX POINTS DE GAUSS
C OUT  TX     : VECTEUR DES ABSCISSES DES TANGENTES AUX NOEUDS
C OUT  TY     : VECTEUR DES ORDONNEES DES TANGENTES AUX NOEUDS
C
C ......................................................................
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INTEGER INO, NNO, NPG, NBS, NBNA, NOE(9,6,3)
      INTEGER IGEOM, IDFDE
      INTEGER IFA, ITYP
      REAL*8  ORIEN, HF
      REAL*8  JAC(27), NX(27), NY(27), NZ(27), TX(3), TY(3)
      CHARACTER*2 DIM
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
      CHARACTER*6  NOMPRO
      PARAMETER ( NOMPRO = 'CALNOR' )

      INTEGER I, J, JNO, MNO, IPG,IDEC, JDEC, KDEC
      INTEGER LENOEU

      REAL*8 X1, Y1, X2, Y2, X3, Y3
      REAL*8 X(3), Y(3), SX(9,9), SY(9,9), SZ(9,9)
      LOGICAL  LTEATT, LAXI

C ----------------------------------------------------------------------
C
C====
C 1. --- CAS DU 2D ----------------------------------------------------
C====
C
      IF ( DIM.EQ.'2D' ) THEN
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
C              X1          X2          X3
C               O-----------O-----------O
C              INO         MNO         JNO
C
C         POINTS  1 --> INO PREMIER POINT DE L'ARETE COURANTE
C                 2 --> JNO DEUXIEME POINT  DE L'ARETE COURANTE
C                 3 --> MNO NOEUD MILIEU S'IL EXISTE (NBNA=3)
C
      IF (INO.EQ.NBS) THEN
        JNO=1
      ELSE
        JNO=INO+1
      ENDIF
C
      X1=ZR(IGEOM-1+2*(INO-1)+1)
      Y1=ZR(IGEOM-1+2*(INO-1)+2)
      X2=ZR(IGEOM-1+2*(JNO-1)+1)
      Y2=ZR(IGEOM-1+2*(JNO-1)+2)
C
      IF ( NBNA.EQ.3 ) THEN
        MNO = NBS + INO
        X3 = ZR(IGEOM+2*MNO-2)
        Y3 = ZR(IGEOM+2*MNO-1)
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
      IF (NBNA.EQ.3) THEN
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
      DO 16 , I = 1 , NBNA
        TX(I) =  NY(I)
        TY(I) = -NX(I)
   16 CONTINUE
C      
C====
C 2. -- CAS DU 3D ------------------------------------------------------
C====
C
      ELSE IF ( DIM.EQ.'3D' ) THEN
C
      CALL R8INIR(27,0.D0,NX,1)
      CALL R8INIR(27,0.D0,NY,1)
      CALL R8INIR(27,0.D0,NZ,1)
C
      DO 20 IPG=1,NPG
C
        DO 210 I=1,NNO
          LENOEU=NOE(I,IFA,ITYP)
          IDEC=IGEOM-1+3*(LENOEU-1)
C          
          DO 220 J=1,NNO
            JNO=NOE(J,IFA,ITYP)
            JDEC=IGEOM-1+3*(JNO-1)
            SX(I,J)=ZR(IDEC+2)*ZR(JDEC+3)-ZR(IDEC+3)*ZR(JDEC+2)
            SY(I,J)=ZR(IDEC+3)*ZR(JDEC+1)-ZR(IDEC+1)*ZR(JDEC+3)
            SZ(I,J)=ZR(IDEC+1)*ZR(JDEC+2)-ZR(IDEC+2)*ZR(JDEC+1)
C
  220     CONTINUE
C
  210   CONTINUE
C
C --- CALCUL DE LA NORMALE AU POINT IPG  -----------------
C
        KDEC=2*(IPG-1)*NNO
C
        DO 230 I=1,NNO
          IDEC=2*(I-1)
C
          DO 240 J=1,NNO
            JDEC=2*(J-1)
            NX(IPG)=NX(IPG)-ZR(IDFDE-1+KDEC+IDEC+1)
     &              *ZR(IDFDE-1+KDEC+JDEC+2)*SX(I,J)
            NY(IPG)=NY(IPG)-ZR(IDFDE-1+KDEC+IDEC+1)
     &              *ZR(IDFDE-1+KDEC+JDEC+2)*SY(I,J)
            NZ(IPG)=NZ(IPG)-ZR(IDFDE-1+KDEC+IDEC+1)
     &              *ZR(IDFDE-1+KDEC+JDEC+2)*SZ(I,J)
  240     CONTINUE
C
  230   CONTINUE

C
C   JACOBIEN
        JAC(IPG)=SQRT(NX(IPG)**2+NY(IPG)**2+NZ(IPG)**2)
        NX(IPG)=NX(IPG)/JAC(IPG)
        NY(IPG)=NY(IPG)/JAC(IPG)
        NZ(IPG)=NZ(IPG)/JAC(IPG)
C
  20  CONTINUE
C      
C ----- PROBLEME ------------------------------------------------------
C
      ELSE
C 
        CALL UTMESS('F',NOMPRO,'DIMENSION '//DIM//' INCONNUE.')
C
      ENDIF
C      
C ----------------------------------------------------------------------
C
      END
