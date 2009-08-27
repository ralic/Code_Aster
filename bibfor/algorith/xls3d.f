      SUBROUTINE XLS3D(CALLST,JLTSV,JLTSL,JLNSV,JLNSL,NBNO,JCOOR,
     &                 NBMAF,JDLIMA,NBSEF,JDLISE,JCONX1,JCONX2,NOMA)

      IMPLICIT REAL*8 (A-H,O-Z)

      CHARACTER*8 NOMA
      INTEGER     JLTSV,JLTSL,JLNSV,JLNSL,NBNO,JCOOR
      LOGICAL     CALLST

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/08/2009   AUTEUR GENIAUT S.GENIAUT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE GENIAUT S.GENIAUT

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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32    JEXNUM,JEXATR
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

      REAL*8        DDOT,DMIN,EPS,EPS1,EPS2,EPS3
      INTEGER       IMAFIS,INOMA,INOSE,ISEFIS,ITRI,JCONX1,JCONX2,JMA
      INTEGER       JDLIMA,JDLISE,N1,N2,NBNOMA,NBSEF,NMAABS,NNOS
      INTEGER       NSEABS,NTRI,NUM,NUNOC,ITYPMA,JNNOS
      REAL*8        PADIST,R8MAEM,XLN,XLT
      INTEGER       INO,NBMAF,NUNO(4),NUNOSE(2),I,NBNOTT(3)
      REAL*8        AB(3),AC(3),AP(3),VN(3),VNT(3),BC(3)
      REAL*8        A(3),P(3),B(3),C(3),M(3),PM(3),VNREF(3)
      REAL*8        NORME,PS,PS1,PS2,D
      LOGICAL       MA2FF
      CHARACTER*8   TYPMA
      CHARACTER*19  MAI
      CHARACTER*8   NOMAIL
      REAL*8        MPRIM(3),PMPRIM(3),COS,SIN,VECT(3),NOVE,PRONOR,ANGLE
      REAL*8        ANGLEM,R8PREM

      CALL JEMARQ()

      MAI=NOMA//'.TYPMAIL'
      CALL JEVEUO(MAI,'L',JMA)
C
C     TABLEAU POUR STOCKER LE NOMBRE DE NOEUDS SOMMETS
C     DES MAILLES DE FISSURE
      CALL WKVECT('&&XLS3D.NBNO_MA_FONDFISS','V V I',NBMAF,JNNOS)
      DO 5 IMAFIS=1,NBMAF
        NMAABS=ZI(JDLIMA+IMAFIS-1)
        ITYPMA=ZI(JMA-1+NMAABS)
        CALL PANBNO(ITYPMA,NBNOTT)
        ZI(JNNOS+IMAFIS-1)=NBNOTT(1)
 5    CONTINUE
C
C     BOUCLE SUR TOUS LES NOEUDS P DU MAILLAGE
      DO 11 INO=1,NBNO
        P(1)=ZR(JCOOR-1+3*(INO-1)+1)
        P(2)=ZR(JCOOR-1+3*(INO-1)+2)
        P(3)=ZR(JCOOR-1+3*(INO-1)+3)

C       CALCUL DE LSN
C       -------------
        DMIN=R8MAEM()
C       RECHERCHE DE LA MAILLE LA PLUS PROCHE :
C       BOUCLE SUR NOEUDS DE MAFIS
        DO 2 IMAFIS=1,NBMAF
          NMAABS=ZI(JDLIMA-1+(IMAFIS-1)+1)
          NBNOMA=ZI(JCONX2+NMAABS) - ZI(JCONX2+NMAABS-1)
          IF ((NBNOMA.EQ.4).OR.(NBNOMA.EQ.8)) NTRI=4
          IF ((NBNOMA.EQ.3).OR.(NBNOMA.EQ.6)) NTRI=1

C         BOUCLE SUR LE NOMBRE DE TRIANGLES DE LA MAILLE
          DO 21 ITRI=1,NTRI

            INOMA=1
            IF (ITRI.EQ.4) INOMA=4
            NUNO(INOMA)=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+INOMA-1)
            A(1)=ZR(JCOOR-1+3*(NUNO(INOMA)-1)+1)
            A(2)=ZR(JCOOR-1+3*(NUNO(INOMA)-1)+2)
            A(3)=ZR(JCOOR-1+3*(NUNO(INOMA)-1)+3)

            INOMA=2
            IF (ITRI.EQ.2) INOMA=4
            NUNO(INOMA)=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+INOMA-1)
            B(1)=ZR(JCOOR-1+3*(NUNO(INOMA)-1)+1)
            B(2)=ZR(JCOOR-1+3*(NUNO(INOMA)-1)+2)
            B(3)=ZR(JCOOR-1+3*(NUNO(INOMA)-1)+3)

            INOMA=3
            IF (ITRI.EQ.3) INOMA=4
            NUNO(INOMA)=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+INOMA-1)
            C(1)=ZR(JCOOR-1+3*(NUNO(INOMA)-1)+1)
            C(2)=ZR(JCOOR-1+3*(NUNO(INOMA)-1)+2)
            C(3)=ZR(JCOOR-1+3*(NUNO(INOMA)-1)+3)

            DO 211 I=1,3
              AB(I)=B(I)-A(I)
              BC(I)=C(I)-B(I)
              AP(I)=P(I)-A(I)
              AC(I)=C(I)-A(I)
 211        CONTINUE

C           CALCUL DE LA NORMALE À LA MAILLE TRIA3
C           PROJECTION DE P SUR LA MAILLE VOIR R5.03.50-B
            CALL PROVEC(AB,AC,VN)
            CALL NORMEV(VN,NORME)
            CALL PROVEC(AP,VN,VNT)
            PS=DDOT(3,VNT,1,AC,1)
            EPS1=-1*PS/NORME
            PS=DDOT(3,VNT,1,AB,1)
            EPS2=PS/NORME
            EPS3=1-EPS1-EPS2

C           SI M EST DS LE SECTEUR 1
            IF (EPS1.LT.0.D0) THEN
              PS=DDOT(3,AC,1,AC,1)
              PS1=DDOT(3,AB,1,AC,1)
              EPS2=EPS2+EPS1*PS1/PS
              EPS1=0.D0
            END IF
C           SI M EST DS LE SECTEUR 2
            IF (EPS2.LT.0.D0) THEN
              PS=DDOT(3,AB,1,AB,1)
              PS1=DDOT(3,AB,1,AC,1)
              EPS1=EPS1+EPS2*PS1/PS
              EPS2=0.D0
            END IF
C           SI M EST DS LE SECTEUR 3
            IF (EPS3.LT.0.D0) THEN
              PS=DDOT(3,BC,1,BC,1)
              PS1=DDOT(3,AB,1,BC,1)
              PS2=DDOT(3,AC,1,BC,1)
              EPS1=(-1.D0*EPS1*PS1+(1.D0-EPS2)*PS2)/PS
              EPS2=1.D0-EPS1
            END IF

C          ON FINIT DE RAMENER LES POINTS ENCORE DEHORS
           IF (EPS1.LT.0.D0) EPS1=0.D0
           IF (EPS2.LT.0.D0) EPS2=0.D0
           IF (EPS1.GT.1.D0) EPS1=1.D0
           IF (EPS2.GT.1.D0) EPS2=1.D0

           DO 212 I=1,3
             M(I)=A(I)+EPS1*AB(I)+EPS2*AC(I)
             PM(I)=M(I)-P(I)
 212       CONTINUE

C          CALCUL DE LA DISTANCE PM
           D=PADIST(3,P,M)

C          ON VÉRIFIE QUE CETTE NORMALE EST ORIENTÉE COMME LA
C          PRÉCENDENTE, À PART POUR LE 1ER TRIANGLE DE LA 1ER MAILLE!
           IF ((IMAFIS.NE.1) .OR. (ITRI.NE.1))THEN
             PS=DDOT(3,VN,1,VNREF,1)
             IF (PS.LT.0) VN(1)=-1*VN(1)
             IF (PS.LT.0) VN(2)=-1*VN(2)
             IF (PS.LT.0) VN(3)=-1*VN(3)
           END IF

C          ON GARDE CETTE NORMALE COMME RÉFÉR POUR LA MAILLE SUIVANTE
           DO 213 I=1,3
             VNREF(I)=VN(I)
 213       CONTINUE

C          MISE EN MÉMOIRE DE LSN POUR LA MAILLE LA PLUS PROCHE
           IF (D.LT.DMIN) THEN
             DMIN=D
             XLN=DDOT(3,VN,1,PM,1)
           END IF

 21      CONTINUE

 2     CONTINUE

       ZR(JLNSV-1+(INO-1)+1)=XLN
       ZL(JLNSL-1+(INO-1)+1)=.TRUE.

C      CALCUL DE LST
C      -------------

        IF (.NOT.CALLST) THEN
          XLT = -1.D0
          GOTO 888
        ENDIF

        DMIN=R8MAEM()
        ANGLEM=R8MAEM()
C
C       RECHERCHE DU SEGMENT LE PLUS PROCHE : BOUCLE SUR SEG DE FONFIS
        DO 3 ISEFIS=1,NBSEF

          NSEABS=ZI(JDLISE-1+(ISEFIS-1)+1)
          CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NSEABS),NOMAIL)

          INOSE=1
          NUNOSE(INOSE)=ZI(JCONX1-1+ZI(JCONX2+NSEABS-1)+INOSE-1)
          INOSE=2
          NUNOSE(INOSE)=ZI(JCONX1-1+ZI(JCONX2+NSEABS-1)+INOSE-1)

C         BOUCLE SUR LES MAILLES DE MAFIS POUR TROUVER LA BONNE MAILLE
          MA2FF=.FALSE.
          DO 31 IMAFIS=1,NBMAF

            NMAABS=ZI(JDLIMA-1+(IMAFIS-1)+1)
C           ON RÉCUPÈRE LES NUMEROS DS NOEUDS DE LA MAILLE ET ON TESTE
            N1=0
            N2=0

            DO 32 INOMA=1,ZI(JNNOS+IMAFIS-1)
              NUM=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+INOMA-1)
              IF (NUNOSE(1).EQ.NUM) N1=1
              IF (NUNOSE(2).EQ.NUM) N2=1
C             POUR RÉCUPÉRER UN 3EME POINT (SOMMET) DE LA MAILLE
C             QUI NE SOIT PAS SUR LE FOND
              IF ((NUNOSE(1).NE.NUM).AND.(NUNOSE(2).NE.NUM)) NUNOC=NUM
 32         CONTINUE

            IF ((N1*N2).EQ.1) THEN

              MA2FF=.TRUE.
              DO 33 I=1,3
                A(I)=ZR(JCOOR-1+3*(NUNOSE(1)-1)+I)
                B(I)=ZR(JCOOR-1+3*(NUNOSE(2)-1)+I)
                C(I)=ZR(JCOOR-1+3*(NUNOC-1)+I)
                AB(I)=B(I)-A(I)
                AP(I)=P(I)-A(I)
                AC(I)=C(I)-A(I)
 33           CONTINUE

C             CALCUL DE LA NORMALE À LA MAILLE
              CALL PROVEC(AB,AC,VN)
              CALL NORMEV(VN,NORME)

C             CALCUL DE LA NORMALE INTERIEURE AU SEGMENT
              CALL PROVEC(AB,VN,VNT)
              CALL NORMEV(VNT,NORME)
              VN(1)=-1.D0*VNT(1)
              VN(2)=-1.D0*VNT(2)
              VN(3)=-1.D0*VNT(3)

C             ON GARDE CETTE NORMALE COMME RÉF POUR LE SEG SUIVANT
              VNREF(1)=VN(1)
              VNREF(2)=VN(2)
              VNREF(3)=VN(3)

C             PROJECTION SUR LE SEGMENT
              PS=DDOT(3,AP,1,AB,1)
              PS1=DDOT(3,AB,1,AB,1)
              EPS=PS/PS1

              DO 344 I=1,3
                MPRIM(I)=A(I)+EPS*AB(I)
 344           CONTINUE

C             ON RAMÈNE M SUR LES BORDS S'IL LE FAUT
              IF (EPS.GT.1.D0) EPS=1.D0
              IF (EPS.LT.0.D0) EPS=0.D0

              DO 34 I=1,3
                M(I)=A(I)+EPS*AB(I)
                PM(I)=M(I)-P(I)
                PMPRIM(I)=MPRIM(I)-P(I)
 34           CONTINUE

C              CALCUL DE L'ANGLE (PM,PM') 
C                  OU M EST LE PROJETÉ RAMENÉ
C                  ET M' LE PROJETÉ AVANT RAMENAGE
C              COS A = <U,V> / (||U|| * ||V||)
C              SIN A = ||U^V|| / (||U|| * ||V||)
              CALL PROVEC(PM,PMPRIM,VECT)
              CALL NORMEV(VECT,NOVE)
              PRONOR = SQRT(PM(1)**2+PM(2)**2+PM(3)**2+
     &                 PMPRIM(1)**2+PMPRIM(2)**2+PMPRIM(3)**2)
              IF (PRONOR.NE.0.D0) THEN
                COS = (PM(1)*PMPRIM(1)+PM(2)*PMPRIM(2)+PM(3)*PMPRIM(3)) 
     &                                                / PRONOR
                SIN = NOVE / PRONOR 
                ANGLE = ATAN2(SIN,COS)
              ELSE
                COS = 0.D0
                SIN = 0.D0
                ANGLE=0.D0
              ENDIF

C             CALCUL DE LA DISTANCE PM
              D=PADIST(3,P,M)

C             MISE EN MÉMOIRE DE LSN=PM.N POUR LE SEG LE PLUS PROCHE
              IF (     (DMIN-D).GT.R8PREM().OR.
     &             (ABS(DMIN-D).LE.R8PREM().AND.ANGLE.LT.ANGLEM) ) THEN
                DMIN=D
                ANGLEM = ANGLE
                XLT=DDOT(3,VN,1,PM,1)
              END IF

            END IF

 31       CONTINUE

          IF (.NOT.MA2FF) CALL U2MESS('F','XFEM2_17')
 3      CONTINUE

 888   CONTINUE
       ZR(JLTSV-1+(INO-1)+1)=XLT
       ZL(JLTSL-1+(INO-1)+1)=.TRUE.

 11   CONTINUE

      CALL JEDETR('&&XLS3D.NBNO_MA_FONDFISS')

      CALL JEDEMA()
      END
