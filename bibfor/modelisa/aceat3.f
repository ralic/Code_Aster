      SUBROUTINE ACEAT3(NOMA,NOMU,NBTUY,NBPART,NBMAP,ELPAR,NOPAR,IVR,
     &                IFM,NBZK,NOZK,COZK,ISENS,COOR,EPSI,CRIT,NNO,NMMT)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*8       NOMA,NOMU,CRIT
      INTEGER NBPART,NBTUY,NBMAP(NBPART),ELPAR(NBPART,NBTUY),IVR(3)
      INTEGER NOPAR(NBPART,NNO,NBTUY),NBZK,NOZK(NBZK),ISENS(NBPART),IFM
      INTEGER NMMT(*),NNO,ICMP,IAVANT,NO4,NBCMP,ICOUD2
      REAL*8  COZK(3*NBZK),COOR(*),COOR3(12),ZK1(3),ZK2(3),ZK3(3)
      REAL*8  ANGL1(3),ANGL2(3),ANGL3(3),EPSI,ANGL4(3)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C ----------------------------------------------------------------------
C     AFFE_CARA_ELEM
C     AFFECTATION DES CARACTERISTIQUES POUR LES TUYAUX DANS UNE CARTE
C ----------------------------------------------------------------------
C IN  NOMA   : NOM DU MAILLAGE
C IN  NOMU   : NOM DU CONCEPT RESULTAT
C IN  NBTUY  : NOMBRE D'ELEMENTS TUYAU
C IN  NBPAT  : NOMBRE DE PARTIES CONNEXES DE TUYAUX
C IN  NBMAP  : NOMBRE DE MAILLES PAR PARTIE
C IN  ELPAR  : NUMERO DES MAILLES DE CHAQUE PARTIE DANS L'ORDRE
C IN  NOPAR  : NUMERO DES NOEUDS  DE CHAQUE PARTIE DANS L'ORDRE
C IN  IVR    : (3) = INFO 2
C IN  IFM    : FICHIER MESSAGES
C IN  NBZK   : NOMBRE D'OCCURENCES DE GENE_TUYAU
C IN  NOZK   : NUMEROS DES NOEUDS OU ZK EST DONNE
C IN  COZK   : COORDONNES DES ZK
C IN  ISENS  : SENS DE PARCOURS DES MAILLES
C IN  COOR   : COORDONNES DES NOEUDS
C IN  NMMT   : INDIQUE SI MODI_METRIQUE POUR CHAQUE MAILLE
C ----------------------------------------------------------------------
      CHARACTER*8  NOMMAI,NOMNO1,NOMNO2,NOMNO3,NOMNO4
      CHARACTER*19 CARTOR,MLGNMA,MLGNNO
      CHARACTER*24 TMPNOR, TMPVOR
      INTEGER JDCMPO,JDVLVO,IZK,IOK1,IOK2,IPA,IMFIN,I,IMA,NUMMAI
      INTEGER NO1,NO2,NO3,ICOUDE,IM0,NBDROI,NBCOUD
      REAL*8  PGL1(3,3),PGL2(3,3),PGL3(3,3),NORME,PGL4(3,3)
C
C-----------------------------------------------------------------------
      REAL*8 DN1N2 ,OMEGA ,RAYON ,THETA ,VX ,VY ,VZ 

C-----------------------------------------------------------------------
      CALL JEMARQ()
C
C     VERIFICATION QUE LES NOEUDS DONNES SONT DES EXTREMITES
C --- AFFECTATION DES VALEURS DU TAMPON DANS LA CARTE ORIENTATION :
C     -------------------------------------------------------------
      MLGNMA = NOMA//'.NOMMAI'
      MLGNNO = NOMA//'.NOMNOE'
      CARTOR = NOMU//'.CARORIEN'
      TMPNOR = CARTOR//'.NCMP'
      TMPVOR = CARTOR//'.VALV'
      CALL JEVEUO(TMPNOR,'E',JDCMPO)
      CALL JEVEUO(TMPVOR,'E',JDVLVO)
      ZK8(JDCMPO)   = 'ALPHA'
      ZK8(JDCMPO+ 1) ='BETA'
      ZK8(JDCMPO+ 2) ='GAMMA'
      ZK8(JDCMPO+ 3) ='ALPHA2'
      ZK8(JDCMPO+ 4) ='BETA2'
      ZK8(JDCMPO+ 5) ='GAMMA2'
      ZK8(JDCMPO+ 6) ='ALPHA3'
      ZK8(JDCMPO+ 7) ='BETA3'
      ZK8(JDCMPO+ 8) ='GAMMA3'
      ICMP=8
      IF (NNO.EQ.4) THEN
        ZK8(JDCMPO+ 9) ='ALPHA4'
        ZK8(JDCMPO+10) ='BETA4'
        ZK8(JDCMPO+11) ='GAMMA4'
        ICMP=11
      ENDIF
      ZK8(JDCMPO+ICMP+1) ='ICOUDE'
      ZK8(JDCMPO+ICMP+2) ='DN1N2'
      ZK8(JDCMPO+ICMP+3) ='RCOURB'
      ZK8(JDCMPO+ICMP+4) ='ANGCOU'
      ZK8(JDCMPO+ICMP+5) ='ANGZZK'
C
C     LES NOEUDS ASSOCIES A GENE_TUYAUX DOIVENT ETRE UNE
C     DES EXTREMITES
C
      DO 62 IPA=1,NBPART
         ISENS(IPA)=0
62    CONTINUE
      DO 60 IZK=1,NBZK
         IOK1=0
         IOK2=0
         DO 61 IPA=1,NBPART
            IMFIN=NBMAP(IPA)
            IF (NOZK(IZK).EQ.(NOPAR(IPA,1,1))) THEN
               IOK1=1
               IF (ISENS(IPA).EQ.0) THEN
                  ISENS(IPA)=IZK
               ELSE
                  CALL U2MESS('F','MODELISA_24')
               ENDIF
            ENDIF
            IF (NOZK(IZK).EQ.(NOPAR(IPA,2,IMFIN))) THEN
               IOK2=1
               IF (ISENS(IPA).EQ.0) THEN
                  ISENS(IPA)=-IZK
               ELSE
                  CALL U2MESS('F','MODELISA_24')
               ENDIF
            ENDIF
61       CONTINUE
         IF((IOK1+IOK2).NE.1) THEN
            CALL U2MESS('F','MODELISA_25')
         ENDIF
60    CONTINUE
C
C     STOCKAGE DANS LA CARTE DES PARAMETRES GEOMETRIQUES CALCULES
C     PAR ANGCOU ET DE LA GENERATRICE CONTINUE SUR LES TUYAUX
C
      NBDROI=0
      NBCOUD=0
C
      DO 68 IPA=1,NBPART
         IF (IVR(3).EQ.1) WRITE(IFM,1000) IPA,NBMAP(IPA)
         IZK=ISENS(IPA)
         IF (IZK.EQ.0) THEN
C
C        PAS DE VECTEUR FOURNI : ON EN CREE UN
C
            NO1=NOPAR(IPA,1,1)
            NO2=NOPAR(IPA,2,1)
            VX = COOR(3*NO2-3+1)-COOR(3*NO1-3+1)
            VY = COOR(3*NO2-3+2)-COOR(3*NO1-3+2)
            VZ = COOR(3*NO2-3+3)-COOR(3*NO1-3+3)
            ZK1(1)=-VY
            ZK1(2)=VX
            ZK1(3)=0.D0
            CALL NORMEV(ZK1,NORME)
            IF (NORME.LT.1.D-4) THEN
               ZK1(1)=0.D0
               ZK1(2)=-VZ
               ZK1(3)=VY
               CALL NORMEV(ZK1,NORME)
            ENDIF
         ELSE
            DO 64 I=1,3
               ZK1(I)=COZK(3*(ABS(IZK)-1)+I)
64          CONTINUE
            CALL NORMEV(ZK1,NORME)
         ENDIF
         IAVANT=-1
         DO 67 IM0=1,NBMAP(IPA)
            IF (IZK.GE.0) THEN
               IMA=IM0
            ELSE
               IMA=NBMAP(IPA)-IM0+1
            ENDIF
               NO1=NOPAR(IPA,1,IMA)
               NO2=NOPAR(IPA,2,IMA)
               NO3=NOPAR(IPA,3,IMA)
               IF (NNO.EQ.4) THEN
                  NO4=NOPAR(IPA,4,IMA)
               ENDIF
               NUMMAI=ELPAR(IPA,IMA)
               DO 65 I=1,3
                  COOR3(I  )=COOR(3*NO1-3+I)
                  COOR3(3+I)=COOR(3*NO2-3+I)
                  COOR3(6+I)=COOR(3*NO3-3+I)
                  IF (NNO.EQ.4) THEN
                     COOR3(9+I)=COOR(3*NO4-3+I)
                  ENDIF
65             CONTINUE
               IF (NNO.EQ.3) THEN
               CALL ANGCOU(COOR3,ZK1,IZK,ICOUDE,ZK2,RAYON,THETA,
     &         ANGL1,ANGL2,ANGL3,PGL1,PGL2,PGL3,OMEGA,DN1N2,EPSI,CRIT,
     &         ZK3)
               DO 631 I=1,3
                  ANGL4(I)=0.D0
631            CONTINUE
               ELSEIF (NNO.EQ.4) THEN
               CALL ANGCO4(COOR3,ZK1,IZK,ICOUDE,ZK2,RAYON,THETA,
     &         ANGL1,ANGL2,ANGL3,ANGL4,PGL1,PGL2,PGL3,PGL4,OMEGA,DN1N2,
     &         EPSI,CRIT)
               ENDIF
               DO 63 I=1,3
                  ZR(JDVLVO-1+  I)= ANGL1(I)
                  ZR(JDVLVO-1+3+I)= ANGL2(I)
                  ZR(JDVLVO-1+6+I)= ANGL3(I)
63             CONTINUE
               ICMP=9
               NBCMP=14
               IF (NNO.EQ.4) THEN
                  DO 74 I=1,3
                     ZR(JDVLVO-1+9+I)= ANGL4(I)
74                CONTINUE
                  ICMP=12
                  NBCMP=17
               ENDIF

               IF (ICOUDE.EQ.0) THEN
                  NBDROI=NBDROI+1
               ELSE
                  NBCOUD=NBCOUD+1
               ENDIF

C              MODI_METRIQUE

               IF (NMMT(NUMMAI).EQ.0) THEN
                  ICOUD2=ICOUDE+10
               ELSEIF (NMMT(NUMMAI).EQ.1) THEN
                  ICOUD2=ICOUDE
               ELSE
                  CALL JENUNO(JEXNUM(MLGNMA,NUMMAI),NOMMAI)
                  CALL U2MESK('F','MODELISA_26',1,NOMMAI)
               ENDIF

               ZR(JDVLVO-1+ICMP+1) = ICOUD2
               ZR(JDVLVO-1+ICMP+2) = DN1N2
               ZR(JDVLVO-1+ICMP+3) = RAYON
               ZR(JDVLVO-1+ICMP+4) = THETA
               ZR(JDVLVO-1+ICMP+5) = OMEGA

               CALL NOCART(CARTOR,3,' ','NUM',1,' ',NUMMAI,' ',NBCMP)
C
               IF (IVR(3).EQ.1) THEN
                  CALL JENUNO(JEXNUM(MLGNMA,NUMMAI),NOMMAI)
                  CALL JENUNO(JEXNUM(MLGNNO,NO1),NOMNO1)
                  CALL JENUNO(JEXNUM(MLGNNO,NO2),NOMNO2)
                  CALL JENUNO(JEXNUM(MLGNNO,NO3),NOMNO3)
                  IF (NNO.EQ.4) THEN
                     CALL JENUNO(JEXNUM(MLGNNO,NO4),NOMNO4)
                  ELSE
                     NOMNO4=' '
                  ENDIF
                  IF (ICOUDE.NE.IAVANT) THEN
                    IF (NNO.EQ.3) THEN
                       IF (ICOUDE.EQ.0) THEN
                          WRITE(IFM,1031)
                       ELSE
                          WRITE(IFM,1032)
                       ENDIF
                    ELSEIF (NNO.EQ.4) THEN
                       IF (ICOUDE.EQ.0) THEN
                          WRITE(IFM,1031)
                       ELSE
                          WRITE(IFM,1032)
                       ENDIF
                     ENDIF
                     IAVANT=ICOUDE
                  ENDIF
                  IF (IZK.GE.0) THEN
                     IF (ICOUDE.EQ.0) THEN
                         WRITE(IFM,1010) NOMMAI,NOMNO1,NOMNO2,NOMNO3,
     &                   NOMNO4,
     &                  (ZK1(I),I=1,3),(ANGL1(I),I=1,3)
                     ELSE
                         WRITE(IFM,1011) NOMMAI,NOMNO1,NOMNO2,NOMNO3,
     &                   NOMNO4,
     &                  (ZK1(I),I=1,3),(ZK2(I),I=1,3),RAYON,THETA,OMEGA
     &             ,(ANGL1(I),I=1,3),(ANGL2(I),I=1,3),(ANGL3(I),I=1,3)
                     ENDIF
                  ELSE
                     IF (ICOUDE.EQ.0) THEN
                         WRITE(IFM,1010) NOMMAI,NOMNO1,NOMNO2,NOMNO3,
     &                   NOMNO4,
     &                  (ZK1(I),I=1,3),(ANGL1(I),I=1,3)
                     ELSE
                         WRITE(IFM,1011) NOMMAI,NOMNO1,NOMNO2,NOMNO3,
     &                   NOMNO4,
     &                  (ZK2(I),I=1,3),(ZK1(I),I=1,3),RAYON,THETA,OMEGA
     &             ,(ANGL1(I),I=1,3),(ANGL2(I),I=1,3),(ANGL3(I),I=1,3)
                     ENDIF
                  ENDIF
               ENDIF
               DO 69 I=1,3
                  ZK1(I)=ZK2(I)
69             CONTINUE
 67         CONTINUE
 68   CONTINUE
C
      WRITE(IFM,1055) NBDROI
      WRITE(IFM,1056) NBCOUD

 1001 FORMAT(
     &3X,'MAILLE  NOEUD1  NOEUD2  NOEUD3   TYPE   ',
     &'Z1_X',8X,'Z1_Y',8X,'Z1_Z',8X,'ALPHA1',6X,'BETA1',7X,'GAMMA1')
 1002 FORMAT(
     &3X,'MAILLE  NOEUD1  NOEUD2  NOEUD3   TYPE   ',
     &'Z1_X',8X,'Z1_Y',8X,'Z1_Z',8X,'Z2_X',8X,'Z2_Y',8X,'Z2_Z',
     &8X,'RAYON',7X,'ANGLE',7X,'OMEGA',
     &7X,'ALPHA1',6X,'BETA1',7X,'GAMMA1',
     &6X,'ALPHA2',6X,'BETA2',7X,'GAMMA2',
     &6X,'ALPHA3',6X,'BETA3',7X,'GAMMA3')

 1031 FORMAT(
     &3X,'MAILLE  NOEUD1  NOEUD2  NOEUD3  NOEUD4   TYPE   ',
     &'Z1_X',8X,'Z1_Y',8X,'Z1_Z',8X,'ALPHA1',6X,'BETA1',7X,'GAMMA1')
 1032 FORMAT(
     &3X,'MAILLE  NOEUD1  NOEUD2  NOEUD3  NOEUD4   TYPE   ',
     &'Z1_X',8X,'Z1_Y',8X,'Z1_Z',8X,'Z2_X',8X,'Z2_Y',8X,'Z2_Z',
     &8X,'RAYON',7X,'ANGLE',7X,'OMEGA',
     &7X,'ALPHA1',6X,'BETA1',7X,'GAMMA1',
     &6X,'ALPHA2',6X,'BETA2',7X,'GAMMA2',
     &6X,'ALPHA3',6X,'BETA3',7X,'GAMMA3')

 1000 FORMAT(3X,'TUYAUTERIE NUMERO : ',I6,' NOMBRE DE MAILLES : ',I6)

 1010 FORMAT(3X,5A8,1X,'DROIT',1X,9(D11.4,1X))
 1011 FORMAT(3X,5A8,1X,'COUDE',1X,18(D11.4,1X))
C
 1040 FORMAT(3X,5A8,1X,'DROIT',1X,9(D11.4,1X))
 1041 FORMAT(3X,5A8,1X,'COUDE',1X,18(D11.4,1X))
 1055 FORMAT(3X,'NOMBRE TOTAL D ELEMENTS TUYAU DROITS ',1X,I6)
 1056 FORMAT(3X,'NOMBRE TOTAL D ELEMENTS TUYAU COUDES ',1X,I6)
C
      CALL JEDETR(TMPNOR)
      CALL JEDETR(TMPVOR)
C
      CALL JEDEMA()
      END
