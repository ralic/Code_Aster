      SUBROUTINE INMAT2(NDIM,NNO,NNOS,NBFPG,ELREFE,X,NBPG)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/08/2003   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE VABHHTS J.PELLET
      IMPLICIT NONE

C BUT: ROUTINE D'INITIALISATION DES MATRICES DE PASSAGE DES POINTS
C      DE GAUSS AUX NOEUDS SOMMETS
C           &INEL.//ELREFE//.A  : MATRICE CARREE
C           &INEL.//ELREFE//.B  : MATRICE RECTANGULAIRE

C ENTREES:
C        ---> NDIM        : DIMENSION DE L'ESPACE DE REFERENCE
C        ---> NNO         : NOMBRE DE NOEUDS DE L'ELEMENT
C        ---> NNOS        : NOMBRE DE SOMMETS DE L'ELEMENT
C        ---> NBFPG       : NOMBRE DE FAMILLE DE POINTS DE GAUSS
C        ---> ELREFE       : NOM DE L'ELEMENT
C        ---> XSO,YSO,ZSO : COORDONNEES DES SOMMETS
C        ---> NBPG        : TABLEAU DES NOMBRES DE PTS DE GAUSS

      CHARACTER*8 ELREFE
      CHARACTER*16 ELREFL
      CHARACTER*24 CHMAT1,CHMAT2

      REAL*8 X(3),XG(3),COOPG(3*27),POIPG(27)
      REAL*8 AA,BB,CC,ZERO
      INTEGER NBPG(10),NBPG1,IFFT,DIMA,DIMB
      INTEGER NDIM,NNO,NNOS,NBFPG

C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------

      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL,FAUX
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)

C --- FIN DECLARATIONS NORMALISEES JEVEUX ----------------------------


      INTEGER JMAT,JMATSI,I,JM,JN,KP,LF,J,LM,L,LN,IFAP
C DEB ------------------------------------------------------------------

      CALL JEMARQ()
      ZERO = 0.D0
      FAUX=.FALSE.


      ELREFL = ELREFE
      CHMAT1 = '&INEL.'//ELREFL//'.A'
      CHMAT2 = '&INEL.'//ELREFL//'.B'

      DIMA = NNOS*NNOS + 2

      CALL WKVECT(CHMAT1,'G V R',DIMA,JMAT)
      ZR(JMAT-1+1) = NNOS


C     ------------------------------------------------------------------
      IF (ELREFE.EQ.'MEDKTR3' .OR. ELREFE.EQ.'MEDSTR3' .OR.
     &    ELREFE.EQ.'MEGRDKT') THEN

        NBPG1 = NBPG(1)
        DIMB = NNOS*NBPG1 + 2
        CALL WKVECT(CHMAT2,'G V R',DIMB,JMATSI)
        ZR(JMATSI-1+1) = NNOS
        ZR(JMAT-1+2) = NBPG1
        ZR(JMATSI-1+2) = NBPG1
        JMAT = JMAT + 2
        JMATSI = JMATSI + 2

        AA = 5.D0/3.D0
        BB = -1.D0/3.D0
        DO 10 I = 1,9
          ZR(JMAT-1+I) = BB
   10   CONTINUE
        ZR(JMAT-1+1) = AA
        ZR(JMAT-1+5) = AA
        ZR(JMAT-1+9) = AA


C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'MEDKQU4' .OR. ELREFE.EQ.'MEDSQU4' .OR.
     &         ELREFE.EQ.'MEQ4QU4') THEN

        NBPG1 = NBPG(1)
        DIMB = NNOS*NBPG1 + 2
        CALL WKVECT(CHMAT2,'G V R',DIMB,JMATSI)
        ZR(JMATSI-1+1) = NNOS
        ZR(JMAT-1+2) = NBPG1
        ZR(JMATSI-1+2) = NBPG1
        JMAT = JMAT + 2
        JMATSI = JMATSI + 2

        AA = 1.D0 + SQRT(3.D0)/2.D0
        BB = -0.5D0
        CC = 1.D0 - SQRT(3.D0)/2.D0

        DO 20 I = 1,16
          ZR(JMAT-1+I) = BB
   20   CONTINUE

        ZR(JMAT-1+1) = AA
        ZR(JMAT-1+3) = CC
        ZR(JMAT-1+6) = AA
        ZR(JMAT-1+8) = CC
        ZR(JMAT-1+9) = CC
        ZR(JMAT-1+11) = AA
        ZR(JMAT-1+14) = CC
        ZR(JMAT-1+16) = AA


C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'TRIA3' .OR. ELREFE.EQ.'TRIA6' .OR.
     &         ELREFE.EQ.'TRIA3H' .OR. ELREFE.EQ.'TRIA6H' .OR.
     &         ELREFE.EQ.'TRIA3L' .OR. ELREFE.EQ.'TRIA6D' .OR.
     &         ELREFE.EQ.'TRIL6' .OR. ELREFE.EQ.'TRII3' .OR.
     &         ELREFE.EQ.'TRII6' .OR. ELREFE.EQ.'TRIA7') THEN


        IF (ELREFE.EQ.'TRIA3H' .OR. ELREFE.EQ.'TRIA6H') THEN
C         POUR L INSTANT LES ELEMENTS TRIANGLE THM TRAVAILLENT
C         AVEC LA DEUXIEME FAMILLE DE POINTS DE GAUSS
          IFAP = 2
        ELSE
          IFAP = 1
        END IF
        CALL ELRFGF(ELREFE,IFAP,NBPG,27*3,COOPG,27,POIPG)

        NBPG1 = NBPG(IFAP)
        DIMB = NNOS*NBPG(IFAP) + 2
        CALL WKVECT(CHMAT2,'G V R',DIMB,JMATSI)
        ZR(JMATSI-1+1) = NNOS
        ZR(JMAT-1+2) = NNOS
        JMAT = JMAT + 2
        ZR(JMATSI-1+2) = NBPG(IFAP)
        JMATSI = JMATSI + 2

        IF (ELREFE.EQ.'TRIA6D' .OR. ELREFE.EQ.'TRIA3L' .OR.
     &      ELREFE.EQ.'TRIL6') THEN
          AA = 1.D0
          BB = 0.D0
        ELSE
          AA = 5.D0/3.D0
          BB = -1.D0/3.D0
        END IF

        DO 30 I = 1,9
          ZR(JMAT-1+I) = BB
   30   CONTINUE
        ZR(JMAT) = AA
        ZR(JMAT+4) = AA
        ZR(JMAT+8) = AA

        ZR(JMATSI) = 1.0D0
        ZR(JMATSI+1) = 1.0D0
        ZR(JMATSI+2) = 1.0D0

        IF (ELREFE.EQ.'TRIA6H') THEN
          CALL WKVECT('&&INIMAT.M','V V R',NNOS*NNOS,JM)
          CALL WKVECT('&&INIMAT.N','V V R',NNOS*NBPG(IFAP),JN)
          CALL JEVEUO('&&JNI099.FFT','L',IFFT)

          DO 60 KP = 0,NBPG1 - 1
            LF = KP*NNOS
            XG(1) = COOPG(NDIM*KP+1)
            XG(2) = COOPG(NDIM*KP+2)
            CALL ELRFVF('TRIA3',XG,NNOS,ZR(JN+LF),NNO)

            DO 50 I = 1,NNOS
              DO 40 J = 1,NNOS
                LM = NNOS* (I-1) + J
                ZR(JM+LM-1) = ZR(JM+LM-1) + ZR(JN+LF+I-1)*ZR(JN+LF+J-1)
   40         CONTINUE
   50       CONTINUE
   60     CONTINUE

          CALL MGAUSS(ZR(JM),ZR(JN),NNOS,NNOS,NBPG1,ZERO,FAUX)

          DO 80 I = 1,NNOS
            L = (I-1)*NBPG1
            DO 70 KP = 1,NBPG1
              LN = (KP-1)*NNOS
              ZR(JMATSI+L+KP-1) = ZR(JN+LN+I-1)
   70       CONTINUE
   80     CONTINUE

          CALL JEDETR('&&INIMAT.M')
          CALL JEDETR('&&INIMAT.N')
          CALL JEDETR('&&INIMAT.F')

        END IF


C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'QUAD4' .OR. ELREFE.EQ.'QUA8D' .OR.
     &         ELREFE.EQ.'QUAD4L' .OR. ELREFE.EQ.'QUAD8' .OR.
     &         ELREFE.EQ.'QUAS8' .OR. ELREFE.EQ.'QUAI4' .OR.
     &         ELREFE.EQ.'QUAI8' .OR. ELREFE.EQ.'QUAD9' .OR.
     &         ELREFE.EQ.'QUAS4') THEN

        NBPG1 = NBPG(1)
        DIMB = NNOS*NBPG1 + 2
        CALL WKVECT(CHMAT2,'G V R',DIMB,JMATSI)

        CALL ELRFGF(ELREFE,1,NBPG,27*3,COOPG,27,POIPG)

        ZR(JMATSI-1+1) = NNOS
        ZR(JMAT-1+2) = NNOS
        JMAT = JMAT + 2
        ZR(JMATSI-1+2) = NBPG1
        JMATSI = JMATSI + 2

        IF (ELREFE.EQ.'QUAD4L' .OR. ELREFE.EQ.'QUA8D') THEN
          AA = 1.D0
          BB = 0.D0
          CC = 0.D0
        ELSE
          AA = 1.D0 + SQRT(3.D0)/2.D0
          BB = -0.5D0
          CC = 1.D0 - SQRT(3.D0)/2.D0
        END IF
        DO 90 I = 1,16
          ZR(JMAT-1+I) = BB
   90   CONTINUE

        ZR(JMAT-1+1) = AA
        ZR(JMAT-1+3) = CC
        ZR(JMAT-1+6) = AA
        ZR(JMAT-1+8) = CC
        ZR(JMAT-1+9) = CC
        ZR(JMAT-1+11) = AA
        ZR(JMAT-1+14) = CC
        ZR(JMAT-1+16) = AA


        IF (ELREFE.EQ.'QUAD8' .OR. ELREFE.EQ.'QUAS8' .OR.
     &      ELREFE.EQ.'QUAD9' .OR. ELREFE.EQ.'QUAI8') THEN


          CALL WKVECT('&&INIMAT.M','V V R',NNOS*NNOS,JM)
          CALL WKVECT('&&INIMAT.N','V V R',NNOS*NBPG1,JN)



C     RECUPERATION DU POINTEUR IFFT


          CALL JEVEUO('&&JNI099.FFT','L',IFFT)

          DO 120 KP = 0,NBPG1 - 1
            LF = KP*NNOS
            XG(1) = COOPG(NDIM*KP+1)
            XG(2) = COOPG(NDIM*KP+2)
            CALL ELRFVF('QUAD4   ',XG,NNOS,ZR(JN+LF),NNO)

            DO 110 I = 1,NNOS
              DO 100 J = 1,NNOS
                LM = NNOS* (I-1) + J
                ZR(JM+LM-1) = ZR(JM+LM-1) + ZR(JN+LF+I-1)*ZR(JN+LF+J-1)

  100         CONTINUE
  110       CONTINUE
  120     CONTINUE

          CALL MGAUSS(ZR(JM),ZR(JN),NNOS,NNOS,NBPG1,ZERO,FAUX)

          DO 140 I = 1,NNOS
            L = (I-1)*NBPG1
            DO 130 KP = 1,NBPG1
              LN = (KP-1)*NNOS
              ZR(JMATSI+L+KP-1) = ZR(JN+LN+I-1)
  130       CONTINUE
  140     CONTINUE
        END IF


        IF (ELREFE.EQ.'QUAS4') THEN
          CALL WKVECT('&&INIMAT.M','V V R',NNOS*NNOS,JM)
          CALL WKVECT('&&INIMAT.N','V V R',NNOS*NBPG1,JN)

C     RECUPERATION DU POINTEUR IFFT
          CALL JEVEUO('&&JNI099.FFT','L',IFFT)

          DO 141 I = 1,NNOS
            L = (I-1)*NBPG1
            DO 131 KP = 1,NBPG1
              LN = (KP-1)*NNOS
              ZR(JMATSI+L+KP-1) = 1.D0
  131       CONTINUE
  141     CONTINUE
        END IF

        CALL JEDETR('&&INIMAT.M')
        CALL JEDETR('&&INIMAT.N')
        CALL JEDETR('&&INIMAT.F')


C     ------------------------------------------------------------------
      ELSE
        CALL UTMESS('F','INMAT2','L''ELEMENT DE TYPE '//ELREFE//
     &              ' N''EST PAS REFERENCE')
      END IF

      CALL JEDEMA()

      END
