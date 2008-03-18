      SUBROUTINE JNI002(ELREFA,NMAXOB,LIOBJ,NBOBJ)
      IMPLICIT NONE
      CHARACTER*8 ELREFA
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/03/2008   AUTEUR CNGUYEN C.NGUYEN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE VABHHTS J.PELLET
C ======================================================================
C BUT : INITIALISER LES ELREFA
C ======================================================================

C ----------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN DECLARATIONS NORMALISEES  JEVEUX ---------------------

      INTEGER NBPGMX,NBNOMX,NBFAMX
      PARAMETER (NBPGMX=1000,NBNOMX=27,NBFAMX=20)

      INTEGER NBPG(NBFAMX),IRET,NDIM,NNO,NNOS,NBFPG
      INTEGER NMAXOB,NBOBJ,LONFAM,IFAM,LON2,DECAL,IDIM
      INTEGER IPG,INO,NDERIV,JVI,JVR,NPG,NNO2,JDIM
      REAL*8 XNO(3*NBNOMX),VOL,RVIDE,R8VIDE
      REAL*8 XPG(3*NBPGMX),POIPG(NBPGMX)
      REAL*8 FF(NBNOMX),DFF(3,NBNOMX),DFF2(3,3,NBNOMX)
      CHARACTER*24 LIOBJ(NMAXOB)
      CHARACTER*8 NOFPG(NBFAMX)
      
C     NBPGMX, NBNOMX, NBFAMX SE REFERER A ELRACA
      
C DEB ------------------------------------------------------------------


      NBOBJ = 2
      CALL ASSERT(NMAXOB.GT.NBOBJ)
      LIOBJ(1) = '&INEL.'//ELREFA//'.ELRA_I'
      LIOBJ(2) = '&INEL.'//ELREFA//'.ELRA_R'



      CALL JEEXIN('&INEL.'//ELREFA//'.ELRA_I',IRET)
      IF (IRET.GT.0) GO TO 150

      CALL ELRACA(ELREFA,NDIM,NNO,NNOS,NBFPG,NOFPG,NBPG,XNO,VOL)
      CALL ASSERT((NDIM.GE.0) .AND. (NDIM.LE.3))
      CALL ASSERT((NNO.GT.0) .AND. (NNO.LE.NBNOMX))
      CALL ASSERT((NBFPG.GT.0) .AND. (NBFPG.LE.NBFAMX))


      CALL WKVECT(LIOBJ(1),'V V I',4+NBFPG,JVI)
      ZI(JVI-1+1) = NDIM
      ZI(JVI-1+2) = NBFPG
      ZI(JVI-1+3) = NNO
      ZI(JVI-1+4) = NNOS
      LON2 = 0
      DO 10,IFAM = 1,NBFPG
        NPG = NBPG(IFAM)
        CALL ASSERT((NPG.GT.0) .AND. (NPG.LE.NBPGMX))
        ZI(JVI-1+4+IFAM) = NPG

C       ON VEUT STOCKER : W(IPG),GEOM(IDIM,IPG)
C                         FF(INO,IPG) ET
C                         DFF(IDIM,INO,IPG)
C                         DFF2(IDIM,JDIM,INO,IPG)
C                         MAPGNO(INO,IPG)
        LONFAM = NPG
        LONFAM = LONFAM + NPG*NDIM
        LONFAM = LONFAM + NPG*NNO
        LONFAM = LONFAM + NPG*NNO*NDIM
        LONFAM = LONFAM + NPG*NNO*NDIM*NDIM
        LONFAM = LONFAM + 2 + NPG*NNO

        LON2 = LON2 + LONFAM
   10 CONTINUE

      CALL WKVECT(LIOBJ(2),'V V R',LON2,JVR)

      DECAL = 0
      DO 140,IFAM = 1,NBFPG

C       -- COORDONNEES ET POIDS DES POINTS DE GAUSS :
C       ------------------------------------------------
        CALL ELRAGA(ELREFA,NOFPG(IFAM),NDIM,NPG,XPG,POIPG)
        DO 20,IPG = 1,NPG
          DECAL = DECAL + 1
          ZR(JVR-1+DECAL) = POIPG(IPG)
   20   CONTINUE
        DO 40,IPG = 1,NPG
          DO 30,IDIM = 1,NDIM
            DECAL = DECAL + 1
            ZR(JVR-1+DECAL) = XPG(NDIM* (IPG-1)+IDIM)
   30     CONTINUE
   40   CONTINUE


C       -- VALEURS DES FONCTIONS DE FORME :
C       ------------------------------------------------
        DO 60,IPG = 1,NPG
          CALL ELRFVF(ELREFA,XPG(NDIM* (IPG-1)+1),NBNOMX,FF,NNO)
          DO 50,INO = 1,NNO
            DECAL = DECAL + 1
            ZR(JVR-1+DECAL) = FF(INO)
   50     CONTINUE
   60   CONTINUE


C       -- DERIVEES 1ERES DES FONCTIONS DE FORME :
C       ------------------------------------------------
        DO 90,IPG = 1,NPG
          CALL ELRFDF(ELREFA,XPG(NDIM* (IPG-1)+1),3*NBNOMX,DFF,NNO,
     &                NDERIV)
          CALL ASSERT(NDERIV.EQ.NDIM)
          DO 80,INO = 1,NNO
            DO 70,IDIM = 1,NDIM
              DECAL = DECAL + 1
              ZR(JVR-1+DECAL) = DFF(IDIM,INO)
   70       CONTINUE
   80     CONTINUE
   90   CONTINUE


C       -- DERIVEES 2EMES DES FONCTIONS DE FORME :
C       ------------------------------------------------
        DO 130,IPG = 1,NPG
          CALL ELRFD2(ELREFA,XPG(NDIM* (IPG-1)+1),9*NBNOMX,DFF2,NNO2,
     &                NDERIV)
          IF (NDERIV.EQ.0) THEN
            CALL ASSERT(NNO2.EQ.0)
            RVIDE = R8VIDE()
          ELSE
            CALL ASSERT(NDERIV.EQ.NDIM)
            CALL ASSERT(NNO2.EQ.NNO)
          END IF
          DO 120,INO = 1,NNO
            DO 110,JDIM = 1,NDIM
              DO 100,IDIM = 1,NDIM
                DECAL = DECAL + 1
                IF (NDERIV.NE.0) THEN
                  ZR(JVR-1+DECAL) = DFF2(IDIM,JDIM,INO)
                ELSE
                  ZR(JVR-1+DECAL) = RVIDE
                END IF
  100         CONTINUE
  110       CONTINUE
  120     CONTINUE
  130   CONTINUE


C       -- MATRICE GAUSS -> NOEUDS : MAPGNO
C       ------------------------------------------------
C       ON STOCKE DANS L'ORDRE : NNO,NPG,MAPGNO
        CALL INMAT4(ELREFA,NNO,NNOS,NPG,NOFPG(IFAM),ZR(JVR-1+DECAL+1))
        DECAL = DECAL + 2 + NPG*NNO


  140 CONTINUE

  150 CONTINUE

      END
