      SUBROUTINE JNI002(ELREFA,NMAXOB,LIOBJ,NBOBJ)
      IMPLICIT NONE
      CHARACTER*8 ELREFA
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/09/2003   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
      PARAMETER (NBPGMX=27,NBNOMX=27,NBFAMX=10)

      INTEGER NBPG(NBFAMX),IRET,NDIM,NNO,NNOS,NBFPG
      INTEGER NMAXOB,NBOBJ,LONFAM,IFAM,LON2,DECAL,IDIM
      INTEGER IPG,INO,NDERIV,JVI,JVR,NPG
      REAL*8 XNO(3*NBNOMX),VOL
      REAL*8 XPG(3*NBPGMX),POIPG(NBPGMX)
      REAL*8 FF(NBNOMX),DFF(3*NBNOMX)
      CHARACTER*24 LIOBJ(NMAXOB)
      CHARACTER*8 NOFPG(NBFAMX)
C DEB ------------------------------------------------------------------


      NBOBJ = 2
      CALL ASSERT(NMAXOB.GT.NBOBJ)
      LIOBJ(1) = '&INEL.'//ELREFA//'.ELRA_I'
      LIOBJ(2) = '&INEL.'//ELREFA//'.ELRA_R'



      CALL JEEXIN('&INEL.'//ELREFA//'.ELRA_I',IRET)
      IF (IRET.GT.0) GO TO 120

      CALL ELRACA(ELREFA,NDIM,NNO,NNOS,NBFPG,NOFPG,NBPG,XNO,VOL)
      CALL ASSERT((NDIM.GT.0) .AND. (NDIM.LE.3))
      CALL ASSERT((NNO.GT.0) .AND. (NNO.LE.NBNOMX))
      CALL ASSERT((NBFPG.GT.0) .AND. (NBFPG.LE.NBFAMX))


      CALL WKVECT(LIOBJ(1),'G V I',4+NBFPG,JVI)
      ZI(JVI-1+1) = NDIM
      ZI(JVI-1+2) = NBFPG
      ZI(JVI-1+3) = NNO
      ZI(JVI-1+4) = NNOS
      LON2 = 0
      DO 10,IFAM = 1,NBFPG
        NPG = NBPG(IFAM)
        CALL ASSERT((NPG.GT.0) .AND. (NPG.LE.NBPGMX))
        ZI(JVI-1+4+IFAM) = NPG

C       ON VEUT STOCKER : W(PG),GEOM(PG,IDIM)
C                         FF(PG,INO) ET DFF(PG,INO,IDIM)
C                         MAPGNO(INO,PG) ET MANOPG(PG,INO)
        LONFAM = NPG* (NDIM+1)
        LONFAM = LONFAM + NPG*NNO* (NDIM+1)
        LONFAM = LONFAM + 2 + NPG*NNO

        LON2 = LON2 + LONFAM
   10 CONTINUE

      CALL WKVECT(LIOBJ(2),'G V R',LON2,JVR)

      DECAL = 0
      DO 110,IFAM = 1,NBFPG

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


C       -- DERIVEES DES FONCTIONS DE FORME :
C       ------------------------------------------------
        DO 100,IPG = 1,NPG
          CALL ELRFDF(ELREFA,XPG(NDIM* (IPG-1)+1),3*NBNOMX,DFF,NNO,
     &                NDERIV)
          CALL ASSERT(NDERIV.EQ.NDIM)
          DO 90,INO = 1,NNO
            DO 80,IDIM = 1,NDIM
              DECAL = DECAL + 1
              ZR(JVR-1+DECAL) = DFF(NDIM* (INO-1)+IDIM)
   70         CONTINUE
   80       CONTINUE
   90     CONTINUE
  100   CONTINUE


C       -- MATRICE GAUSS -> NOEUDS : MAPGNO
C       ------------------------------------------------
C       ON STOCKE DANS L'ORDRE : NNO,NPG,MAPGNO
        CALL INMAT4(ELREFA,NNO,NNOS,NPG,NOFPG(IFAM),ZR(JVR-1+DECAL+1))
        DECAL = DECAL + 2 + NPG*NNO


  110 CONTINUE


  120 CONTINUE

      END
