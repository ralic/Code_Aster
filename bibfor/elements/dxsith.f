      SUBROUTINE DXSITH(NOMTE,MATER,SIGMA)
      IMPLICIT  NONE
      INTEGER      MATER
      REAL*8       SIGMA(*)
      CHARACTER*16 NOMTE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/10/2011   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       CALCUL DES CONTRAINTES VRAIES
C        (==SIGMA_MECA - SIGMA_THER).
C
C ----------------------------------------------------------------------
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                     ZK24
      CHARACTER*32                              ZK32
      CHARACTER*80                                       ZK80
      COMMON /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER    NBEPSG
      PARAMETER (NBEPSG=8)

      INTEGER NDIM,NNOEL,NNOS,NPG,IPOIDS,ICOOPG,IVF,IDFDX,IDFD2,JGANO
      INTEGER I,J,ICOU,ICPG,IGAUH,IPG,IRET,IBID,NBCMP,NBCOU,NPGH
      INTEGER JNBSPI,ITAB(8)

      REAL*8 D(4,4),REPERE(7),INST,ZERO,EPSTH(NBEPSG)

      CHARACTER*4 FAMI

      LOGICAL DKG
C
C ----------------------------------------------------------------------
C
      FAMI = 'RIGI'
      CALL ELREF5(' ',FAMI,NDIM,NNOEL,NNOS,NPG,IPOIDS,ICOOPG,
     &                                         IVF,IDFDX,IDFD2,JGANO)
C
      ZERO = 0.0D0
      CALL VECINI(7,ZERO,REPERE)
C
      DKG    = .FALSE.
C
      NBCMP = 6
C
      IF ((NOMTE.EQ.'MEDKTG3').OR.
     &    (NOMTE.EQ.'MEDKQG4')) THEN
        DKG = .TRUE.
      END IF
C
C --- RECUPERATION DE L'INSTANT
C     -------------------------
      CALL TECACH ( 'ONN', 'PTEMPSR', 8, ITAB, IRET )
      IBID = ITAB(1)
      IF (IRET.EQ.0) THEN
         INST = ZR(IBID)
      ELSE
         INST = ZERO
      END IF
C
C --- RECUPERATION DU NOMBRE DE COUCHE ET DE SOUS-POINT
C     -------------------------------------------------
      IF (DKG) THEN
        NBCOU = 1
        NPGH = 1
      ELSE
        CALL JEVECH('PNBSP_I','L',JNBSPI)
        NPGH = 3
        NBCOU = ZI(JNBSPI-1+1)
        IF (NBCOU.LE.0) CALL U2MESS('F','ELEMENTS_46')
      ENDIF
C
C --- BOUCLE SUR LES POINTS DE GAUSS DE LA SURFACE:
C     ---------------------------------------------
      DO 100 IPG=1,NPG
        DO 110 ICOU=1,NBCOU
          DO 120 IGAUH=1,NPGH
            ICPG=NBCMP*NPGH*NBCOU*(IPG-1)+
     &            NBCMP*NPGH*(ICOU-1)+
     &             NBCMP*(IGAUH-1)
C
C         -- INTERPOLATION DE ALPHA EN FONCTION DE LA TEMPERATURE
C         ----------------------------------------------------
            CALL VERIFT('RIGI',IPG,IGAUH,'+',MATER,'ELAS',
     &           1,EPSTH(1),IRET)

            EPSTH(2) = EPSTH(1)
            EPSTH(3) = ZERO
            EPSTH(4) = ZERO
            EPSTH(5) = ZERO
            EPSTH(6) = ZERO

C           -- CALCUL DE LA MATRICE DE HOOKE
C           --------------------------------
            CALL DMATCP ( 'RIGI',MATER, INST,'+',IPG,IGAUH,REPERE,D )

C           -- CALCUL DES CONTRAINTES VRAIES (==SIGMA_MECA - SIGMA_THER)
C           -- AU POINT D'INTEGRATION COURANT
C           ------------------------------------------------------------
            DO 130 I=1,4
               DO 131 J=1,4
                  SIGMA(ICPG+I)=SIGMA(ICPG+I)-EPSTH(J)*D(I,J)
 131           CONTINUE
 130        CONTINUE
C
 120      CONTINUE
 110    CONTINUE
 100  CONTINUE

      END
