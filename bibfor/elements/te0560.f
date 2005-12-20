      SUBROUTINE TE0560 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE, PHENOM
C ......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/04/2004   AUTEUR JMBHH01 J.M.PROIX 
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
C
C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES POUR
C                          LA MODELISATION ASSE_GRIL
C                          OPTION : 'RIGI_MECA' ET 'MASS_MECA'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER        NDDL, LGRIL, IMATE, NBV, NBRES,IDFDE2
      PARAMETER    ( NDDL = 6, NBRES=1 )
      CHARACTER*2    CODRET(NBRES)
      REAL*8         VALRES(NBRES), TPG
      CHARACTER*8    NOMRES(NBRES)
      INTEGER        NNO,NNOS,NPG1,NPG2,IPOI1,IPOI2,IDFDE1,IGEOM,
     &               JGANO ,NDIM,IVF2,IVF1
      REAL*8         MATP(24,24), MATV(300)
      REAL*8         YT,YN,RAPP,RAID(2,2),ROTD(2,2),ROTL(3),MASS,
     &               A(NDDL,NDDL,4,4),
     &               RTRA(2),RTOR(2),PGL(3,3),CO3D(12)
      INTEGER        NP,I,J,K,L,II,IK,IJKL,IMATUU,NDL,NVEC,IACCE,IVECT
      REAL*8         POIDS,DFDX(4),DFDY(4),VFROT(4),COORD(8)
      REAL*8         DX,DY,DZ,ALPHA,BETA,R8DGRD,XNORM,PJDX,PJDY,PJDZ,PS
C ......................................................................
C
      CALL UTMESS('F','TE0560','L''ELEMENT "MEGRQU4" N''EXISTE PLUS') 
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOI1,IVF1,IDFDE1,JGANO)
      CALL ELREF4(' ','MASS',NDIM,NNO,NNOS,NPG2,IPOI2,IVF2,IDFDE2,JGANO)
      NDL  = NDDL * NNO
      NVEC = NDL * ( NDL + 1 ) / 2
C
C     ---------------------------------------------------
C     ---- RECUPERATION LOI DE COMPORTEMENT MATERIAU ----
C     ---------------------------------------------------
      CALL JEVECH('PMATERC','L',IMATE)
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
      IF (PHENOM.EQ.'ELAS') THEN
      NOMRES(1) = 'RHO'
      NBV = 1
      TPG = 0.0D0
      VALRES(1) = 0.0D0
      CALL RCVALA(ZI(IMATE),' ',PHENOM,0,'   ',TPG,NBV,NOMRES,VALRES,
     &             CODRET,'F')
      ENDIF
C     --------------------------------------------------
C     --- RECUPERATION CARACTERISTIQUES GEOMETRIQUES ---
C     --------------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL DXQPGL ( ZR(IGEOM) , PGL )
C     ----------------------------------------------------
C     --- RECUPERATION DES CARACTERISTIQUES MECANIQUES ---
C     ----------------------------------------------------
      CALL JEVECH ('PCACOQU','L',LGRIL)
      LGRIL = LGRIL-1
      YT    =  ZR(LGRIL+3)
      YN    =  ZR(LGRIL+4)
      RAPP  =  ZR(LGRIL+5)
      ALPHA =  ZR(LGRIL+1) * R8DGRD()
      BETA  =  ZR(LGRIL+2) * R8DGRD()
C     ----------------------------------------------------
C     --- CHANGEMENT DE REPERE                         ---
C     ----------------------------------------------------
      DX = COS(BETA)*COS(ALPHA)
      DY = COS(BETA)*SIN(ALPHA)
      DZ = SIN(BETA)
      XNORM = SQRT (DX*DX + DY*DY + DZ*DZ)
      DX = DX/XNORM
      DY = DY/XNORM
      DZ = DZ/XNORM
      PS = DX*PGL(3,1) + DY*PGL(3,2) + DZ*PGL(3,3)
      PJDX = DX - PS*PGL(3,1)
      PJDY = DY - PS*PGL(3,2)
      PJDZ = DZ - PS*PGL(3,3)
      XNORM = SQRT (PJDX*PJDX + PJDY*PJDY + PJDZ*PJDZ)
      PJDX = PJDX/XNORM
      PJDY = PJDY/XNORM
      PJDZ = PJDZ/XNORM
      PGL(1,1) = PGL(3,1)
      PGL(1,2) = PGL(3,2)
      PGL(1,3) = PGL(3,3)
      PGL(2,1) = PJDX
      PGL(2,2) = PJDY
      PGL(2,3) = PJDZ
      PGL(3,1) = PGL(1,2)*PGL(2,3)-PGL(1,3)*PGL(2,2)
      PGL(3,2) = PGL(1,3)*PGL(2,1)-PGL(1,1)*PGL(2,3)
      PGL(3,3) = PGL(1,1)*PGL(2,2)-PGL(1,2)*PGL(2,1)
C
C --- CHANGEMENT DE REPERE : GLOBAL --> LOCAL
      CALL UTPVGL(NNO,3,PGL,ZR(IGEOM),CO3D)
      DO 2 I = 1, NNO
         II = 3 * (I-1)
         DO 3 J = 1, 2
            COORD(2*(I-1)+J) = CO3D(J+1+II)
 3       CONTINUE
 2    CONTINUE
C     ---------------------------
C     --- CALCUL DES TENSEURS ---
C     ---------------------------
      RAID(1,1) = ZR(LGRIL+5+5)  * YT / YN
      RAID(2,1) = ZR(LGRIL+6+5)  * YN / YT
      RAID(1,2) = ZR(LGRIL+11+5) * YT / YN
      RAID(2,2) = ZR(LGRIL+12+5) * YN / YT
      RTRA(1)   = ZR(LGRIL+4+5)  * YT / YN
      RTRA(2)   = ZR(LGRIL+10+5) * YN / YT
C
      ROTD(1,1) = ZR(LGRIL+8+5)  * YT / YN
      ROTD(2,1) = ZR(LGRIL+9+5)  * YN / YT
      ROTD(1,2) = ZR(LGRIL+14+5) * YT / YN
      ROTD(2,2) = ZR(LGRIL+15+5) * YN / YT
      RTOR(1)   = ZR(LGRIL+7+5)  * YT / YN
      RTOR(2)   = ZR(LGRIL+13+5) * YN / YT
C
      MASS      = RAPP * RAPP / (YT*YN)
      ROTL(1)   = ZR(LGRIL+2+5) * MASS
      ROTL(2)   = ZR(LGRIL+3+5) * MASS
      ROTL(3)   = ZR(LGRIL+1+5) * MASS
C
      MASS      = MASS * VALRES(1)
C     ------------------------------------
C     --- INITIALISATION A ZERO DE A   ---
C     ------------------------------------
      DO 10 J = 1, NNO
         DO 10 I = 1, NNO
            DO 10 L = 1, NDDL
               DO 10 K = 1, NDDL
                  A(K,L,I,J) = 0.D0
 10   CONTINUE
C ---------------------------------------------------------------------
      IF(OPTION .EQ. 'RIGI_MECA       ') THEN
C     ---------------------------------------------------
C     --- CALCUL DE LA MATRICE ELEMENTAIRE DE RAIDEUR ---
C     ---------------------------------------------------
C     --- DEPLACEMENT DE MEMBRANE ET ROTATION ---
C     -------------------------------------------
      DO 20 NP = 1, NPG1
        CALL DFDM2D(NNO,NP,IPOI1,IDFDE1,COORD(1),DFDX,DFDY,POIDS)
C
        DO 21 I = 1, NNO
           DO 22 J = 1, I
              A(2,2,I,J) = A(2,2,I,J) + POIDS *
     &                   ( RAID(1,1)*DFDX(I)*DFDX(J)
     &                   + RAID(2,1)*DFDY(I)*DFDY(J))
              A(3,3,I,J) = A(3,3,I,J) + POIDS *
     &                   ( RAID(1,2)*DFDX(I)*DFDX(J)
     &                   + RAID(2,2)*DFDY(I)*DFDY(J))
              A(1,1,I,J) = A(1,1,I,J) + POIDS *
     &                   ( RTRA(1)*DFDX(I)*DFDX(J)
     &                   + RTRA(2)*DFDY(I)*DFDY(J))
              A(5,5,I,J) = A(5,5,I,J) + POIDS *
     &                   ( ROTD(2,2)*DFDX(I)*DFDX(J)
     &                   + ROTD(2,1)*DFDY(I)*DFDY(J))
              A(6,6,I,J) = A(6,6,I,J) + POIDS *
     &                   ( ROTD(1,2)*DFDX(I)*DFDX(J)
     &                   + ROTD(1,1)*DFDY(I)*DFDY(J))
              A(4,4,I,J) = A(4,4,I,J) + POIDS *
     &                   ( RTOR(1)*DFDX(I)*DFDX(J)
     &                   + RTOR(2)*DFDY(I)*DFDY(J))
 22        CONTINUE
 21     CONTINUE
 20   CONTINUE
C ---------------------------------------------------------------------
C     --- ROTATIONS LOCALES ---
C     -------------------------
      DO 30 NP = 1, NPG2
        CALL DFDM2D(NNO,NP,IPOI2,IDFDE2,COORD(1),DFDX,DFDY,POIDS)
C
        DO 31 I = 1, NNO
           VFROT(I) = ZR(IVF2 + (I-1))
 31     CONTINUE
        DO 32 I = 1, NNO
           DO 33 J = 1, I
              A(5,5,I,J) = A(5,5,I,J) + POIDS *
     &                   ( ROTL(2)*VFROT(I)*VFROT(J))
              A(6,6,I,J) = A(6,6,I,J) + POIDS *
     &                   ( ROTL(1)*VFROT(I)*VFROT(J))
              A(4,4,I,J) = A(4,4,I,J) + POIDS *
     &                   ( ROTL(3)*VFROT(I)*VFROT(J))
 33        CONTINUE
 32     CONTINUE
 30   CONTINUE
C ---------------------------------------------------------------------
      ELSE IF ( OPTION .EQ. 'MASS_MECA       ' .OR.
     +          OPTION .EQ. 'M_GAMMA         ' ) THEN
C
C     ---------------------------------------------------
C     --- CALCUL DE LA MATRICE ELEMENTAIRE DE MASSE   ---
C     ---------------------------------------------------
      DO 40 NP = 1, NPG2
        CALL DFDM2D(NNO,NP,IPOI2,IDFDE2,COORD(1),DFDX,DFDY,POIDS)
C
        DO 41 I = 1, NNO
           VFROT(I) = ZR(IVF2 + (I-1))
 41     CONTINUE
        DO 42 I = 1, NNO
           DO 43 J = 1, I
              DO 44 K = 1, 3
                 A(K,K,I,J) = A(K,K,I,J) + POIDS *
     &                      ( MASS*VFROT(I)*VFROT(J))
 44           CONTINUE
 43        CONTINUE
 42     CONTINUE
 40   CONTINUE
C ---------------------------------------------------------------------
      ELSE
         CALL UTMESS('F','TE0560','OPTION NON TRAITEE')
      END IF
C     -------------------------------------------------
C     --- PASSAGE DU REPERE LOCAL AU REPERE GLOBAL  ---
C     -------------------------------------------------
      DO 600 I = 1, NNO
        DO 610 J = 1, I
            CALL CHMALG(A(1,1,I,J),PGL,NDDL,NDDL)
 610     CONTINUE
 600  CONTINUE
C ---------------------------------------------------------------------
C --- PASSAGE DE LA MATRICE RECTANGULAIRE A LA MATRICE TRIANGULAIRE ---
C ---------------------------------------------------------------------
      IF ( OPTION .EQ. 'M_GAMMA' ) THEN
         CALL JEVECH('PDEPLAR','L',IACCE)
         CALL JEVECH('PVECTUR','E',IVECT)
         DO 210 K = 1,NVEC
            MATV(K) = 0.0D0
 210     CONTINUE
         DO 500 K = 1, NDDL
            DO 510 L = 1, NDDL
               DO 520 I = 1, NNO
                  IK = ((I-1)*NDDL+K-1) * ((I-1)*NDDL+K) / 2
                  DO 530 J = 1, I
                     IJKL = IK + (J-1)*NDDL + L
                     MATV(IJKL) = A(K,L,I,J)
 530              CONTINUE
 520           CONTINUE
 510        CONTINUE
 500     CONTINUE
         CALL VECMA(MATV,NVEC,MATP,NDL)
         CALL PMAVEC('ZERO',NDL,MATP,ZR(IACCE),ZR(IVECT))
C
      ELSE
         CALL JEVECH('PMATUUR','E',IMATUU)
         DO 400 K = 1, NDDL
            DO 410 L = 1, NDDL
C  IL Y A ECRASEMENT SI ON INVERSE L'ORDR_T DES BOUCL_TS 400 ET 410
               DO 420 I = 1, NNO
                  IK = ((I-1)*NDDL+K-1) * ((I-1)*NDDL+K) / 2
                  DO 430 J = 1, I
                     IJKL = IK + (J-1)*NDDL + L
                     ZR(IMATUU+IJKL-1) = A(K,L,I,J)
 430              CONTINUE
 420           CONTINUE
 410        CONTINUE
 400     CONTINUE
      ENDIF
C
      END
