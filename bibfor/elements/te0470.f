      SUBROUTINE TE0470( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*16        OPTION , NOMTE , PHENOM
C .....................................................................C
C .....................................................................C
C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES            C
C                          ELEMENTS 3D COEUR HOMOGENEISE               C
C                          OPTION : 'MASS_MECA      '                  C
C                                                                      C
C    - ARGUMENTS:                                                      C
C        DONNEES:      OPTION       -->  OPTION DE CALCUL              C
C                      NOMTE        -->  NOM DU TYPE ELEMENT           C
C .....................................................................C
C .....................................................................C
      INTEGER       NBRES, NDDL
      PARAMETER   ( NBRES = 3 , NDDL = 7)
      CHARACTER*24  CARAC,FF
      CHARACTER*8   NOMRES(NBRES),ELREFE,FAMI,POUM
      INTEGER ICODRE(NBRES),KPG,SPT
      REAL*8        VALRES(NBRES), TPG, PGL(3,3)
      INTEGER       NNO1,NNO2, NPG1(2,2), NPG2(2,2), NPG, N, NBV
      INTEGER       IMATUU,ICARAC,IFF,IMATE,IGEOM,LORIEN,LSECT,ITYPE
      INTEGER       KP, K1, K2, K3, I, J, IK, IJKL, K, L, IJ,IJL,LCORR
      INTEGER       IPOI2, IVF2P, IVF2G, IDDX2G, IDDY2G, IDDZ2G,
     &              IPOI3, IVF3F, IDDX3F, IDDY3F, IDDZ3F, IVF3G,
     &              IDDX3G, IDDY3G, IDDZ3G,IPOI4, IVF4P, IVF4F, IDDX4F,
     &              IDDY4F, IDDZ4F, IVF4G, IDDX4G, IDDY4G,IDDZ4G
      REAL*8        MASS(3,3), RDP(3,3), D(2,2),MTOR
      REAL*8        DPDX(8), DPDY(8), DPDZ(8),POIDS2, POIDS3, POIDS4,
     &              FFDPL2(8), FFROT2(8),FFDPL4(8), FFROT4(8),XK(2),
     &              DFPDX3(20), DFPDY3(20), DFPDZ3(20),
     &              DFPDX4(20), DFPDY4(20), DFPDZ4(20)
      REAL*8        A(7,7,8,8),B(1,7,9:20,8),C(1,1,9:20,9:20)
      REAL*8        RHOPOU,RHOFLU,YS,YF,XIY,XIZ,RAPP,
     &              AYZ,B11,B12,B22,COORD(60),YCELL,XLONG
C
C     ----------------------------------------
C     --- RECUPERATION FONCTIONS DE FORMES ---
C     ----------------------------------------
      IF (NOMTE.EQ.'MECA_POHO_HEXA8') THEN
        ELREFE='POHOH8'
      ELSE
        ELREFE='POHOH20'
      ENDIF
      CARAC='&INEL.'//ELREFE//'.CARAC'
      FF   ='&INEL.'//ELREFE//'.FF'
C --- FAMILLES DES POINTS DE GAUSS (VOIR INI100)
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO1 = ZI(ICARAC  )
      NNO2 = ZI(ICARAC+1)
      N = 1
      DO 1 I = 1, 2
        DO 1 J = 1, 2
          N = N+1
          NPG1(I,J) = ZI(ICARAC+N)
          NPG2(I,J) = ZI(ICARAC+N+4)
 1    CONTINUE
C --- ADRESSES DES FONCTIONS DE FORMES
      CALL JEVETE(FF,'L',IFF)
      NPG = NPG1(1,1)*NPG1(1,1)*NPG1(1,2)
      IFF = IFF + NPG + 10 * ( NPG * 2 * NNO1 ) + 10 * NPG * NNO1
C
      NPG    = NPG1(2,1) * NPG1(2,1) * NPG1(2,2)
      IPOI2  = IFF
      IVF2P  = IPOI2  + NPG
      IVF2G  = IVF2P  + NPG * 2 * NNO1
      IDDX2G = IVF2G  + NPG * NNO1
      IDDY2G = IDDX2G + NPG * NNO1
      IDDZ2G = IDDY2G + NPG * NNO1
C
      IPOI3  = IDDZ2G + NPG * NNO1
      NPG    = NPG2(1,1) * NPG2(1,1) * NPG2(1,2)
      IVF3F  = IPOI3  + NPG
      IDDX3F = IVF3F  + NPG * NNO2
      IDDY3F = IDDX3F + NPG * NNO2
      IDDZ3F = IDDY3F + NPG * NNO2
      IVF3G  = IDDZ3F + NPG * NNO2
      IDDX3G = IVF3G  + NPG * NNO1
      IDDY3G = IDDX3G + NPG * NNO1
      IDDZ3G = IDDY3G + NPG * NNO1
C
      IPOI4  = IDDZ3G + NPG * NNO1
      NPG    = NPG2(2,1) * NPG2(2,1) * NPG2(2,2)
      IVF4P  = IPOI4  + NPG
      IVF4F  = IVF4P  + NPG * 2 * NNO1
      IDDX4F = IVF4F  + NPG * NNO2
      IDDY4F = IDDX4F + NPG * NNO2
      IDDZ4F = IDDY4F + NPG * NNO2
      IVF4G  = IDDZ4F + NPG * NNO2
      IDDX4G = IVF4G  + NPG * NNO1
      IDDY4G = IDDX4G + NPG * NNO1
      IDDZ4G = IDDY4G + NPG * NNO1
C     ---------------------------------------------------
C     ---- RECUPERATION LOI DE COMPORTEMENT MATERIAU ----
C     ---------------------------------------------------
      CALL JEVECH('PMATERC','L',IMATE)
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,ICODRE)
      IF (PHENOM.EQ.'ELAS') THEN
         NOMRES(1)  = 'RHO'
         NBV = 1
      ELSE
         CALL U2MESS('F','ELEMENTS3_98')
      ENDIF
      FAMI='FPG1'
      KPG=1
      SPT=1
      POUM='+'
      TPG = 0.D0
      CALL RCVALB(FAMI,KPG,SPT,POUM,ZI(IMATE),' ',PHENOM,0,'   ',TPG,
     &            NBV,NOMRES,VALRES, ICODRE,1)
      RHOPOU  = VALRES(1)
      CALL RCCOMA(ZI(IMATE),'FLUIDE',PHENOM,ICODRE)
      IF (PHENOM.EQ.'FLUIDE') THEN
         NOMRES(1)  = 'RHO'
         NBV = 1
      ELSE
         CALL U2MESS('F','ELEMENTS3_98')
      ENDIF
      TPG = 0.D0
      CALL RCVALB(FAMI,KPG,SPT,POUM,ZI(IMATE),' ',PHENOM,0,'   ',TPG,
     &            NBV,NOMRES,VALRES, ICODRE,1)
      RHOFLU  = VALRES(1)
C     ----------------------------------------------------------------
C     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
C     ----------------------------------------------------------------
      CALL JEVECH ('PCAGNPO', 'L',LSECT)
      LSECT = LSECT-1
      ITYPE =  NINT(ZR(LSECT+23))
C     --- SECTION INITIALE ---
      AYZ   =  ZR(LSECT+1)
      XIY   =  ZR(LSECT+2)
      XIZ   =  ZR(LSECT+3)
C
      IF ( ITYPE .GT. 0 ) THEN
         CALL U2MESS('F','ELEMENTS3_99')
      ENDIF
C     -------------------------------------------
C     --- RECUPERATION DES TERMES CORRECTEURS ---
C     -------------------------------------------
      CALL JEVECH ('PCAPOUF','L',LCORR)
      LCORR = LCORR-1
      YCELL =  ZR(LCORR+5)
      B11   =  ZR(LCORR+1)/YCELL
      B22   =  ZR(LCORR+2)/YCELL
      B12   =  ZR(LCORR+3)/YCELL
      YF    =  ZR(LCORR+4)/YCELL
      YS    =  1 - YF
      RAPP  =  ZR(LCORR+6)
      RAPP = RAPP * RAPP / YCELL
C     --------------------------------------------------
C     --- RECUPERATION CARACTERISTIQUES GEOMETRIQUES ---
C     --------------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)
C --- RECUPERATION DES ORIENTATIONS
      CALL JEVECH ('PCAORIE', 'L',LORIEN)
C --- CALCUL DE LA MATRICE DE PASSAGE
      CALL MATROT ( ZR(LORIEN) , PGL )
C --- CHANGEMENT DE REPERE : GLOBAL --> LOCAL
      CALL UTPVGL(NNO2,3,PGL,ZR(IGEOM),COORD)
C     -------------------------------------
C     --- RECUPERATION MATRICE DE MASSE ---
C     -------------------------------------
      CALL JEVECH('PMATUUR','E',IMATUU)
C     ---------------------------
C     --- CALCUL DES TENSEURS ---
C     ---------------------------
      MASS(1,1) = RHOFLU * B11 + RHOPOU * AYZ * RAPP
      MASS(1,2) = RHOFLU * B12
      MASS(2,1) = RHOFLU * B12
      MASS(2,2) = RHOFLU * B22 + RHOPOU * AYZ * RAPP
      MASS(3,3) = RHOPOU * AYZ * RAPP
      MTOR      = RHOPOU * (XIY + XIZ) * RAPP
C
      RDP(1,1) = (YF - B11) * RHOFLU
      RDP(1,2) =     - B12  * RHOFLU
      RDP(2,1) =     - B12  * RHOFLU
      RDP(2,2) = (YF - B22) * RHOFLU
      RDP(3,3) =  YF        * RHOFLU
C
      D(1,1) = ( B11 + YS ) * RHOFLU
      D(1,2) =   B12        * RHOFLU
      D(2,1) =   B12        * RHOFLU
      D(2,2) = ( B22 + YS ) * RHOFLU
C     ------------------------------------------
C     --- INITIALISATION A ZERO DE A, B ET C ---
C     ------------------------------------------
      DO 50 J = 1, NNO1
         DO 50 I = 1, NNO1
            DO 50 L = 1, NDDL
               DO 50 K = 1, NDDL
                  A(K,L,I,J) = 0.D0
 50   CONTINUE
C --- LES MATRICES B ET C NE SONT UTILISEES QUE POUR DES MAILLES HEXA20
      DO 60 J = 1, NNO1
         DO 60 I = NNO1+1, NNO2
            DO 60 L = 1, NDDL
               B(1,L,I,J) = 0.D0
 60   CONTINUE
      DO 70 J = NNO1+1, NNO2
         DO 70 I = NNO1+1, 20
            C(1,1,I,J) = 0.D0
 70   CONTINUE
C     ---------------------------------------------
C     --- CHANGEMENT DE SIGNES POUR LE PASSAGE  ---
C     --- DDL ROTATION A DERIVEE DEPLACEMENT    ---
C     ---------------------------------------------
      XK(1) =  1.D0
      XK(2) = -1.D0
C     ---------------------------------------------
C     --- TERME CORRECTEUR : FONCTION D'HERMITE ---
C     --- POUR LES DDLS DE ROTATION             ---
C     ---------------------------------------------
      XLONG = (COORD(3*(5-1)+1)-COORD(1))*0.5D0
C
C     --- CALCUL DE LA MATRICE ELEMENTAIRE DE MASSE ---
C     -------------------------------------------------
C     --- MATRICE DE MASSE : POUTRE-FLEXION ---
C     -----------------------------------------
      NPG = NPG1(2,1) * NPG1(2,1) * NPG1(2,2)
      DO 100 KP = 1, NPG
C
         K1 = (KP-1) * 2 * NNO1
         K2 = (KP-1) * NNO1
C --- CALCUL DES FONCTIONS DE FORME ET DE LEURS DERIVEES
         CALL DPFCH3 (NNO1, NNO1, ZR(IPOI2+KP-1),
     &                ZR(IDDX2G+K2), ZR(IDDY2G+K2), ZR(IDDZ2G+K2),
     &                COORD(1),
     &                ZR(IDDX2G+K2), ZR(IDDY2G+K2), ZR(IDDZ2G+K2),
     &                DPDX, DPDY, DPDZ, POIDS2 )
C
         DO 110 I = 1, NNO1
            FFDPL2(I) = ZR(IVF2P + K1 + I - 1)
            FFROT2(I) = ZR(IVF2P + K1 + I + NNO1 - 1 ) * XLONG
 110     CONTINUE
C --- REMPLISSAGE MATRICE ELEMENTAIRE
          DO 120 I = 1, NNO1
            DO 130 J = 1, I
              DO 140 K = 1, 2
                DO 150 L = 1, 2
                  A(K+1,L+1,I,J) = A(K+1,L+1,I,J) +
     &                MASS(K,L) * POIDS2 * FFDPL2(I) * FFDPL2(J)
                  A(7-K,L+1,I,J) = A(7-K,L+1,I,J) + XK(K) *
     &                MASS(K,L) * POIDS2 * FFROT2(I) * FFDPL2(J)
                  A(K+1,7-L,I,J) = A(K+1,7-L,I,J) + XK(L) *
     &                MASS(K,L) * POIDS2 * FFDPL2(I) * FFROT2(J)
                  A(7-K,7-L,I,J) = A(7-K,7-L,I,J) + XK(K)*XK(L)*
     &                MASS(K,L) * POIDS2 * FFROT2(I) * FFROT2(J)
 150            CONTINUE
 140          CONTINUE
 130        CONTINUE
 120      CONTINUE
 100   CONTINUE
C      ---------------------------------------------------
C      --- MATRICE DE MASSE : COUPLAGE POUTRE / FLUIDE ---
C      ---------------------------------------------------
       NPG = NPG2(2,2) * NPG2(2,1) *  NPG2(2,1)
       DO 200 KP = 1, NPG
C
          K1 = (KP-1) * 2 * NNO1
          K2 = (KP-1) * NNO2
          K3 = (KP-1) * NNO1
C --- CALCUL DES FONCTIONS DE FORME ET DE LEURS DERIVEES
          DO 210 I = 1, NNO1
             FFDPL4(I) = ZR(IVF4P + K1 + I - 1)
             FFROT4(I) = ZR(IVF4P + K1 + I + NNO1 - 1 ) * XLONG
 210      CONTINUE
C
          CALL DPFCH3 (NNO1, NNO2, ZR(IPOI4+KP-1),
     &                 ZR(IDDX4F+K2), ZR(IDDY4F+K2), ZR(IDDZ4F+K2),
     &                 COORD(1),
     &                 ZR(IDDX4G+K3), ZR(IDDY4G+K3), ZR(IDDZ4G+K3),
     &                 DFPDX4, DFPDY4, DFPDZ4, POIDS4 )
C --- REMPLISSAGE DES MATRICES ELEMENTAIRES
          DO 220 I = 1, NNO1
            DO 230 J = 1, I
              DO 240 K = 1, 2
                A(7,K+1,I,J) = A(7,K+1,I,J) - POIDS4 *
     &            FFDPL4(J) * (D(K,1) * DFPDY4(I) + D(K,2) * DFPDZ4(I))
                A(7,7-K,I,J) = A(7,7-K,I,J) - POIDS4 * XK(K) *
     &            FFROT4(J) * (D(K,1) * DFPDY4(I) + D(K,2) * DFPDZ4(I))
                A(K+1,7,I,J) = A(K+1,7,I,J) - POIDS4 *
     &            FFDPL4(I) * (D(K,1) * DFPDY4(J) + D(K,2) * DFPDZ4(J))
                A(7-K,7,I,J) = A(7-K,7,I,J) - POIDS4 * XK(K) *
     &            FFROT4(I) * (D(K,1) * DFPDY4(J) + D(K,2) * DFPDZ4(J))
 240          CONTINUE
 230        CONTINUE
 220      CONTINUE
C
          DO 250 I = NNO1+1, NNO2
            DO 260 J = 1, NNO1
              DO 270 K = 1,2
                B(1,K+1,I,J) = B(1,K+1,I,J) - POIDS4 *
     &            FFDPL4(J) * (D(K,1) * DFPDY4(I) + D(K,2) * DFPDZ4(I))
                B(1,7-K,I,J) = B(1,7-K,I,J) - POIDS4 * XK(K) *
     &            FFROT4(J) * (D(K,1) * DFPDY4(I) + D(K,2) * DFPDZ4(I))
 270            CONTINUE
 260         CONTINUE
 250      CONTINUE
C
 200   CONTINUE
C      --------------------------------------------------------------
C      --- MATRICE DE MASSE : FLUIDE ET POUTRE-TRACTION ET TORSON ---
C      --------------------------------------------------------------
       NPG = NPG2(1,1) * NPG2(1,1) * NPG2(1,2)
       DO 300 KP = 1, NPG
C
          K1 = (KP-1) * NNO1
          K2 = (KP-1) * NNO2
C --- CALCUL DES FONCTIONS DE FORME ET DE LEURS DERIVEES
          CALL DPFCH3 (NNO1, NNO2, ZR(IPOI3+KP-1),
     &                 ZR(IDDX3F+K2), ZR(IDDY3F+K2), ZR(IDDZ3F+K2),
     &                 COORD(1),
     &                 ZR(IDDX3G+K1), ZR(IDDY3G+K1), ZR(IDDZ3G+K1),
     &                 DFPDX3,DFPDY3,DFPDZ3, POIDS3 )
C
        DO 301 I = 1, NNO1
            FFDPL2(I) = ZR(IVF3G + K1 + I - 1)
 301    CONTINUE
C
         DO 310 I = 1, NNO1
            DO 320 J = 1, I
            A(1,1,I,J) = A(1,1,I,J) + POIDS3 *
     &              ( MASS(3,3) * FFDPL2(I) * FFDPL2(J) )
C
            A(4,4,I,J) = A(4,4,I,J) + POIDS3 *
     &              ( MTOR * FFDPL2(I) * FFDPL2(J) )
C
            A(7,7,I,J) = A(7,7,I,J) - POIDS3 *
     &              ( RDP(1,1) * DFPDY3(I) * DFPDY3(J)
     &              + RDP(1,2) * DFPDZ3(I) * DFPDY3(J)
     &              + RDP(2,1) * DFPDY3(I) * DFPDZ3(J)
     &              + RDP(2,2) * DFPDZ3(I) * DFPDZ3(J)
     &              + RDP(3,3) * DFPDX3(I) * DFPDX3(J) )
 320        CONTINUE
 310     CONTINUE
         DO 340 I = NNO1+1, NNO2
            DO 350 J = 1, NNO1
            B(1,7,I,J) = B(1,7,I,J) - POIDS3 *
     &              ( RDP(1,1) * DFPDY3(I) * DFPDY3(J)
     &              + RDP(1,2) * DFPDZ3(I) * DFPDY3(J)
     &              + RDP(2,1) * DFPDY3(I) * DFPDZ3(J)
     &              + RDP(2,2) * DFPDZ3(I) * DFPDZ3(J)
     &              + RDP(3,3) * DFPDX3(I) * DFPDX3(J) )
 350       CONTINUE
 340     CONTINUE
         DO 360 I = NNO1+1, NNO2
            DO 370 J = NNO1+1, I
            C(1,1,I,J) = C(1,1,I,J) - POIDS3 *
     &              ( RDP(1,1) * DFPDY3(I) * DFPDY3(J)
     &              + RDP(1,2) * DFPDZ3(I) * DFPDY3(J)
     &              + RDP(2,1) * DFPDY3(I) * DFPDZ3(J)
     &              + RDP(2,2) * DFPDZ3(I) * DFPDZ3(J)
     &              + RDP(3,3) * DFPDX3(I) * DFPDX3(J) )
 370       CONTINUE
 360     CONTINUE
C
 300  CONTINUE
C     -------------------------------------------------
C     --- PASSAGE DU REPERE LOCAL AU REPERE GLOBAL  ---
C     -------------------------------------------------
      DO 600 I = 1, NNO1
        DO 610 J = 1, I
            CALL CHMALG(A(1,1,I,J),PGL,NDDL,NDDL)
 610     CONTINUE
 600  CONTINUE
      DO 620 I = NNO1+1, NNO2
        DO 630 J = 1, NNO1
            CALL CHMALG(B(1,1,I,J),PGL,1,NDDL)
 630     CONTINUE
 620  CONTINUE
C ---------------------------------------------------------------------
C --- PASSAGE DE LA MATRICE RECTANGULAIRE A LA MATRICE TRIANGULAIRE ---
C ---------------------------------------------------------------------
      DO 400 K = 1, NDDL
         DO 410 L = 1, NDDL
C   IL Y A ECRASEMENT SI ON INTERVERTIE L'ORDRE DES BOUCLES 400 ET 410
            DO 420 I = 1, NNO1
               IK = ((I-1)*NDDL+K-1) * ((I-1)*NDDL+K) / 2
               DO 430 J = 1, I
                  IJKL = IK + (J-1)*NDDL + L
                  ZR(IMATUU+IJKL-1) = A(K,L,I,J)
 430           CONTINUE
 420        CONTINUE
 410     CONTINUE
 400  CONTINUE
C   BOUCLE EXECUTEE QUE POUR DES MAILLES HEXA20
      IMATUU = IMATUU + (NDDL*NNO1)*(NDDL*NNO1 + 1) / 2
      DO 500 I = NNO1+1, NNO2
         IJ = (I-NNO1-1)*NDDL*NNO1 + (I-NNO1-1)*(I-NNO1)/2
         DO 510 J = 1, NNO1
            IJL = IJ + (J-1)*NDDL
            DO 520 L = 1, NDDL
               ZR(IMATUU + IJL + (L-1)) = B(1,L,I,J)
 520        CONTINUE
 510     CONTINUE
         IJL = IJ + NNO1 * NDDL
         DO 530 J = NNO1 + 1, I
            ZR(IMATUU + IJL + (J-NNO1-1)) = C(1,1,I,J)
 530     CONTINUE
 500  CONTINUE
C
      END
