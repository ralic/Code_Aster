      SUBROUTINE TE0471 ( OPTION , NOMTE )
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
      IMPLICIT NONE
      CHARACTER*16        OPTION , NOMTE , PHENOM
C .....................................................................C
C .....................................................................C
C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES            C
C                          ELEMENTS 3D COEUR HOMOGENEISE               C
C                          OPTION : 'RIGI_MECA      '                  C
C                                                                      C
C    - ARGUMENTS:                                                      C
C        DONNEES:      OPTION       -->  OPTION DE CALCUL              C
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C .....................................................................C
C......................................................................C
      INTEGER       NBRES, NDDL
      PARAMETER   ( NBRES=4, NDDL = 7)
      CHARACTER*24  CARAC,FF
      CHARACTER*8   NOMRES(NBRES),ELREFE
      CHARACTER*2   CODRET(NBRES)
      REAL*8        VALRES(NBRES),TPG,PGL(3,3)
      INTEGER       NNO1,NNO2, NPG1(2,2), NPG2(2,2), NPG, N, NBV,LL
      INTEGER       KP, K1, K2, I, J, K, L, IK, IJKL,IJ,IJL,LCORR
      INTEGER       IMATUU,ICARAC,IFF,IMATE,IGEOM,LORIEN,LSECT,ITYPE
      INTEGER       IPOIDS,IVF1,IDPDX1,IDPDY1,IDPDZ1,IDSDX1,IDSDY1,
     &              IDSDZ1,IDSXY1,IDSXZ1,IDSYZ1,IDPDX2,IDPDY2,IDPDZ2,
     &              IDSDX2,IDSDY2,IDSDZ2,IDSXY2,IDSXZ2,IDSYZ2,IVF2,
     &              IVF3, IPOI3, IDPDX3, IDPDY3, IDPDZ3,
     &              IVF4, IDPDX4, IDPDY4, IDPDZ4
      REAL*8        RAID(3), A(7,7,8,8),POIDS2,POIDS
      REAL*8        E,NU,XIY,XIZ,RTOR,RAPP,
     &              XJX,AYZ,XK(2),COORD(60),YCELL,XLONG
      REAL*8        DSDYZ(16),DFPDX2(8),DFPDY2(8),DFPDZ2(8),
     &              DSDXX(16),DSDYY(16),DSDZZ(16),DSDXY(16),DSDXZ(16),
     &              D2FDPL(8), D2FROT(8),B(1,7,9:20,8),C(1,1,9:20,9:20)
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX --------------------
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
      CALL JEVETE(FF,'L',IFF)
      NPG = NPG1(1,1) * NPG1(1,1) * NPG1(1,2)
      IPOIDS = IFF
      IVF1   = IPOIDS + NPG
      IDPDX1 = IVF1   + NPG * 2 * NNO1
      IDPDY1 = IDPDX1 + NPG * 2 * NNO1
      IDPDZ1 = IDPDY1 + NPG * 2 * NNO1
      IDSDX1 = IDPDZ1 + NPG * 2 * NNO1
      IDSDY1 = IDSDX1 + NPG * 2 * NNO1
      IDSDZ1 = IDSDY1 + NPG * 2 * NNO1
      IDSXY1 = IDSDZ1 + NPG * 2 * NNO1
      IDSXZ1 = IDSXY1 + NPG * 2 * NNO1
      IDSYZ1 = IDSXZ1 + NPG * 2 * NNO1
      IVF2   = IDSYZ1 + NPG * 2 * NNO1
      IDPDX2 = IVF2   + NPG * NNO1
      IDPDY2 = IDPDX2 + NPG * NNO1
      IDPDZ2 = IDPDY2 + NPG * NNO1
      IDSDX2 = IDPDZ2 + NPG * NNO1
      IDSDY2 = IDSDX2 + NPG * NNO1
      IDSDZ2 = IDSDY2 + NPG * NNO1
      IDSXY2 = IDSDZ2 + NPG * NNO1
      IDSXZ2 = IDSXY2 + NPG * NNO1
      IDSYZ2 = IDSXZ2 + NPG * NNO1
C
      NPG = NPG1(1,1)*NPG1(1,1)*NPG1(1,2)
      IFF = IFF + NPG + 10 * ( NPG * 2 * NNO1 ) + 10 * NPG * NNO1
      NPG = NPG1(2,1)*NPG1(2,1)*NPG1(2,2)
      IFF = IFF + NPG + NPG * 2 * NNO1 + 4 * NPG * NNO1
C
      NPG = NPG2(1,1) * NPG2(1,1) * NPG2(1,2)
      IPOI3  = IFF
      IVF3   = IPOI3  + NPG
      IDPDX3 = IVF3   + NPG * NNO2
      IDPDY3 = IDPDX3 + NPG * NNO2
      IDPDZ3 = IDPDY3 + NPG * NNO2
      IVF4   = IDPDZ3 + NPG * NNO2
      IDPDX4 = IVF4   + NPG * NNO1
      IDPDY4 = IDPDX4 + NPG * NNO1
      IDPDZ4 = IDPDY4 + NPG * NNO1
C     -------------------------------------------------
C     --- RECUPERATION LOI DE COMPORTEMENT MATERIAU ---
C     -------------------------------------------------
      CALL JEVECH('PMATERC','L',IMATE)
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
      IF (PHENOM.EQ.'ELAS') THEN
         NOMRES(1)  = 'E'
         NOMRES(2)  = 'NU'
         NOMRES(3)  = 'RHO'
         NBV = 3
      ELSE
         CALL UTMESS('F','TE0471',
     &        'COMPORTEMENT COEUR HOMOGENEISE INEXISTANT')
      ENDIF
      TPG = 0.D0
      CALL RCVALA(ZI(IMATE),' ',PHENOM,0,'   ',TPG,NBV,NOMRES,VALRES,
     &             CODRET,'FM')
      E       = VALRES(1)
      NU      = VALRES(2)
      CALL RCCOMA(ZI(IMATE),'FLUIDE',PHENOM,CODRET)
      IF (PHENOM.EQ.'FLUIDE') THEN
         NOMRES(1)  = 'RHO'
         NBV = 1
      ELSE
         CALL UTMESS('F','TE0471',
     &        'COMPORTEMENT COEUR HOMOGENEISE INEXISTANT')
      ENDIF
      TPG = 0.D0
      CALL RCVALA(ZI(IMATE),' ',PHENOM,0,'   ',TPG,NBV,NOMRES,VALRES,
     &             CODRET,'FM')
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
      XJX   =  ZR(LSECT+8)
C
      IF ( ITYPE .GT. 0 ) THEN
         CALL UTMESS('F','ELEMENTS HOMOGENEISES (TE0471)',
     +   ' : SEULE LES POUTRES A SECTIONS CONSTANTES SONT ADMISES !')
      ENDIF
C     -------------------------------------------
C     --- RECUPERATION DES TERMES CORRECTEURS ---
C     -------------------------------------------
      CALL JEVECH ('PCAPOUF','L',LCORR)
      LCORR = LCORR-1
      YCELL =  ZR(LCORR+5)
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
C     ---------------------------------------
C     --- RECUPERATION MATRICE DE RAIDEUR ---
C     ---------------------------------------
      CALL JEVECH('PMATUUR','E',IMATUU)
C     ---------------------------
C     --- CALCUL DES TENSEURS ---
C     ---------------------------
      RAID(1) = E * XIY * RAPP
      RAID(2) = E * XIZ * RAPP
      RAID(3) = E * AYZ * RAPP
      RTOR    = (E /(2.D0 * (1.D0 + NU))) * XJX * RAPP
C     ---------------------------------------------
C     --- TERME CORRECTEUR : FONCTION D'HERMITE ---
C     --- POUR LES DDLS DE ROTATION             ---
C     ---------------------------------------------
      XLONG = (COORD(3*(5-1)+1)-COORD(1))*0.5D0
C
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
C     ---------------------------------------------------
C     --- CALCUL DE LA MATRICE ELEMENTAIRE DE RAIDEUR ---
C     ---------------------------------------------------
C     --- FLEXION ---
C     ---------------
      NPG = NPG1(1,2) * NPG1(1,1) * NPG1(1,1)
      DO 100 KP = 1, NPG
C
         K1 = (KP-1) * 2 * NNO1
         K2 = (KP-1) * NNO1
C
C        --- CALCUL DES DERIVEES SECONDES
C
          CALL DSFCH3 (NNO1,2*NNO1,ZR(IPOIDS+KP-1),
     &         ZR(IDPDX1+K1),ZR(IDPDY1+K1),ZR(IDPDZ1+K1),
     &         ZR(IDSDX1+K1),ZR(IDSDY1+K1),ZR(IDSDZ1+K1),
     &         ZR(IDSXY1+K1),ZR(IDSXZ1+K1),ZR(IDSYZ1+K1),
     &         COORD(1),
     &         ZR(IDPDX2+K2),ZR(IDPDY2+K2),ZR(IDPDZ2+K2),
     &         ZR(IDSDX2+K2),ZR(IDSDY2+K2),ZR(IDSDZ2+K2),
     &         ZR(IDSXY2+K2),ZR(IDSXZ2+K2),ZR(IDSYZ2+K2),
     &         DSDXX,DSDYY,DSDZZ,DSDXY,DSDYZ,DSDXZ,POIDS)
C
         DO 105 J = 1, NNO1
            D2FDPL(J) = DSDXX(J)
            D2FROT(J) = DSDXX(J+NNO1) * XLONG
 105     CONTINUE
C
         XK(1) = POIDS
         XK(2) =-POIDS

         DO 120 I = 1, NNO1
             DO 130 J = 1, I
               DO 140 K = 1, 2
                 A(K+1,K+1,I,J) = A(K+1,K+1,I,J) +
     &                RAID(K) * POIDS * D2FDPL(I) * D2FDPL(J)
                 A(7-K,K+1,I,J) = A(7-K,K+1,I,J) +
     &                RAID(K) * XK(K) * D2FROT(I) * D2FDPL(J)
                 A(K+1,7-K,I,J) = A(K+1,7-K,I,J) +
     &                RAID(K) * XK(K) * D2FDPL(I) * D2FROT(J)
                 A(7-K,7-K,I,J) = A(7-K,7-K,I,J) +
     &                RAID(K) * POIDS * D2FROT(I) * D2FROT(J)
 140          CONTINUE
 130        CONTINUE
 120      CONTINUE
 100  CONTINUE
C     ---------------------------------------------
C     ---- TRACTION ET COMPRESSION ET TORSION -----
C     ---------------------------------------------
      NPG = NPG2(1,1) * NPG2(1,1) * NPG2(1,2)
      DO 200 KP = 1, NPG
         K1 = (KP-1) * NNO1
C --- CALCUL DES FONCTIONS DE FORME ET DE LEURS DERIVEES
        CALL DPFCH3 (NNO1, NNO1, ZR(IPOI3+KP-1),
     &                ZR(IDPDX4+K1), ZR(IDPDY4+K1), ZR(IDPDZ4+K1),
     &                COORD(1),
     &                ZR(IDPDX4+K1), ZR(IDPDY4+K1), ZR(IDPDZ4+K1),
     &                DFPDX2, DFPDY2, DFPDZ2, POIDS2 )
C
         DO 210 I = 1, NNO1
            DO 220 J = 1, I
            A(1,1,I,J) = A(1,1,I,J) + POIDS2 *
     &              ( RAID(3) * DFPDX2(I) * DFPDX2(J) )
C
            A(4,4,I,J) = A(4,4,I,J) + POIDS2 *
     &              ( RTOR * DFPDX2(I) * DFPDX2(J) )
 220        CONTINUE
 210     CONTINUE
 200   CONTINUE
C     -------------------------------------------------
C     --- PASSAGE DU REPERE LOCAL AU REPERE GLOBAL  ---
C     -------------------------------------------------
      DO 600 I = 1, NNO1
         DO 610 J = 1, I
            CALL CHMALG(A(1,1,I,J),PGL,NDDL,NDDL)
 610     CONTINUE
 600  CONTINUE
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
         DO 530 J = NNO1+1, I
            ZR(IMATUU + IJL + (J-NNO1-1)) = C(1,1,I,J)
 530     CONTINUE
 500  CONTINUE
C----------------------------------------------------------------------
      END
