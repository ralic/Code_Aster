      SUBROUTINE ACMATA(JVECTN, JVECTU, JVECTV, NBORDR, KWORK,
     &                    SOMPGW, JRWORK, TSPAQ, IPG, JVECPG,JDTAUM, 
     &                    JRESUN,NOMMET,NOMMAT,VRESPC)
      IMPLICIT   NONE
      INTEGER    JVECTN, JVECTU, JVECTV, NBORDR, KWORK
      INTEGER    SOMPGW, JRWORK, TSPAQ, IPG, JVECPG, JDTAUM,JRESUN
      CHARACTER*16  NOMMET
      CHARACTER*8   NOMMAT
      REAL*8     VRESPC(24)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 20/06/2011   AUTEUR TRAN V-X.TRAN 
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
C TOLE CRP_20 CRP_21
C ---------------------------------------------------------------------
C BUT: POUR LA FATIGUE A AMPLITUDE CONSTANTE
C      DETERMINER LE PLAN DES MAX DES TAU_MAX ET CALCULER DES GRANDEURS
C
C
C REMARQUE: CETTE SUBROUTINE EST APPLICABLE POUR UN NOEUD OU IPG EGALE
C           A 1 ET SOMPGW = SOMNOW,JVECPG = JVECNO  
C ----------------------------------------------------------------------
C ARGUMENTS :
C     JVECTN  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
C                     VECTEURS NORMAUX.
C     JVECTU  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
C                     VECTEURS u DU PLAN DE CISAILLEMENT.
C     JVECTV  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
C                     VECTEURS v DU PLAN DE CISAILLEMENT.
C     NBORDR  : IN  : NOMBRE DE NUMEROS D'ORDRE.
C     KWORK   : IN  : KWORK = 0 ON TRAITE LA 1ERE MAILLE DU PAQUET DE
C                               MAILLES ;
C                     KWORK = 1 ON TRAITE LA IEME (I>1) MAILLE DU PAQUET
C                               MAILLES.
C     SOMPGW  : IN  : SOMME DES POINTS DE GAUSS DES N MAILLES PRECEDANT 
C                     LA MAILLE COURANTE.
C     JRWORK  : IN  : ADRESSE DU VECTEUR DE TRAVAIL CONTENANT 
C                     L'HISTORIQUE DES TENSEURS DES CONTRAINTES
C                     ATTACHES A CHAQUE POINT DE GAUSS DES MAILLES
C                     DU <<PAQUET>> DE MAILLES.
C     TSPAQ   : IN  : TAILLE DU SOUS-PAQUET DU <<PAQUET>> DE MAILLES
C                     COURANT.
C     IPG     : IN  : IEME POINT DE GAUSS.
C     JVECPG  : IN  : ADRESSE DU VECTEUR DE TRAVAIL CONTENANT 
C                     LES COMPOSANTES u ET v DU VECTEUR TAU 
C                     (CISAILLEMENT), POUR TOUS LES NUMEROS
C                     D'ORDRE DE CHAQUE VECTEUR NORMAL.
C    JDTAU      IN    ADRESSE DU VECTEUR DE TRAVAIL CONTENANT
C                     LES VALEURS DE DELTA_TAU_MAX POUR CHAQUE VECTEUR.
C    JVECN      IN    ADRESSE DU VECTEUR DE TRAVAIL CONTENANT
C                     LA VALEUR DU POINTEUR PERMETTANT D'ACCEDER AU
C                     VECTEUR NORMAL ASSOCIE A DELTA_TAU_MAX.
C    NOMMET     IN    NOM DE METHOD D'APPROCHEMENT DE CERCLE ("CERCLE 
C                     EXACT" ET "CERCLE APPROCHE")
C    VALA       IN    VALEUR DU PARAMETRE a ASSOCIE AU CRITERE.
C    COEFPA     IN    COEFFICIENT DE PASSAGE CISAILLEMENT - UNIAXIAL.
C   VRSESU      OUT   TABLEAU DES RESULTATS (GRANDEURS ET DOMMAGE).
C                     POUR L'INSTANT, LA DIMENSION DE VRESU EST 24
C ----------------------------------------------------------------------
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
C     ------------------------------------------------------------------
      INTEGER      I, J, K, N
      INTEGER      NBVEC, DIM ,MNMAX(2), JVPG1, JVPG2
      INTEGER      JVECN2, JVECU2, JVECV2, JVECN1, JVECU1, JVECV1
      INTEGER      ADRS, ICODRE
C
      REAL*8       EPSILO, GAMMA, PI, R8PI, DPHI
      REAL*8       GAMMAM, PHIM, DGAM2, DPHI2, PHI0, DTAUM(2)
      REAL*8       NXM(2), NYM(2), NZM(2)
      REAL*8       SIXX, SIYY, SIZZ, SIXY, SIXZ, SIYZ, FXM(2), FYM(2)
      REAL*8       FZM(2), EPSXX, EPSYY, EPSZZ, EPSXY, EPSXZ, EPSYZ
      REAL*8       NORM(2), NORMAX(2), SNORM(2), EPSXM(2), EPSYM(2)
      REAL*8       EPSZM(2), EPNORM(2), EPNMAX(2), SEPNMX(2), NORMOY(2)
      REAL*8       EPNMOY(2), R8B, VALE, VALNU, C1, C2
      REAL*8       PHYDRO, PHYDRM

C     ------------------------------------------------------------------
C
C234567  

      CALL WKVECT( '&&ACMATA.VECT_NORMA1', 'V V R', 27, JVECN1 )
      CALL WKVECT( '&&ACMATA.VECT_TANGU1', 'V V R', 27, JVECU1 )
      CALL WKVECT( '&&ACMATA.VECT_TANGV1', 'V V R', 27, JVECV1 )
      CALL WKVECT( '&&ACMATA.VECT_NORMA2', 'V V R', 27, JVECN2 )
      CALL WKVECT( '&&ACMATA.VECT_TANGU2', 'V V R', 27, JVECU2 )
      CALL WKVECT( '&&ACMATA.VECT_TANGV2', 'V V R', 27, JVECV2 )
      
      CALL WKVECT( '&&ACMATA.VECTPG1', 'V V R', 18*NBORDR, JVPG1 )
      CALL WKVECT( '&&ACMATA.VECTPG2', 'V V R', 18*NBORDR, JVPG2 )
      
      EPSILO = 1.0D-7
      PI = R8PI()   
      
                                                            
C PROJECTION DE L'HISTORIQUE DU CISAILLEMENT DANS UN PLAN.

      NBVEC = 209
      
      CALL TAURLO(NBVEC, JVECTN, JVECTU, JVECTV, NBORDR, KWORK,
     &            SOMPGW, JRWORK, TSPAQ, IPG, JVECPG)
            

C CALCMAX DES DELTA_TAU MAX ET DU VECTEUR NORMAL ASSOCIE POUR
C LE PE GAUSS COURANT DE LA MAILLE COURANTE.

C 1/ RA ZERO DU VECTEUR DE TRAVAIL CONTENANT LES VALEURS DE
C    DAU POUR UN POINT DE GAUSS ET DU VECTEUR DE TRAVAIL
C    PANT DE POINTER SUR LE VECTEUR NORMAL ASSOCIE.

C 2/ CDU RAYON CIRCONSCRIT

      CALL RAYCIR(JVECPG, JDTAUM, JRESUN, NBORDR, NBVEC, NOMMET)
  
C 3/ CDU 1ER MAX DES DELTA_TAU ET DU VECTEUR NORMAL ASSOCIE

      DTAUM(1) = 0.0D0
      DTAUM(2) = 0.0D0
      MNMAX(1) = 1
      MNMAX(2) = 1
      
      DO 430 I=1, NBVEC
        IF ( ZR(JDTAUM + (I-1)) .GT. EPSILO ) THEN
           IF ( (ZR(JDTAUM + (I-1))-DTAUM(1))/ZR(JDTAUM + (I-1))
     &           .GT. EPSILO ) THEN
              DTAUM(2) = DTAUM(1)
              MNMAX(2) = MNMAX(1)
              DTAUM(1) = ZR(JDTAUM + (I-1))
              MNMAX(1) = I
           ENDIF
           IF ( ((ZR(JDTAUM + (I-1))-DTAUM(2))/ZR(JDTAUM + (I-1))
     &           .GT. EPSILO)  .AND. (I .NE. MNMAX(1)) ) THEN
              DTAUM(2) = ZR(JDTAUM + (I-1))
              MNMAX(2) = I
           ENDIF
        ENDIF
 430  CONTINUE

C 4/ P RAFFINEMENT CONCERNANT LA DETERMINATION DU VECTEUR NORMAL
C    EAX DES DELTA_TAU (DETERMINATION DU VECTEUR NORMAL A 2
C    DPRES).

      PHYDRO = 0.0D0
      PHYDRM = 0.0D0
      DIM = 27

      DO 440 K=1, 2
         NORM(K) = 0.0D0
         NORMAX(K) = 0.0D0
         SNORM(K) = 0.0D0
         EPNORM(K) = 0.0D0
         EPNMAX(K) = 0.0D0
         SEPNMX(K) = 0.0D0
         NXM(K) = ZR(JVECTN + (MNMAX(K)-1)*3)
         NYM(K) = ZR(JVECTN + (MNMAX(K)-1)*3 + 1)
         NZM(K) = ZR(JVECTN + (MNMAX(K)-1)*3 + 2)
         GAMMAM = ATAN2(SQRT(ABS(1.0D0-NZM(K)**2)),NZM(K))
         IF (GAMMAM .LT. 0.0D0) THEN
            GAMMAM = GAMMAM + PI
         ENDIF

         IF ((ABS(NYM(K)) .LT. EPSILO) .AND.
     &       (ABS(NXM(K)) .LT. EPSILO)) THEN
           PHIM = 0.0D0
         ELSE
           PHIM = ATAN2(ABS(NYM(K)),NXM(K))
         ENDIF
         IF (PHIM .LT. 0.0D0) THEN
           PHIM = PHIM + PI
         ENDIF

         IF (ABS(GAMMAM) .LT. EPSILO) THEN
            GAMMA = 5.0D0*(PI/180.0D0)
            DPHI2 = 60.0D0*(PI/180.0D0)
            PHI0 = 0.0D0
            N = 0

            CALL VECNUV(1, 6, GAMMA, PHI0, DPHI2, N, 1, DIM,
     &                  ZR(JVECN2), ZR(JVECU2), ZR(JVECV2))

            GAMMA = 0.0D0
            PHI0 = PI

            CALL VECNUV(1, 1, GAMMA, PHI0, DPHI2, N, 1, DIM,
     &                  ZR(JVECN2), ZR(JVECU2), ZR(JVECV2))

            NBVEC = 7
            CALL TAURLO(NBVEC, JVECN2, JVECU2, JVECV2, NBORDR,
     &                  KWORK, SOMPGW, JRWORK, TSPAQ, IPG, JVPG2)
         ELSE
            DGAM2 = 2.0D0*(PI/180.0D0)
            DPHI2 = DGAM2/SIN(GAMMAM)
            N = 0
            DO 460 J=1, 3
               GAMMA = GAMMAM + (J-2)*DGAM2

               CALL VECNUV(1, 3, GAMMA, PHIM, DPHI2, N, 2, DIM,
     &                     ZR(JVECN2), ZR(JVECU2), ZR(JVECV2))

 460        CONTINUE

            NBVEC = 9
            CALL TAURLO(NBVEC, JVECN2, JVECU2, JVECV2, NBORDR,
     &                  KWORK, SOMPGW, JRWORK, TSPAQ, IPG, JVPG2)
         ENDIF

C 4-1/E A ZERO DU VECTEUR DE TRAVAIL CONTENANT LES VALEURS DE
C     TAU POUR UN POINT DE GAUSS ET DU VECTEUR DE TRAVAIL
C     TANT DE POINTER SUR LE VECTEUR NORMAL ASSOCIE.

C 4-2/L DU RAYON CIRCONSCRIT
  
         CALL RAYCIR(JVPG2, JDTAUM, JRESUN, NBORDR, NBVEC, NOMMET)

C 4-3/L DU 2EME MAX DES DELTA_TAU ET DU VECTEUR NORMAL ASSOCIE

         DTAUM(K) = 0.0D0
         MNMAX(K) = 1

         DO 480 I=1, NBVEC
            IF ( ZR(JDTAUM + (I-1)) .GT. DTAUM(K)) THEN
               DTAUM(K) = ZR(JDTAUM + (I-1))
               MNMAX(K) = I
            ENDIF
 480     CONTINUE

C 5/ DE RAFFINEMENT CONCERNANT LA DETERMINATION DU VECTEUR NORMAL
C    EAX DES DELTA_TAU (DETERMINATION DU VECTEUR NORMAL A 1
C    DRES).

         NXM(K) = ZR(JVECN2 + (MNMAX(K)-1)*3)
         NYM(K) = ZR(JVECN2 + (MNMAX(K)-1)*3 + 1)
         NZM(K) = ZR(JVECN2 + (MNMAX(K)-1)*3 + 2)
         GAMMAM = ATAN2(SQRT(ABS(1.0D0-NZM(K)**2)),NZM(K))
         IF (GAMMAM .LT. 0.0D0) THEN
            GAMMAM = GAMMAM + PI
         ENDIF

         IF ((ABS(NYM(K)) .LT. EPSILO) .AND.
     &       (ABS(NXM(K)) .LT. EPSILO)) THEN
           PHIM = 0.0D0
         ELSE
           PHIM = ATAN2(ABS(NYM(K)),NXM(K))
         ENDIF
         IF (PHIM .LT. 0.0D0) THEN
           PHIM = PHIM + PI
         ENDIF

         IF (ABS(GAMMAM) .LT. EPSILO) THEN
            GAMMA = 1.0D0*(PI/180.0D0)
            DPHI2 = 60.0D0*(PI/180.0D0)
            PHI0 = 0.0D0
            N = 0

            CALL VECNUV(1, 6, GAMMA, PHI0, DPHI2, N, 1, DIM,
     &                  ZR(JVECN1), ZR(JVECU1), ZR(JVECV1))

            GAMMA = 0.0D0
            PHI0 = PI

            CALL VECNUV(1, 1, GAMMA, PHI0, DPHI2, N, 1, DIM,
     &                  ZR(JVECN1), ZR(JVECU1), ZR(JVECV1))

            NBVEC = 7
            CALL TAURLO(NBVEC, JVECN1, JVECU1, JVECV1, NBORDR,
     &                  KWORK, SOMPGW, JRWORK, TSPAQ, IPG, JVPG1)
         ELSE
            DGAM2 = 1.0D0*(PI/180.0D0)
            DPHI2 = DGAM2/SIN(GAMMAM)
            N = 0
            DO 510 J=1, 3
               GAMMA = GAMMAM + (J-2)*DGAM2

               CALL VECNUV(1, 3, GAMMA, PHIM, DPHI2, N, 2, DIM,
     &                  ZR(JVECN1), ZR(JVECU1), ZR(JVECV1))

 510        CONTINUE

            NBVEC = 9
            CALL TAURLO(NBVEC, JVECN1, JVECU1, JVECV1, NBORDR,
     &                  KWORK, SOMPGW, JRWORK, TSPAQ, IPG, JVPG1)
         ENDIF

C 5-1/E A ZERO DU VECTEUR DE TRAVAIL CONTENANT LES VALEURS DE
C     TAU POUR UN POINT DE GAUSS ET DU VECTEUR DE TRAVAIL
C     TANT DE POINTER SUR LE VECTEUR NORMAL ASSOCIE.


C 5-2/L DU RAYON CIRCONSCRIT
        
         CALL RAYCIR(JVPG1, JDTAUM, JRESUN, NBORDR, NBVEC, NOMMET)
    
C 5-3/L DU 2EME MAX DES DELTA_TAU ET DU VECTEUR NORMAL ASSOCIE

         DTAUM(K) = 0.0D0
         MNMAX(K) = 1

         DO 530 I=1, NBVEC
            IF ( ZR(JDTAUM + (I-1)) .GT. DTAUM(K)) THEN
               DTAUM(K) = ZR(JDTAUM + (I-1))
               MNMAX(K) = I
            ENDIF
 530     CONTINUE

         NXM(K) = ZR(JVECN1 + (MNMAX(K)-1)*3)
         NYM(K) = ZR(JVECN1 + (MNMAX(K)-1)*3 + 1)
         NZM(K) = ZR(JVECN1 + (MNMAX(K)-1)*3 + 2)
         GAMMAM = ATAN2(SQRT(ABS(1.0D0-NZM(K)**2)),NZM(K))
         IF (GAMMAM .LT. 0.0D0) THEN
            GAMMAM = GAMMAM + PI
         ENDIF

         IF ((ABS(NYM(K)) .LT. EPSILO) .AND.
     &       (ABS(NXM(K)) .LT. EPSILO)) THEN
           PHIM = 0.0D0
         ELSE
           PHIM = ATAN2(ABS(NYM(K)),NXM(K))
         ENDIF
         IF (PHIM .LT. 0.0D0) THEN
           PHIM = PHIM + PI
         ENDIF

C CALCLA CONTRAINTE NORMALE MAXIMALE SUR LE PLAN CRITIQUE,
C DE LRAINTE NORMALE MOYENNE SUR LE PLAN CRITIQUE,
C DE LRMATION NORMALE MAXIMALE SUR LE PLAN CRITIQUE,
C DE LRMATION NORMALE MOYENNE SUR LE PLAN CRITIQUE.
    
         CALL RCVALE(NOMMAT,'ELAS',0,'        ',R8B,1,'E       ',
     &               VALE,ICODRE,0)
         IF (ICODRE .EQ. 1) THEN
            CALL U2MESS('F','PREPOST_11')
         ENDIF
         CALL RCVALE(NOMMAT,'ELAS',0,'        ',R8B,1,'NU      ',
     &               VALNU,ICODRE,0)
         IF (ICODRE .EQ. 1) THEN
            CALL U2MESS('F','PREPOST_12')
         ENDIF
         C1 = (1+VALNU)/VALE
         C2 = VALNU/VALE

         DO 540 I=1, NBORDR
            ADRS = (I-1)*TSPAQ+KWORK*SOMPGW*6+(IPG-1)*6
            SIXX = ZR(JRWORK + ADRS + 0)
            SIYY = ZR(JRWORK + ADRS + 1)
            SIZZ = ZR(JRWORK + ADRS + 2)
            SIXY = ZR(JRWORK + ADRS + 3)
            SIXZ = ZR(JRWORK + ADRS + 4)
            SIYZ = ZR(JRWORK + ADRS + 5)

C CALCLA PRESSION HYDROSTATIQUE MAXIMALE = Max_t(1/3 Tr[SIG])

            IF ( K .LT. 2 ) THEN

C ON C PHYDRM QU'UNE FOIS, PARCE QUE LA PRESSION HYDROSTATIQUE
C EST ANTE PAR RAPPORT AU vect_n.

               PHYDRO = (SIXX + SIYY + SIZZ)/3.0D0

               IF (PHYDRO .GT. PHYDRM) THEN
                  PHYDRM = PHYDRO
               ENDIF
            ENDIF

            EPSXX = C1*SIXX - C2*(SIXX + SIYY + SIZZ)
            EPSYY = C1*SIYY - C2*(SIXX + SIYY + SIZZ)
            EPSZZ = C1*SIZZ - C2*(SIXX + SIYY + SIZZ)
            EPSXY = C1*SIXY
            EPSXZ = C1*SIXZ
            EPSYZ = C1*SIYZ

C CALCvect_F = [SIG].vect_n

            FXM(K) = SIXX*NXM(K) + SIXY*NYM(K) + SIXZ*NZM(K)
            FYM(K) = SIXY*NXM(K) + SIYY*NYM(K) + SIYZ*NZM(K)
            FZM(K) = SIXZ*NXM(K) + SIYZ*NYM(K) + SIZZ*NZM(K)

C CALCNORM = vect_F.vect_n

            NORM(K) = FXM(K)*NXM(K) + FYM(K)*NYM(K) +
     &                FZM(K)*NZM(K)

            IF (ABS(NORM(K)) .GT. NORMAX(K)) THEN
               NORMAX(K) = NORM(K)
            ENDIF

            SNORM(K) = SNORM(K) + NORM(K)

C CALCvect_EPS = [EPS].vect_n

            EPSXM(K) = EPSXX*NXM(K) + EPSXY*NYM(K) + EPSXZ*NZM(K)
            EPSYM(K) = EPSXY*NXM(K) + EPSYY*NYM(K) + EPSYZ*NZM(K)
            EPSZM(K) = EPSXZ*NXM(K) + EPSYZ*NYM(K) + EPSZZ*NZM(K)

C CALCEPSILON NORMALE = vect_EPS.vect_n

            EPNORM(K) = EPSXM(K)*NXM(K) + EPSYM(K)*NYM(K) +
     &                  EPSZM(K)*NZM(K)

            IF (ABS(EPNORM(K)) .GT. EPNMAX(K)) THEN
               EPNMAX(K) = EPNORM(K)
            ENDIF

            SEPNMX(K) = SEPNMX(K) + EPNORM(K)
 540     CONTINUE

         NORMOY(K) = SNORM(K)/NBORDR
         EPNMOY(K) = SEPNMX(K)/NBORDR


 440  CONTINUE

C CONSON D'UN CHAM_ELEM SIMPLE PUIS D'UN CHAM_ELEM CONTENANT
C POURE POINT DE GAUSS DE CHAQUE MAILLE MAX DE DTAU_MAX ET LE
C VECTRMAL ASSOCIE.


      VRESPC(1) = DTAUM(1)
      VRESPC(2) = NXM(1)
      VRESPC(3) = NYM(1)
      VRESPC(4) = NZM(1)
      VRESPC(5) = NORMAX(1)
      VRESPC(6) = NORMOY(1)
      VRESPC(7) = EPNMAX(1)
      VRESPC(8) = EPNMOY(1)
      VRESPC(9) = 0.0D0
      VRESPC(10) = 0.0D0
      VRESPC(11) = 0.0D0
      VRESPC(12) = DTAUM(2)
      VRESPC(13) = NXM(2)
      VRESPC(14) = NYM(2)
      VRESPC(15) = NZM(2)
      VRESPC(16) = NORMAX(2)
      VRESPC(17) = NORMOY(2)
      VRESPC(18) = EPNMAX(2)
      VRESPC(19) = EPNMOY(2)
      VRESPC(20) = 0.0D0
      VRESPC(21) = 0.0D0
      VRESPC(22) = 0.0D0
      VRESPC(23) = 0.0D0
      VRESPC(24) = 0.0D0
      
      CALL JEDETR('&&ACMATA.VECT_NORMA1')
      CALL JEDETR('&&ACMATA.VECT_TANGU1')
      CALL JEDETR('&&ACMATA.VECT_TANGV1')
      CALL JEDETR('&&ACMATA.VECT_NORMA2')
      CALL JEDETR('&&ACMATA.VECT_TANGU2')
      CALL JEDETR('&&ACMATA.VECT_TANGV2')
      CALL JEDETR('&&ACMATA.VECTPG1')
      CALL JEDETR('&&ACMATA.VECTPG2')
      END
