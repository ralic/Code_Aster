      SUBROUTINE PROJQ1(MATYP,NBNO,NDIM,
     &                  COORDA,COORDB,COORDC,COORDD,COORDP,
     &                  PROJ,MOYEN,LISSA,TANGDF,VLISSA,DIAGNO,TOLEIN,
     &                  TOLEOU,
     &                  NORM,TANG,
     &                  COORDM,COEF,OLDJEU,JEU,
     &                  DIAG,ARETE,NOEUD,DEBORD)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 25/10/2004   AUTEUR MABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_21
C TOLE CRP_20
      IMPLICIT     NONE
      CHARACTER*4  MATYP
      INTEGER      NDIM
      INTEGER      NBNO
      REAL*8       COORDA(3)
      REAL*8       COORDB(3)
      REAL*8       COORDC(3)
      REAL*8       COORDD(3)
      REAL*8       COORDP(3)
      INTEGER      PROJ
      INTEGER      TANGDF
      INTEGER      MOYEN
      INTEGER      LISSA
      REAL*8       VLISSA(9)
      INTEGER      DIAGNO
      REAL*8       TOLEIN
      REAL*8       TOLEOU
      REAL*8       NORM(3)
      REAL*8       TANG(6)
      REAL*8       COORDM(3)
      REAL*8       COEF(NBNO)
      REAL*8       OLDJEU
      REAL*8       JEU
      INTEGER      DIAG(2)
      INTEGER      ARETE(4)
      INTEGER      NOEUD(4)
      REAL*8       DEBORD 
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : PROJQU
C ----------------------------------------------------------------------
C
C "PROJECTION" D'UN NOEUD ESCLAVE P SUR UN QUADRANGLE MAITRE.
C CAS 1: ON CHERCHE LE TRIANGLE (CONTENANT 3 SOMMETS DU QUADRANGLE)
C   DONT IL EST LE PLUS PROCHE ET ON ECRIT LA RELATION DE NON 
C   PENETRATION
C   ENTRE LE NOEUD ESCLAVE ET LES 3 SOMMETS DE CE TRIANGLE.
C   LES NOEUDS MILIEUX NE SONT JAMAIS PRIS EN COMPTE.
C
C IN  MATYP  : TYPE DE MAILLE (QUAD4 , QUAD8 , QUAD9 )
C IN  NDIM   : DIMENSION DU PB
C IN  NBNO   : NOMBRE DE NOEUDS (4, 8 OU 9)
C IN  COORDA : COORDONNEES DU SOMMET A DU QUADRANGLE
C IN  COORDB : COORDONNEES DU SOMMET B DU QUADRANGLE
C IN  COORDC : COORDONNEES DU SOMMET C DU QUADRANGLE
C IN  COORDD : COORDONNEES DU SOMMET D DU QUADRANGLE
C IN  COORDP : COORDONNEES DU NOEUD ESCLAVE P
C IN  PROJ   : PROJECTION LINEAIRE (1) OU QUADRATIQUE (2) SUR LA MAILLE
C              OU PAS DE NOUVELLE PROJECTION (0)
C IN  TANGDF : INDICATEUR DE PRESENCE D'UN VECT_Y DEFINI PAR 
C              L'UTILISATEUR
C               0 PAS DE VECT_Y
C               1 UN VECT_Y EST DEFINI
C IN  MOYEN  : NORMALES D'APPARIEMENT
C               0 MAIT 
C               1 MAIT_ESCL 
C IN  LISSA  : LISSAGE DES NORMALES 
C               0 PAS DE LISSAGE 
C               1 LISSAGE 
C IN  VLISSA : NORMALES LISSEES
C IN  DIAGNO : NIVEAU DE DIAGNOSTIC
C               0 PAS DE VERIFICATIONS 
C               1 VERIFICATIONS FINES
C IN  TOLEIN : TOLERANCE <IN> POUR LA PROJECTION GEOMETRIQUE
C               ( SUR ARETE OU NOEUD )
C I/O NORM   : NORMALE ENTRANTE A LA MAILLE MAITRE
C I/O TANG   : VECTEURS TANGENTS 
C OUT COORDM : COORDONNEES DE LA "PROJECTION" M
C OUT COEF   : VALEURS EN M DES FONCTIONS DE FORME ASSOCIEES
C              AUX NOEUDS MAITRES
C OUT OLDJEU : JEU AVANT CORRECTION DES PROJECTIONS TOMBANT HORS DE
C              LA MAILLE MAITRE
C OUT JEU    : JEU DANS LA DIRECTION (NORM) DE LA NORMALE ENTRANTE
C              A LA MAILLE MAITRE (PM.NORM)
C OUT DIAG   : DETECTION DE PROJECTION SUR PSEUDO_DIAGONALES
C                 (1: SUR L'ARETE, 0 NON)
C              DIAG(1) : ARETE AC
C              DIAG(2) : ARETE BD
C OUT ARETE  : DETECTION DE PROJECTION SUR ARETE
C                 (1: SUR L'ARETE, 0: NON)
C              ARETE(1) : SEGMENT AB
C              ARETE(2) : SEGMENT BC
C              ARETE(3) : SEGMENT CD
C              ARETE(3) : SEGMENT DA
C OUT NOEUD  : DETECTION DE PROJECTION SUR NOEUD 
C                 (1: SUR LE NOEUD, 0: NON)
C              NOEUD(1) : NOEUD A
C              NOEUD(2) : NOEUD B
C              NOEUD(3) : NOEUD C
C              NOEUD(4) : NOEUD D
C OUT DEBORD : PROJECTION HORS DE LA MAILLE
C              >0 : PROJECTION HORS DE LA MAILLE
C              <0 : PROJECTION SUR LA MAILLE
C
C ----------------------------------------------------------------------
C
      REAL*8       DEMI, UN, DEUX, QUATRE
      PARAMETER  ( DEMI   =  0.5D0  )
      PARAMETER  ( UN     =  1.0D0  )
      PARAMETER  ( DEUX   =  2.0D0  )
      PARAMETER  ( QUATRE =  4.0D0  )
      INTEGER      K,KMIN
      REAL*8       OLDJ(4),NEWJ(4),COORM(12),NORMAL(12),COEFFI(12)
      REAL*8       R8GAEM,TANGEN(24),KSI1,KSI2,JEUMIN
      REAL*8       AM(3), AB(3), BC(3), AD(3), DC(3)
      REAL*8       LAB, LBC, LAD, LDC
      REAL*8       DEBEN(4)
      REAL*8       OUTSID(2)
      INTEGER      ARETT1(3)
      INTEGER      NOEUT1(3)
      INTEGER      ARETT2(3)
      INTEGER      NOEUT2(3)
      INTEGER      ARETT3(3)
      INTEGER      NOEUT3(3)
      INTEGER      ARETT4(3)
      INTEGER      NOEUT4(3)
      INTEGER      OUTTRI(4)           
C
C ----------------------------------------------------------------------
C

      DO 10 K = 1,NBNO
        COEF(K) = 0.D0
 10   CONTINUE

 
C
C ----------------------------------------------------------------------
C --- CONTROLE DE LA PROJECTION
C --- LE NOEUD ESCLAVE SE PROJETE HORS DE LA MAILLE MAITRE 
C ---    SI    DEBORD.GT.0.DO
C ---    SINON DEBORD.LT.0.D0
C ----------------------------------------------------------------------
C
      DEBORD    = -1.D0
      OUTSID(1) = -1.D0  
      OUTSID(2) = -1.D0   
      OUTTRI(1) = 0
      OUTTRI(2) = 0
      OUTTRI(3) = 0
      OUTTRI(4) = 0
      KMIN      = 0
C
C ----------------------------------------------------------------------
C --- CONTROLE DE LA PROJECTION
C --- LE NOEUD ESCLAVE SE PROJETE SUR ARETES/NOEUDS/DIAGONALES 
C ----------------------------------------------------------------------
C
      DIAG(1)   = 0
      DIAG(2)   = 0
      DO 1 K = 1,3
        ARETT1(K) = 0
        ARETT2(K) = 0
        ARETT3(K) = 0
        ARETT4(K) = 0
        NOEUT1(K) = 0
        NOEUT2(K) = 0
        NOEUT3(K) = 0
        NOEUT4(K) = 0
1     CONTINUE


C
C ======================================================================
C                             PROJECTION LINEAIRE
C ======================================================================
C
C --- ON CHERCHE LE TRIANGLE (CONTENANT 3 SOMMETS DU QUADRANGLE)
C --- DONT LE NOEUD ESCLAVE EST LE PLUS PROCHE.
C

C
C --- PROJECTION SUR LES 4 TRIANGLES
C


        NORMAL(1) = NORM(1)
        NORMAL(2) = NORM(2)
        NORMAL(3) = NORM(3)
        TANGEN(1) = TANG(1)
        TANGEN(2) = TANG(2)
        TANGEN(3) = TANG(3)
        CALL PROJTR ('TRI3',3,NDIM,
     &    COORDA,COORDB,COORDC,COORDP,
     &    PROJ,MOYEN,LISSA,TANGDF,VLISSA,1,TOLEIN,
     &    NORMAL(1),TANGEN(1),
     &    COORM(1),COEFFI(1),OLDJ(1),NEWJ(1),
     &    ARETT1,NOEUT1,DEBEN(1))

        NORMAL(4) = NORM(1)
        NORMAL(5) = NORM(2)
        NORMAL(6) = NORM(3)
        TANGEN(7) = TANG(1)
        TANGEN(8) = TANG(2)
        TANGEN(9) = TANG(3)
        CALL PROJTR ('TRI3',3,NDIM,
     &    COORDA,COORDC,COORDD,COORDP,
     &    PROJ,MOYEN,LISSA,TANGDF,VLISSA,1,TOLEIN,
     &    NORMAL(4),TANGEN(7),
     &    COORM(4),COEFFI(4),OLDJ(2),NEWJ(2),
     &    ARETT2,NOEUT2,DEBEN(2))

        NORMAL(7)  = NORM(1)
        NORMAL(8)  = NORM(2)
        NORMAL(9)  = NORM(3)
        TANGEN(13) = TANG(1)
        TANGEN(14) = TANG(2)
        TANGEN(15) = TANG(3)
        CALL PROJTR ('TRI3',3,NDIM,
     &    COORDA,COORDB,COORDD,COORDP,
     &    PROJ,MOYEN,LISSA,TANGDF,VLISSA,1,TOLEIN,
     &    NORMAL(7),TANGEN(13),
     &    COORM(7),COEFFI(7),OLDJ(3),NEWJ(3),
     &    ARETT3,NOEUT3,DEBEN(3))

        NORMAL(10) = NORM(1)
        NORMAL(11) = NORM(2)
        NORMAL(12) = NORM(3)
        TANGEN(19) = TANG(1)
        TANGEN(20) = TANG(2)
        TANGEN(21) = TANG(3)
        CALL PROJTR ('TRI3',3,NDIM,
     &    COORDB,COORDC,COORDD,COORDP,
     &    PROJ,MOYEN,LISSA,TANGDF,VLISSA,1,TOLEIN,
     &    NORMAL(10),TANGEN(19),
     &    COORM(10),COEFFI(10),OLDJ(4),NEWJ(4),
     &    ARETT4,NOEUT4,DEBEN(4))

C
C --- GESTION DES DEBORDEMENTS DE MAILLE SUR LES TRIANGLES
C
      DO 4 K = 1,4
        IF (DEBEN(K).GT.0.D0) THEN
           IF (TOLEOU.GT.0.D0) THEN               
              IF (DEBEN(K).GT.TOLEOU) THEN
                OUTTRI(K) = 1
              ENDIF
           ENDIF  
        ENDIF
4     CONTINUE
C
C --- PROJECTION SUR LES PSEUDOS-DIAGONALES ?
C
       IF (ARETT1(3).EQ.1) THEN 
          DIAG(1) = 1
        ENDIF
        IF (ARETT2(1).EQ.1) THEN 
          DIAG(1) = 1
        ENDIF
        IF (ARETT3(2).EQ.1) THEN 
          DIAG(2) = 1
        ENDIF
        IF (ARETT4(3).EQ.1) THEN 
          DIAG(2) = 1
        ENDIF

C
C --- ON CHOISIT LE TRIANGLE REALISANT LA PLUS PETITE DISTANCE
C --- AVANT CORRECTION. LA RELATION DE NON PENETRATION EST
C --- ECRITE ENTRE LE NOEUD ESCLAVE ET LES 3 SOMMETS DE CE TRIANGLE.
C
        JEUMIN = R8GAEM()
        DO 20 K = 1,4
        

          IF ((OLDJ(K).LE.JEUMIN).AND.
     &        (ABS(OLDJ(K)-JEUMIN).GT.1D-15)) THEN

           KMIN = K
C
C   GESTION DES CONFLITS D'ARETES
C
           IF ((KMIN.EQ.1).AND.DIAG(1).EQ.1) THEN
             IF (OLDJ(3).LT.OLDJ(4)) THEN
               IF (OLDJ(3).LE.OLDJ(1)) THEN
                 KMIN = 3
               ENDIF
             ELSE
               IF (OLDJ(4).LE.OLDJ(1)) THEN
                 KMIN = 4
               ENDIF
             ENDIF
           ENDIF
           IF ((KMIN.EQ.2).AND.DIAG(1).EQ.1) THEN
             IF (OLDJ(3).LT.OLDJ(4)) THEN
               IF (OLDJ(3).LE.OLDJ(2)) THEN
                 KMIN = 3
               ENDIF
             ELSE
               IF (OLDJ(4).LE.OLDJ(2)) THEN
                 KMIN = 4
               ENDIF
             ENDIF
           ENDIF
           IF ((KMIN.EQ.3).AND.DIAG(2).EQ.1) THEN
             IF (OLDJ(1).LT.OLDJ(2)) THEN
               IF (OLDJ(1).LE.OLDJ(3)) THEN
                 KMIN = 1
               ENDIF
             ELSE
               IF (OLDJ(2).LE.OLDJ(3)) THEN
                 KMIN = 2
               ENDIF
             ENDIF
           ENDIF
           IF ((KMIN.EQ.4).AND.DIAG(2).EQ.1) THEN
             IF (OLDJ(1).LT.OLDJ(2)) THEN
               IF (OLDJ(1).LE.OLDJ(4)) THEN
                 KMIN = 1
               ENDIF
             ELSE
               IF (OLDJ(2).LE.OLDJ(4)) THEN
                 KMIN = 2
               ENDIF
             ENDIF
           ENDIF

           IF ((DIAG(1).EQ.1).AND.(DIAG(2).EQ.1)) THEN
             KMIN = 1
           ENDIF

           JEUMIN = OLDJ(KMIN)
          END IF
 20     CONTINUE
        IF (KMIN.EQ.0) THEN
          CALL UTMESS('F','PROJQ1',
     &         'LE NOEUD ESCLAVE N A PAS PU S APPARIER')
        ENDIF
C
C --- SI ON PROJETTE "EN DEHORS" DU TRIANGLE
C --- ET SI ON NE PROJETTE PAS SUR LA DIAGONALE -> TEST DE DEBORDEMENT
C

        IF (OUTTRI(KMIN).EQ.1) THEN
          IF ((KMIN.EQ.1).OR.(KMIN.EQ.2)) THEN
            IF (DIAG(1).EQ.0) THEN
              OUTSID(1) = DEBEN(KMIN)
              OUTSID(2) = DEBEN(KMIN) 
            ELSE 
              OUTSID(1) = -1.D0  
              OUTSID(2) = -1.D0 
            ENDIF
          ELSE
            IF (DIAG(2).EQ.0) THEN
              OUTSID(1) = DEBEN(KMIN)
              OUTSID(2) = DEBEN(KMIN) 
            ELSE 
              OUTSID(1) = -1.D0  
              OUTSID(2) = -1.D0  
            ENDIF
          ENDIF         
        ELSE
          OUTSID(1) = -1.D0  
          OUTSID(2) = -1.D0 
        ENDIF

        JEU    = NEWJ(KMIN)
        OLDJEU = OLDJ(KMIN)


C --- RECUPERATION NORMALES ET TANGENTES
        DO 30 K = 1,3
          NORM(K)   = NORMAL(3*(KMIN-1)+K)
          COORDM(K) = COORM(3*(KMIN-1)+K)
          TANG(K)   = TANGEN(6*(KMIN-1)+K)
          TANG(K+3) = TANGEN(6*(KMIN-1)+K+3)
 30     CONTINUE

 
        IF ((MATYP.EQ.'QUA4').OR.(PROJ.EQ.1)) THEN
           IF (KMIN.EQ.1) THEN
              COEF(1) = COEFFI(1)
              COEF(2) = COEFFI(2)
              COEF(3) = COEFFI(3)
              COEF(4) = 0.D0
           ELSE IF (KMIN.EQ.2) THEN
              COEF(1) = COEFFI(4)
              COEF(2) = 0.D0
              COEF(3) = COEFFI(5)
              COEF(4) = COEFFI(6)
           ELSE IF (KMIN.EQ.3) THEN
              COEF(1) = COEFFI(7)
              COEF(2) = COEFFI(8)
              COEF(3) = 0.D0
              COEF(4) = COEFFI(9)
           ELSE
              COEF(1) = 0.D0
              COEF(2) = COEFFI(10)
              COEF(3) = COEFFI(11)
              COEF(4) = COEFFI(12)
           ENDIF
        ELSE IF ((MATYP.EQ.'QUA8').AND.(PROJ.EQ.2)) THEN
           IF (KMIN.EQ.1) THEN
              DO 100 K = 1, NDIM
                 AM(K) = COORDM(K) - COORDA(K)
                 AB(K) = COORDB(K) - COORDA(K)
                 BC(K) = COORDC(K) - COORDB(K)
 100          CONTINUE
              CALL PSCAL (NDIM,AM,AB,KSI1)
              CALL PSCAL (NDIM,AB,AB,LAB)
              KSI1 = KSI1 / LAB
              CALL PSCAL (NDIM,AM,BC,KSI2)
              CALL PSCAL (NDIM,BC,BC,LBC)
              KSI2 = KSI2 / LBC
           ELSE IF (KMIN.EQ.2) THEN
              DO 200 K = 1, NDIM
                 AM(K) = COORDM(K) - COORDA(K)
                 DC(K) = COORDC(K) - COORDD(K)
                 AD(K) = COORDD(K) - COORDA(K)
 200          CONTINUE
              CALL PSCAL (NDIM,AM,DC,KSI1)
              CALL PSCAL (NDIM,DC,DC,LDC)
              KSI1 = KSI1 / LDC
              CALL PSCAL (NDIM,AM,AD,KSI2)
              CALL PSCAL (NDIM,AD,AD,LAD)
              KSI2 = KSI2 / LAD
           ELSE IF (KMIN.EQ.3) THEN
              DO 300 K = 1, NDIM
                 AM(K) = COORDM(K) - COORDA(K)
                 AB(K) = COORDB(K) - COORDA(K)
                 AD(K) = COORDD(K) - COORDA(K)
 300          CONTINUE
              CALL PSCAL (NDIM,AM,AB,KSI1)
              CALL PSCAL (NDIM,AB,AB,LAB)
              KSI1 = KSI1 / LAB
              CALL PSCAL (NDIM,AM,AD,KSI2)
              CALL PSCAL (NDIM,AD,AD,LAD)
              KSI2 = KSI2 / LAD
           ELSE
              DO 400 K = 1, NDIM
                 AM(K) = COORDM(K) - COORDA(K)
                 DC(K) = COORDC(K) - COORDD(K)
                 BC(K) = COORDC(K) - COORDB(K)
 400          CONTINUE
              CALL PSCAL (NDIM,AM,DC,KSI1)
              CALL PSCAL (NDIM,DC,DC,LDC)
              KSI1 = KSI1 / LDC
              CALL PSCAL (NDIM,AM,BC,KSI2)
              CALL PSCAL (NDIM,BC,BC,LBC)
              KSI2 = KSI2 / LBC
           ENDIF

           IF (KSI1.LT.0.0D0) THEN 
             OUTSID(1) = ABS(KSI1)
             KSI1 = 0.0D0
           ENDIF
           IF (KSI1.GT.1.0D0) THEN 
             OUTSID(1) = 1-KSI1
             KSI1 = 1.0D0
           ENDIF
           IF (KSI2.LT.0.0D0) THEN 
             OUTSID(2) = ABS(KSI2) 
             KSI2 = 0.0D0
           ENDIF
           IF (KSI2.GT.1.0D0) THEN 
             OUTSID(2) = 1-KSI2 
             KSI2 = 1.0D0
           ENDIF

           COEF(1) = - DEMI*(UN-KSI1)*(UN-KSI2)*(DEUX-KSI1-KSI2)
           COEF(2) = - DEMI*KSI1*(UN-KSI2)*(UN+KSI1-KSI2)
           COEF(3) = - DEMI*KSI1*KSI2*(KSI1+KSI2)
           COEF(4) = - DEMI*(UN-KSI1)*KSI2*(UN-KSI1+KSI2)
           COEF(5) = - QUATRE*KSI1*(UN-KSI1)*(UN-KSI2)
           COEF(6) = - QUATRE*KSI1*KSI2*(UN-KSI2)
           COEF(7) = - QUATRE*KSI1*KSI2*(UN-KSI1)
           COEF(8) = - QUATRE*KSI2*(UN-KSI1)*(UN-KSI2)
        ENDIF

C
C --- PROJECTION HORS DE LA MAILLE ?
C --- SI OUI: DEBORD EST LA VALEUR DE DEBORDEMENT
C
      IF ((OUTSID(1).GT.0.D0).OR.(OUTSID(2).GT.0.D0)) THEN
           DEBORD = MAX(OUTSID(1),OUTSID(2))
      ENDIF



C --- VERIFICATIONS DES PROJECTIONS SUR ARETES ET NOEUDS
      IF (DIAGNO.NE.0) THEN
         CALL PRDIQ1(KMIN,
     &               ARETT1,NOEUT1,ARETT2,NOEUT2,
     &               ARETT3,NOEUT3,ARETT4,NOEUT4,
     &               ARETE,NOEUD)
      ENDIF

 999  CONTINUE


      END
