      SUBROUTINE PROJQU (NUTYP,NDIM,NBNO,PROJ,COORDA,COORDB,COORDC,
     &                   COORDD,COORDP,NORM,COORDM,COEF,
     &                   OLDJEU,JEU,TANG,PRONOR,TANGDF,VLISSA,VERIP)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/04/2004   AUTEUR MABBAS M.ABBAS 
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
      IMPLICIT NONE
C
      INTEGER      NUTYP,NBNO,PROJ,PRONOR,TANGDF,NDIM
      REAL*8       COORDA(3),COORDB(3),COORDC(3),COORDD(3),COORDP(3)
      REAL*8       NORM(3),COORDM(3),COEF(NBNO),OLDJEU,JEU,VLISSA(24)
      REAL*8       VERIP

C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : PROJEC
C ----------------------------------------------------------------------
C
C "PROJECTION" D'UN NOEUD ESCLAVE P SUR UN QUADRANGLE MAITRE.
C EN FAIT ON CHERCHE LE TRIANGLE (CONTENANT 3 SOMMETS DU QUADRANGLE)
C DONT IL EST LE PLUS PROCHE ET ON ECRIT LA RELATION DE NON PENETRATION
C ENTRE LE NOEUD ESCLAVE ET LES 3 SOMMETS DE CE TRIANGLE.
C LES NOEUDS MILIEUX NE SONT JAMAIS PRIS EN COMPTE.
C
C IN  NUTYP  : TYPE DE MAILLE (QUAD4 , QUAD8 , QUAD9 )
C IN  NDIM   : DIMENSION DU PB
C IN  NBNO   : NOMBRE DE NOEUDS (4, 8 OU 9)
C IN  PROJ   : PROJECTION LINEAIRE (1) OU QUADRATIQUE (2) SUR LA MAILLE
C              OU PAS DE NOUVELLE PROJECTION (0)
C IN  COORDA : COORDONNEES DU SOMMET A DU QUADRANGLE
C IN  COORDB : COORDONNEES DU SOMMET B DU QUADRANGLE
C IN  COORDC : COORDONNEES DU SOMMET C DU QUADRANGLE
C IN  COORDD : COORDONNEES DU SOMMET D DU QUADRANGLE
C IN  COORDP : COORDONNEES DU NOEUD ESCLAVE P
C OUT NORM   : DIRECTION PM (NORMEE POUR ETRE UNITAIRE)
C OUT COORDM : COORDONNEES DE LA "PROJECTION" M
C OUT COEF   : VALEURS EN M DES FONCTIONS DE FORME ASSOCIEES
C              AUX NOEUDS MAITRES
C OUT OLDJEU : JEU AVANT CORRECTION DES PROJECTIONS TOMBANT HORS DE
C              LA MAILLE MAITRE
C OUT JEU    : JEU DANS LA DIRECTION (NORM) DE LA NORMALE ENTRANTE
C              A LA MAILLE MAITRE (PM.NORM)
C
C ----------------------------------------------------------------------
C
      INTEGER K, KMIN, NTQUA4, NTQUA8, NTQUA9
      REAL*8  OLDJ(4),NEWJ(4),COORM(12),NORMAL(12),COEFFI(12),JEUMIN
      REAL*8  R8GAEM,TANG(6),TANGEN(24),KSI1,KSI2
      REAL*8  DEMI, UN, DEUX, QUATRE
      REAL*8  AM(3), AB(3), BC(3), AD(3), DC(3)
      REAL*8  LAB, LBC, LAD, LDC
      INTEGER  DIAG(2)
      REAL*8       TOLE
      INTEGER      ARETE(3)
      INTEGER      NOEUD(3)

      CHARACTER*32   JEXNOM
      
      PARAMETER  ( DEMI   =  0.5D0  )
      PARAMETER  ( UN     =  1.0D0  )
      PARAMETER  ( DEUX   =  2.0D0  )
      PARAMETER  ( QUATRE =  4.0D0  )



      DIAG(1)=0
      DIAG(2)=0

C --- TOLERANCE POUR DETECTER LE POINT SUR L'ARETE
      TOLE = 1D-3



C
C ----------------------------------------------------------------------
C
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QUAD4') , NTQUA4 )
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QUAD8') , NTQUA8 )
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QUAD9') , NTQUA9 )
      DO 10 K = 1,NBNO
        COEF(K) = 0.D0
 10   CONTINUE
C
C ======================================================================
C                             PROJECTION LINEAIRE
C ======================================================================
C
C --- ON CHERCHE LE TRIANGLE (CONTENANT 3 SOMMETS DU QUADRANGLE)
C --- DONT LE NOEUD ESCLAVE EST LE PLUS PROCHE.
C
        NORMAL(1) = NORM(1)
        NORMAL(2) = NORM(2)
        NORMAL(3) = NORM(3)
        TANGEN(1) = TANG(1)
        TANGEN(2) = TANG(2)
        TANGEN(3) = TANG(3)
        CALL PROJTR (6,NDIM,3,PROJ,COORDA,COORDB,COORDC,COORDP,
     &    NORMAL(1),COORM(1),COEFFI(1),OLDJ(1),NEWJ(1),TANGEN(1),
     &    PRONOR,TANGDF,VLISSA,VERIP,TOLE,ARETE,NOEUD)


        IF (ARETE(3).EQ.1) THEN 
          DIAG(1) = 1
        ENDIF

        NORMAL(4) = NORM(1)
        NORMAL(5) = NORM(2)
        NORMAL(6) = NORM(3)
        TANGEN(7) = TANG(1)
        TANGEN(8) = TANG(2)
        TANGEN(9) = TANG(3)


        CALL PROJTR (6,NDIM,3,PROJ,COORDA,COORDC,COORDD,COORDP,
     &    NORMAL(4),COORM(4),COEFFI(4),OLDJ(2),NEWJ(2),TANGEN(7),
     &    PRONOR,TANGDF,VLISSA,VERIP,TOLE,ARETE,NOEUD)
        IF (ARETE(1).EQ.1) THEN 
          DIAG(1) = 1
        ENDIF

        NORMAL(7)  = NORM(1)
        NORMAL(8)  = NORM(2)
        NORMAL(9)  = NORM(3)
        TANGEN(13) = TANG(1)
        TANGEN(14) = TANG(2)
        TANGEN(15) = TANG(3)

        CALL PROJTR (6,NDIM,3,PROJ,COORDA,COORDB,COORDD,COORDP,
     &    NORMAL(7),COORM(7),COEFFI(7),OLDJ(3),NEWJ(3),TANGEN(13),
     &    PRONOR,TANGDF,VLISSA,VERIP,TOLE,ARETE,NOEUD)
        IF (ARETE(2).EQ.1) THEN 
          DIAG(2) = 1
        ENDIF

        NORMAL(10) = NORM(1)
        NORMAL(11) = NORM(2)
        NORMAL(12) = NORM(3)
        TANGEN(19) = TANG(1)
        TANGEN(20) = TANG(2)
        TANGEN(21) = TANG(3)

        CALL PROJTR (6,NDIM,3,PROJ,COORDB,COORDC,COORDD,COORDP,
     &    NORMAL(10),COORM(10),COEFFI(10),OLDJ(4),NEWJ(4),TANGEN(19),
     &    PRONOR,TANGDF,VLISSA,VERIP,TOLE,ARETE,NOEUD)
        IF (ARETE(3).EQ.1) THEN 
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


C   GESTION DES CONFLITS D'ARETES

           IF ((KMIN.EQ.1).AND.DIAG(1).EQ.1) THEN
             IF (OLDJ(3).LT.OLDJ(4)) THEN
               KMIN = 3
             ELSE
               KMIN = 4
             ENDIF
           ENDIF
           IF ((KMIN.EQ.2).AND.DIAG(1).EQ.1) THEN
             IF (OLDJ(3).LT.OLDJ(4)) THEN
               KMIN = 3
             ELSE
               KMIN = 4
             ENDIF
           ENDIF
           IF ((KMIN.EQ.3).AND.DIAG(2).EQ.1) THEN
             IF (OLDJ(1).LT.OLDJ(2)) THEN
               KMIN = 1
             ELSE
               KMIN = 2
             ENDIF
           ENDIF
           IF ((KMIN.EQ.4).AND.DIAG(2).EQ.1) THEN
             IF (OLDJ(1).LT.OLDJ(2)) THEN
               KMIN = 1
             ELSE
               KMIN = 2
             ENDIF
           ENDIF

           IF ((DIAG(1).EQ.1).AND.(DIAG(2).EQ.1)) THEN
             KMIN = 1
           ENDIF
           JEUMIN = OLDJ(KMIN)
        END IF
 20     CONTINUE
        JEU    = NEWJ(KMIN)
        OLDJEU = OLDJ(KMIN)
        DO 30 K = 1,3
          NORM(K)   = NORMAL(3*(KMIN-1)+K)
          COORDM(K) = COORM(3*(KMIN-1)+K)
          TANG(K)   = TANGEN(6*(KMIN-1)+K)
          TANG(K+3) = TANGEN(6*(KMIN-1)+K+3)
 30     CONTINUE
 

        IF ((NUTYP.EQ.NTQUA4).OR.(PROJ.EQ.1)) THEN
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
        ELSE IF ((NUTYP.EQ.NTQUA8).AND.(PROJ.EQ.2)) THEN
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
           IF (KSI1.LT.0.0D0) KSI1 = 0.0D0
           IF (KSI1.GT.1.0D0) KSI1 = 1.0D0
           IF (KSI2.LT.0.0D0) KSI2 = 0.0D0
           IF (KSI2.GT.1.0D0) KSI2 = 1.0D0

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
C

      END
