      SUBROUTINE  CAURTG(NOMTE,NCMP,SIGMAU,SIGRTG)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/12/2009   AUTEUR PROIX J-M.PROIX 
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
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C      CAURTG  -- PASSAGE DES CONTRAINTES DE CAUCHY SIGMAU
C                 CALCULEES DANS LE REPERE UTILISATEUR
C                 VERS LE REPERE UTILISATEUR TOURNE DE
C                 LA ROTATION FAISANT PASSER DE L'ETAT
C                 INITIAL A L'ETAT DEFORME DANS LE CAS GROT_GDEP .
C                 SIGRTG DESIGNE LES CONTRAINTES DE CAUCHY DANS
C                 CE DERNIER REPERE .
C
C   ARGUMENT        E/S  TYPE         ROLE
C    NOMTE          IN     K16      NOM DU TYPE D'ELEMENT
C    NCMP           IN     I        NOMBRE DE COMPOSANTES DU TENSEUR
C                                   DES CONTRAINTES
C    SIGMAU(NCMP,1) IN     R        VECTEUR DES CONTRAINTES
C                                   DE CAUCHY DANS LE REPERE UTILISATEUR
C    SIGRTG(NCMP,1) VAR    R        VECTEUR DES CONTRAINTES DE CAUCHY
C                                   TOURNE DU REPERE UTILISATEUR VERS
C                                   LE REPERE TRANSFORME DU REPERE
C                                   UTILISATEUR PAR LA GRANDE ROTATION
C                                   CALCULEE POUR COMP_ELAS - GROT_GDEP
C
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
           CHARACTER*16  NOMTE
           REAL*8        SIGMAU(NCMP,1), SIGRTG(NCMP,1)
           INTEGER       NCMP
C -----  VARIABLES LOCALES
           REAL*8        VECTHE(9,3), VECTA(9,2,3)   
           REAL*8        VECTPT(9,2,3), VECTN(9,3), VECNPH(9,3)
           REAL*8        BLAM(9,3,3), XAB(3,3), SIGMAD(3,3), SIGMAT(3,3)
           REAL*8        DROT(3,3), TETAG(3)
C
           LOGICAL       LGREEN  
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     ---------------
      LGREEN = .FALSE.
C
C --- RECUPERATION DE LA CARTE DE COMPORTEMENT :
C     ----------------------------------------
      CALL JEVECH('PCOMPOR','L',ICOMPO)
C
        IF (ZK16(ICOMPO+2).EQ.'GROT_GDEP') THEN
          LGREEN = .TRUE.
        ENDIF
C
C --- RECUPERATION DU CHAMP DE DEPLACEMENT DANS LE CAS GROT_GDEP :
C     ---------------------------------------------------------
      IF (LGREEN) THEN
        CALL TECACH('OON','PDEPLAR',1,IDEPL,IRET)
      ELSE
        GOTO 9999
      ENDIF
C
C --- RECUPERATION DES COORDONNEES DES NOEUDS DANS LA GEOMETRIE 
C --- INITIALE :
C     --------
      CALL JEVECH('PGEOMER','L',IGEOM)
C
C --- RECUPERATION DES OBJETS INITIALISES :
C     -----------------------------------
      CALL JEVETE('&INEL.'//NOMTE(1:8)//'.DESI',' ',LZI)
C
C --- NOMBRE DE NOEUDS (NB1 : SERENDIP, NB2 : LAGRANGE) :
C     -------------------------------------------------
      NB1 = ZI(LZI+1-1)
      NB2 = ZI(LZI+2-1)
C
      CALL JEVETE('&INEL.'//NOMTE(1:8)//'.DESR',' ',LZR)
C
C --- AFFECTATION DES VECTEURS DE TRANSLATION ET DE ROTATION :
C     ------------------------------------------------------
      DO 10 IN = 1, NB1
        DO 20 II = 1, 3
          VECTHE(IN,II) = ZR(IDEPL+6*(IN-1)+II+3-1)
  20    CONTINUE
  10  CONTINUE
C
      DO 30 II = 1, 3
        VECTHE(NB2,II) = ZR(IDEPL+6*NB1+II-1)
  30  CONTINUE
C
C --- DETERMINATION DES REPERES LOCAUX AUX NOEUDS DANS LA 
C --- CONFIGURATION INITIALE 
C --- VECTA DESIGNE LES VECTEURS COVARIANTS DANS LE PLAN MOYEN A
C ---       CHAQUE NOEUD
C --- VECTN DESIGNE LES VECTEURS NORMAUX AU PLAN MOYEN
C --- VECTPT DESIGNE LES REPERES LOCAUX ORTHORNORMES EN CHAQUE
C --- NOEUD DANS LA CONFIGURATION INITIALE :
C     ------------------------------------
      CALL VECTAN(NB1,NB2,ZR(IGEOM),ZR(LZR),VECTA,VECTN,VECTPT)
C
C ---   MISE DU VECTEUR DES CONTRAINTES DANS LE REPERE UTILISATEUR
C ---   SOUS LA FORME D'UN TENSEUR 3X3 :
C       ------------------------------
      DO 40 I = 1, NB2
C
        TETAG(1) = VECTHE(I,1)
        TETAG(2) = VECTHE(I,2)
        TETAG(3) = VECTHE(I,3)
        CALL MAROTA(TETAG,DROT)
C
        SIGMAT(1,1) = SIGMAU(1,I)
        SIGMAT(2,2) = SIGMAU(2,I)
        SIGMAT(3,3) = SIGMAU(3,I)
        SIGMAT(1,2) = SIGMAU(4,I)
        SIGMAT(2,1) = SIGMAT(1,2)
        IF (NCMP.EQ.6) THEN
          SIGMAT(1,3) = SIGMAU(5,I)
          SIGMAT(2,3) = SIGMAU(6,I)
          SIGMAT(3,1) = SIGMAT(1,3)
          SIGMAT(3,2) = SIGMAT(2,3)
        ENDIF
C
C ---   ROTATION DU TENSEUR DES CONTRAINTES DE CAUCHY DE LA
C ---   ROTATION FAISANT PASSER DE L'ETAT INITAL A L'ETAT DEFORME :
C       ---------------------------------------------------------
        CALL UTBTAB('ZERO',3,3,SIGMAT,DROT,XAB,SIGMAD)
C
C ---   AFFECTATION DU VECTEUR EN SORTIE DES CONTRAINTES
C ---   DE CAUCHY DANS LE REPERE UTILISATEUR TOURNE :
C       -------------------------------------------
        SIGRTG(1,I) = SIGMAD(1,1)
        SIGRTG(2,I) = SIGMAD(2,2)
        SIGRTG(3,I) = SIGMAD(3,3)
        SIGRTG(4,I) = SIGMAD(1,2)
        IF (NCMP.EQ.6) THEN
          SIGRTG(5,I) = SIGMAD(1,3)
          SIGRTG(6,I) = SIGMAD(2,3)
        ENDIF
C
  40  CONTINUE
C
 9999 CONTINUE
C.============================ FIN DE LA ROUTINE ======================
      END
