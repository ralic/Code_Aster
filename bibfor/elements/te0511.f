      SUBROUTINE TE0511 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C.......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 28/01/2003   AUTEUR DURAND C.DURAND 
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
C     BUT: -- CALCUL
C               - DU TAUX DE TRIAXIALITE DES CONTRAINTES (TRIAX)
C               - DE LA CONTRAINTES EQUIVALENTE D'ENDOMAGEMENT (SIGMA*)
C
C        TAUX DE TRIAXIALITE : TRIAX
C        ---------------------------
C           TRIAX    = SIGMA_H / SIGMA_EQ
C
C    OU     S        = SIGMA - 1/3 TRACE(SIGMA) I
C           SIGMA_EQ = ( 3/2 S : S ) ** 1/2
C           SIGMA_H  = 1/3 TRACE(SIGMA)
C           SIGMA    = TENSEUR DES CONTRAINTES
C           S        = DEVIATEUR DE SIGMA
C           I        = TENSEUR IDENTITE
C
C        CONTRAINTES EQUIVALENTE D'ENDOMAGEMENT : SIGMA*
C        -----------------------------------------------
C           SI_ENDO  = SIGMA_EQ (2/3(1+NU) + 3(1-2 NU) TRIAX**2 )**1/2
C
C     OU    NU       = COEFFICIENT DE POISSON
C.......................................................................
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER  ZI
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
      INTEGER  NBSIGM, NBNOEU, NBNOSO, NBPGAU, NBDIM, NBPAR
      INTEGER  MXCMEL, NBPGMX, NBRES, I, K, NNO, NNOS, NPG
      INTEGER  NBSIG, IGAU, INDIC, INO, NDIM, IADZI
      INTEGER  IMATE, ITEMPE, IDTRIA, ICONPG, ICONNO, IAZK24
      PARAMETER          ( MXCMEL = 162 )
      PARAMETER          ( NBPGMX =  27 )
      PARAMETER          ( NBRES  =   2 )
      REAL*8             SIGMA(MXCMEL), SIGEQ(NBPGMX), TRSIG(NBPGMX)
      REAL*8             SENDO(NBPGMX), SENDON(NBPGMX), R8PREM
      REAL*8             TRIAX(NBPGMX), TRIAXN(NBPGMX), VALRES(NBRES)
      REAL*8             ZERO, UN, DEUX, TROIS, UNTIER, DETIER, TRDEMI
      REAL*8             VALPAR, XNU, COE1, COE2, R8VIDE
      CHARACTER*2        CODRES(NBRES)
      CHARACTER*8        MODELI , NOMPAR , NOMRES(NBRES), NOMAIL
      CHARACTER*16       PHENOM
C.......................................................................
      DATA NOMRES / 'E', 'NU' /
C.......................................................................
C
      ZERO   = 0.0D0
      UN     = 1.0D0
      DEUX   = 2.0D0
      TROIS  = 3.0D0
      UNTIER = 1.0D0 / 3.0D0
      DETIER = 2.0D0 / 3.0D0
      TRDEMI = 3.0D0 / 2.0D0
      MODELI(1:2) = NOMTE(3:4)
C
      DO 10 I = 1, MXCMEL
         SIGMA(I)   = ZERO
  10  CONTINUE
C
      DO 20 I = 1, NBPGMX
         SIGEQ(I)  = ZERO
         TRSIG(I)  = ZERO
         SENDO(I)  = ZERO
         SENDON(I) = ZERO
         TRIAX(I)  = ZERO
         TRIAXN(I) = ZERO
  20  CONTINUE
C
C
C --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
C     -----------------------------------------------
C
      CALL JEVECH ( 'PMATERC', 'L', IMATE  )
      CALL TECACH(.FALSE.,.FALSE.,'PTEMPER',1,ITEMPE)
C
C
C --- RECUPERATION DU TYPE DU MATERIAU DANS PHENOM
C     --------------------------------------------
C
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRES)
C
      IF ( ITEMPE .EQ. 0 ) THEN
         NBPAR  = 0
         NOMPAR = ' '
         VALPAR = ZERO
      ELSE
         NBPAR  = 1
         NOMPAR = 'TEMP'
         VALPAR = ZR(ITEMPE)
      ENDIF
C
      CALL RCVALA ( ZI(IMATE), PHENOM, NBPAR, NOMPAR,VALPAR,NBRES,
     +                                      NOMRES,VALRES,CODRES,'FM')
C
      XNU  = VALRES(2)
      COE1 = DETIER * (UN + XNU)
      COE2 = TROIS * (UN - DEUX * XNU)
C
C
C --- RECUPERATION DES CARACTERISTIQUES DU TYPE D'ELEMENT :
C     ---------------------------------------------------
C ----     NOMBRE DE CONNECTIVITES :
      NNO   = NBNOEU(' ')
C ----     NOMBRE DE NOEUDS SOMMETS :
      NNOS  = NBNOSO(NOMTE)
C ----     NOMBRE DE POINTS D'INTEGRATION :
      NPG   = NBPGAU(NOMTE)
C ----     DIMENSION DE L'ELEMENT :
      NDIM  = NBDIM(NOMTE)
C ----     NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT :
      NBSIG = NBSIGM(MODELI)
C
C --- RECUPERATION  ET DES L'ADRESSES DU TAUX DE TRIAXIALITE DES
C --- CONTRAINTES (TRIAX) ET DE LA CONTRAINTES EQUIVALENTE
C --- D'ENDOMAGEMENT (SENDO)
C     -----------------------------------------------------------
      CALL JEVECH ( 'PTRIAXS' , 'E' , IDTRIA )
C
C
C ---- RECHERCHE DU TYPE DU CHAMP D'ENTREE, DEFINI AUX NOEUDS OU
C ---- AUX POINTS DE GAUSS
C      -----------------------------------------------------------
      CALL TECACH(.TRUE.,.FALSE.,'PCONTPG',1,ICONPG)
      CALL TECACH(.TRUE.,.FALSE.,'PCONTNO',1,ICONNO)
C
C
C     ------------------------------------------------------------------
C              CHAMP DE CONTRAINTES DEFINI AUX POINTS DE GAUSS
C     ------------------------------------------------------------------
C
      IF ( ICONPG .NE. 0 ) THEN
C
C ---    AFFECTATION DU VECTEUR DE TRAVAIL SIGMA REPRESENTANT
C ---    LE TENSEUR DE CONTRAINTES
C        -------------------------
         K = 0
         DO 40 IGAU = 1, NPG
            DO 30 I = 1, NBSIG
               K = K+1
               SIGMA(I+(IGAU-1)*NBSIG) = ZR(ICONPG+K-1)
  30        CONTINUE
  40     CONTINUE
C
C ---    CALCUL DU DEVIATEUR DES CONTRAINTES
C        -----------------------------------
C
         DO 50 IGAU = 1, NPG
C
           INDIC = (IGAU-1)*NBSIG
           TRSIG(IGAU) = UNTIER       * ( SIGMA(INDIC+1)
     +                 + SIGMA(INDIC+2) + SIGMA(INDIC+3) )
C
           SIGMA(INDIC+1) =  SIGMA(INDIC+1) - TRSIG(IGAU)
           SIGMA(INDIC+2) =  SIGMA(INDIC+2) - TRSIG(IGAU)
           SIGMA(INDIC+3) =  SIGMA(INDIC+3) - TRSIG(IGAU)
C
  50      CONTINUE
C
C ---    CALCUL DE SIGEQ
C        ---------------
         DO 60 IGAU = 1, NPG
C
           INDIC = (IGAU-1)*NBSIG
           SIGEQ(IGAU) = SIGMA(INDIC+1) * SIGMA(INDIC+1)
     +                    + SIGMA(INDIC+2) * SIGMA(INDIC+2)
     +                    + SIGMA(INDIC+3) * SIGMA(INDIC+3)
     +                    + SIGMA(INDIC+4) * SIGMA(INDIC+4) * DEUX
           IF(NDIM.EQ.3) SIGEQ(IGAU) = SIGEQ(IGAU)
     +                    + SIGMA(INDIC+5) * SIGMA(INDIC+5) * DEUX
     +                    + SIGMA(INDIC+6) * SIGMA(INDIC+6) * DEUX
           SIGEQ(IGAU) = (SIGEQ(IGAU) * TRDEMI) ** 0.5D0
C
  60      CONTINUE
C
C ---    CALCUL DU TAUX DE TRIAXIALITE DES CONTRAINTES (TRIAX)
C        -----------------------------------------------------
C
         DO 70 IGAU = 1, NPG
            IF ( ABS(SIGEQ(IGAU)) .LE. R8PREM() ) THEN
               CALL TECAEL ( IADZI, IAZK24 )
               NOMAIL = ZK24(IAZK24-1+3)(1:8)
               CALL UTDEBM('A','TE0511',
     +                         'LA CONTRAINTE EQUIVALENTE EST NULLE')
               CALL UTIMPK ( 'S', ' POUR LA MAILLE ', 1, NOMAIL )
               CALL UTFINM ()
               DO 72 INO = 1, NNO
                  TRIAXN(INO) = R8VIDE()
  72           CONTINUE
               GOTO 7777
            ENDIF
            TRIAX(IGAU) = TRSIG(IGAU) / SIGEQ(IGAU)
  70     CONTINUE
C
C ---    CALCUL DE LA CONTRAINTES EQUIVALENTE D'ENDOMAGEMENT (SENDO)
C        -------------------------------------------------------------
C
         DO 80 IGAU = 1, NPG
           SENDO(IGAU) =
     +       (COE1 * SIGEQ(IGAU)**2 + COE2 * TRSIG(IGAU)**2) ** 0.5D0
  80      CONTINUE
C
C ---    CALCUL DES VALEURS AUX NOEUDS :
C        -------------------------------
         CALL PPGANO(NNOS, NPG, 1, TRIAX, TRIAXN)
         CALL PPGANO(NNOS, NPG, 1, SENDO, SENDON)
C
C
C     ------------------------------------------------------------------
C                 CHAMP DE CONTRAINTES DEFINI AUX NOEUDS
C     ------------------------------------------------------------------
C
      ELSEIF ( ICONNO .NE. 0 ) THEN
C
C ---    AFFECTATION DU VECTEUR DE TRAVAIL SIGMA REPRESENTANT
C ---    LE TENSEUR DE CONTRAINTES
C         -------------------------
         K = 0
         DO 100 INO = 1, NNO
            DO 90 I = 1, NBSIG
               K = K+1
               SIGMA(I+(INO-1)*NBSIG) = ZR(ICONNO+K-1)
  90        CONTINUE
  100    CONTINUE
C
C ---    CALCUL DU DEVIATEUR DES CONTRAINTES
C        -----------------------------------
C
         DO 110 INO = 1, NNO
C
            INDIC = (INO-1)*NBSIG
            TRSIG(INO) = UNTIER       * ( SIGMA(INDIC+1)
     +                  + SIGMA(INDIC+2) + SIGMA(INDIC+3) )
C
            SIGMA(INDIC+1) =  SIGMA(INDIC+1) - TRSIG(INO)
            SIGMA(INDIC+2) =  SIGMA(INDIC+2) - TRSIG(INO)
            SIGMA(INDIC+3) =  SIGMA(INDIC+3) - TRSIG(INO)
C
  110      CONTINUE
C
C ---    CALCUL DE SIGEQ
C        ---------------
C
         DO 120 INO = 1, NNO
C
            INDIC = (INO-1)*NBSIG
            SIGEQ(INO) = SIGMA(INDIC+1) * SIGMA(INDIC+1)
     +                     + SIGMA(INDIC+2) * SIGMA(INDIC+2)
     +                     + SIGMA(INDIC+3) * SIGMA(INDIC+3)
     +                     + SIGMA(INDIC+4) * SIGMA(INDIC+4) * DEUX
            IF(NDIM.EQ.3) SIGEQ(INO) = SIGEQ(INO)
     +                     + SIGMA(INDIC+5) * SIGMA(INDIC+5) * DEUX
     +                     + SIGMA(INDIC+6) * SIGMA(INDIC+6) * DEUX
            SIGEQ(INO) = (SIGEQ(INO) * TRDEMI) ** 0.5D0
C
  120     CONTINUE
C
C ---    CALCUL DU TAUX DE TRIAXIALITE DES CONTRAINTES (TRIAX)
C        -----------------------------------------------------
C
         DO 130 INO = 1, NNO
            IF ( ABS(SIGEQ(INO)) .LE. R8PREM() ) THEN
               CALL TECAEL ( IADZI, IAZK24 )
               NOMAIL = ZK24(IAZK24-1+3)(1:8)
               CALL UTDEBM('A','TE0511',
     +                         'LA CONTRAINTE EQUIVALENTE EST NULLE')
               CALL UTIMPK ( 'S', ' POUR LA MAILLE ', 1, NOMAIL )
               CALL UTFINM ()
               DO 132 INDIC = 1, NNO
                  TRIAXN(INDIC) = R8VIDE()
  132          CONTINUE
               GOTO 7777
            ENDIF
            TRIAXN(INO) = TRSIG(INO) / SIGEQ(INO)
  130    CONTINUE
C
C ---    CALCUL DE LA CONTRAINTES EQUIVALENTE D'ENDOMAGEMENT (SENDO)
C        -------------------------------------------------------------
C
         DO 140 INO = 1, NNO
            SENDON(INO) =
     +       (COE1 * SIGEQ(INO)**2 + COE2 * TRSIG(INO)**2) ** 0.5D0
  140    CONTINUE
C
C     ------------------------------------------------------------------
C ---    CHAMP DE CONTRAINTES MAL DEFINI
      ELSE
         CALL UTMESS('F','TE0511','LE TYPE DU CHAMP DE CONTRAINTES'//
     +                  ' EST INCOMPATIBLE AVEC L''OPTION : '//OPTION)
      ENDIF
C
C
C --- RECUPERATION  ET AFFECTATION DU DU TAUX DE TRIAXIALITE DES
C --- CONTRAINTES (TRIAX) ET DE LA CONTRAINTES EQUIVALENTE
C --- D'ENDOMAGEMENT (SENDO)
C
 7777 CONTINUE
C
      DO 150 INO = 1, NNO
           ZR(IDTRIA+2*(INO-1)  ) = TRIAXN(INO)
           ZR(IDTRIA+2*(INO-1)+1) = SENDON(INO)
  150 CONTINUE
C
      END
