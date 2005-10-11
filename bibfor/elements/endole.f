      SUBROUTINE ENDOLE(OPTION,MODELE,LIGREL,MATE,COMPOR,
     &                        CHTEM1,CHSIG1,CHVAR1,
     &                        CHTEM2,CHSIG2,CHVAR2,
     &                        CHEND2,CHELE1,CHELE2)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/10/2005   AUTEUR LEBOUVIE F.LEBOUVIER 
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
      IMPLICIT REAL*8 (A-H,O-Z)

C     BUT: -- CALCUL
C               - DU TAUX DE TRIAXIALITE DES CONTRAINTES (TRIAX)
C               - DE LA CONTRAINTES EQUIVALENTE D'ENDOMMAGEMENT (SIGMA*)
C               - DE L'ENDOMMAGEMENT DE LEMAITRE-SERMAGE (DOMLE)

C        TAUX DE TRIAXIALITE : TRIAX
C        ---------------------------
C           TRIAX    = SIGMA_H / SIGMA_EQ
C
C    OU     SIG      = SIGMA - 1/3 TRACE(SIGMA) I
C           SIGMA_EQ = ( 3/2 SIG : SIG ) ** 1/2
C           SIGMA_H  = 1/3 TRACE(SIGMA)
C           SIGMA    = TENSEUR DES CONTRAINTES
C           S        = DEVIATEUR DE SIGMA
C           I        = TENSEUR IDENTITE
C
C        CONTRAINTES EQUIVALENTE D'ENDOMMAGEMENT : SIGMA*
C        -----------------------------------------------
C           SI_ENDO  = SIGMA_EQ (2/3(1+NU) + 3(1-2 NU) TRIAX**2 )**1/2
C
C     OU    NU       = COEFFICIENT DE POISSON
C
C        LOI D'ENDOMMAGEMENT DE LEMAITRE-SERMAGE: DOMLE
C        ----------------------------------------------
C           D° = PUISSANCE([Y/VAR_S];EXPS)*p°
C           
C        LOI D'ENDOMMAGEMENT DE LEMAITRE-SERMAGE INTEGREE: DOMLE
C        -------------------------------------------------------
C           DOMLE(+) = 1 - PUISSANCE([PUISSANCE(1-DOMLE(-);2*EXPS+1)
C                      - (2*EXPS+1/2)
C                      *(PUISSANCE(K(+);EXPS)+PUISSANCE(K(-);EXPS))
C                      *(p(+) - p(-))];1/2*EXPS+1)
C     OU 
C           K(+) = PUISSANCE(SIGMA*(+);2) / (E(+)*VAR_S(+))
C           
C  ARGUMENT  E/S  TYPE  ROLE
C  --------  ---  ----  ----
C   OPTION   IN     K*  OPTION DE CALCUL : 'ENDO_[ELNO]_ELGA'
C   MODELE   IN     K*  NOM DU MODELE SUR-LEQUEL ON FAIT LE CALCUL
C   LIGREL   IN     K*  LIGREL DU MODELE
C   MATE     IN     K*  NOM DU CHAMP CHAMP MATERIAU
C   CHTEMi  IN     K*  NOM DU CHAMP DE TEMPERATURE A L'ORDRE i
C   CHSIGi   IN     K*  NOM DU CHAMP DES CONTRAINTES AUX
C                           POINTS D'INTEGRATION A L'ORDRE i
C   CHEND2  IN     K*  NOM DU CHAMP D'ENDOMMAGEMENT AUX
C                           POINTS D'INTEGRATION
C   CHELEi  OUT    K*  NOM DU CHAMP DES INDICATEURS LOCAUX A L'ORDRE i

C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
      CHARACTER*(*) OPTION,MODELE,LIGREL
      CHARACTER*(*) MATE,CHEND2,COMPOR
      CHARACTER*(*) CHELE1,CHTEM1,CHSIG1,CHVAR1
      CHARACTER*(*) CHELE2,CHTEM2,CHSIG2,CHVAR2
C -----  VARIABLES LOCALES
      CHARACTER*8 LPAIN(9),LPAOUT(1)
      CHARACTER*16 OPT2
      CHARACTER*24 LCHIN(9),LCHOUT(1)
      
      INTEGER NBIN,NBOUT
      
C.========================= DEBUT DU CODE EXECUTABLE ==================
C LISTE DES GRANDEURS DISPONIBLES DANS "gener_me3d_3.cata"
C ENDO_ELGA      512 IN__  
C     ECONTPG PCONTGM  ECONTPG PCONTGP  NTEMPER PTEMPMR  NTEMPER PTEMPER
C     ZVARIPG PVARIMR  ZVARIPG PVARIPR  CMATERC PMATERC  ETRIAPG PTRIAGM
C                    OUT__ ETRIAPG PTRIAGP  
C ENDO_ELNO_ELGA 512 IN__  ETRIAPG PTRIAGP
C                    OUT__ ETRIANO PTRIANO  
C
C ---- CHAMP DE CONTRAINTES D'ENTREE DEFINI AUX POINTS DE GAUSS
C      --------------------------------------------------------
      IF (OPTION.EQ.'ENDO_ELGA') THEN
         LPAIN(1) = 'PCONTGM'
         LCHIN(1) = CHSIG1
         LPAIN(2) = 'PCONTGP'
         LCHIN(2) = CHSIG2
         LPAIN(3) = 'PTEMPMR'
         LCHIN(3) = CHTEM1
         LPAIN(4) = 'PTEMPER'
         LCHIN(4) = CHTEM2
         LPAIN(5) = 'PVARIMR'
         LCHIN(5) = CHVAR1
         LPAIN(6) = 'PVARIPR'
         LCHIN(6) = CHVAR2
         LPAIN(7) = 'PMATERC'
         LCHIN(7) = MATE
         LPAIN(8) = 'PTRIAGM'
         LCHIN(8) = CHELE1
         LPAIN(9) = 'PCOMPOR'
         LCHIN(9) = COMPOR
         NBIN = 9
         
         LPAOUT(1) = 'PTRIAGP'
         LCHOUT(1) = CHELE2
         NBOUT = 1
C
C ---- CHAMP DE CONTRAINTES D'ENTREE DEFINI AUX NOEUDS CALCULE
C ---- A PARTIR DU CHAMP AUX PT DE GAUSS
C ---- OPTION = 'ENDO_ELNO_ELGA'
C      -------------------------
      ELSE
         LPAIN(1) = 'PTRIAGP'
         LCHIN(1) = CHEND2
         NBIN = 1

         LPAOUT(1) = 'PTRIANO'
         LCHOUT(1) = CHELE2
         NBOUT = 1
      ENDIF

      OPT2 = OPTION
      CALL CALCUL('S',OPT2,LIGREL,NBIN,LCHIN,LPAIN,
     &            NBOUT,LCHOUT,LPAOUT,'G')

C.============================ FIN DE LA ROUTINE ======================
      END
