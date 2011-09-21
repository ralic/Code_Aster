      SUBROUTINE MAT152(OPTION,MODEL,MOINT,NOCHAM,IVALK,NBMO,
     +                  MAX,MAY,MAZ,NUM)
      IMPLICIT NONE
C---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C---------------------------------------------------------------------
C AUTEUR : G.ROUSSEAU
C
C ROUTINE PERMETTANT LE CALCUL DE MATRICE ASSEMBLEES INTERVENANT
C DANS LE CALCUL DES COEFFICIENTS AJOUTES :
C MAX, MAY, MAZ : MATRICES INDEPENDANTES DU MODE POUR
C CALCULER LA MASSE AJOUTEE
C BI : MATRICES MODALES INTERVENANT DANS LE CALCUL DE L AMORTISSEMENT
C      ET DE LA RAIDEUR
C
C IN : K* : MODEL : DIMENSION DU MODELE 2D,3D OU AXI
C IN : K* : MOINT : MODELE D INTERFACE
C IN : K* : NOCHAM : NOM DU CHAMP AUX NOEUDS DE DEPL_R
C IN : I  : IVALK : ADRESSE DU TABLEAU DES NOMS DES CHAMNOS
C IN : I  : NBMO  : NOMBRE DE MODES OU DE CHAMNOS UTILISATEURS
C OUT : K19 : MAX,MAY,MAZ : NOMS DES MATRICES POUR LE CALCUL DE MASSE
C OUT : K14 : NUM : NUMEROTATION DES DDLS THERMIQUES D 'INTERFACE
C---------------------------------------------------------------------
C--------- DEBUT DES COMMUNS JEVEUX ----------------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16           ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     --- FIN DES COMMUNS JEVEUX ------------------------------------
      LOGICAL       EXIGEO
      INTEGER       NBMO,IMODE,IMADE,IRET
      INTEGER       IVALK
      INTEGER       N5,N6,N7
      CHARACTER*1   DIR
      CHARACTER*2   MODEL
      CHARACTER*3   INCR
      CHARACTER*8   MODMEC
      CHARACTER*8   MOINT
      CHARACTER*8   LPAIN(2),LPAOUT(1)
      CHARACTER*9   OPTION
      CHARACTER*14  NUM
      CHARACTER*19  MAX,MAY,MAZ,CHAMNO
      CHARACTER*24  CHGEOM,LCHIN(2)
      CHARACTER*24  NOMCHA,NOCHAM
      CHARACTER*24  LIGRMO,MADE
      INTEGER       GETEXM
      INTEGER      IARG
C -----------------------------------------------------------------
C--------------------------------------------------------------
C CALCUL DES MATR_ELEM AX ET AY DANS L'OPTION FLUX_FLUI_X ET _Y
C---------------SUR LE MODELE INTERFACE(THERMIQUE)-------------

      CALL JEMARQ()
      N7=0
      IF (GETEXM(' ','CHAM_NO') .EQ. 1) THEN
         CALL GETVID(' ','CHAM_NO',0,IARG,0,CHAMNO,N6)
         N7 =-N6
      ENDIF
      CALL GETVID(' ','MODE_MECA',0,IARG,1,MODMEC,N5)
C------ RECUPERATION D'ARGUMENTS COMMUNS AUX CALCULS DES DEUX ---
C-------------------------MATR_ELEM -----------------------------

       LIGRMO = MOINT(1:8)//'.MODELE'
       CALL MEGEOM(MOINT(1:8),NOCHAM,EXIGEO,CHGEOM)
       LPAIN(1) = 'PGEOMER'
       LCHIN(1) = CHGEOM
       LPAIN(2) = 'PACCELR'
       LPAOUT(1) = 'PMATTTR'
       MAZ=' '
C----------------------------------------------------------------
C-----------CALCUL DE LA MATRICE AZ DES N(I)*N(J)*NZ-------------
C----------------------------------------------------------------
       IF(MODEL.EQ.'3D') THEN
         DIR='Z'
         CALL CALMAA(MOINT,' ',DIR,LIGRMO,LCHIN(1),LPAIN(1)
     &             ,LPAOUT(1),NUM,MAZ)


       ENDIF
C----------------------------------------------------------------
C-----------CALCUL DE LA MATRICE AX DES N(I)*N(J)*NX-------------
C----------------------------------------------------------------

       DIR='X'
       CALL CALMAA(MOINT,' ',DIR,LIGRMO,LCHIN(1),LPAIN(1)
     &             ,LPAOUT(1),NUM,MAX)


C----------------------------------------------------------------
C-----------CALCUL DE LA MATRICE AY DES N(I)*N(J)*NY-------------
C----------------------------------------------------------------

       DIR='Y'
       CALL CALMAA(MOINT,' ',DIR,LIGRMO,LCHIN(1),LPAIN(1),LPAOUT(1)
     &            ,NUM,MAY)

C----------------------------------------------------------------
C----------CALCUL DE LA MATRICE MODALE DES DN(I)*DN(J)*
C-----------------------MODE*NORMALE-----------------------------
C----------------------------------------------------------------
        IF (OPTION.EQ.'AMOR_AJOU'.OR.OPTION.EQ.'RIGI_AJOU')THEN

           CALL WKVECT('&&MAT152.MADE','V V K24',NBMO,IMADE)

           DO 6000 IMODE = 1,NBMO
               INCR='BID'
               IF (N7.GT.0) THEN
                  LCHIN(2)=ZK8(IVALK+IMODE-1)
               ELSE
                  CALL RSEXCH(MODMEC,'DEPL',IMODE,NOMCHA,IRET)
                  LCHIN(2) =NOMCHA
               ENDIF
               CALL CODENT(IMODE,'D0',INCR(1:3))
               CALL CA2MAM(MOINT,INCR,LIGRMO,LCHIN,LPAIN,
     &                  LPAOUT,NUM,MADE)
               ZK24(IMADE+IMODE-1)= MADE
6000       CONTINUE

        ENDIF
      CALL JEDEMA()
       END
