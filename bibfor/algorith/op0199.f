      SUBROUTINE OP0199 ( IERR )
      IMPLICIT NONE
      INTEGER             IERR
C---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/10/2004   AUTEUR REZETTE C.REZETTE 
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
C---------------------------------------------------------------------
C OPERATEUR CALCULANT LA FORCE AJOUTEE : CALC_FORC_AJOU
C
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
      INTEGER       IBID, NBMO, NBMODE, NDBLE, INDICE, IFM, NIV
      INTEGER       TABAD(5), IADESC, IAREFE, I, IADIRG, IMADE
      INTEGER       IPHI1, IPHI2, IPRSTO, IRET, ITXSTO
      INTEGER       ITYSTO, ITZSTO, IVALK, IVALE
      INTEGER       N1, N2, N3, N4, N5, N6, N7, N8, N9
      REAL*8        RBID, TPS(6), MIJ, CIJ, KIJ
      COMPLEX*16    CBID
      LOGICAL       VRAI
      CHARACTER*2   MODEL
      CHARACTER*3   ND
      CHARACTER*5   K5BID
      CHARACTER*8   NOMRES, K8BID, MODMEC, PHIBAR, MOINT, CHAR
      CHARACTER*8   MOFLUI, MA, MATERI, NOMCMP(6), NUMGEN, MODGEN
      CHARACTER*14  NU, NUM
      CHARACTER*16  TYPRES, NOMCOM
      CHARACTER*19  MAX, MAY, MAZ, CHAMNO, NOMNUM, SOLVEU
      CHARACTER*24  BLANC, TIME, NOCHAM, MATE
C
C -----------------------------------------------------------------
       DATA NOMCMP / 'INST    ', 'DELTAT  ', 'THETA   ',
     &               'KHI     ', 'R       ', 'RHO     ' /
       DATA TPS    / 0.0D0, 2*1.0D0, 3*0.0D0 /
       DATA SOLVEU / '&&OP0199.SOLVEUR' /

C-----------------------------------------------------------------

      CALL JEMARQ()
C
      CALL GETRES ( NOMRES, TYPRES, NOMCOM )
C
      CALL INFMAJ
      CALL INFNIV ( IFM, NIV )
C
      NBMO   = 0
      NDBLE  = 0
      VRAI   = .TRUE.
      TIME   = '&TIME'
      NOMNUM = ' '
      MATERI = ' '
      MATE   = ' '
C
C --- RECUPERATION DES ARGUMENTS DE LA COMMANDE
C
      CALL GETVID(' ', 'MODELE_FLUIDE'   ,0,1,1, MOFLUI, N1 )
      CALL GETVID(' ', 'CHARGE'          ,0,1,1, CHAR  , N2 )
      CALL GETVID(' ', 'MODELE_INTERFACE',0,1,1, MOINT , N3 )
      CALL GETVID(' ', 'CHAM_MATER'      ,0,1,1, MATERI, N4 )
      CALL GETVID(' ', 'MODE_MECA'       ,0,1,1, MODMEC, N5 )
      CALL GETVID(' ', 'NUME_DDL_GENE'   ,0,1,1, NUMGEN, N6 )
      CALL GETVID(' ', 'MODELE_GENE'     ,0,1,1, MODGEN, N7 )
      CALL GETVID(' ', 'POTENTIEL'       ,0,1,1, PHIBAR, N8 )
      CALL GETVTX(' ', 'NOEUD_DOUBLE'    ,0,1,1, ND    , N9 )
C
C --- LECTURE DES PARAMETRES  SOLVEUR
C
      CALL CRESOL ( SOLVEU, K5BID )
C
      IF ( N4 .NE. 0 )  CALL RCMFMC ( MATERI , MATE )
C
      IF ( N6 .NE. 0 )  NOMNUM = NUMGEN//'      .NUME'
C
      IF ( N5 .NE. 0 ) THEN
         CALL RSORAC ( MODMEC, 'LONUTI', IBID, RBID, K8BID, CBID,
     +                 RBID, 'ABSOLU', NBMODE, 1, IBID )
         NBMO = NBMODE
         CALL RSEXCH ( MODMEC, 'DEPL', 1, NOCHAM, IRET )
      ENDIF
C
      IF ( N7.NE.0 ) THEN
         IF ( ND .EQ. 'OUI' )  NDBLE = 1
      ENDIF
C
C--------------------------------------------------------------
C --- CALCUL DE LA MATRICE ASSEMBLEE DE RIGIDITE DU FLUIDE
C--------------------------------------------------------------
C
      CALL RIGFLU ( MOFLUI, TIME, NOMCMP, TPS, N2, CHAR, MATE,
     &              SOLVEU, MA, NU )
C
C--------------------------------------------------------------
C CALCUL DES MATR_ELEM AX ET AY DANS L'OPTION FLUX_FLUI_X ET _Y
C---------------SUR LE MODELE INTERFACE(THERMIQUE)-------------
C CALCUL DES MATRICES MODALES BI POUR L OPTION AMOR_AJOU
C--------------------------------------------------------------
C
      CALL MAT152 ( 'MASS_AJOU', MODEL, MOINT, NOCHAM, IVALK, NBMO,
     +               MAX, MAY, MAZ, NUM )
C
      CALL JEEXIN ( '&&MAT152.MADE', IRET ) 
      IF ( IRET.GT.0 ) CALL JEVEUO ( '&&MAT152.MADE', 'E', IMADE )
C
C================================================================
C CALCUL ET STOCKAGE DES POTENTIELS INSTATIONNAIRES PHI1 ET PHI2
C CORRESPONDANT RESPECTIVEMENT AUX EFFETS INERTIELS
C ET AUX EFFETS D'AMORTISSEMENT ET DE RAIDEUR DU FLUIDE
C SUR LA STRUCTURE
C================================================================
C
      CALL PHI199 ( MODEL, MATE, MA, NU,NUM,NBMO,SOLVEU,INDICE,TABAD)
C
C--------------------------------------------------------------
C VERIFICATION D EXISTENCE DE VECTEUR DE CHAMPS AUX NOEUDS CREES
C DS PHI152 ILS SERONT ENSUITE EXPLOITES DS CAL152 ENTRE AUTRES
C VECTEUR DE NOMS DU POTENTIEL INSTATIONNAIRE PHI1 : MASSE AJOU
C ON Y STOCKE LES NOMS DES POTENTIELS INSTATIONNAIRES POUR
C CHAQUE MODE DE STRUCTURE
C
      CALL JEEXIN ( '&&OP0199.PHI1', IRET )
      IF (IRET.GT.0) CALL JEVEUO ( '&&OP0199.PHI1', 'E', IPHI1 )
      CALL JEEXIN ( '&&OP0199.PHI2', IRET )
      IF (IRET.GT.0) CALL JEVEUO ( '&&OP0199.PHI2', 'E', IPHI2 )
C
C=====================================================================
C---------------------------------------------------------------------
C              CALCUL SUR MODELE GENERALISE
C---------------------------------------------------------------------
C=====================================================================
C
      IF ( N7.GT.0 ) THEN
         CALL CALMDG ( MODEL, MODGEN, NOMNUM, NUM, NU, MA, MATE, MOINT,
     &                 MOFLUI, NDBLE, ITXSTO, ITYSTO, ITZSTO, IPRSTO,
     &                 NBMO, IADIRG )
      ENDIF
C
C=============================================================
C--------REMPLISSAGE DU  .VALE : CALCUL DU VECTEUR AJOUTE
C=============================================================
C
C---------------------------------------------------------------
      IF ((N7.GT.0).OR.(INDICE.EQ.1)) THEN

C CALCUL DU VECTEUR AJOUTE - PRODUITS SCALAIRES SUR MODELE
C GENERALISE - CAS DE LA SOUS-STRUCTURATION DYNAMIQUE
C OU BIEN CAS DE MODES RESTITUES SUR MAILLAGE SQUELETTE

         IF (INDICE.EQ.1) THEN
             ITXSTO = TABAD(1)
             ITYSTO = TABAD(2)
             ITZSTO = TABAD(3)
             IPRSTO = TABAD(4)
             IADIRG = TABAD(5)
             NBMO=NBMODE
         ENDIF
      ELSE
C
C --- CREATION DE L OBJET VECT_GENE RESULTAT
C
         CALL WKVECT(NOMRES//'           .VALE','G V R',NBMO,IVALE)
         CALL WKVECT(NOMRES//'           .REFE','G V K24',2,IAREFE)
         CALL WKVECT(NOMRES//'           .DESC','G V I',3,IADESC)
C
C --- REMPLISSAGE DU .REFE ET .VALE
C
         ZK24(IAREFE)   = MODMEC
         ZK24(IAREFE+1) = NOMNUM   
         ZI(IADESC)   = 1
         ZI(IADESC+1) = NBMO
C
         DO 10 I = 1 , NBMO
C
            BLANC = ' '
            CALL CAL152 ( 'MASS_AJOU', MAX, MAY, MAZ, MODEL, BLANC,
     &                    IPHI1, IPHI2, IMADE, MODMEC, CHAMNO, NUM,
     &                    VRAI, I, 1, MIJ, CIJ, KIJ )
C
            ZR(IVALE+I-1) = MIJ
C
10      CONTINUE
      ENDIF
C
C
      CALL JEDETC('G','&&RIGFLU',1)
      CALL JEDETC('G','&&CALMAA',1)
C
      CALL JEDEMA()
      END
