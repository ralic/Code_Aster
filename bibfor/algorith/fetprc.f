      SUBROUTINE FETPRC(NBSD,NBI,VD1,VD2,VDO,MATAS,VDDL,PRECO,INFOFE,
     &                  IREX,IFIV)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/01/2005   AUTEUR BOITEAU O.BOITEAU 
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
C-----------------------------------------------------------------------
C    - FONCTION REALISEE:  PRECONDITIONNEMENT AU SENS FETI
C                          PRECO='SANS'  : VO = V1
C                          PRECO='LUMPE' : VO = ML-1 * V1
C
C      IN   NBSD: IN   : NOMBRE DE SOUS-DOMAINES
C      IN    NBI: IN   : NOMBRE DE NOEUDS D'INTERFACE
C      IN    VD1: VR8  : VECTEUR V DE TAILLE NBI (INPUT)
C      IN    VD2: VR8  : VECTEUR AUXILIAIRE DE TAILLE NBI 
C      OUT   VDO: VR8  : VECTEUR OUTPUT DE TAILLE NBI
C      IN  MATAS: CH19 : NOM DE LA MATR_ASSE GLOBALE
C      IN   VDDL: VIN  : VECTEUR DES NBRES DE DDLS DES SOUS-DOMAINES
C      IN  PRECO: K24 : TYPE DE PRECONDITIONNEMENT
C     IN IREX/IFIV: IN : ADRESSE DU VECTEUR AUXILAIRE EVITANT DES APPELS
C                        JEVEUX.
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       28/01/04 (OB): CREATION.
C----------------------------------------------------------------------
C RESPONSABLE BOITEAU O.BOITEAU
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INTEGER      NBSD,NBI,VDDL(NBSD),IREX,IFIV
      REAL*8       VD1(NBI),VDO(NBI),VD2(NBI)
      CHARACTER*19 MATAS
      CHARACTER*24 PRECO,INFOFE

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
      
C DECLARATION VARIABLES LOCALES
      INTEGER      IDD,IFETM,NBDDL,IDD1,JXSOL,J,OPTION,LMAT,JXSOL2,K,
     &             IFM
      CHARACTER*32 JEXNUM
      
C ROUTINE AVEC MOINS DE MONITORING, APPELS JEVEUX.. CAR APPELLEE TRES
C SOUVENT
      IFM=ZI(IFIV)
      IF (PRECO(1:4).EQ.'SANS') THEN
C PAS DE PRECONDITIONNEMENT: ZR(IR1)=ZR(IRG) (VD0=VD1)
        CALL DCOPY(NBI,VD1,1,VDO,1)
C MONITORING
        IF (INFOFE(1:1).EQ.'T')
     &    WRITE(IFM,*)'<FETI/FETPRC> SANS PRECONDITIONNEMENT'
                   
      ELSE IF (PRECO(1:5).EQ.'LUMPE') THEN      
C PRECONDITIONNEMENT LUMPE: ZR(IR1)=ML-1*ZR(IRG) (VD0=ML-1*VD1)
C MONITORING
        IF (INFOFE(1:1).EQ.'T')
     &    WRITE(IFM,*)'<FETI/FETPRC> PRECONDITIONNEMENT LUMPE'

C INIT. VECTEUR SOLUTION ET AUX
        DO 10 J=1,NBI
          VDO(J)=0.D0
   10   CONTINUE         
C --------------------------------------------------------------------
C ----  BOUCLE SUR LES SOUS-DOMAINES
C -------------------------------------------------------------------- 
        DO 40 IDD=1,NBSD
          IDD1=IDD-1
C DESCRIPTEUR DE LA MATRICE DU SOUS-DOMAINE     
          K=IFIV+2+IDD1*4     
          LMAT=ZI(K)
C NBRE DE DDL DU SOUS-DOMAINE IDD       
          NBDDL=VDDL(IDD)
C VECTEURS AUXILIAIRES DE TAILLE NDDL(SOUS_DOMAINE_IDD)
          JXSOL=ZI(K+2)
          JXSOL2=ZI(K+3)     
                
C EXTRACTION DU VECTEUR V AU SOUS-DOMAINE IDD: (RIDD)T * V
          CALL FETREX(2,IDD,NBI,VD1,NBDDL,ZR(JXSOL),IREX)

C PRODUIT:  (KI) * (RIDD)T * V
          CALL MRMULT('ZERO',LMAT,ZR(JXSOL),'R',ZR(JXSOL2),1)

C RESTRICTION DU SOUS-DOMAINE IDD SUR L'INTERFACE: (RIDD) * ...
          CALL FETREX(1,IDD,NBDDL,ZR(JXSOL2),NBI,VD2,IREX)
C CUMUL DANS LE VECTEUR VDO=SOMME(I=1,NBSD)(RI * ((KI)+ * RIT * V))
          CALL DAXPY(NBI,1.D0,VD2,1,VDO,1)

   40   CONTINUE             
      ELSE
        CALL UTMESS('F','FETPRC','OPTION DE CALCUL NON PREVUE !')      
      ENDIF
          
      END
