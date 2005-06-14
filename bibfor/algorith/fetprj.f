      SUBROUTINE FETPRJ(NBI,VI,VO,JGITGI,LRIGID,DIMGI,OPTION,
     &                  SDFETI,IPIV,NBSD,VSDF,VDDL,MATAS,GI,LSTOGI,
     &                  INFOFE,IREX,IPRJ)
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
C TOLE CRP_4
C-----------------------------------------------------------------------
C    - FONCTION REALISEE:  CALCUL AU SENS FETI DE:
C          * LA PROJECTION COMPLETE P=I-GI.(GIT.GI)-1.GIT SI OPTION=1
C               P(VI) EST STOCKEE DANS LES NBI COMPOSANTES DE VO
C          * LA PROJECTION PARTIELLE P'=(GIT.GI)-1.GIT SI OPTION=2
C               P'(VI) EST STOCKEE DANS LES DIMGI PREMIERES COMPOSANTES
C                DE VO
C
C      IN    NBI: IN   : NOMBRE DE NOEUDS D'INTERFACE
C      IN     VI: VR8  : VECTEUR INPUT DE TAILLE NBI
C      OUT    VO: VR8  : VECTEUR OUTPUT DE TAILLE NBI SI OPTION=1
C      IN JGITGI: IN  : ADRESSE OBJET JEVEUX (GI)T*GI
C      IN LRIGID: LO  : LOGICAL INDIQUANT LA PRESENCE D'AU MOINS UN
C         SOUS-DOMAINES FLOTTANT
C      IN  DIMGI:  IN : TAILLE DE GIT*GI
C      IN OPTION:  IN  : 1 -> PROJECTION., 2-> RECONSTRUCTION ALPHA SOL.
C      IN SDFETI: CH19 : SD DECRIVANT LE PARTIONNEMENT FETI
C   IN/OUT IPIV: VIN : ADRESSE VECTEUR DECRIVANT LE PIVOTAGE LAPACK
C                     POUR INVERSER (GIT)*GI
C     IN   VSDF: VIN : VECTEUR MATR_ASSE.FETF INDIQUANT SI SD FLOTTANT
C     IN   VDDL: VIN : VECTEUR DES NBRES DE DDLS DES SOUS-DOMAINES
C     IN   NBSD:  IN : NOMBRE DE SOUS-DOMAINES
C     IN   MATAS: K19 : NOM DE LA MATRICE DE RIGIDITE GLOBALE
C     IN    GI : MATR8: MATRICE GI
C     IN  LSTOGI: LO : TRUE, GI STOCKE, FALSE, RECALCULE
C     IN IREX/IPRJ: IN : ADRESSE DU VECTEUR AUXILAIRE EVITANT DES APPELS
C                        JEVEUX.
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       28/01/04 (OB): CREATION.
C       04/06/04 (OB): MODIFICATION POUR MODES DE CORPS RIGIDES.
C----------------------------------------------------------------------
C RESPONSABLE BOITEAU O.BOITEAU
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INTEGER      NBSD,NBI,JGITGI,DIMGI,OPTION,VSDF(NBSD),VDDL(NBSD),
     &             IPIV,IREX,IPRJ
      REAL*8       VI(NBI),VO(NBI),GI(NBI,DIMGI)
      LOGICAL      LRIGID,LSTOGI
      CHARACTER*19 SDFETI,MATAS
      CHARACTER*24 INFOFE

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER*4          ZI4
      COMMON  / I4VAJE / ZI4(1)
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
      INTEGER      JGITVI,JGITV1,I,J,IDECAI,K,IDECAO,L,M,IFM,
     &             NBSDF,GII,GII1,NBDDL,NBMC,IFETR,IMC,IDD,INFOL8
      INTEGER*4    INFOLA
      REAL*8       RAUX,DDOT
      CHARACTER*24 NOMSDR     
      CHARACTER*32 JEXNUM

C ROUTINE AVEC MOINS DE MONITORING, JEVEUX.. CAR APPELLEE SOUVENT     
      IFM=ZI(IPRJ)        
      IF ((OPTION.NE.1).AND.(OPTION.NE.2))
     &  CALL UTMESS('F','FETPRJ','OPTION DE CALCUL NON PREVUE !')

C---------------------------------------------------------------------
C --------------------------------------------------------------------
C AUCUN MODE DE CORPS RIGIDE P=ID (OPTION=1 OBLIGEATOIRE)
C --------------------------------------------------------------------
C---------------------------------------------------------------------
      IF (.NOT.LRIGID) THEN
      
        IF (OPTION.NE.1)
     &    CALL UTMESS('F','FETPRJ','OPTION DE CALCUL INCOHERENTE !')
        CALL DCOPY(NBI,VI,1,VO,1)
       
      ELSE
C---------------------------------------------------------------------
C --------------------------------------------------------------------
C PRESENCE DE MODES DE CORPS RIGIDES P (OPTION=1) OU P' (OPTION=2)
C --------------------------------------------------------------------
C---------------------------------------------------------------------

C --------------------------------------------------------------------
C CONSTITUTION DE (GI)T*VI STOCKE DANS '&&FETPRJ.GITVI.R'
C --------------------------------------------------------------------
        JGITVI=ZI(IPRJ+1)
        JGITV1=JGITVI-1
        
        IF (LSTOGI) THEN        
          CALL DGEMV('T',NBI,DIMGI,1.D0,GI,NBI,VI,1,0.D0,ZR(JGITVI),1)
        ELSE

          DO 9 I=1,DIMGI
            ZR(JGITV1+I)=0.D0
    9     CONTINUE      
          NOMSDR=MATAS//'.FETR'      
          NBSDF=0
          GII=ZI(IPRJ+2)
          GII1=GII-1
          DO 30 IDD=1,NBSD
            NBDDL=VDDL(IDD)
            NBMC=VSDF(IDD)                        
            IF (NBMC.NE.-1) THEN
              NBSDF=NBSDF+1
              CALL JEVEUO(JEXNUM(NOMSDR,NBSDF),'L',IFETR)
              DO 20 IMC=1,NBMC
                JGITV1=JGITV1+1
                CALL FETREX(1,IDD,NBDDL,ZR(IFETR+(IMC-1)*NBDDL),NBI,
     &                  ZR(GII),IREX)
                DO 10 I=1,NBI
                  ZR(JGITV1)=ZR(JGITV1)+ZR(GII1+I)*VI(I)
   10           CONTINUE
   20         CONTINUE
              CALL JELIBE(JEXNUM(NOMSDR,NBSDF))
            ENDIF
   30     CONTINUE
          JGITV1=JGITVI-1
          
        ENDIF
        
C --------------------------------------------------------------------
C CONSTITUTION DE ((GI)T*GI)-1*(GI)T*VI STOCKE DANS '&&FETPRJ.GITGI.R'
C --------------------------------------------------------------------
        INFOLA=0
        INFOL8=0
C DESCENTE-REMONTEE MATRICE SYMETRIQUE INDEFINIE (STOCKEE PAR PAQUET)
C VIA LAPACK
        CALL DSPTRS('L',DIMGI,1,ZR(JGITGI),ZI4(IPIV),ZR(JGITVI),DIMGI,
     &  INFOLA)
        INFOL8=INFOLA
        IF (INFOL8.NE.0) THEN
          CALL UTDEBM('F','FETPRJ','SYSTEME (GI)T*GI PROBABLEMENT')
          CALL UTIMPI('S','  NON INVERSIBLE: ',0,I)
          CALL UTIMPI('L','PB LAPACK DGETRS: ',1,INFOL8)
          CALL UTFINM()
        ENDIF
        
        IF (OPTION.EQ.1) THEN
C --------------------------------------------------------------------
C CONSTITUTION DE V0=VI-GI*((GI)T*GI)-1*(GI)T*VI (OPTION=1)
C --------------------------------------------------------------------
          CALL DCOPY(NBI,VI,1,VO,1)
          
          IF (LSTOGI) THEN          
            CALL DGEMV('N',NBI,DIMGI,-1.D0,GI,NBI,ZR(JGITVI),1,1.D0,
     &        VO,1)
          ELSE          
            NBSDF=0
            DO 200 IDD=1,NBSD
              NBDDL=VDDL(IDD)
              NBMC=VSDF(IDD)
                        
              IF (NBMC.NE.-1) THEN
                NBSDF=NBSDF+1
                CALL JEVEUO(JEXNUM(NOMSDR,NBSDF),'L',IFETR)
                DO 190 IMC=1,NBMC
                  JGITV1=JGITV1+1
                  RAUX=-ZR(JGITV1)         
                  CALL FETREX(1,IDD,NBDDL,ZR(IFETR+(IMC-1)*NBDDL),NBI,
     &                      ZR(GII),IREX)
                  CALL DAXPY(NBI,RAUX,ZR(GII),1,VO,1)  
  190           CONTINUE
                CALL JELIBE(JEXNUM(NOMSDR,NBSDF))
              ENDIF
  200       CONTINUE
            
          ENDIF
                  
C MONITORING
          IF (INFOFE(1:1).EQ.'T')
     &      WRITE(IFM,*)'<FETI/FETPRJ> LAMBDA = P * LAMBDA'
C          IF (INFOFE(4:4).EQ.'T') THEN
C            DO 300 I=1,NBI
C              WRITE(IFM,*)I,'  ',VI(I),' ',VO(I)
C  300       CONTINUE
C          ENDIF
                    
        ELSE
C --------------------------------------------------------------------
C CONSTITUTION DE V0=((GI)T*GI)-1*(GI)T*VI (OPTION=2)
C --------------------------------------------------------------------
          CALL DCOPY(DIMGI,ZR(JGITVI),1,VO,1)

C MONITORING
          IF (INFOFE(1:1).EQ.'T')
     &      WRITE(IFM,*)'<FETI/FETPRJ> ALPHA = P'' * RESIDU'         
C          IF (INFOFE(4:4).EQ.'T') THEN
C            WRITE(IFM,*)'<FETI/FETPRJ> INPUT/OUTPUT'
C            DO 320 I=1,DIMGI
C              WRITE(IFM,*)I,'  ',VI(I),' ',VO(I)
C  320       CONTINUE
C          ENDIF                        
        ENDIF   
      ENDIF                
      END
