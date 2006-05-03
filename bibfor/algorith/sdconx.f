      SUBROUTINE SDCONX(RHON,CRIT,FROTT,MU,RHOT,SEUIL0,STACO0,INTE,
     &                                          COECH,ALGOLA,IOCCC,FISS)


C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/05/2006   AUTEUR GENIAUT S.GENIAUT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE GENIAUT S.GENIAUT

      IMPLICIT NONE

      REAL*8       RHON,MU,RHOT,SEUIL0,COECH
      INTEGER      CRIT(2),IOCCC
      CHARACTER*8  FROTT,STACO0,INTE,ALGOLA,FISS
C
C   CRÉATION DE LA STRUCTURE DE DONNÉE CONTACT X-FEM
C
C   IN
C       RHON   : COEF_REGU_CONT
C       CRIT   : ITER_GEOM_MAXI ET ITER_CONT_MAXI
C       FROTT  : 'SANS' OU 'COULOMB'
C       MU     : COEFFICIENT DE FROTTEMENT DE COULOMB
C       RHOT   : COEF_REGU_FROT
C       SEUIL0 : SEUIL_INIT
C       STACO0 : CONTACT INITIAL ("OUI" OU "NON")
C       INTE   : SCHEMA D'INTEGRATION ("GAUSS" OU "NOEUD")
C       COECH  : COEFFICIENT DE MISE À L'ECHELLE DES TERMES DE PRESSION
C                DE CONTACT (CAD DE DE TYPE 'LAGS_C')
C       ALGOLA : ALGORITHME DE RESTRICTION DES LAGRANGES
C       IOCCC  : OCCURENCE DU MOT-CLÉ FACTEUR CONTACT
C       FISS   : SD FISS_XFEM
C 
C   OUT
C      FISS  : SD FISS_XFEM AVEC DONNÉES SUR LE CONTACT

      INTEGER      JCMCF,JECPD,JMETH,JXFEM

C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXATR
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
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------

      CALL JEMARQ()

C     SI LE MOT-CLÉ FACTEUR CONTACT N'EST PAS RENSEIGNÉ, CHOIX DES
C     PARAMÈTRES POUR "NE PAS PRENDRE EN COMPTE" LE CONTACT
C                   VOIR BOOK VI 11/04/2006)
      IF (IOCCC.EQ.0) THEN
        RHON=100.D0
        CRIT(1)=4
        STACO0='NON'
        INTE='FPG4'
        COECH=1.D0
        ALGOLA='NON'
        FROTT='SANS'
      ENDIF

C     ------------------------------------------------------------------
C          CREATION DU .CARACF
C     ------------------------------------------------------------------

      CALL WKVECT(FISS//'.CONTACT.CARACF','G V R',9,JCMCF)

C     NB DE ZONES DE CONTACT
      ZR(JCMCF-1+1)=1.D0

C     SCHÉMAS D'INTÉGRATION 
C     =1 SI INTEGRATION = 'GAUSS' : SCHÉMA EXACT À 12 POINTS (FPG12)
C     =4 SI INTEGRATION = 'FPG4'  : SCHÉMA RÉDUIT À 4 POINTS (FGP4)
C     =6 SI INTEGRATION = 'FPG6'  : SCHÉMA RÉDUIT À 6 POINTS (FGP6)
C     =7 SI INTEGRATION = 'FPG7'  : SCHÉMA RÉDUIT À 7 POINTS (FGP7)
      IF (INTE.EQ.'GAUSS') THEN
        ZR(JCMCF-1+2)=1.D0
      ELSEIF (INTE.EQ.'FPG4') THEN
        ZR(JCMCF-1+2)=4.D0
      ELSEIF (INTE.EQ.'FPG6') THEN
        ZR(JCMCF-1+2)=6.D0
      ELSEIF (INTE.EQ.'FPG7') THEN
        ZR(JCMCF-1+2)=7.D0
      ELSE
        CALL UTMESS('F','SDCONX','CHOIX INCORRECT DU SCHEMA '//
     &                  'D''INTEGRATION NUMERIQUE POUR LE CONTACT')
      ENDIF      

C     COEF_REGU_CONT
      ZR(JCMCF-1+3)=RHON      

C     COEF_REGU_FROT
      ZR(JCMCF-1+4)=RHOT

      IF (FROTT.EQ.'COULOMB') THEN
C       COEF DE COULOMB POUR LE FROTTEMENT
        ZR(JCMCF-1+5)=MU               
C       COEF DE MALEK (=1 SI FROTTEMENT='SANS' 
C                      =3 SI FROTTEMENT='COULOMB')
        ZR(JCMCF-1+6)=3.D0     
      ELSEIF (FROTT.EQ.'SANS') THEN
        ZR(JCMCF-1+4)=0.D0
        ZR(JCMCF-1+5)=0.D0  
        ZR(JCMCF-1+6)=1.D0
        SEUIL0=0.D0
      ENDIF

C     SEUIL_INIT
      ZR(JCMCF-1+7)=SEUIL0    

C     COEFFICIENT DE MISE À L'ECHELLE DES TERMES DE PRESSION DE CONTACT 
      ZR(JCMCF-1+8)=COECH   

C     ALGORITHME DE RESTRICTION DE L'ESPACE DES MULITPLICATEURS
      IF (ALGOLA.EQ.'NON') THEN
        ZR(JCMCF-1+9)=0.D0
      ELSEIF (ALGOLA.EQ.'VERSION1') THEN
        ZR(JCMCF-1+9)=1.D0
      ELSEIF (ALGOLA.EQ.'VERSION2') THEN
        ZR(JCMCF-1+9)=2.D0
      ELSE
        CALL UTMESS('F','SDCONX','CHOIX INCORRECT DE L'''//
     &    'ALGORITHME DE RESTRICTION DE L''ESPACE DES MULITPLICATEURS')
      ENDIF

C     ------------------------------------------------------------------
C          CREATION DU .ECPDON
C     ------------------------------------------------------------------

      CALL WKVECT(FISS//'.CONTACT.ECPDON','G V I',6,JECPD)

C     NB TOTAL DE ZONES DE CONTACT
      ZI(JECPD-1+1)=1

C     INDICATEUR D'AXIS (=1 SI MODL_AXIS='OUI' 
C                        =0 SI MODL_AXIS='NON')
      ZI(JECPD-1+2)=0

C     ITER_CONT_MAXI
      ZI(JECPD-1+3)=CRIT(1)    

C     ITER_FROT_MAXI
      ZI(JECPD-1+4)=CRIT(2) 

C     ITER_GEOM_MAXI
      ZI(JECPD-1+5)=1

C     STATUT DE CONTACT INITIAL
      IF (STACO0.EQ.'OUI') ZI(JECPD-1+6)=1
      IF (STACO0.EQ.'NON') ZI(JECPD-1+6)=0      

C     ------------------------------------------------------------------
C          CREATION DU .METHCO
C     ------------------------------------------------------------------
                 
      CALL WKVECT(FISS//'.CONTACT.METHCO','G V I',7,JMETH)

C     N° DE LA MÉTHODE UTILISÉE (=6 SI MÉTHODE CONTINUE)
      ZI(JMETH-1+7)=6

C     ------------------------------------------------------------------
C          CREATION DU .XFEM
C     ------------------------------------------------------------------

      CALL WKVECT(FISS//'.CONTACT.XFEM','G V I',2,JXFEM)

C     INDICATEUR DE PRÉSENCE DE LA MÉTHODE CONTINUE AVEC X-FEM
      ZI(JXFEM-1+1)=1

C     INDICATEUR POUR SAVOIR S'IL FAUDRA ZAPPER LA VÉRIFICATION DE LA
C     CONDITION D'INTERPÉNÉTRATION
      IF (IOCCC.EQ.0) ZI(JXFEM-1+2)=1

      CALL JEDEMA()
      END
