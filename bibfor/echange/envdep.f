      SUBROUTINE ENVDEP(NUMPAS,NBPAL,DT,DTSTO,NEQ,TEMPS,
     &      DEP,VIT,NUMDDL,VROTAT,NOPAL, TYPAL, FINPAL,PRDEFF,CNPAL)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ECHANGE  DATE 16/02/2010   AUTEUR GREFFET N.GREFFET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE GREFFET N.GREFFET
C **********************************************************************
C *   LOGICIEL CODE_ASTER - COUPLAGE ASTER/EDYOS - COPYRIGHT EDF 2009  *
C **********************************************************************

CPAT_FONCTION

C  ENVDEP : FONCTION
C  -----------------
                                                        
C CE SSP PERMET D'ENVOYER A EDYOS LES DEPLACEMENTS ET VITESSES CALCULES
C PAR ASTER.
C AU PREMIER PAS DE TEMPS, ON ENVOIT DES DEPLACEMENTS ET VITESSES NULS
                                              
C L'AXE DU ROTOR EST SUPPOSE ETRE EN Z (EN X DANS CADYRO)
           
CPAT_FONCTION

C=======================================================================

C  REFERENCES BIBLIOGRAPHIQUES
C  ---------------------------

C  NOTE HI-26/03/007A 
C  "DEVELOPPEMENT D'UN MODE PRODUCTION POUR CALCIUM: MANUEL UTILISATEUR"
C  FAYOLLE ERIC, DEMKO BERTRAND (CS SI)  JUILLET 2003

C  LES APPELS YACS SONT STRICTEMENTS IDENTIQUES A CEUX DE CALCIUM A 
C  L'EXCEPTION DU RAJOUT D'UN PREMIER ARGUMENT (ICOMPO) CORRESPONDANT A
C  UNE ADRESSE NECESSAIRE A L'EXECUTION DE YACS

C ======================================================================
C  DEVELOPPEMENTS ET CORRECTIONS D'ANOMALIES
C  -----------------------------------------
C  DATE: 13/02/09   AUTEUR: P. VAUGRANTE    ANOMALIE: DEVELOPPEMENT
C  DATE:            AUTEUR:                 ANOMALIE:
C  DATE:            AUTEUR:                 ANOMALIE:
C  DATE:            AUTEUR:                 ANOMALIE:
C ======================================================================

C  VARIABLES UTILISEES
C  -------------------
C
C  _____________________________________________________________________
C !    NOM      !   TYPE      !                  ROLE                  !
C !_____________!_____________!________________________________________!
C !             !             !                                        !
C ! TEMPS       !  REEL*8     !  IN: INSTANT COURANT                   !
C !             !             !                                        !
C ! DT          !  REEL*8     !  IN: PAS DE TPS ASTER (EDYOS) INITIAL  !
C !             !             !                                        !
C ! DTSTO       !  REEL*8     !  IN: PAS DE TEMPS POUR STOCKAGE EDYOS  !
C !             !             !                                        !
C ! NEQ         !  ENTIER     !  IN: DIMENSION (PAR JEVEUX) POUR       !
C !             !             !  LES DEPLACEMENTS ET LES VITESSES      !
C !             !             !                                        !
C ! DEP(NEQ)    !  REEL*8     !  IN: VECTEUR DES DEPLACEMENTS GLOBAUX  !
C !             !             !                                        !
C ! VIT(NEQ)    !  REEL*8     !  IN: VECTEUR DES VITESSES GLOBALES     !
C !             !             !                                        !
C ! NUMPAS      !  ENTIER     !  IN: NUMERO D'ITERATION                !
C !             !             !  FAIT CORRESPONDRE LES INSTANTS        !
C !             !             !  POUR LES ECHANGES VIA YACS            !
C !             !             !                                        !
C ! NUMDDL      !  CHARACTER  !  IN: NOM D'UN NUME_DDL OU D'UN CHAM_NO !
C !             !             !  (VARIABLE ASTER) (='NDDL')            !
C !             !             !                                        !
C ! COMP        !  CHARACTER  !  NOM DES COMPOSANTES ASTER             !
C !             !             !                                        !
C ! VROTAT      !  REEL*8     !  IN: VITESSE DE ROT. INITIALE DU ROTOR !
C !             !             !                                        !
C ! RZPR        !  REEL*8     !  VARIABLE NON UTILISEE PAR EDYOS (=> 0)!
C !             !             !                                        !
C ! POSVIT(11)  !  REEL*8     !  PARAMETRES DE TYPE R ENVOYES A EDYOS  !
C !             !             !  (TEMPS, DT, DEP_X, DEP_Y, VIT_X,VIT_Y !
C !             !             !   DEP_RX, DEP_RY, RZPR, vit_rot,dtsto) !
C !             !             !                                        !
C ! INFO        !  ENTIER     !  FLAG DE RETOUR DE YACS INDIQUANT SI LE!
C !             !             !  TRANSFERT S'EST BIEN EFFECTUE (INFO=0)!
C !             !             !  DANS LE CAS CONTRAIRE CE FLAG EST     !
C !             !             !  INTERPRETE PAR LE SSP ERRCOU          !
C !             !             !                                        !
C ! TR8         !  REEL*8     !  NE SERT A RIEN, C'EST JUSTE UNE VAR.  !
C !             !             !  NECESSAIRE POUR LES APPELS YACS       !
C !             !             !  (UTILE LORSQUE YACS UTILISE LE TEMPS  !
C !             !             !  POUR LA CORRESPONDANCE)               !
C !             !             !                                        !
C ! NOMPRG      !  CHARACTER  !  NOM DU SSP (POUR ECRITURE DANS ERRCOU)!
C !             !             !                                        !
C !             !             !                                        !
C !_____________!_____________!________________________________________!



C INCLUDE CALCIUM.H 
C  _____________________________________________________________________
C !         !             !                                            !
C ! LENVAR  !  ENTIER     !  LONGUEUR DES NOMS DES VARIABLES ECHANGEES !
C !         !             !                                            !
C ! NOMVAR  !  CHARACTER  !  NOM DE LA VARIABLE ECHANGEE AVEC EDYOS    !
C !         !  (*LENVAR)  !  (CE NOM ET SA CORESPONDACE EDYOS EST      !
C !         !             !  DEFINI DANS LES FICHIERS UTILISES PAR     !
C !         !             !  YACS : *.PY ET *.XML)                     !
C !         !             !                                            !
C ! CPITER  !  ENTIER     !  CORRESPOND A CP_ITERATION POUR YACS       !
C !         !             !  VAUT 41 ET SIGNIFIE QUE YACS FAIT         !
C !         !             !  CORRESPONDRE LES NUMEROS D'ITERATION      !
C !         !             !  ENTRE ASTER ET EDYOS (VOIR BIBLIOGRAPHIE) !
C !_________!_____________!____________________________________________!
 

C COMMON YACS
C  _____________________________________________________________________
C !         !             !                                            !
C ! ICOMPO  ! ENTIER*8    ! ADRESSE NECESSAIRE AUX APPELS YACS         !
C !         !             !                                            !
C !         !             !                                            !
C !_________!_____________!____________________________________________!
C

C COMMON CADY
C  _____________________________________________________________________
C !         !             !                                            !
C ! NBPAL   ! ENTIER      ! IN : NOMBRE DE PALIERS                     !
C !         !             !                                            !
C ! IFM     ! ENTIER      ! IN : UNITE LOGIQUE POUR L'ECRITURE         !
C !         !             !     (PRINCIPALEMENT POUR LE DEBUGGAGE)     !
C !         !             !                                            !
C !_________!_____________!____________________________________________!


C COMMON PALIERS
C  _____________________________________________________________________
C !             !             !                                        !
C !             !             !                                        !
C !FINPAL(NBPAL)!  CHARACTER*3!  IN : TERMINAISON POUR LES PALIERS     !
C !             !             !  PALIER N°I => _I                      !
C !             !             !                                        !
C ! PALMAX      !  ENTIER     !  NOMBRE MAXIMUM DE PALIERS             !
C !             !             !                                        !
C ! TYPE_PAT    !  CHARACTER*6!TYPE DES PALIERS (NON UTILISE DS INIPAT)!
C !             !  (PALMAX)   !                                        !
C !             !             !                                        !
C !             !             !                                        !
C !_____________!_____________!________________________________________!
C


C COMMON NOEUDS_PALIERS
C  ________________________________________________________________
C !                  !             !                               !
C ! CNPAL(PALMAX)    !  CHARACTER  !  NOM DU NOEUD ASTER           !
C !                  !             !                               !
C ! NOPAL(PALMAX)    !  ENTIER     !  NUMERO DU NOEUD ASTER        !
C !                  !             !                               !
C ! DIMNAS           !  CHARACTER  !  NOMBRE DE DDL POUR UN NOEUD  !
C !                  !             !                               !
C !__________________!_____________!_______________________________!
C

C "COMMON" ASTER
C --------------

C  COMMON ZI (TYPE: INTEGER) (NOM = '&ADR_YACS')
C  _______________________________________________________________
C !          !             !                                     !
C ! ICOMPO   !  ADR        !  ADRESSE NECESSAIRE AUX APPELS YACS !
C !__________!_____________!_____________________________________!
C





C  COMMON ZI (TYPE: INTEGER) (NOM = 'N_PAL')
C  ___________________________________________________________________
C !            !             !                                       !
C ! NBPAL      !  ADR        !  NOMBRE DE PALIERS POUR L'ETUDE       !
C !            !             !                                       !
C ! NOPAL(IPAL)!  ADR+1      !  NUMERO DU NOEUD ASTER POUR LE PALIER !
C !            !  +(IPAL-1)  !  CONSIDERE                            !
C !____________!_____________!_______________________________________!
C


C  COMMON ZK8 (TYPE: CHARACTER*8) (NOM = 'C_PAL')
C  ____________________________________________________________________
C !             !             !                                       !
C ! TYPAL(IPAL) ! ADR+(IPAL-1)!  TYPE DU PALIER CONSIDERE             !
C !             !             !                                       !
C ! FINPAL(IPAL)!  ADR+PALMAX !  TERMINAISON POUR LE PALIER CONSIDERE !
C !             !  +(IPAL-1)  !  PALIER N°I => _I                     !
C !             !             !                                       !
C ! CNPAL(IPAL) ! ADR+2*PALMAX!  NOM DU NOEUD ASTER POUR LE PALIER    !
C !             !  +(IPAL-1)  !  CONSIDERE                            !
C !_____________!_____________!_______________________________________!
C

C
C=======================================================================
C  SOUS PROGRAMME(S) APPELE(S) : CP* (YACS), ERRCOU.F
C                                
C-----------------------------------------------------------------------
C  SOUS PROGRAMME(S) APPELANT(S) :  OP0115.F, OP0111.F
C
C***********************************************************************
C%W% %D% %T%
C TOLE CRP_4 CRP_6  CRS_512

      IMPLICIT NONE


C     ARGUMENTS
C     =========
      INTEGER       NUMPAS, NEQ, NBPAL
      REAL*8        TEMPS, DEP(NBPAL,*),VIT(NBPAL,*),DT,DTSTO,VROTAT
      CHARACTER*14  NUMDDL
      LOGICAL       PRDEFF

C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------

      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM

C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------


C     VARIABLES INTERNES
C     ==================
      INTEGER       IFM, NIV,L,CONV
      CHARACTER*8   NOMPRG
      PARAMETER(NOMPRG='ENVDEP')

      CHARACTER*3   COMP(6)
      INTEGER       NNO,NDDL
      INTEGER*4     IPAT,IPAL,INFO,NPAS
      REAL*8        VITPAL(6),DEPPAL(6),TR8
      REAL*8        POSVIT(11),RZPR
      INTEGER*4     ONZE
      PARAMETER  (ONZE=11)  
      REAL*8        PI
C      PARAMETER (PI=3.14159265359)   
C      PARAMETER (PI=3.14116)                
      
      
C     ANCIENS INCLUDE (CALCIUM.H)
C     ===========================
      INTEGER*4     LENVAR
      PARAMETER (LENVAR = 144)
      CHARACTER*(LENVAR) NOMVAR
      INTEGER*4     CPITER
      PARAMETER (CPITER= 41)


C     COMMON
C     ======
      INTEGER     ICOMPO
   
      INTEGER       PALMAX 
      PARAMETER (PALMAX=20)
      CHARACTER*6   TYPAL(PALMAX)
      CHARACTER*3   FINPAL(PALMAX)
      
      INTEGER       NOPAL(PALMAX) 
      CHARACTER*6  CNPAL(PALMAX)    

      INTEGER       IADR,IADRI,IADRK,IAPP
      CHARACTER*24  AYACS   
C
C     DEBUT SSP
C     =========
C
      CALL JEMARQ()
      CALL INFDBG('YACS_EDYOS',IFM,NIV)
C
C     initialisation de pi
C     --------------------
      PI = ATAN(1.D0)*4.D0  
C      
C     ASSIGNATION DES NOMS POUR LES ADRESSES DANS LES COMMON ASTER
C     ------------------------------------------------------------ 
      AYACS='&ADR_YACS'
C
C     RECUPERATION DES DONNEES DANS LES "COMMON" ASTER
C     ================================================
C
C     RECUPERATION DE L'UNITE D'ECRITURE
C     ----------------------------------        
C
C     RECUPERATION DE L'ADRESSE YACS
C     ------------------------------        
      CALL JEVEUO(AYACS,'L',IADR)
      ICOMPO=ZI(IADR) 
C      
C     RZPR: ROTATION INITIALE (PAS UTILISÉE PAR EDYOS (XAV))
C     ------------------------------------------------------
      RZPR =0.D0
C      
C     RECHERCHE DES DDL CONCERNES POUR LES VALEURS EN SORTIE
C     ------------------------------------------------------
      COMP(1)='DX'
      COMP(2)='DY'
      COMP(3)='DZ'
      COMP(4)='DRX'
      COMP(5)='DRY'
      COMP(6)='DRZ'
C
C     REMPLISSAGE DU VECTEUR POSVIT POUR EDYOS
C     ----------------------------------------
      POSVIT(1)=TEMPS
      POSVIT(2)=DT
C
C     DEBUT DE LA BOUCLE SUR LES PALIERS
C
      DO 10 IPAL=1,NBPAL
C
C      SUBROUTINE POSDDL ( TYPE, RESU, NOEUD, CMP, NUNOE, NUDDL )
C     ------------------------------------------------------------------
C     DONNE LE NUMERO DU NOEUD
C           NUNOE = 0 SI LE NOEUD N'EXISTE PAS
C     DONNE LE NUMERO DU DDL ASSOCIE AU NOEUD ET A SA COMPOSANTE
C           NUDDL = 0 SI LE COUPLE (NOEUD,COMPOSANTE) N'EXISTE PAS
C     ------------------------------------------------------------------
C IN  TYPE   : TYPE DU RESU
C IN  RESU   : NOM D'UN NUME_DDL OU D'UN CHAM_NO
C IN  NOEUD  : NOM DU NOEUD
C IN  CMP    : NOM DE LA COMPOSANTE
C OUT NUNOE  : NUMERO LOCAL DU NOEUD
C OUT NUDDL  : NUMERO DU DDL ASSOCIE AU NOEUD DE COMPOSANTE CMP
C     ------------------------------------------------------------------
C
          IF(NUMPAS .LE. 1)THEN  
C            PREMIER PAS DE TEMPS => DEPLACEMENTS ET VITESSES NULS
             POSVIT(3)=0.D0  
             POSVIT(4)=0.D0   
             POSVIT(5)=0.D0  
             POSVIT(6)=0.D0 
             POSVIT(7)=0.D0  
             POSVIT(8)=0.D0 
             POSVIT(9)=RZPR
             POSVIT(10) = (VROTAT/30.D0)*PI
             IF (NIV.GE.2)
     &         WRITE(IFM,*)' ASTEREDYOS : ',NOMPRG,' VIT_ROTAT LUE ',
     &         POSVIT(10)
C
             POSVIT(11)=DTSTO    
C
          ELSE          
             DO 20 IPAT=1,6
               DEPPAL(IPAT)=DEP(IPAL,IPAT)
               VITPAL(IPAT)=VIT(IPAL,IPAT)
   20        CONTINUE
             POSVIT(3)=DEPPAL(1)  
             POSVIT(4)=DEPPAL(2)   
             POSVIT(5)=VITPAL(1)  
             POSVIT(6)=VITPAL(2) 
             POSVIT(7)=DEPPAL(4)  
             POSVIT(8)=DEPPAL(5) 
             POSVIT(9)=RZPR
             POSVIT(10)=(VROTAT/30.D0)*PI
             POSVIT(11)=DTSTO             
          ENDIF
C
C         ECRITURE DES VALEURS ENVOYEES   
          IF (NIV.GE.2) THEN
             WRITE(IFM,*)' *****  ASTEREDYOS :',NOMPRG,'  NUMPAS = ',
     &           NUMPAS
             WRITE(IFM,*)' ASTEREDYOS : ',NOMPRG,
     &           '  : ENVOI DES DEPLA A EDYOS'
             WRITE(IFM,*)' ASTEREDYOS : ',NOMPRG,
     &           '  - NUMERO PALIER : ',IPAL
             WRITE(IFM,*)' ASTEREDYOS : ',NOMPRG,
     &           '  TEMPS,PAS DE TEMPS: ',POSVIT(1),POSVIT(2)
             WRITE(IFM,*)' ASTEREDYOS : ',NOMPRG,'  DEPX  DEPY: ',
     &           POSVIT(3),POSVIT(4)
             WRITE(IFM,*)' ASTEREDYOS : ',NOMPRG,'  VITX  VITY: ',
     &           POSVIT(5),POSVIT(6)
             WRITE(IFM,*)' ASTEREDYOS : ',NOMPRG,'  ROTX  ROTY  ',
     &           POSVIT(7),POSVIT(8)
             WRITE(IFM,*)' ASTEREDYOS : ',NOMPRG,
     &           '  RZPR (PAS UTILISEE): ',POSVIT(9)
             WRITE(IFM,*)' ASTEREDYOS : ',NOMPRG,'  OMEGA : ',
     &           POSVIT(10)
             WRITE(IFM,*)' ASTEREDYOS : ',NOMPRG,
     &           '  PAS DE TEMPS DE STOCKAGE:',POSVIT(11)
             WRITE(IFM,*)' ASTEREDYOS : ',NOMPRG,'  ITERATION : ',
     &           NUMPAS
             WRITE(IFM,*)' *****  ASTEREDYOS : ',NOMPRG,'    ****** '
          ENDIF
C    
C
C         TR8 = TEMPS PIPEAU NE SERVANT A RIEN
          TR8 = 0.D0
          NOMVAR='POSITION'//FINPAL(IPAL)
C
C         ENVOI DES DEPLACEMENTS ET VITESSES A EDYOS
          NPAS=NUMPAS
          IF (PRDEFF) THEN
            CALL CPEDB (ICOMPO,CPITER,TR8,NPAS,NOMVAR,ONZE,
     &                POSVIT,INFO)
            CALL ERRCOU (NOMPRG,NPAS,NOMVAR,INFO,ONZE,ONZE)
          ENDIF
   10 CONTINUE
C     FIN DE LA BOUCLE SUR LES PALIERS
      CALL JEDEMA()
C
      END
