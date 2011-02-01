      SUBROUTINE RECFOR(NUMPAS,NBPAL,FORCE,
     &                  TYPAL, FINPAL, CNPAL, PRDEFF, CONV )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ECHANGE  DATE 31/01/2011   AUTEUR GREFFET N.GREFFET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE GREFFET N.GREFFET
C **********************************************************************
C *   LOGICIEL CODE_ASTER - COUPLAGE ASTER/EDYOS - COPYRIGHT EDF 2009  *
C **********************************************************************

CPAT_FONCTION

C  RECFOR : FONCTION
C  -----------------

C
C    CE SSP PERMET (DANS UNE BOUCLE SUR LES PALIERS) DE :
C            - LIRE LES EFFORTS EN PROVENANCE D'EDYOS
C            - SI TYPE PALIER = PAPANL LIRE LES VARIABLES RETOURNEES PAR
C      EDYOS (AU 16/02/09 ON NE SE SERT PAS DE CES VARIABLES DANS ASTER)
C              (ELLES SERVENT A CONSTITUER DES FICHIERS DE REPRIS =>
C              CETTE ECRITURE SERA SUPPRIMEE DANS EDYOS)
C            - IDEM SI TYPALIER = PACONL

C
              
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
C  ____________________________________________________________________
C !    NOM    !   TYPE     !                 ROLE                      !
C !___________!____________!___________________________________________!
C !           !            !                                           !
C !           !            !                                           !
C ! NEQ       !  ENTIER    ! IN: DIMENSION (ENVOYEE PAR JEVEUX) POUR   !
C !           !            ! LES EFFORTS                               !
C !           !            !                                           !
C !           !            !                                           !
C ! NUMPAS    !  ENTIER    ! IN: NUMERO D'ITERATION                    !
C !           !            ! PERMET DE FAIRE CORRESPONDRE LES INSTANTS !
C !           !            ! POUR LES ECHANGES VIA YACS                !
C !           !            !                                           !
C ! NUMDDL    !  CHARACTER ! IN : NOM D'UN NUME_DDL OU D'UN CHAM_NO    !
C !           !            ! (VARIABLE ASTER) (='NDDL')                !
C !           !            !                                           !
C ! FORCE(NEQ)!  REEL*8    ! PARAMETRES DE TYPE REEL RECU D'EDYOS      !
C !           !            ! (REACTION EN X, REACTION EN Y)            !
C !           !            !                                           !
C ! COMP      !  CHARACTER ! NOM DES COMPOSANTES ASTER                 !
C !           !            !                                           !
C ! PARAMR(6) !  REEL*8    ! PARAMETRES DE TYPE REEL ENVOYES PAR EDYOS !
C !           !            ! (INDICE DE CONVERGENCE, REACTION EN X,    !
C !           !            !  REACTION EN Y,)      !
C !           !            !                                           !
C ! INFO      !  ENTIER    ! FLAG DE RETOUR DE YACS INDIQUANT SI LE    !
C !           !            ! TRANSFERT S'EST BIEN EFFECTUE (INFO=0)    !
C !           !            ! DANS LE CAS CONTRAIRE CE FLAG EST         !
C !           !            ! INTERPRETE PAR LE SSP ERRCOU              !
C !           !            !                                           !
C ! TR8       !  REEL*8    ! NE SERT A RIEN, C'EST JUSTE UNE VARIABLE  !
C !           !            ! NECESSAIRE POUR LES APPELS YACS           !
C !           !            ! (UTILE LORSQUE YACS UTILISE LE TEMPS      !
C !           !            ! POUR LA CORRESPONDANCE)                   !
C !           !            !                                           !
C ! NOMPRG    !  CHARACTER ! NOM DU SSP (POUR ECRITURE DANS ERRCOU)    !
C !           !            !                                           !
C ! NPAS      ! ENTIER     ! IN: NUMERO D'ITERATION EN I*4             !
C !           !            !                                           !
C ! IDIM      !  ENTIER    ! DIMENSION DE LA VARIABLE ECHANGEE         !
C !           !            ! (TELLE QUE PROGRAMMEE)                    !
C !           !            !                                           !
C ! NLU       !  ENTIER    ! DIMENSION DE LA VARIABLE ECHANGEE         !
C !           !            ! (RENVOYE PAR YACS)                        !
C !           !            !                                           !
C ! CFRTA     !  REEL*8    ! COUPLE DE FROTTEMENT TOTAL SUR L'ARBRE    !
C !           !            ! DIMENSIONNE (POUR PACONL)                 !
C !           !            ! (PAS UTILISE PAR ASTER LE 16/02/09)       !
C !           !            !                                           !
C ! PALMAX    !  ENTIER    ! NOMBRE MAXIMUM DE PALIERS                 !
C !           !            !                                           !
C !___________!____________!___________________________________________!



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
C ! CPITER  !  ENTIER     !  CORRESPOND A CPITER POUR YACS       !
C !         !             !  VAUT 41 ET SIGNIFIE QUE YACS FAIT         !
C !         !             !  CORRESPONDRE LES NUMEROS D'ITERATION      !
C !         !             !  ENTRE ASTER ET EDYOS (VOIR BIBLIOGRAPHIE) !
C !_________!_____________!____________________________________________!
 



C "COMMON" ASTER
C --------------

C  COMMON ZI (TYPE: INTEGER) (NOM = '&ADR_YACS')
C  ____________________________________________________________________
C !        !             !                                            !
C ! ICOMPO !  ADR        !  ADRESSE NECESSAIRE AUX APPELS YACS        !
C !________!_____________!____________________________________________!
C





C  COMMON ZI (TYPE: INTEGER) (NOM = 'N_PAL')
C  __________________________________________________________________
C !            !             !                                      !
C ! NBPAL      !  ADR        !  NOMBRE DE PALIERS POUR L'ETUDE      !
C !            !             !                                      !
C ! NOPAL(IPAL)!  ADR+1      !  NUMERO DU NOEUD ASTER POUR LE PALIER!
C !            !  +(IPAL-1)  !  CONSIDERE                           !
C !____________!_____________!______________________________________!
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
C TOLE CRS_512 CRP_4 



      IMPLICIT NONE

C     ARGUMENTS
C     =========
      INTEGER       NUMPAS, NBPAL
      REAL*8        FORCE(NBPAL,*),CONV

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

C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------


C     VARIABLES INTERNES
C     ==================
      INTEGER       IFM, NIV
      CHARACTER*8   NOMPRG
      PARAMETER(NOMPRG='RECFOR')
      
      INTEGER*4     IPAT,IPAL,INFO,IDIM,NPAS,NLU
      REAL*8        PARAMR(6),TR8

C     REPRISE NON PROGRAMMEE AU 16/02/09 MAIS PAR CONCORDANCE EDYOS 
C      (PAPANL, PACONL)    
      LOGICAL       REPRIS
      PARAMETER(REPRIS=.FALSE.)
      
C     A SUPPRIMER QUAND REPRIS = FALSE
      INTEGER*4     NSMAX
      PARAMETER (NSMAX = 24 )
      REAL*8        MVTPAT (6,NSMAX) , MVTCON (6,NSMAX)   

      
C     ANCIENS INCLUDE (CALCIUM.H)
C     ===========================
      INTEGER*4     LENVAR
      PARAMETER (LENVAR = 144)
      CHARACTER*(LENVAR) NOMVAR
      INTEGER*4     CPITER
      PARAMETER (CPITER= 41)


C     DECLARATIONS POUR "COMMON" JEVEUX
C     =================================
      INTEGER       ICOMPO   
      INTEGER       PALMAX
      PARAMETER (PALMAX=20)
      INTEGER       IADR
      CHARACTER*3   FINPAL(PALMAX)
      CHARACTER*6   TYPAL(PALMAX)
      CHARACTER*8   CNPAL(PALMAX)
      CHARACTER*24  AYACS
C
      LOGICAL  PRDEFF
C        
C     DEBUT DU SSP
C     ============
      CALL JEMARQ()
      NIV = 0
      CALL INFDBG('YACS_EDYOS',IFM,NIV)
C
C     ASSIGNATION DES NOMS POUR LES ADRESSES DANS LES COMMON ASTER
C     ------------------------------------------------------------
      AYACS='&ADR_YACS'
      CONV = 1.D0
C     RECUPERATION DES DONNEES DANS LES "COMMON" ASTER
C     ================================================

C
C     RECUPERATION DE L'ADRESSE YACS
C     ------------------------------        
      CALL JEVEUO(AYACS,'L',IADR)
      ICOMPO=ZI(IADR) 
C      BOUCLE SUR LES PALIERS (NOEUDS ASTER)
      DO 10 IPAL=1,NBPAL
          FORCE(IPAL,1) = 0.D0    
          FORCE(IPAL,2) = 0.D0    
          FORCE(IPAL,3) = 0.D0    
C
C        LECTURE DES EFFORTS EN PROVENANCE D'EDYOS 
C        -----------------------------------------
          NOMVAR = 'FORCETE'//FINPAL(IPAL)
          TR8=0.D0 
          IF(TYPAL(IPAL).NE.'PACONL')THEN
             IDIM=3
          ELSE
             IDIM=4
          ENDIF  
          DO 20 IPAT=1,6
             PARAMR(IPAT)=0.D0
   20     CONTINUE
          NPAS=NUMPAS
          CALL CPLDB(ICOMPO,CPITER,TR8,TR8,NPAS,
     &               NOMVAR,IDIM,NLU,PARAMR,INFO)
          CALL ERRCOU (NOMPRG,NPAS,NOMVAR,INFO,IDIM,NLU)
C         ECRITURE DES VALEURS RECUES
          IF (NIV.GE.2) THEN
            WRITE(IFM,*)'==== ASTEREDYOS :',NOMPRG,' NUMPAS =======',
     &        NUMPAS
            WRITE(IFM,*)'ASTEREDYOS: ',NOMPRG,
     &        ' ASTER  LECTURE EFFORTS EDYOS'
            WRITE(IFM,*)'ASTEREDYOS: ',NOMPRG,' NUMERO PALIER: ',
     &        IPAL  
            WRITE(IFM,*)'ASTEREDYOS: ',NOMPRG,' TYPE PALIER: ',
     &        TYPAL(IPAL)
            WRITE(IFM,*)'ASTEREDYOS: ',NOMPRG,' NOEUD PALIER: ',
     &        CNPAL(IPAL)     
            WRITE(IFM,*)'ASTEREDYOS: ',NOMPRG,' CONVERGENCE: ',PARAMR(1)
            WRITE(IFM,*)'ASTEREDYOS: ',NOMPRG,' WX : ',PARAMR(2)
            WRITE(IFM,*)'ASTEREDYOS: ',NOMPRG,' WY : ',PARAMR(3)
            WRITE(IFM,*)'ASTEREDYOS: ',NOMPRG,' PACONL: PARAMR(4): ',
     &        PARAMR(4)
            WRITE(IFM,*)'ASTEREDYOS: '
            WRITE(IFM,*)'=========  ASTEREDYOS :  ',NOMPRG,'   ========'
          ENDIF
C
C         VERIFICATION DE LA CONVERGENCE
C         ------------------------------
          CONV = PARAMR(1)
          IF(CONV.LE.0.D0) CALL U2MESS('A','EDYOS_45')
C
C         AFFECTATION DES EFFORTS RECU D'EDYOS
C         ------------------------------------
C
C         REACTION HYDRAULIQUE EN X (CORRESPOND AU Z DE EDYOS)
C          ICOMP=N_DDL(IPAL,1)  
          FORCE(IPAL,1) = PARAMR(2)
C   
C         REACTION HYDRAULIQUE EN Y
C          ICOMP=N_DDL(IPAL,2)  
          FORCE(IPAL,2) = PARAMR(3)   
C          IF(TYPAL(IPAL) .EQ. 'PACONL')THEN
C            CFRTA = PARAMR(4)
C          ENDIF
C         EN CAS DE REPRIS ULTERIEURE (A SUPPRIMER)
C         ------------------------------------------
C         (AU 16/02/09 ON NE FAIT RIEN AVEC CES DONNEES JUSTE POUR LA
C             CONCORDANCE AVEC EDYOS)
C
          IF(REPRIS) THEN
             IDIM=6*NSMAX
             NOMVAR = 'REPRISEASTER'//FINPAL(IPAL)
C            LECTURE DES DONNEES ASSOCIES AUX PATINS (PAPANL)
C            ------------------------------------------------
             IF(TYPAL(IPAL) .EQ. 'PAPANL')THEN
               IF(PRDEFF)THEN
                 IF (NIV.GE.2)
     &             WRITE(IFM,*)'ASTEREDYOS : PAPANL - NOMVAR ',NOMVAR
                 NPAS=NUMPAS
                 CALL CPLDB(ICOMPO,CPITER,TR8,TR8,NPAS,
     &                     NOMVAR,IDIM,NLU,MVTPAT,INFO)
                 CALL ERRCOU (NOMPRG,NPAS,NOMVAR,INFO,IDIM,NLU)
               ENDIF
             ENDIF
C            LECTURE DES DONNEES ASSOCIES A PACONL
C            -------------------------------------
             IF(TYPAL(IPAL) .EQ. 'PACONL')THEN                
               IF(PRDEFF)THEN
                 IF (NIV.GE.2)
     &             WRITE(IFM,*)'ASTEREDYOS : PAPANL - NOMVAR ',NOMVAR
                 NPAS=NUMPAS
                 CALL CPLDB(ICOMPO,CPITER,TR8,TR8,NPAS,
     &                     NOMVAR,IDIM,NLU,MVTCON,INFO)
                 CALL ERRCOU (NOMPRG,NPAS,NOMVAR,INFO,IDIM,NLU)
               ENDIF
             ENDIF
          ENDIF
C         FIN DE CONSTITUTION DU FICHIER EN CAS DE REPRISE
   10 CONTINUE
C      FIN DE LA BOUCLE SUR LES PALIERS
      CALL JEDEMA()
C
      END
