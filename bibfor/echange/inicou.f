      SUBROUTINE INICOU(NBPAS, TINIT, TFIN, DT, DTSTO, VROTAT)  
      IMPLICIT NONE
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


C  INICOU : FONCTION

C    CE SSP PERMET (EN DEHORS DES BOUCLES TEMPORELLES) DE :
C        - INITIALISER LA LIAISON AVEC LE COUPLEUR

C        - DANS UNE BOUCLE SUR LES PALIERS
C            - ENVOYER (A EDYOS)LE NOM DU PALIER CONSIDERE 

C            - RECEVOIR (D'EDYOS)LE TYPE DU PALIER CONSIDERE 

C            - D'ENVOYER LES PARAMETRES REELS INITIAUX (TEMPS INITIAL
C              ET FINAL, PAS DE TEMPS, PAS DE TEMPS DE STOCKAGE,
C              VITESSE DE ROTATION EN TOURS/MINUTE)
             
 
C            - D'ENVOYER LES PARAMETRES ENTIERS INITIAUX (NOMBRE DE
C              PAS DE TEMPS EDYOS (VOIR COHERENCE AVEC VALEURS
C              PRECEDENTES), 0 ?)
             

C     INICOU ECHANGE UNIQUEMENT AVEC INITCO (EDYOS)


C=======================================================================

C  REFERENCES BIBLIOGRAPHIQUES
C  ---------------------------

C  NOTE HI-26/03/007A 
C  "DEVELOPPEMENT D'UN MODE PRODUCTION POUR CALCIUM: MANUEL UTILISATEUR"
C  FAYOLLE ERIC, DEMKO BERTRAND (CS SI)  JUILLET 2003

C  LES APPELS YACS SONT STRICTEMENTS IDENTIQUES A CEUX DE CALCIUM A 
C  L'EXCEPTION DU RAJOUT D'UN PREMIER ARGUMENT (ICOMPO) CORRESPONDANT 
C  A UNE ADRESSE NECESSAIRE A L'EXECUTION DE YACS

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
C !    NOM      !   TYPE    !                 ROLE                     !
C !_____________!___________!__________________________________________!
C !             !           !                                          !
C ! NBPAS       !  ENTIER*8 ! IN: NOMBRE DE PAS DE TEMPS TOTAL         !
C !             !           !                                          !
C ! TINIT       !  REEL*8   ! IN: INSTANT INITIAL                      !
C !             !           !                                          !
C ! TFIN        !  REEL*8   ! IN: INSTANT FINAL                        !
C !             !           !                                          !
C ! DT          !  REEL*8   ! IN: PAS DE TEMPS ASTER (ET EDYOS) INITIAL!
C !             !           !                                          !
C ! DTSTO       !  REEL*8   ! IN: PAS DE TEMPS POUR LE STOCKAGE EDYOS  !
C !             !           !                                          !
C ! VROTAT      !  REEL*8   ! IN: VITESSE DE ROTATION (TR/MN) DU ROTOR !
C !             !           !                                          !
C ! NUMPAS      !  ENTIER   ! NUMERO D'ITERATION (1 DANS CE SSP)       !
C !             !           ! PERMET DE FAIRE CORRESPONDRE LES INSTANTS!
C !             !           ! POUR LES ECHANGES VIA YACS               !
C !             !           !                                          !
C ! NLU         !  ENTIER   ! DIMENSION DE LA VARIABLE ECHANGEE        !
C !             !           ! (RENVOYE PAR YACS)                       !
C !             !           !                                          !
C ! UN,DEUX,CINQ!  ENTIER   ! DIMENSION DE LA VARIABLE ECHANGEE        !
C !             !           ! (TELLE QUE PROGRAMMEE DANS LE SSP)       !
C !             !           !                                          !
C ! NOMPAL      !CHARACTER*8! NOM DU PALIER CONSIDERE (ENVOYE A EDYOS) !
C !             !           !                                          !
C ! TYPPAL      !CHARACTER*6! TYPE DU PALIER CONSIDERE (RECU D'EDYOS)  !
C !             !           !                                          !
C ! PARAMR(6)   !  REEL*8   ! PARAMETRES DE TYPE REEL ENVOYES A EDYOS  !
C !             !           ! (TINIT, TFIN, DT, DTSTO, VROTAT)         !
C !             !           !                                          !
C ! PARAMI(2)   !  ENTIER   ! PARAMETRES DE TYPE ENTIER ENVOYES A EDYOS!
C !             !           ! (NBPAS, FLAG)                            !
C !             !           ! LE FLAG EST UN ENTIER (SI = 0 => CALCUL  !
C !             !           ! NORMAL, SI > 0 => REPRISE DE CALCUL)     !
C !             !           !                                          !
C ! INFO        !  ENTIER   ! FLAG DE RETOUR DE YACS INDIQUANT SI LE   !
C !             !           ! TRANSFERT S'EST BIEN EFFECTUE (INFO=0)   !
C !             !           ! DANS LE CAS CONTRAIRE CE FLAG EST        !
C !             !           ! INTERPRETE PAR LE SSP ERRCOU             !
C !             !           !                                          !
C ! TR8         !  REEL*8   ! NE SERT A RIEN, C'EST JUSTE UNE VARIABLE !
C !             !           ! NECESSAIRE POUR LES APPELS YACS          !
C !             !           ! (UTILE LORSQUE YACS UTILISE LE TEMPS     !
C !             !           ! POUR LA CORRESPONDANCE - VOIR CPITER)    !
C !             !           !                                          !
C ! TR4         !  REEL*4   ! NE SERT A RIEN, C'EST JUSTE UNE VARIABLE !
C !             !           ! NECESSAIRE POUR LES APPELS YACS          !
C !             !           ! (UTILE LORSQUE YACS UTILISE LE TEMPS     !
C !             !           ! POUR LA CORRESPONDANCE - VOIR CPITER)    !
C !             !           !                                          !
C ! PALMAX      !  ENTIER   ! NOMBRE MAXIMUM DE PALIERS (PARAMETER)    !
C !             !           !                                          !
C ! NOMPRG      !  CHARACTER! NOM DU SSP (POUR ECRITURE DANS ERRCOU)   !
C !             !           !                                          !
C !             !           !                                          !
C !_____________!___________!__________________________________________!



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
 



C "COMMON" ASTER
C --------------

C  COMMON ZI (TYPE: INTEGER) (NOM = '&ADR_YACS')
C  _____________________________________________________________________
C !          !             !                                           !
C ! ICOMPO   !  ADR        !  ADRESSE NECESSAIRE AUX APPELS YACS       !
C !__________!_____________!___________________________________________!
C





C  COMMON ZI (TYPE: INTEGER) (NOM = 'N_PAL')
C  _____________________________________________________________________
C !             !             !                                        !
C ! NBPAL       !  ADR        !  NOMBRE DE PALIERS POUR L'ETUDE        !
C !             !             !                                        !
C ! NOPAL(IPAL) !  ADR+1      !  NUMERO DU NOEUD ASTER POUR LE PALIER  !
C !             !  +(IPAL-1)  !  CONSIDERE                             !
C !_____________!_____________!________________________________________!
C


C  COMMON ZK8 (TYPE: CHARACTER*8) (NOM = 'C_PAL')
C  _____________________________________________________________________
C !               !             !                                      !
C ! TYPAL(IPAL)   ! ADR+(IPAL-1)!  TYPE DU PALIER CONSIDERE            !
C !               !             !                                      !
C ! FINPAL(IPAL)  !  ADR+PALMAX !  TERMINAISON POUR LE PALIER CONSIDERE!
C !               !  +(IPAL-1)  !  PALIER N°I => _I                    !
C !               !             !                                      !
C ! CNPAL(IPAL)   ! ADR+2*PALMAX!  NOM DU NOEUD ASTER POUR LE PALIER   !
C !               !  +(IPAL-1)  !  CONSIDERE                           !
C !_______________!_____________!______________________________________!
C


C
C=======================================================================
C  SOUS PROGRAMME(S) APPELE(S) : CP* (YACS), ERRCOU.F
C                                
C-----------------------------------------------------------------------
C  SOUS PROGRAMME(S) APPELANT(S) :  OP0115.F
C
C***********************************************************************
C%W% %D% %T%
C TOLE CRS_512 CRP_4 


C     ARGUMENTS
C     =========
      REAL*8        VROTAT, TINIT, TFIN ,DT , DTSTO, TMIN
      INTEGER       NBPAS     


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
C
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C     COMMON
C     ======
      INTEGER       ICOMPO
      INTEGER       NBPAL
C    
      INTEGER       PALMAX 
      PARAMETER (PALMAX=20)
      CHARACTER*3   FINPAL(PALMAX)
C     VARIABLES INTERNES
C     ==================
      CHARACTER*8   NOMPRG
      PARAMETER(NOMPRG='INICOU')

      INTEGER       IFM, NIV
      INTEGER*4     IAPP, NUMPAS, INFO, NLU, PARAMI(2)
      INTEGER*4     UN, DEUX, SIX
      PARAMETER (UN = 1, DEUX=2, SIX=6)      
      REAL*4        TR4
      REAL*8        TR8 , PARAMR(SIX)  
      CHARACTER*6   TYPPAL
      CHARACTER*8   NOMPAL 
C
C     ANCIENS INCLUDE (CALCIUM.H)
C     ===========================
      INTEGER*4     LENVAR
      PARAMETER (LENVAR = 144)
      CHARACTER*(LENVAR) NOMVAR
      INTEGER*4     CPITER
      PARAMETER (CPITER= 41)
C
      INTEGER       IADR,IADRI,IADRK
      CHARACTER*24  CPAL, NPAL,AYACS   
C        
C                          
C======================================================================
C
      TMIN=1.D-11

      CALL JEMARQ()
      NIV = 0
      CALL INFDBG('YACS_EDYOS',IFM,NIV)
      
      
C     ASSIGNATION DES NOMS POUR LES ADRESSES DANS LES COMMON ASTER
C     ------------------------------------------------------------
      CPAL='C_PAL'
      NPAL='N_PAL'
      AYACS='&ADR_YACS'
C
C     APPEL DU SSP DE LECTURE DES DONNEES (SUR LES PALIERS) 
C      => COMMON PALIERS
C     ----------------------------------------------------------------

C     RECUPERATION DES DONNEES DANS LES "COMMON" ASTER
C     ================================================


C     RECUPERATION DE L'ADRESSE YACS
C     ------------------------------        
      CALL JEVEUO(AYACS,'L',IADR)
      ICOMPO=ZI(IADR) 


C     RECUPERATION DES DONNEES ENTIERES SUR LES PALIERS 
C     -------------------------------------------------     
      CALL JEVEUO(NPAL,'L',IADRI)
      NBPAL=ZI(IADRI)    


C     RECUPERATION DES DONNEES ENTIERES SUR LES PALIERS            
      CALL JEVEUO(CPAL,'L',IADRK)
      DO 20 IAPP=1,NBPAL
          FINPAL(IAPP)=ZK8(IADRK+(IAPP-1)+PALMAX)
  20  CONTINUE



C
C------------------------------ECRITURE NOM PALIER----------------------
C
      NUMPAS = 1
      TR4 = 0.D0
      TR8 = 0.D0
      
      IF (NIV.GE.2)
     &  WRITE(IFM,*)'ASTEREDYOS: ',NOMPRG,'  NUM_PAS = ',NUMPAS,' ==='
      
      
C     DEBUT DE LA BOUCLE SUR LES PALIERS
      DO 100 IAPP = 1 , NBPAL
C
C        CREATION DU NOM DE VARIABLE YACS POUR LE NOM DU PALIER
C 
         NOMPAL = 'ANA1'//FINPAL(IAPP)
         NOMVAR = 'NOM_'//NOMPAL
C
C        --------   ENVOI DU NOM DU PALIER A EDYOS -------------
C
         IF (NIV.GE.2) THEN
           WRITE(IFM,'(A23,3X,A12)')'ASTEREDYOS: VARIABLE1: ',NOMVAR
           WRITE(IFM,'(A23,3X,A12)')'ASTEREDYOS: NOMPALIER: ',NOMPAL
         ENDIF
         INFO = 0
         CALL CPECH (ICOMPO,CPITER,TR4,NUMPAS,NOMVAR,UN,
     &               NOMPAL,INFO)
         CALL ERRCOU (NOMPRG,NUMPAS,NOMVAR,INFO,UN,UN)
C
         IF (NIV.GE.2) THEN
           WRITE(IFM,*)' '     
           WRITE(IFM,*)' '      
           WRITE(IFM,*)'ASTEREDYOS: ',NOMPRG,' PALIER SEND : ',
     &        NOMPAL(1:8)
         ENDIF
C
C
C        CREATION DU NOM DE VARIABLE YACS POUR LE TYPE DU PALIER
C 
         NOMVAR='TYPE_'//NOMPAL
         TYPPAL='      '
 
C
C        ----------  RECEPTION DU TYPE DU PALIER FROM EDYOS ----------
C
         CALL CPLCH (ICOMPO,CPITER,TR4,TR4,NUMPAS,NOMVAR,UN,
     &               NLU,TYPPAL,INFO)
         CALL ERRCOU (NOMPRG,NUMPAS,NOMVAR,INFO,UN,NLU)


         IF (NIV.GE.2) THEN
           WRITE(IFM,*)'ASTEREDYOS: ',NOMPRG,' TYPE DU PALIER: ',
     &        TYPPAL(1:6)
         ENDIF

C        -------ENVOI DES PARAMETRES REELS A EDYOS------------


         PARAMR ( 1 ) =  TINIT
         PARAMR ( 2 ) =  TFIN
         PARAMR ( 3 ) =  DT
         PARAMR ( 4 ) =  DTSTO
         PARAMR ( 5 ) =  VROTAT
         PARAMR ( 6 ) =  TMIN
         DTSTO=DT 
         NOMVAR='PARAMREEL'//FINPAL(IAPP)
C
         CALL CPEDB (ICOMPO,CPITER,TR8,NUMPAS,NOMVAR,SIX,
     &               PARAMR,INFO)
         CALL ERRCOU (NOMPRG,NUMPAS,NOMVAR,INFO,SIX,SIX)


C        ECRITURE DES PARAMETRES ENVOYES 
         IF (NIV.GE.0) THEN
           WRITE(IFM,*)'ASTEREDYOS :',NOMPRG,
     &       '- ASTER - ENVOI PARAMR A EDYOS'
           WRITE(IFM,*)'ASTEREDYOS : ',NOMPRG,' - TEMPS INITIAL : ',
     &        PARAMR(1)
           WRITE(IFM,*)'ASTEREDYOS : ',NOMPRG,' - TEMPS FINAL : ',
     &        PARAMR(2)
           WRITE(IFM,*)'ASTEREDYOS : ',NOMPRG,' - PAS DE TEMPS : ',
     &        PARAMR(3)
           WRITE(IFM,*)'ASTEREDYOS : ',NOMPRG,' - PAS STOCKAGE : ',
     &        PARAMR(4)
           WRITE(IFM,*)'ASTEREDYOS : ',NOMPRG,' - OMEGA : ',PARAMR(5)
           WRITE(IFM,*)'ASTEREDYOS : ',NOMPRG,' - PAS MINIMUM  : ',
     &        PARAMR(6)     
         ENDIF
C

C
C        -----------ENVOI PARAMETRES ENTIERS A EDYOS ----------------
C

         NOMVAR='PARAMENTI'//FINPAL(IAPP)
 
         PARAMI(1)=NBPAS
C         CALCUL NORMAL (PAS DE REPRISE) => PARAMI(2) = 0
         PARAMI(2)=0

         CALL CPEEN (ICOMPO,CPITER,TR4,NUMPAS,NOMVAR,DEUX,
     &               PARAMI,INFO)
         CALL ERRCOU (NOMPRG,NUMPAS,NOMVAR,INFO,DEUX,DEUX)


C        ECRITURE DES PARAMETRES ENVOYES 
         IF (NIV.GE.2) THEN
           WRITE(IFM,*)'ASTEREDYOS :',NOMPRG,
     &         '- ASTER - ENVOI PARAMI A EDYOS'
           WRITE(IFM,*)'ASTEREDYOS : ',NOMPRG,' - NBPAS : ',PARAMI(1)
           WRITE(IFM,*)'ASTEREDYOS : ',NOMPRG,' - PARAMI(2) (=0) :',
     &          PARAMI(2)
           WRITE(IFM,*)'** ASTEREDYOS: FIN DE ',NOMPRG,
     &          ' POUR LE PALIER:',IAPP
         ENDIF
C

 100  CONTINUE
C
C     FIN DE LA BOUCLE SUR LES PALIERS
C
      CALL JEDEMA()
C
      END
