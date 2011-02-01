      SUBROUTINE LECDON(FICEXT,UNITPA,PRDEFF) 
      IMPLICIT          NONE
      INTEGER           UNITPA
      LOGICAL           PRDEFF,FICEXT
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 31/01/2011   AUTEUR GREFFET N.GREFFET 
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
C  LECDON : FONCTION
C  -----------------
C CE SSP PERMET DE LIRE LES DONNEES RELATIVES AUX PALIERS CONTENUES DANS
C  LE FICHIER FIC_DON, SOIT:
C          - LE NOMBRE DE PALIERS
C          - (POUR CHAQUE PALIER) LE NOEUD ASTER D'APPLICATION ET 
C          LE TYPE DE PALIER
C=======================================================================
C  REFERENCES BIBLIOGRAPHIQUES
C  ---------------------------
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
C  ___________________________________________________________________
C !    NOM   !   TYPE      !                  ROLE                    !
C !__________!_____________!__________________________________________!
C !          !             !                                          !
C ! CARAC"I" !  CHARACTER  !  VARIABLE TAMPON PERMETTANT DE FAIRE     !
C !          !             !  CORRESPONDRE A UN NOMBRE SON EQUIVALENT !
C !          !             !  EN CARACERES                            !
C !          !             !                                          !
C ! UNITPA   !  ENTIER     !  UNITE DE LECTURE                        !
C !          !             !                                          !
C ! CTYPE    !  CHARACTER*6!  TYPE DU PALIER LU                       !
C !          !             !                                          !
C ! IPAL     !  ENTIER     !  INDICE DE BOUCLE SUR LES PALIERS        !
C !          !             !                                          !
C ! NUMPAL   !  ENTIER     !  NUMERO DU PALIER LU                     !
C !          !             !                                          !
C ! NOMPRG   !  CHARACTER  !  NOM DU SSP (POUR ECRITURE DANS ERRCOU)  !
C !          !             !                                          !
C ! PALMAX   !  ENTIER     !  NOMBRE MAXIMUM DE PALIERS (PARAMETER)   !
C !          !             !                                          !
C ! DIMNAS   !  CHARACTER  !  NOMBRE DE DDL POUR UN NOEUD             !
C !          !             !                                          !
C !__________!_____________!__________________________________________!






C  COMMON ZI (TYPE: INTEGER) (NOM = 'NPAL')
C  _____________________________________________________________________
C !              !             !                                       !
C ! NBPAL        !  ADR        !  NOMBRE DE PALIERS POUR L'ETUDE       !
C !              !             !                                       !
C !______________!_____________!_______________________________________!
C


C  COMMON ZK8 (TYPE: CHARACTER*8) (NOM = 'C_PAL')
C  _____________________________________________________________________
C !              !             !                                       !
C ! TYPPAL(IPAL) ! ADR+(IPAL-1)!  TYPE DU PALIER CONSIDERE             !
C !              !             !                                       !
C ! FINPAL(IPAL) !  ADR+PALMAX !  TERMINAISON POUR LE PALIER CONSIDERE !
C !              !  +(IPAL-1)  !  PALIER N°I => _I                     !
C !              !             !                                       !
C ! CNPAL(IPAL)  ! ADR+2*PALMAX!  NOM DU NOEUD ASTER POUR LE PALIER    !
C !              !  +(IPAL-1)  !  CONSIDERE                            !
C !______________!_____________!_______________________________________!
C
C TOLE CRS_512 CRP_4

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
      INTEGER           IFM, NIV, ULISOP
      CHARACTER*8       NOMPRG
      PARAMETER(NOMPRG='LECDON')
C     
      CHARACTER*1       CARAC1 
      CHARACTER*2       CARAC2
C
      INTEGER           IPAL, NUMPAL, NGR, N2
      INTEGER           NBPAL, IRET
      CHARACTER*6       CTYPE
C    
      INTEGER           PALMAX 
      PARAMETER (PALMAX=20)
      CHARACTER*6       TYPPAL(PALMAX)
      CHARACTER*3       FINPAL(PALMAX)
      CHARACTER*8       CNPAL(PALMAX),CNOD, K8B   
C
      INTEGER           ZCPAL, ZNPAL  
      CHARACTER*16      K16NOM
      CHARACTER*24      CPAL, NPAL
C
      CALL JEMARQ()
      NIV = 0
      CALL INFDBG('YACS_EDYOS',IFM,NIV)
C
C     ASSIGNATION DES NOMS POUR LES ADRESSES DANS LES COMMON ASTER
C     ------------------------------------------------------------
      CPAL='C_PAL'
      NPAL='N_PAL'
C
C     RESERVATION MEMOIRE POUR LES "COMMON"  ASTER
C     -------------------------------------------- 
      CALL JEEXIN(CPAL,IRET)
      IF (IRET.EQ.0) THEN
        CALL WKVECT(CPAL,'G V K8',(3*PALMAX),ZCPAL)
      ELSE
        CALL JEVEUO(CPAL,'E',ZCPAL)
      ENDIF
      CALL JEEXIN(NPAL,IRET)
      IF (IRET.EQ.0) THEN
        CALL WKVECT(NPAL,'G V I',(1+PALMAX),ZNPAL)
      ELSE
        CALL JEVEUO(NPAL,'E',ZNPAL)
      ENDIF

      IF ( FICEXT ) THEN
C   
C     LECTURE DU FICHIER FIC_DON
C     -------------------------- 
      IF (NIV.GE.2)
     &  WRITE(IFM,*)'ASTEREDYOS: ',NOMPRG,
     &  ' DEBUT LECTURE DU FICHIER FIC_DON UNITE LOGIQUE ',UNITPA
      K16NOM ='                '
      IF ( ULISOP ( UNITPA, K16NOM ) .EQ. 0 )  THEN
        CALL ULOPEN ( UNITPA,' ',' ','NEW','O')
      ENDIF
      READ(UNITPA,*)NBPAL
      IF (NIV.GE.2)
     & WRITE(IFM,*)'ASTEREDYOS: ',NOMPRG,' ON A LU NBPAL =',NBPAL
      IF (NBPAL.GT.PALMAX) CALL U2MESS('F','EDYOS_43')
C 
C     REMPLISSAGE "COMMON" ASTER POUR LE NOMBRE DE PALIERS 
C     ----------------------------------------------------
      ZI(ZNPAL)=NBPAL
C
C     BOUCLE DE LECTURE SUR LES PALIERS
C     ---------------------------------
      DO 100 IPAL=1,NBPAL
         CNOD='        '
         READ(UNITPA,*)NUMPAL,CNOD,CTYPE
         TYPPAL(NUMPAL)=CTYPE
         CNPAL(IPAL)=CNOD
C
         IF(IPAL.LT.10)THEN
           WRITE(CARAC1,'(I1)')IPAL
           FINPAL(IPAL)='_'//CARAC1
         ELSE
           WRITE(CARAC2,'(I2)')IPAL
           FINPAL(IPAL)='_'//CARAC2
         ENDIF
C 
C
C   REMPLISSAGE "COMMON" ASTER POUR LES PALIERS (TYPE,TERMINAISON,NOEUD)
C   --------------------------------------------------------------------
         ZK8(ZCPAL+(IPAL-1))=CTYPE
         ZK8(ZCPAL+PALMAX+(IPAL-1))=FINPAL(IPAL)
         ZK8(ZCPAL+(2*PALMAX)+(IPAL-1))=CNPAL(IPAL)
         IF (NIV.GE.2) 
     &      WRITE(IFM,*)'ASTEREDYOS : LECDON : CTYPE - FINPAL - CNPAL=',
     &      CTYPE,' -- ',FINPAL(IPAL),' -- ',CNPAL(IPAL)
C 
C   REMPLISSAGE "COMMON" ASTER POUR LES NUMEROS DES NOEUDS DES PALIERS 
C   ------------------------------------------------------------------
         ZI(ZNPAL+1+(IPAL-1))=IPAL
 100  CONTINUE    
C
C     FIN DE BOUCLE DE LECTURE SUR LES PALIERS
C     ----------------------------------------
      PRDEFF = .TRUE.
C 
C     ECRITURE DES VARIABLES LUES 
C     ---------------------------     
      IF (NIV.GE.2) THEN
        WRITE(IFM,*)'ASTEREDYOS: ',NOMPRG,
     &              ' - FIN LECTURE DU FICHIER FIC_DON '
        WRITE(IFM,*)'ASTEREDYOS: ',NOMPRG,' - NOMBRE DE PALIERS: ',NBPAL
        DO 110 IPAL=1,NBPAL
          WRITE(IFM,*)'ASTEREDYOS PALIER :',IPAL,' TYPE :',TYPPAL(IPAL),
     &               ' NOEUD ASTER : ',CNPAL(IPAL)
 110    CONTINUE
      ENDIF
C
      IF ( ULISOP ( UNITPA, K16NOM ) .NE. 0 )
     &   CALL ULOPEN ( -UNITPA,' ',' ','NEW','O')
      
      ELSE
C
C   LECTURE DES INFOS DEPUIS LE FICHIER DE COMMANDE
C   --------------------------------------------------------------------
        CALL GETFAC( 'PALIER_EDYOS' , NBPAL )
        IF (NBPAL.GT.PALMAX) CALL U2MESS('F','EDYOS_43')
        ZI(ZNPAL)=NBPAL
        DO 201 IPAL = 1, NBPAL
          CALL GETVTX('PALIER_EDYOS','GROUP_NO',IPAL,1,0,K8B,N2)
          IF ( ABS(N2) .EQ. 0 ) THEN
            CALL GETVTX('PALIER_EDYOS','NOEUD',IPAL,1,0,K8B,N2)
            IF ( ABS(N2) .EQ. 0 ) THEN
              CALL U2MESS('F','EDYOS_49')
            ELSE
              NGR = -N2
              NGR = 1
              CALL GETVTX('PALIER_EDYOS','NOEUD',IPAL,1,NGR,
     &                 CNPAL(IPAL),N2)              
            ENDIF
          ELSE
            NGR = -N2
            NGR = 1
            CALL GETVTX('PALIER_EDYOS','GROUP_NO',IPAL,1,NGR,
     &                 CNPAL(IPAL),N2)
          ENDIF
          CALL GETVTX('PALIER_EDYOS','TYPE_EDYOS',IPAL,1,NGR,
     &                 CTYPE,N2)
          ZK8(ZCPAL+(2*PALMAX)+(IPAL-1))=CNPAL(IPAL)
          ZK8(ZCPAL+(IPAL-1))=CTYPE
 201    CONTINUE

          NIV = 3
      
        DO 101 IPAL=1,NBPAL
C
          IF(IPAL.LT.10)THEN
            WRITE(CARAC1,'(I1)')IPAL
            FINPAL(IPAL)='_'//CARAC1
          ELSE
            WRITE(CARAC2,'(I2)')IPAL
            FINPAL(IPAL)='_'//CARAC2
          ENDIF
          ZK8(ZCPAL+PALMAX+(IPAL-1))=FINPAL(IPAL)
          IF (NIV.GE.2) 
     &      WRITE(IFM,*)'ASTEREDYOS : LECDON : CTYPE - FINPAL - CNPAL=',
     &      ZK8(ZCPAL+(IPAL-1)),' -- ',FINPAL(IPAL),' -- ',
     &      ZK8(ZCPAL+(2*PALMAX)+(IPAL-1))
          ZI(ZNPAL+1+(IPAL-1))=IPAL
 101    CONTINUE
        IF (NIV.GE.2) THEN
          WRITE(IFM,*)'ASTEREDYOS: ',NOMPRG,
     &              ' - FIN LECTURE ARGUMENTS PALIERS '
          WRITE(IFM,*)'ASTEREDYOS: ',NOMPRG,' - NOMBRE PALIERS: ',NBPAL
        ENDIF
      ENDIF
      
      
      CALL JEDEMA()
C
      END
