      SUBROUTINE PKTEMP (NOMMAT, TCOEF )
      IMPLICIT   NONE
      CHARACTER*8 NOMMAT
      CHARACTER*24 TCOEF
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 30/01/2006   AUTEUR LEBOUVIE F.LEBOUVIER 
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
C ======================================================================
C
C     OPERATEUR POST_K1_K2_K3 : RECUPERTAION DES CARACTERISTIQUES
C                               MATERIAU LORSQU'ELLES DEPENDENT DE
C                               LA TEMPERATURE
C     ------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
      CHARACTER*32       JEXNUM , JEXNOM  , JEXATR
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
      INTEGER  NBPAT, N1, N2, NBORDR, IRET, JORDR, IORD, NORDR, JPARA
      INTEGER  JINFC, NBCHAR, JLCHA, ICHAR, INO, NBNOFO, JNOFO, NUMNOE
      INTEGER  JCHTH, JCNSD, JCNSV, JCNSL, NCMP, JTEMP, NBPATR, IAD
      INTEGER  IBID
      PARAMETER(NBPAT=9,NBPATR=8)     
      REAL*8 PREC,VALPAR, VALRES(2), E, NU, COEFD, UNMNU2, UNPNU
      REAL*8 COEFD3, COEFG, COEFG3, R8DEPI, VALRET(NBPATR)
      COMPLEX*16  CBID
      CHARACTER*2  TYPPAT(NBPAT), CODRET(2)
      CHARACTER*8  NOMPAT(NBPAT), NOMRES, RESU, CRIT, K8B, MA, FOND
      CHARACTER*8  THER, NOMPAR, NOMVAL(2), NOMNOE
      CHARACTER*16 CONCEP, NOMCMD
      CHARACTER*19 CHAMP, CHAMS
      CHARACTER*24 KNUM, CHTHER, FONNOE
      DATA  NOMPAT / 'INST' , 'NOEUD' , 'TEMP' , 'E' , 'NU', 'COEFD',
     &               'COEFD3', 'COEFG', 'COEFG3'/
      DATA  TYPPAT / 'R','K8','R','R','R','R','R','R','R' /
C ----------------------------------------------------------------------
      CALL JEMARQ()
C     
      CALL GETRES ( NOMRES , CONCEP , NOMCMD )
C 
      CALL TBCRSD( TCOEF, 'V' )
      CALL TBAJPA( TCOEF, NBPAT, NOMPAT, TYPPAT)
C
      CALL GETVID ( ' ', 'RESULTAT' , 1,1,1, RESU, N1 )
      CALL GETVID ( ' ', 'MAILLAGE',  1,1,1, MA,   IRET )
      CALL GETVR8 ( ' ', 'PRECISION', 1,1,1, PREC, N1 )
      CALL GETVTX ( ' ', 'CRITERE'  , 1,1,1, CRIT, N2 )
      CALL GETVID ( ' ', 'FOND_FISS', 1,1,1, FOND, IRET )
C
C --- NOEUDS DU FOND DE FISSURE
      FONNOE = FOND//'.FOND      .NOEU'
      CALL JELIRA ( FONNOE, 'LONMAX', NBNOFO, K8B )
      CALL JEVEUO ( FONNOE, 'L', JNOFO )
C
C --- NUMEROS D'ORDRE 
      KNUM = '&&PKTEMP.NUME_ORDRE'
      CALL RSUTNU ( RESU, ' ', 1, KNUM, NBORDR, PREC, CRIT, IRET )
      IF (IRET.NE.0) THEN
         CALL UTMESS('F','PKTEMP','ERREUR(S) DANS LES DONNEES')
      ENDIF
      CALL JEVEUO ( KNUM, 'L', JORDR )
C
      NOMVAL(1) = 'E'
      NOMVAL(2) = 'NU'
      NOMPAR='TEMP'
C
C     CREATION DE LA TABLE DES CARACTERISTIQUES MATERIAU
C     ==================================================
C
C --- BOUCLE SUR LES NUMEROS D'ORDRE
      DO 10 IORD = 1, NBORDR
         NORDR = ZI(JORDR+IORD-1)
         CALL RSADPA(RESU,'L',1,'EXCIT',NORDR,0,JPARA,K8B)
         CALL JEVEUO(ZK24(JPARA)(1:19)//'.INFC','L',JINFC)
         CALL JEVEUO(ZK24(JPARA)(1:19)//'.LCHA','L',JLCHA)
         NBCHAR=ZI(JINFC)
C
C ---    ON RECUPERE LE CHARGEMENT THERMIQUE
         DO 20 ICHAR = 1 , NBCHAR
            CHTHER=ZK24(JLCHA+ICHAR-1)(1:8)//'.CHME.TEMPE.TEMP'
            CALL JEEXIN(CHTHER,IRET)
            IF(IRET.NE.0)GOTO 100
20       CONTINUE
100      CONTINUE
C
C ---    ON RECUPERE LA SD_RESULTAT THERMIQUE ...
         CALL JEVEUO(CHTHER,'L',JCHTH)
         THER=ZK8(JCHTH)
C
C ---    ON Y EXTRAIT LE CHAMP THERMIQUE
         CHAMP='&&CHAMP_DE_TEMPER'
         CALL RSEXCH(THER,'TEMP',NORDR,CHAMP,IRET)
C       
         CALL JEVEUO(CHAMP//'.VALE','L',JTEMP)

C ---    INSTANT DE CALCUL DU RESULTAT THERMIQUE         
         CALL RSADPA (THER, 'L', 1, 'INST', NORDR, 0, IAD, K8B )
         VALRET(1) = ZR(IAD)
C
C ---    BOUCLE SUR LES NOEUDS DU FOND DE FISSURE
         DO 30 INO=1,NBNOFO
           NOMNOE=ZK8(JNOFO+INO-1)
           CALL JENONU(JEXNOM(MA//'.NOMNOE',NOMNOE),NUMNOE)
           VALPAR=ZR(JTEMP+NUMNOE-1)
           CALL RCVALE ( NOMMAT, 'ELAS', 1, NOMPAR, VALPAR, 2,
     +              NOMVAL, VALRES, CODRET, 'F ' )
           E  = VALRES(1)
           NU = VALRES(2)
           COEFD = E * SQRT( R8DEPI() )
           UNMNU2 = 1.D0 - ( NU * NU )
           UNPNU  = 1.D0 + NU
           COEFD = COEFD / ( 8.D0 * UNMNU2 )
           COEFD3 = E*SQRT(R8DEPI()) / ( 8.D0 * UNPNU )
           COEFG = UNMNU2 / E
           COEFG3 = UNPNU / E
           VALRET(2)= VALPAR
           VALRET(3)= E
           VALRET(4)= NU
           VALRET(5)= COEFD
           VALRET(6)= COEFD3
           VALRET(7)= COEFG
           VALRET(8)= COEFG3
           CALL TBAJLI(TCOEF,NBPAT,NOMPAT,IBID,VALRET,CBID,NOMNOE,0)
 30      CONTINUE
C
10    CONTINUE
C
C      CALL TBIMPR(TCOEF,' ','ASTER',8,NBPAT,NOMPAT,0,' ',
C     &   '1PE12.5','RI')
C
      CALL JEDEMA()
C
      END
