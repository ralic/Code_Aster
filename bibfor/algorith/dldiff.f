      SUBROUTINE DLDIFF(T0,LCREA,LAMORT,NEQ,IMAT,
     &                  MASSE,RIGID,AMORT,
     &                  DEP0,VIT0,ACC0,
     &                  NCHAR,NVECA,LIAD,LIFO,
     &                  MODELE,MATE,CARELE,
     &                  CHARGE,INFOCH,FOMULT,NUMEDD,NUME,INPSCO,NBPASE)
      IMPLICIT  REAL*8  (A-H,O-Z)
      CHARACTER*8  MASSE, RIGID, AMORT
      CHARACTER*24 MODELE, CARELE, CHARGE, FOMULT, MATE, NUMEDD
      CHARACTER*24 INFOCH, LIFO(*)
      REAL*8       DEP0(*),VIT0(*),ACC0(*),T0
      INTEGER      NEQ,IMAT(*),LIAD(*),NCHAR,NVECA,NUME
      LOGICAL      LAMORT, LCREA
      CHARACTER*(*) INPSCO
      INTEGER      NBPASE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 15/06/2005   AUTEUR VABHHTS J.PELLET 
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
C TOLE CRP_21
C     ------------------------------------------------------------------
C     CALCUL MECANIQUE TRANSITOIRE PAR INTEGRATION DIRECTE
C     AVEC  METHODE EXPLICITE :  DIFFERENCES CENTREES
C                                                 
C     ------------------------------------------------------------------
C
C  HYPOTHESES :                                                "
C  ----------   SYSTEME CONSERVATIF DE LA FORME  K.U    +    M.U = F
C           OU                                           '     "
C               SYSTEME DISSIPATIF  DE LA FORME  K.U + C.U + M.U = F
C
C     ------------------------------------------------------------------
C  IN  : T0        : INSTANT DE CALCUL INITIAL
C  IN  : LCREA     : LOGIQUE INDIQUANT SI IL Y A REPRISE
C  IN  : LAMORT    : LOGIQUE INDIQUANT SI IL Y A AMORTISSEMENT
C  IN  : NEQ       : NOMBRE D'EQUATIONS
C  IN  : IMAT      : TABLEAU D'ADRESSES POUR LES MATRICES
C  IN  : MASSE     : MATRICE DE MASSE
C  IN  : RIGID     : MATRICE DE RIGIDITE
C  IN  : AMORT     : MATRICE D'AMORTISSEMENT
C  IN  : NCHAR     : NOMBRE D'OCCURENCES DU MOT CLE CHARGE
C  IN  : NVECA     : NOMBRE D'OCCURENCES DU MOT CLE VECT_ASSE
C  IN  : LIAD      : LISTE DES ADRESSES DES VECTEURS CHARGEMENT (NVECT)
C  IN  : LIFO      : LISTE DES NOMS DES FONCTIONS EVOLUTION (NVECT)
C  IN  : MODELE    : NOM DU MODELE
C  IN  : MATE      : NOM DU CHAMP DE MATERIAU
C  IN  : CARELE    : CARACTERISTIQUES DES POUTRES ET COQUES
C  IN  : CHARGE    : LISTE DES CHARGES
C  IN  : INFOCH    : INFO SUR LES CHARGES
C  IN  : FOMULT    : LISTE DES FONC_MULT ASSOCIES A DES CHARGES
C  IN  : NUMEDD    : NUME_DDL DE LA MATR_ASSE RIGID
C  IN  : NUME      : NUMERO D'ORDRE DE REPRISE
C  IN  : NBPASE   : NOMBRE DE PARAMETRE SENSIBLE
C  IN  : INPSCO   : STRUCTURE CONTENANT LA LISTE DES NOMS (CF. PSNSIN)
C  VAR : DEP0      : TABLEAU DES DEPLACEMENTS A L'INSTANT N
C  VAR : VIT0      : TABLEAU DES VITESSES A L'INSTANT N
C  VAR : ACC0      : TABLEAU DES ACCELERATIONS A L'INSTANT N
C
C    ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32      JEXNUM, JEXNOM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ---------------------------

      CHARACTER*1   TYPSOL
      CHARACTER*4   TYP1(3)
      CHARACTER*8   K8B, NOMRES
      CHARACTER*8   MATREI, MAPREI, BASENO, RESULT
      CHARACTER*16  TYPRES, NOMCMD,TYPE(3)
      CHARACTER*19  KREFE
      CHARACTER*19  LISARC
      CHARACTER*24  CHAMNO, LISINS
      CHARACTER*24  SOP,VAPRIN
      REAL*8        TPS1(4) 
      REAL*8        T1, DT, DTM
      REAL*8        OMEG, DEUXPI
      INTEGER       IWK1,IWK2,IFORC1,IDEPL1,IVITE1,IACCE1
      INTEGER       NRPASE,IBID,IRET,JAUX,JSTD,IRESOL

C
C     -----------------------------------------------------------------
      CALL JEMARQ()
C
      TYPSOL = 'R'

C-----RECUPERATION DU NIVEAU D'IMPRESSION
C
      CALL INFNIV(IFM,NIV)
C
C     --- RECUPERATION NOM DE LA COMMANDE ---
C
      CALL GETRES ( NOMRES, TYPRES, NOMCMD )
C
C     --- VECTEURS DE TRAVAIL SUR BASE VOLATILE ---
C
      CALL WKVECT('&&DLDIFF.FORCE1','V V R',NEQ,IFORC1)
      CALL WKVECT('&&DLDIFF.F0'    ,'V V R',NEQ,IWK0  )
      CALL WKVECT('&&DLDIFF.F1'    ,'V V R',NEQ,IWK1  )
      CALL WKVECT('&&DLDIFF.F2'    ,'V V R',NEQ,IWK2  )
      CALL WKVECT('&&DLDIFF.DEPL1' ,'V V R',NEQ,IDEPL1 )
      CALL WKVECT('&&DLDIFF.VITE1' ,'V V R',NEQ,IVITE1 )
      CALL WKVECT('&&DLDIFF.VITE2' ,'V V R',NEQ,IVITE2 ) 
      CALL WKVECT('&&DLDIFF.ACCE1' ,'V V R',NEQ,IACCE1 )
C
      DEUXPI = R8DEPI()
      IARCHI = NUME
      LISINS = ' '
C
C     --- PARAMETRES D'INTEGRATION ---
C
      CALL GETVR8('INCREMENT','INST_FIN',1,1,1,T1,N1)
      CALL GETVR8('INCREMENT','PAS',1,1,1,DT,N1)
      IF (N1.EQ.0) 
     +   CALL UTMESS('F','DLDIFF','METHODE DES DIFFERENCES CENTREES: '//
     +               ' LA DONNEE DU PAS EST OBLIGATOIRE ')
      IF (DT.EQ.0.D0) 
     +   CALL UTMESS('F','DLDIFF','METHODE DES DIFFERENCES CENTREES: '//
     +               ' LE PAS NE PEUT PAS ETRE NUL  ')
      NPATOT = NINT((T1-T0)/DT)
C
C     --- EXTRACTION DIAGONALE M ET CALCUL VITESSE INITIALE---
C
      CALL DISMOI ('I','SUR_OPTION',MASSE,'MATR_ASSE',IBID,SOP,IE)
      IF (SOP.EQ.'MASS_MECA_DIAG') THEN
        CALL EXTDIA (MASSE, NUMEDD, 2, ZR(IWK1))
      ELSE
        CALL UTMESS ('F','DLDIFF_01','LES MATRICES DE MASSE '
     &             //'ELEMENTAIRES DOIVENT OBLIGATOIREMENT AVOIR '
     &             //'ETE CALCULEES AVEC L''OPTION MASS_MECA_DIAG')
      ENDIF
      DO 10 I=1, NEQ
        IF (ZR(IWK1+I-1).NE.0.D0) THEN
           ZR(IWK1+I-1)=1.0D0/ZR(IWK1+I-1)
        ENDIF
        DO 12 NRPASE=0, NBPASE
          VIT0(NEQ*NRPASE+I) = VIT0(NEQ*NRPASE+I)
     &                         -DT/2.0D0*ACC0(NEQ*NRPASE+I)
 12     CONTINUE       
 10   CONTINUE       
C
C     --- VERIFICATION DU PAS DE TEMPS ---
C
      CALL EXTDIA( RIGID, NUMEDD, 2, ZR(IWK2))
      IMAX=0
      DTMAX=DT
      DO  11 I=1,NEQ
       IF (ZR(IWK1+I-1).NE.0.D0) THEN
         OMEG = SQRT( ZR(IWK2+I-1) * ZR(IWK1+I-1) ) 
         DTM = 5.D-02*DEUXPI/OMEG
         IF (DTMAX.GT.DTM) THEN 
           DTMAX=DTM
           IMAX=1
         ENDIF
       ENDIF
 11   CONTINUE       
       IF (IMAX.EQ.1) THEN
          NPATOT = NINT((T1-T0)/DTMAX)
          CALL UTDEBM('F','DLDIFF','PAS TROP GRAND')
          CALL UTIMPR('L','PAS UTILISATEUR TROP GRAND:',1,DT)
          CALL UTIMPR('L','PAS MAX POUR LE CALCUL:',1,DTMAX)       
          CALL UTIMPK('L','!!!!!!!!!!!!!!!!!!!!!!',1,' ')
          CALL UTIMPK('L','AVEC LE PAS MAX,   ',1,' ')
          CALL UTIMPI('L','NB DE PAS DE CALCUL : ',1,NPATOT)
          CALL UTFINM()
       ENDIF
C
C     --- ARCHIVAGE ---
C
      NBSORT = 3
      TYPE(1) = 'DEPL'
      TYPE(2) = 'VITE'
      TYPE(3) = 'ACCE'
      LISARC = '&&DLDIFF.ARCHIVAGE'
      CALL  DYARCH ( NPATOT, LISINS, LISARC, NBORDR, 1, NBEXCL, TYP1 )
      CALL JEVEUO(LISARC,'E',JSTOC)
      IF ( NBEXCL .EQ. NBSORT )
     +   CALL UTMESS('F',NOMCMD,'ON ARCHIVE AU MOINS UN CHAMP.')
      DO 50 I = 1,NBEXCL
         IF (TYP1(I).EQ.'DEPL') THEN
            TYPE(1) = '    '
         ELSEIF (TYP1(I).EQ.'VITE') THEN
            TYPE(2) = '    '
         ELSEIF (TYP1(I).EQ.'ACCE') THEN
            TYPE(3) = '    '
         ENDIF
 50   CONTINUE
C
C     --- AFFICHAGE DE MESSAGES SUR LE CALCUL ---
C
      WRITE(IFM,*)
     +'-------------------------------------------------'
      WRITE(IFM,*)
     +'--- CALCUL PAR INTEGRATION TEMPORELLE DIRECTE ---'
      WRITE(IFM,*)
     +'! LA MATRICE DE MASSE EST         : ',MASSE
      WRITE(IFM,*)
     +'! LA MATRICE DE RIGIDITE EST      : ',RIGID
      IF ( LAMORT ) WRITE(IFM,*)
     +'! LA MATRICE D''AMORTISSEMENT EST : ',AMORT
      WRITE(IFM,*)
     +'! LE NB D''EQUATIONS EST          : ',NEQ
      IF ( NUME.NE.0 ) WRITE(IFM,*)
     +'! REPRISE A PARTIR DU NUME_ORDRE  : ',NUME
      WRITE(IFM,*)'! L''INSTANT INITIAL EST        : ',T0
      WRITE(IFM,*)'! L''INSTANT FINAL EST          : ',T1
      WRITE(IFM,*)'! LE PAS DE TEMPS DU CALCUL EST : ',DT
      WRITE(IFM,*)'! LE NB DE PAS DE CALCUL EST    : ',NPATOT
      WRITE(IFM,*)
     +'----------------------------------------------',' '
C
C --- BOUCLE CREATION CONCEPTS RESULTAT (STANDARD + SENSIBILITE)
      DO 521 , NRPASE = 0 , NBPASE
C
C     --- NOM DES STRUCTURES,  JAUX=3 => LE NOM DU RESULTAT
       JAUX = 3
       CALL PSNSLE ( INPSCO, NRPASE, JAUX, RESULT )
C
C     --- CREATION DE LA STRUCTURE DE DONNEE RESULTAT ---
C
       IF ( LCREA ) THEN
         IARCHI = 0
         CALL RSCRSD(RESULT,TYPRES,NBORDR)
         KREFE(1:19) = RESULT
         CALL WKVECT(KREFE//'.REFD','G V K24',3,LREFE)
         ZK24(LREFE  ) = MASSE
         ZK24(LREFE+1) = AMORT
         ZK24(LREFE+2) = RIGID
         CALL JELIBE(KREFE//'.REFD')
C
         DO 30 ITYPE = 1, NBSORT
            IF ( TYPE(ITYPE) .EQ. '    ' ) GOTO 30
            CALL RSEXCH(RESULT,TYPE(ITYPE),IARCHI,CHAMNO,IER)
            IF ( IER .EQ. 0 ) THEN
            CALL UTMESS('A',NOMCMD,'CHAMP "'//CHAMNO//'" DEJA EXISTANT')
            ELSE IF ( IER .EQ. 100 ) THEN
               CALL VTCREM(CHAMNO,MASSE,'G','R')
            ELSE
               CALL UTMESS('F',NOMCMD,'APPEL ERRONE')
            ENDIF
            CHAMNO(20:24)  = '.VALE'
            CALL JEVEUO(CHAMNO,'E',LVALE)
            IF (ITYPE.EQ.1) THEN
                DO 32 IEQ = 1, NEQ
                   ZR(LVALE+IEQ-1) = DEP0(IEQ+NEQ*NRPASE)
 32             CONTINUE
            ELSE IF (ITYPE.EQ.2) THEN
                DO 33 IEQ = 1, NEQ
                   ZR(LVALE+IEQ-1) = VIT0(IEQ+NEQ*NRPASE)
 33             CONTINUE 
            ELSE 
                DO 34 IEQ = 1, NEQ
                   ZR(LVALE+IEQ-1) = ACC0(IEQ+NEQ*NRPASE)
 34             CONTINUE
            ENDIF  
            CALL JELIBE(CHAMNO)
            CALL RSNOCH(RESULT,TYPE(ITYPE),IARCHI,' ')
 30      CONTINUE
         CALL RSADPA(RESULT,'E',1,'INST',IARCHI,0,LINST,K8B)
         ZR(LINST) = T0
         WRITE(IFM,1000) (TYPE(ITY),ITY=1,3), IARCHI, T0
       ELSE
         IF(NRPASE.EQ.0) NBORDR = NBORDR + NUME
         CALL RSAGSD( RESULT, NBORDR )
       ENDIF
C -- FIN BOUCLE SUR CREATION STRUCTURE DONNEES RESULTATS : NRPASE
521   CONTINUE

      CALL TITRE
C
C ------- BOUCLE SUR LES PAS DE TEMPS
      ISTOP = 0
      IPAS = 0
      TEMPS = T0
      CALL UTTCPU(1, 'INIT', 4, TPS1)
      DO 40 I = 1,NPATOT
        IPAS=IPAS+1
        IF (IPAS.GT.NPATOT) GOTO 9999
        ISTOC = 0
        TEMPS = TEMPS + DT
        CALL UTTCPU(1, 'DEBUT', 4, TPS1)
         
C     --- BOUCLE SUR LES CHARGEMENTS (STANDARD + SENSIBILITE)
        DO 522 , NRPASE = 0 , NBPASE

C     --- NOM DES STRUCTURES,  JAUX=3 => LE NOM DU RESULTAT
         JAUX = 3
         CALL PSNSLE ( INPSCO, NRPASE, JAUX, RESULT )
C
C ------------- CALCUL DES DEPLACEMENTS ET VITESSES
         DO 21 IEQ=1, NEQ
              ZR(IVITE1+IEQ-1)=VIT0(IEQ+NEQ*NRPASE)
     &                         +DT*ACC0(IEQ+NEQ*NRPASE)
              ZR(IDEPL1+IEQ-1)=DEP0(IEQ+NEQ*NRPASE)
     &                         +DT*ZR(IVITE1+IEQ-1)
 21      CONTINUE             
C
C ------------- CALCUL DU SECOND MEMBRE F*
         CALL DLFEXT ( NVECA,NCHAR,TEMPS,NEQ,
     +                 LIAD,LIFO,CHARGE,INFOCH,FOMULT,
     +                 MODELE,MATE,CARELE,NUMEDD,
     +                 NBPASE,NRPASE,INPSCO,ZR(IFORC1))
C
C ------------- FORCE DYNAMIQUE F* = F* - K DEP1 - C VIT1
         CALL DLFDYN ( IMAT(1),IMAT(3),LAMORT,NEQ,
     +                 ZR(IDEPL1),ZR(IVITE1),ZR(IFORC1),ZR(IWK0))
C
C ------------- RESOLUTION DE M . A = F ET DE LA VITESSE STOCKEE
C           --- RESOLUTION AVEC FORCE1 COMME SECOND MEMBRE ---
         DO 20 IEQ=1, NEQ
              ZR(IACCE1+IEQ-1)=ZR(IWK1+IEQ-1)*ZR(IFORC1+IEQ-1)
C           --- VITESSE AUX INSTANTS 'TEMPS + DT' ---   
         ZR(IVITE2+IEQ-1)=ZR(IVITE1+IEQ-1)+(DT/2)*ZR(IACCE1+IEQ-1)
 20      CONTINUE       
C
C ------------- TRANSFERT DES NOUVELLES VALEURS DANS LES ANCIENNES
         CALL DCOPY(NEQ ,ZR(IDEPL1),1,DEP0(1+NEQ*NRPASE), 1)
         CALL DCOPY(NEQ ,ZR(IVITE1),1,VIT0(1+NEQ*NRPASE), 1)
         CALL DCOPY(NEQ ,ZR(IACCE1),1,ACC0(1+NEQ*NRPASE), 1)
C

C  SAUVEGARDE DU CHAMP SOLUTION STANDARD DANS VAPRIN
C
         IF (NRPASE.EQ.0 .AND. NBPASE.GT.0) THEN
            JSTD = 0
            JAUX = 4
            CALL PSNSLE ( INPSCO, JSTD, JAUX, VAPRIN )
            CALL JEEXIN(VAPRIN(1:19)//'.REFE',IRESOL)
            IF (IRESOL.EQ.0) CALL VTCREM(VAPRIN(1:19),MASSE,'V',TYPSOL)
            CALL JEVEUO(VAPRIN(1:19)//'.VALE','E',LVALE)
            DO 426 IEQ = 1, NEQ
              ZR(LVALE-1 +IEQ) = ZR(IDEPL1-1 +IEQ)
  426       CONTINUE
         END IF

C           --- ARCHIVAGE EVENTUEL DANS L'OBJET SOLUTION ---
         IF ( ZI(JSTOC+IPAS-1).EQ.1 ) THEN
            ISTOC = 1
            IF(NRPASE.EQ.0) IARCHI = IARCHI + 1
            CALL DLARCH (IARCHI,TYPE,RESULT,NOMCMD,MASSE,
     *               NEQ,ZR(IDEPL1),ZR(IVITE2),ZR(IACCE1),TEMPS)

C         WRITE(IFM,1000) (TYPE(ITY),ITY=1,3), IARCHI, TEMPS

         ENDIF
C -- FIN BOUCLE SUR LES CHARGEMENTS : NRPASE
522     CONTINUE
C
C ------------- VERIFICATION DU TEMPS DE CALCUL RESTANT
        CALL UTTCPU(1, 'FIN', 4, TPS1)
        IF ( TPS1(1) .LT. 5.D0  .OR. TPS1(4).GT.TPS1(1) ) THEN
           IF ( I .NE. NPATOT ) THEN
            ISTOP = 1
            WRITE(IFM,*)'ARRET PAR MANQUE DE TEMPS CPU'//
     *       ' AU PAS DE TEMPS : ',I,
     *       ' TEMPS MOYEN PAR PAS : ',TPS1(4),
     *       ' TEMPS CPU RESTANT : ',TPS1(1)
            GOTO 9999
           ENDIF
        ENDIF
C
 40   CONTINUE
 9999 CONTINUE
C
C ------------- ARCHIVAGE DU DERNIER INSTANT DE CALCUL
      IF (NBEXCL.NE.0) THEN
        DO 523 , NRPASE = 0 , NBPASE
         IF ( ISTOC.EQ.0 ) THEN
            IF(NRPASE.EQ.0) IARCHI = IARCHI + 1
            CALL RSADPA(RESULT,'E',1,'INST',IARCHI,0,LINST,K8B)
            ZR(LINST) = TEMPS
         ELSE
            CALL RSADPA(RESULT,'L',1,'INST',IARCHI,0,LINST,K8B)
            TEMPS = ZR(LINST)
         ENDIF
         CALL RSEXCH(RESULT,'DEPL',IARCHI,CHAMNO,IER)
         IF ( IER .EQ. 100 ) THEN
            CALL VTCREM(CHAMNO,MASSE,'G','R')
         ELSEIF ( IER .EQ. 0 ) THEN
            GOTO 70
         ELSE
            CALL UTMESS('F',NOMCMD,'APPEL ERRONE')
         ENDIF
         WRITE(IFM,1000) 'DEPL',' ',' ', IARCHI, TEMPS
         CHAMNO(20:24)  = '.VALE'
         CALL JEVEUO(CHAMNO,'E',LVALE)
         DO 72 IEQ = 1,NEQ
            ZR(LVALE+IEQ-1) = ZR(IDEPL1-1+IEQ)
 72      CONTINUE
         CALL JELIBE(CHAMNO)
         CALL RSNOCH(RESULT,'DEPL',IARCHI,' ')
 70      CONTINUE
         CALL RSEXCH(RESULT,'VITE',IARCHI,CHAMNO,IER)
         IF ( IER .EQ. 100 ) THEN
            CALL VTCREM(CHAMNO,MASSE,'G','R')
         ELSEIF ( IER .EQ. 0 ) THEN
            GOTO 74
         ELSE
            CALL UTMESS('F',NOMCMD,'APPEL ERRONE')
         ENDIF
         WRITE(IFM,1000) ' ','VITE',' ', IARCHI, TEMPS
         CHAMNO(20:24)  = '.VALE'
         CALL JEVEUO(CHAMNO,'E',LVALE)
         DO 76 IEQ = 1,NEQ
            ZR(LVALE+IEQ-1) = ZR(IVITE1-1+IEQ)
 76      CONTINUE
         CALL JELIBE(CHAMNO)
         CALL RSNOCH(RESULT,'VITE',IARCHI,' ')
 74      CONTINUE
         CALL RSEXCH(RESULT,'ACCE',IARCHI,CHAMNO,IER)
         IF ( IER .EQ. 100 ) THEN
            CALL VTCREM(CHAMNO,MASSE,'G','R')
         ELSEIF ( IER .EQ. 0 ) THEN
            GOTO 78
         ELSE
            CALL UTMESS('F',NOMCMD,'APPEL ERRONE')
         ENDIF
         WRITE(IFM,1000) ' ',' ','ACCE', IARCHI, TEMPS
         CHAMNO(20:24)  = '.VALE'
         CALL JEVEUO(CHAMNO,'E',LVALE)
         DO 80 IEQ = 1,NEQ
            ZR(LVALE+IEQ-1) = ZR(IACCE1-1+IEQ)
 80      CONTINUE
         CALL JELIBE(CHAMNO)
         CALL RSNOCH(RESULT,'ACCE',IARCHI,' ')
 78      CONTINUE
523     CONTINUE
      ENDIF

      IF (ISTOP.EQ.1)
     +       CALL UTEXCP(28,'OP0048','ARRET PAR MANQUE DE TEMPS CPU')
C
C     --- DESTRUCTION DES OBJETS DE TRAVAIL ---
C
      CALL JEDETC('V','&&',1)
      CALL JEDETC('V','.CODI',20)
      CALL JEDETC('V','.MATE_CODE',9)
C
 1000 FORMAT(1P,3X,'CHAMP(S) STOCKE(S):',3(1X,A4),
     +             ' NUME_ORDRE:',I8,' INSTANT:',D12.5)
C
      CALL JEDEMA()
      END
