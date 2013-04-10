      SUBROUTINE TENSCA(TABLCA,ICABL,NBNOCA,NBF0,F0,DELTA,TYPREL,TRELAX,
     &                  XFLU,XRET,EA,RH1000,MU0,FPRG,FRCO,FRLI,
     &                  SA,REGL)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/04/2013   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C  DESCRIPTION : CALCUL DE LA TENSION LE LONG D'UN CABLE
C  -----------   APPELANT : OP0180 , OPERATEUR DEFI_CABLE_BP
C
C                EN SORTIE ON COMPLETE LA TABLE RESULTAT
C                LES LIGNES COMPLETEES CORRESPONDENT AU DERNIER CABLE
C                LES CASES RENSEIGNEES CORRESPONDENT AU PARAMETRE
C                <TENSION>
C
C  IN     : TABLCA : CHARACTER*19
C                    NOM DE LA TABLE DECRIVANT LES CABLES
C  IN     : ICABL  : INTEGER , SCALAIRE
C                    NUMERO DU CABLE
C  IN     : NBNOCA : INTEGER , 
C                    CONTIENT LE NOMBRE DE NOEUDS DU CABLE ETUDIE
C  IN     : NBF0   : INTEGER , SCALAIRE
C                    NOMBRE D'ANCRAGES ACTIFS DU CABLE (0, 1 OU 2)
C  IN     : F0     : REAL*8 , SCALAIRE
C                    VALEUR DE LA TENSION APPLIQUEE A L'UN OU AUX DEUX
C                    ANCRAGES ACTIFS DU CABLE
C  IN     : DELTA  : REAL*8 , SCALAIRE
C                    VALEUR DU RECUL DE L'ANCRAGE
C  IN     : TYPREL  : CHARACTER*24
C                    TYPE DE RELAXATION UTILISEE
C  IN     : TRELAX : REAL*8 , SCALAIRE
C                    VALEUR DE LA FONCTION CARACTERISANT L'EVOLUTION DE
C                    LA RELAXATION DE L'ACIER DANS LE TEMPS POUR BPEL
C                    OU NOMBRE D'HEURES POUR LA RELAXATION SI ETCC
C                    UTILE SI RELAX = .TRUE.
C  IN     : XFLU   : REAL*8 , SCALAIRE
C                    VALEUR DU TAUX DE PERTE DE TENSION PAR FLUAGE DU
C                    BETON, EN % DE LA TENSION INITIALE
C  IN     : XRET   : REAL*8 , SCALAIRE
C                    VALEUR DU TAUX DE PERTE DE TENSION PAR RETRAIT DU
C                    BETON, EN % DE LA TENSION INITIALE
C  IN     : EA     : REAL*8 , SCALAIRE
C                    VALEUR DU MODULE D'YOUNG DE L'ACIER
C  IN     : RH1000 : REAL*8 , SCALAIRE
C                    VALEUR DE LA RELAXATION A 1000 HEURES EN %
C  IN     : MU0    : REAL*8 , SCALAIRE
C                    VALEUR DU COEFFICIENT DE RELAXATION DE L'ACIER
C                    PRECONTRAINT POUR BPEL
C                    
C  IN     : FPRG     : REAL*8 , SCALAIRE
C                    VALEUR DE LA CONTRAINTE LIMITE ELASTIQUE DE L'ACIER
C  IN     : FRCO   : REAL*8 , SCALAIRE
C                    VALEUR DU COEFFICIENT DE FROTTEMENT EN COURBE
C                    (CONTACT ENTRE LE CABLE ACIER ET LE MASSIF BETON)
C  IN     : FRLI   : REAL*8 , SCALAIRE
C                    VALEUR DU COEFFICIENT DE FROTTEMENT EN LIGNE
C                    (CONTACT ENTRE LE CABLE ACIER ET LE MASSIF BETON)
C  IN     : SA     : REAL*8 , SCALAIRE
C                    VALEUR DE L'AIRE DE LA SECTION DROITE DU CABLE
C  IN     : REGL   : CHARACTER*4, INDICATION DU REGLEMENT UTILISE
C                    BPEL OU ETCC


C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C
C ARGUMENTS
C ---------
      INCLUDE 'jeveux.h'
      CHARACTER*19  TABLCA
      CHARACTER*4   REGL
      CHARACTER*24  TYPREL
      INTEGER       ICABL, NBNOCA, NBF0
      REAL*8        F0, DELTA, TRELAX, XFLU, XRET, EA, RH1000, MU0, 
     &              FPRG,FRCO, FRLI, SA

C
C VARIABLES LOCALES
C -----------------
      INTEGER       IBID, IDECNO, INO, IPARA, JABSC, JALPH, JF, JTBLP,
     &              JTBNP, NBLIGN, NBPARA, IARG,N1,IRT,JTABX,JTABY,NBVAL
      REAL*8        DF, FLIM, KRELAX, ZERO,FI, DFR,R8PREM,F2
      COMPLEX*16    CBID
      LOGICAL       TROUV1, TROUV2,EXI1,EXI2
      CHARACTER*3   K3B
      CHARACTER*24  ABSCCA, ALPHCA
      CHARACTER*8   NTABLE,K8B
      CHARACTER*19  NEWTAB
      CHARACTER*24  TABX, TABY
C
      CHARACTER*24  PARAM, PARCR(2)
      DATA          PARAM /'TENSION                 '/
      DATA          PARCR /'ABSC_CURV               ',
     &                     'ALPHA                   '/
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      CALL JEMARQ()
C
      ZERO = 0.0D0
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 1   TRAITEMENT DES CAS PARTICULIERS F0 = 0 OU PAS D'ANCRAGE ACTIF
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C     NBNO = NBNOCA(ICABL)
C
      CALL JEVEUO(TABLCA//'.TBNP','L',JTBNP)
      NBLIGN = ZI(JTBNP+1)
      IDECNO = NBLIGN - NBNOCA
C
      IF ( (F0.EQ.0.0D0).OR.(NBF0.EQ.0) ) THEN
         DO 10 INO = 1, NBNOCA
            CALL TBAJLI(TABLCA,1,PARAM,IBID,ZERO,CBID,K3B,IDECNO+INO)
  10     CONTINUE
         GO TO 9999
      ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 2   RECUPERATION DE L'ABSCISSE CURVILIGNE ET DE LA DEVIATION ANGULAIRE
C     CUMULEE LE LONG DU CABLE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      NBPARA = ZI(JTBNP)
      CALL JEVEUO(TABLCA//'.TBLP','L',JTBLP)
      TROUV1 = .FALSE.
      TROUV2 = .FALSE.
      DO 20 IPARA = 1, NBPARA
         IF ( ZK24(JTBLP+4*(IPARA-1)).EQ.PARCR(1) ) THEN
            TROUV1 = .TRUE.
            ABSCCA = ZK24(JTBLP+4*(IPARA-1)+2)
            CALL JEVEUO(ABSCCA,'L',JABSC)
         ENDIF
         IF ( ZK24(JTBLP+4*(IPARA-1)).EQ.PARCR(2) ) THEN
            TROUV2 = .TRUE.
            ALPHCA = ZK24(JTBLP+4*(IPARA-1)+2)
            CALL JEVEUO(ALPHCA,'L',JALPH)
         ENDIF
         IF ( TROUV1 .AND. TROUV2 ) GO TO 30
  20  CONTINUE
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 4   CALCUL DE LA TENSION LE LONG DU CABLE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
  30  CONTINUE

C
      CALL WKVECT('&&TENSCA.F' ,'V V R',NBNOCA,JF )
C
C 4.1 CALCUL DE LA TENSION LE LONG DU CABLE EN PRENANT EN COMPTE LES
C --- PERTES PAR FROTTEMENT ET PAR RECUL DU(DES) ANCRAGE(S)
C    PAS DE DIFFERENCE ENTRE ETCC ET BPEL

      IF ( NBF0.EQ.1 ) THEN
         CALL TENSK1(ICABL,NBNOCA,ZR(JABSC+IDECNO),ZR(JALPH+IDECNO),
     &               F0,DELTA,EA,FRCO,FRLI,SA,ZR(JF))
      ELSE
         CALL TENSK2(ICABL,NBNOCA,ZR(JABSC+IDECNO),ZR(JALPH+IDECNO),
     &               F0,DELTA,EA,FRCO,FRLI,SA,ZR(JF))
      ENDIF

C
C 4.2 PRISE EN COMPTE LE CAS ECHEANT DES PERTES DE TENSION PAR
C --- RELAXATION DE L'ACIER
C
      IF (TYPREL.NE.'SANS') THEN
        IF (RH1000.LE.R8PREM()) CALL U2MESS('A','MODELISA2_70')
      ENDIF  
      IF ( TYPREL.EQ.'BPEL' ) THEN
C----------------------------------    
C     CAS DU BPEL
C-----------------------
         FLIM = FPRG * SA           
         KRELAX = TRELAX * 5.0D-02 * RH1000
           
         DO 40 INO = 1, NBNOCA
            ZR(JF+INO-1) = ZR(JF+INO-1)
     &         * ( 1.0D0 - KRELAX * (ZR(JF+INO-1)/FLIM-MU0) )
  40     CONTINUE
         
      ELSEIF (TYPREL.EQ.'ETCC_DIRECT') THEN
C----------------------------------    
C        CAS ETCC_DIRECT
C----------------------------------
         FLIM = FPRG * SA  
         DO 45 INO = 1, NBNOCA
           FI  = ZR(JF+INO-1) 
           ZR(JF+INO-1) = FI - 0.8D0 *  FI
     &          *  0.66D-05 *RH1000*EXP(9.1D0*FI/FLIM)*
     &         (TRELAX/1000.D0)**(0.75D0*(1.D0-(FI/FLIM) ))

  45     CONTINUE
      ELSEIF (TYPREL.EQ.'ETCC_REPRISE') THEN
C----------------------------------    
C        CAS ETCC_REPRISE
C----------------------------------        
        CALL GETVID ( 'DEFI_CABLE', 'TENSION_CT' ,ICABL,IARG,1,
     &                 NTABLE, N1 )
        IF (N1.EQ.0) THEN
          CALL U2MESS('F','MODELISA2_56')
        ENDIF

        NEWTAB=NTABLE
        TABX   = '&&TENSCA_TABREF_CURV'
        TABY   = '&&TENSCA_TABREF_TENS'
 
        CALL JEEXIN ( NEWTAB//'.TBBA', IRT )
        IF ( IRT .EQ. 0 ) THEN
          CALL U2MESS('F','UTILITAI4_64')
        ENDIF
C     VERIFICATION DE LA PRESENCE DES BONS PARAMETRES
        CALL TBEXIP ( NEWTAB, 'ABSC_CURV', EXI1, K8B )
        CALL TBEXIP ( NEWTAB, 'N', EXI2, K8B )
 
        IF(.NOT.EXI1 .AND. .NOT.EXI2)THEN
          CALL U2MESS('F','MODELISA2_67')
        ENDIF

        CALL TBEXVE ( NEWTAB, 'ABSC_CURV', TABX, 'V', NBVAL, K8B)
        CALL JEVEUO ( TABX, 'L', JTABX )
        CALL TBEXVE ( NEWTAB, 'N', TABY, 'V', NBVAL, K8B)
        CALL JEVEUO ( TABY, 'L', JTABY )
        IF (NBVAL.NE.NBNOCA) THEN
          CALL U2MESS('F','MODELISA2_68')
        ENDIF
C     ON VERIFIE A MINIMA QUE LES ABSCISSES CURVILIGNES SONT IDENTIQUES
C     (MAIS PAS LES COORDONNES EXACTES)
        DO 50 INO=1,NBNOCA
          IF (ZR(JTABX+INO-1)-ZR(JABSC+INO-1).GE.R8PREM() ) THEN
             CALL U2MESS('F','MODELISA2_69')
          ENDIF
50       CONTINUE      
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C  MISE A JOUR DE LA TENSION
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
        DO 60 INO = 1, NBNOCA
           F2 = ZR(JTABY+INO-1)
           ZR(JF+INO-1) = ZR(JF+INO-1) - 0.8D0 * 0.66D-05 *RH1000*
     &             EXP(9.1D0*F2/FPRG/SA)*
     &         (TRELAX/1000.D0)**(0.75D0*(1.D0-(F2/FPRG/SA) ))*F2
  60    CONTINUE


        CALL JEDETR(TABX)
        CALL JEDETR(TABY)     
      
                              
      ENDIF
C
C 4.3 PRISE EN COMPTE LE CAS ECHEANT DES PERTES DE TENSION PAR
C --- FLUAGE ET RETRAIT DU BETON - UNIQUEMENT POUR BPEL
C
      IF (REGL.EQ.'BPEL') THEN
      
        IF ( XFLU+XRET.NE.0.0D0 ) THEN
           DF = ( XFLU + XRET ) * F0
           DO 80 INO = 1, NBNOCA
              ZR(JF+INO-1) = ZR(JF+INO-1) - DF
  80       CONTINUE
        ENDIF

       ENDIF        
        
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 5   MISE A JOUR DES OBJETS DE SORTIE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      DO 90 INO = 1, NBNOCA
         CALL TBAJLI(TABLCA,1,PARAM,
     &               IBID,ZR(JF+INO-1),CBID,K3B,IDECNO+INO)
  90  CONTINUE
C
9999  CONTINUE
      CALL JEDETR('&&TENSCA.F')
      CALL JEDEMA()
C
C --- FIN DE TENSCA.
      END
