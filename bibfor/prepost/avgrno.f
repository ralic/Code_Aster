      SUBROUTINE AVGRNO(VWORK, TDISP, LISNOE, NBNOT, NBORDR, NNOINI,
     &                  NBNOP, NUMPAQ, TSPAQ, NOMCRI, NOMMAI,
     &                  PROAXE, CNSR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 28/06/2005   AUTEUR F1BHHAJ J.ANGLES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE F1BHHAJ J.ANGLES
C TOLE  CRP_20
      IMPLICIT     NONE
      INTEGER      TDISP, NBNOP, LISNOE(NBNOP), NBNOT, NBORDR, NNOINI
      INTEGER      NUMPAQ, TSPAQ
      REAL*8       VWORK(TDISP)
      CHARACTER*8  NOMMAI
      CHARACTER*16 NOMCRI, PROAXE
      CHARACTER*19 CNSR
C ---------------------------------------------------------------------
C BUT: DETERMINER LE PLAN DANS LEQUEL LE DOMMAGE EST MAXIMAL
C ---------------------------------------------------------------------
C ARGUMENTS:
C VWORK     IN    R  : VECTEUR DE TRAVAIL CONTENANT
C                      L'HISTORIQUE DES TENSEURS DES CONTRAINTES
C                      ATTACHES A CHAQUE POINT DE GAUSS DES MAILLES
C                      DU <<PAQUET>> DE MAILLES.
C TDISP     IN    I  : DIMENSION DU VECTEUR VWORK
C LISNOE    IN    I  : LISTE COMPLETE DES NOEUDS A TRAITER.
C NBNOT     IN    I  : NOMBRE TOTAL DE NOEUDS A TRAITER.
C NBORDR    IN    I  : NOMBRE DE NUMERO D'ORDRE STOCKE DANS LA
C                      STRUCTURE DE DONNEES RESULTAT.
C NNOINI    IN    I  : NUMERO DU 1ER NOEUD DU <<PAQUET>> DE
C                      NOEUDS COURANT.
C NBNOP     IN    I  : NOMBRE DE NOEUDS DANS LE <<PAQUET>> DE
C                      NOEUDS COURANT.
C NUMPAQ    IN    I  : NUMERO DU PAQUET DE MAILLES COURANT.
C TSPAQ     IN    I  : TAILLE DU SOUS-PAQUET DU <<PAQUET>> DE MAILLES
C                      COURANT.
C NOMCRI    IN    K16: NOM DU CRITERE AVEC PLANS CRITIQUES.
C NOMMAI    IN    K8 : NOM DU MAILLAGE.
C PROAXE    IN    K16: TYPE DE PROJECTION (UN OU DEUX AXES).
C CNSR      IN    K19: NOM DU CHAMP SIMPLE DESTINE A RECEVOIR LES
C                      RESULTATS.
C
C REMARQUE :
C  - LA TAILLE DU SOUS-PAQUET EST EGALE A LA TAILLE DU <<PAQUET>> DE
C    MAILLES DIVISEE PAR LE NOMBRE DE NUMERO D'ORDRE (NBORDR).
C-----------------------------------------------------------------------
C---- COMMUNS NORMALISES  JEVEUX
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
      CHARACTER*32 ZK32,JEXNOM,JEXNUM,JEXATR
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C-----------------------------------------------------------------------
      INTEGER       I, JVECTN, JVECTU, JVECTV
      INTEGER       JVECN1, JVECU1, JVECV1, JVECN2, JVECU2, JVECV2
      INTEGER       JVNO1, JVNO2, JAXE, JFLAG, JMIMA
      INTEGER       JNPOIN, JVPOIN, JVORDR
      INTEGER       IVTRAV, INTRAV, INPIC, IVPIC, IORPIC
      INTEGER       INCYC, IVMIN, IVMAX, IOMIN, IOMAX
      INTEGER       JSIGN, JPHYD, JGDEQ, JNRUP, JDOEL, JDOTO
      INTEGER       JCNRD, JCNRL, JCNRV, IRET, ICESD, ICESL, ICESV
      INTEGER       TNECES, TDISP2, LOR8EM, LOISEM, JVECNO, IBID, N, K
      INTEGER       NUNOE, IDEB, DIM, J, NGAM, TAB2(18), IFIN
      INTEGER       L, CNBNO, IBIDNO, KWORK, SOMNOW, INOP 
      INTEGER       NBMA, ADRMA, JTYPMA, NBVEC, NVAL, VNORMX
      INTEGER       ICMP, JAD
C
      REAL*8        FATSOC, DGAM, GAMMA, PI, R8PI, DPHI, TAB1(18), PHI0
      REAL*8        VALA, VALB, COEFPA, PSEUIL, CUDOMX
      REAL*8        NXM, NYM, NZM, GAMMAM, EPSILO, PHIM, DPHI2, DGAM2
      REAL*8        VRESU(22)
C
      CHARACTER*2  CODRET, CODWO
      CHARACTER*8  CHMAT1, NOMPAR, NOMRES, NOMMAT, METHOD, K8B
      CHARACTER*10 OPTIO
      CHARACTER*16 PHENOM
      CHARACTER*19 CHMAT, CESMAT, NCNCIN
      CHARACTER*24 TYPMA
      LOGICAL      ENDUR, LSIG0
C
      COMPLEX*16    C16B
C
C-----------------------------------------------------------------------
C234567                                                              012
C-----------------------------------------------------------------------
      DATA  TAB1/ 180.0D0, 60.0D0, 30.0D0, 20.0D0, 15.0D0, 12.857D0,
     &             11.25D0, 10.588D0, 10.0D0, 10.0D0, 10.0D0, 10.588D0,
     &             11.25D0, 12.857D0, 15.0D0, 20.0D0, 30.0D0, 60.0D0 /
C
      DATA  TAB2/ 1, 3, 6, 9, 12, 14, 16, 17, 18, 18, 18, 17, 16, 14,
     &           12, 9, 6, 3 /
C
      EPSILO = 1.0D-7
      PI = R8PI()
C-----------------------------------------------------------------------
C
      CALL JEMARQ()

C CONSTRUCTION DU VECTEUR NORMAL SUR UNE DEMI SPHERE
C CONSTRUCTION DU VECTEUR U DANS LE PLAN TANGENT, SUR UNE DEMI SPHERE
C CONSTRUCTION DU VECTEUR V DANS LE PLAN TANGENT, SUR UNE DEMI SPHERE

      CALL WKVECT('&&AVGRNO.VECT_NORMA', 'V V R', 627, JVECTN)
      CALL WKVECT('&&AVGRNO.VECT_TANGU', 'V V R', 627, JVECTU)
      CALL WKVECT('&&AVGRNO.VECT_TANGV', 'V V R', 627, JVECTV)

      CALL WKVECT('&&AVGRNO.VECT_NORMA1', 'V V R', 27, JVECN1)
      CALL WKVECT('&&AVGRNO.VECT_TANGU1', 'V V R', 27, JVECU1)
      CALL WKVECT('&&AVGRNO.VECT_TANGV1', 'V V R', 27, JVECV1)
      CALL WKVECT('&&AVGRNO.VECT_NORMA2', 'V V R', 27, JVECN2)
      CALL WKVECT('&&AVGRNO.VECT_TANGU2', 'V V R', 27, JVECU2)
      CALL WKVECT('&&AVGRNO.VECT_TANGV2', 'V V R', 27, JVECV2)

      CALL WKVECT('&&AVGRNO.VECTNO1', 'V V R', 18*NBORDR, JVNO1)
      CALL WKVECT('&&AVGRNO.VECTNO2', 'V V R', 18*NBORDR, JVNO2)

      CALL WKVECT('&&AVGRNO.AXE', 'V V R', 209*NBORDR, JAXE)
      CALL WKVECT('&&AVGRNO.FLAG', 'V V I', 209, JFLAG)
      CALL WKVECT('&&AVGRNO.MINMAX', 'V V R', 4*209, JMIMA)

      CALL WKVECT('&&AVGRNO.NB_POIN', 'V V I', 209, JNPOIN)
      CALL WKVECT('&&AVGRNO.VAL_POIN', 'V V R', 209*NBORDR, JVPOIN)
      CALL WKVECT('&&AVGRNO.NUME_ORDR', 'V V I', 209*NBORDR, JVORDR)

      CALL WKVECT('&&AVGRNO.POIN_TRAV','V V R',NBORDR+2,IVTRAV)
      CALL WKVECT('&&AVGRNO.ORDR_TRAV','V V I',2*(NBORDR+2),INTRAV)
      CALL WKVECT('&&AVGRNO.NB_PIC', 'V V I',209, INPIC)
      CALL WKVECT('&&AVGRNO.VAL_PICS','V V R',209*(NBORDR+2),IVPIC)
      CALL WKVECT('&&AVGRNO.NORD_PIC','V V I',209*(NBORDR+2),IORPIC)

      CALL WKVECT('&&AVGRNO.NB_CYCL','V V I', 209, INCYC)
      CALL WKVECT('&&AVGRNO.VAL_MIN','V V R',209*(NBORDR+2),IVMIN)
      CALL WKVECT('&&AVGRNO.VAL_MAX','V V R',209*(NBORDR+2),IVMAX)
      CALL WKVECT('&&AVGRNO.NORD_MIN','V V I',209*(NBORDR+2),IOMIN)
      CALL WKVECT('&&AVGRNO.NORD_MAX','V V I',209*(NBORDR+2),IOMAX)

      CALL WKVECT('&&AVGRNO.SIG_NORM','V V R',209*NBORDR,JSIGN)
      CALL WKVECT('&&AVGRNO.PRES_HYDR','V V R',NBORDR,JPHYD)
      CALL WKVECT('&&AVGRNO.GDR_EQUI','V V R',209*NBORDR,JGDEQ)

      CALL WKVECT('&&AVGRNO.NCYC_RUPT','V V R',209*NBORDR,JNRUP)
      CALL WKVECT('&&AVGRNO.DOMM_ELEM','V V R',209*NBORDR,JDOEL)
      CALL WKVECT('&&AVGRNO.DOMM_TOT','V V R',209,JDOTO)

C OBTENTION DES ADRESSES '.CNSD', '.CNSL' ET '.CNSV' DU CHAMP SIMPLE
C DESTINE A RECEVOIR LES RESULTATS : DOMMAGE_MAX, COORDONNEES VECTEUR
C NORMAL CORRESPONDANT

      CALL JEVEUO(CNSR//'.CNSD','L',JCNRD)
      CALL JEVEUO(CNSR//'.CNSL','E',JCNRL)
      CALL JEVEUO(CNSR//'.CNSV','E',JCNRV)

C RECUPERATION MAILLE PAR MAILLE DU MATERIAU DONNE PAR L'UTILISATEUR

      CALL GETVID(' ','CHAM_MATER',1,1,1,CHMAT1,IRET)
      CHMAT = CHMAT1//'.CHAMP_MAT'
      CESMAT = '&&AVGRNO.CESMAT'
      CALL CARCES(CHMAT,'ELEM',' ','V',CESMAT,IRET)
      CALL JEVEUO(CESMAT//'.CESD','L',ICESD)
      CALL JEVEUO(CESMAT//'.CESL','L',ICESL)
      CALL JEVEUO(CESMAT//'.CESV','L',ICESV)

C DEFINITION DU VECTEUR CONTENANT LES VALEURS DU CISAILLEMENT POUR TOUS
C LES INSTANTS ET TOUS LES PLANS

      TNECES = 209*NBORDR*2
      CALL JEDISP(1, TDISP2)
      TDISP2 =  (TDISP2 * LOISEM()) / LOR8EM()
      IF (TDISP2 .LT. TNECES ) THEN
         CALL UTDEBM('F', 'AVGRNO.1', 'LA TAILLE MEMOIRE '//
     &       ' NECESSAIRE AU VECTEUR DE TRAVAIL DANS '//
     &       ' LEQUEL NOUS STOCKONS LES COMPOSANTES '//
     &       ' u ET v DU VECTEUR TAU EST TROP IMPORTANTE '//
     &       ' PAR RAPPORT A LA PLACE DISPONIBLE.')
         CALL UTIMPI('L', 'TAILLE DISPONIBLE : ', 1, TDISP2)
         CALL UTIMPI('L', 'TAILLE NECESSAIRE : ', 1, TNECES)
         CALL UTFINM( )
      ELSE
         CALL WKVECT( '&&AVGRNO.VECTNO', 'V V R', TNECES, JVECNO )
      ENDIF

C COEFFICIENT PERMETTANT D'UTILISER LES MEMES ROUTINES POUR LES
C CONTRAINTES ET LES DEFORMATIONS

      IF ( NOMCRI(1:12) .EQ. 'FATEMI_SOCIE' ) THEN
         FATSOC = 1.0D4
      ELSE
         FATSOC = 1.0D0
      ENDIF

C CONSTRUCTION DES VECTEURS N, U ET V

      DGAM = 10.0D0

      N = 0
      K = 1
      IDEB = 1
      DIM = 627
      DO 300 J=1, 18
         GAMMA=(J-1)*DGAM*(PI/180.0D0)
         DPHI=TAB1(J)*(PI/180.0D0)
         NGAM=TAB2(J)
         IFIN = NGAM
         PHI0 = DPHI/2.0D0

         CALL VECNUV(IDEB, IFIN, GAMMA, PHI0, DPHI, N, K, DIM,
     &               ZR(JVECTN), ZR(JVECTU), ZR(JVECTV))

 300  CONTINUE

C CONSTRUCTION DU VECTEUR : CONTRAINTE = F(NUMERO D'ORDRE) EN CHAQUE
C NOEUDS DU PAQUET DE MAILLES.
      L = 1
      CNBNO = 0
      KWORK = 0
      SOMNOW = 0
      IBIDNO = 1

      NCNCIN = '&&AVGRNO.CNCINV'
      CALL CNCINV ( NOMMAI, IBID, 0, 'V', NCNCIN )

      TYPMA = NOMMAI//'.TYPMAIL'
      CALL JEVEUO( TYPMA, 'L', JTYPMA )

      DO 400 INOP=NNOINI, NNOINI+(NBNOP-1)

         IF ( INOP .GT. NNOINI ) THEN
            KWORK = 1
            SOMNOW = SOMNOW + 1
         ENDIF

         CNBNO = CNBNO + 1
         IF ( (L*INT(NBNOT/10.0D0)) .LT. CNBNO ) THEN
           WRITE(6,*)NUMPAQ,'   ',(CNBNO-1)
           L = L + 1
         ENDIF

C RECUPERATION DU NOM DU MATERIAU AFFECTE A LA MAILLE OU AUX MAILLES
C QUI PORTENT LE NOEUD COURANT.

         NUNOE = LISNOE(INOP)
         CALL JELIRA( JEXNUM(NCNCIN,NUNOE), 'LONMAX', NBMA, K8B )
         CALL JEVEUO( JEXNUM(NCNCIN,NUNOE), 'L', ADRMA )

         K = 0
         OPTIO = 'DOMA_NOEUD'
         DO 410, I=1, NBMA
            CALL RNOMAT (ICESD, ICESL, ICESV, I, NOMCRI, ADRMA, JTYPMA,
     &                   K, OPTIO, VALA, VALB, COEFPA, NOMMAT)
 410     CONTINUE

         IF (K .EQ. 0) THEN
            CALL UTDEBM('A', 'AVGRNO.2', 'LE NOEUD TRAITE '//
     &                  'N''EST ASSOCIE A AUCUNE MAILLE VOLUMIQUE.')
            CALL UTIMPI('L','NUMERO DU NOEUD = ',1,NUNOE)
            CALL UTIMPI('L','NOMBRE DE MAILLES ATTACHEES AU NOEUD = ',
     &                   1,NBMA)
            CALL UTFINM( )
         ENDIF

C  PROJECTION DE L'HISTORIQUE DU CISAILLEMENT DANS UN PLAN

         CALL JERAZO( '&&AVGRNO.VECTNO', TNECES, 1 )
         
         NBVEC = 209
         CALL PROPLA(NBVEC, ZR(JVECTN), ZR(JVECTU), ZR(JVECTV),
     &               NBORDR, KWORK, SOMNOW, VWORK, TDISP, TSPAQ,
     &               IBIDNO, NOMCRI, FATSOC, ZR(JVECNO))

C CALCUL DU DOMMAGE MAX ET DU VECTEUR NORMAL ASSOCIE POUR
C LE NOEUD COURANT DE LA MAILLE COURANTE.

C 1. REMISE A ZERO DU VECTEUR DE TRAVAIL CONTENANT LES VALEURS DE
C    DELTA_TAU POUR UN NOEUD ET DU VECTEUR DE TRAVAIL
C    PERMETTANT DE POINTER SUR LE VECTEUR NORMAL ASSOCIE.

         CALL JERAZO('&&AVGRNO.AXE', NBVEC*NBORDR, 1)
         CALL JERAZO('&&AVGRNO.FLAG', NBVEC, 1)
         CALL JERAZO('&&AVGRNO.MINMAX', 4*NBVEC, 1)

C 2. ENCADREMENT DES POINTS DANS LE PLAN

         LSIG0 = .FALSE.
         CALL AVENCA(ZR(JVECNO), NBVEC, NBORDR, LSIG0, ZI(JFLAG),
     &               ZR(JMIMA))
         IF (LSIG0) THEN
            CUDOMX = 0.0D0
            NXM = 0.0D0
            NYM = 0.0D0
            NZM = 1.0D0
            GOTO 555
         ENDIF

C 3. PROJECTION DE L'HISTORIQUE DE CHARGEMENT SUR UN OU DEUX AXES

         CALL PROJAX(ZR(JVECNO), NBVEC, NBORDR, PROAXE, ZI(JFLAG),
     &                ZR(JMIMA), ZR(JAXE))

C 4. COMPTAGE RAINFLOW (NORME AFNOR + POSTDAM)

C 4.1 PREMIER FILTRAGE DES PICS DE LA FONCTION

         CALL JERAZO('&&AVGRNO.NB_POIN', NBVEC, 1)
         CALL JERAZO('&&AVGRNO.VAL_POIN', NBVEC*NBORDR, 1)
         CALL JERAZO('&&AVGRNO.NUME_ORDR', NBVEC*NBORDR, 1)

         CALL GETVR8(' ','DELTA_OSCI',1,1,1,PSEUIL,NVAL)

         CALL AVPEAK(ZR(JAXE), NBVEC, NBORDR, PSEUIL, ZI(JFLAG),
     &               ZI(JNPOIN), ZR(JVPOIN), ZI(JVORDR))

C 4.2 REARANGEMENT ET EXTRACTION DES PICS

         CALL JERAZO('&&AVGRNO.POIN_TRAV', NBORDR+2, 1)
         CALL JERAZO('&&AVGRNO.ORDR_TRAV', 2*(NBORDR+2), 1)

         CALL JERAZO('&&AVGRNO.NB_PIC', NBVEC, 1)
         CALL JERAZO('&&AVGRNO.VAL_PICS', NBVEC*(NBORDR+2), 1)
         CALL JERAZO('&&AVGRNO.NORD_PIC', NBVEC*(NBORDR+2), 1)

         METHOD = 'RAINFLOW'
         CALL AVPIC2(METHOD, NBVEC, NBORDR, ZR(IVTRAV), ZI(INTRAV),
     &               ZI(JNPOIN), ZR(JVPOIN), ZI(JVORDR), ZI(INPIC),
     &               ZR(IVPIC), ZI(IORPIC))

C 4.3 COMPTAGE RAINFLOW

         CALL JERAZO('&&AVGRNO.NB_CYCL', NBVEC, 1)
         CALL JERAZO('&&AVGRNO.VAL_MIN', NBVEC*(NBORDR+2), 1)
         CALL JERAZO('&&AVGRNO.VAL_MAX', NBVEC*(NBORDR+2), 1)
         CALL JERAZO('&&AVGRNO.NORD_MIN', NBVEC*(NBORDR+2), 1)
         CALL JERAZO('&&AVGRNO.NORD_MAX', NBVEC*(NBORDR+2), 1)

         CALL AVRAIN(NBVEC, NBORDR, ZI(INTRAV), ZI(INPIC),
     &               ZR(IVPIC), ZI(IORPIC), FATSOC, ZI(INCYC),
     &               ZR(IVMIN), ZR(IVMAX), ZI(IOMIN), ZI(IOMAX))

C 5. CALCUL DE LA CONTRAINTE EQUIVALENTE AU SENS DES CRITERES

         CALL JERAZO('&&AVGRNO.SIG_NORM', NBVEC*NBORDR, 1)
         CALL JERAZO('&&AVGRNO.PRES_HYDR', NBORDR, 1)

         IF (( NOMCRI(1:9) .EQ. 'DOMM_MAXI' ) .OR.
     &       ( NOMCRI(1:12) .EQ. 'FATEMI_SOCIE' )) THEN

C 5.1 CALCUL DE LA CONTRAINTE NORMALE

            CALL JXVERI('MESSAGE','VERI18')

            CALL AVSIGN(NBVEC, NBORDR, ZR(JVECTN),
     &                  VWORK, TDISP, KWORK,
     &                  SOMNOW, TSPAQ, IBIDNO, NOMCRI, ZR(JSIGN))

         ELSEIF ( NOMCRI(1:16) .EQ. 'DANG_VAN_MODI_AV' ) THEN

C 5.2 CALCUL DE LA PRESSION HYDROSTATIQUE

            CALL JXVERI('MESSAGE','VERI18')

            CALL AVPHYD(NBORDR, VWORK, TDISP, KWORK, SOMNOW,
     &                  TSPAQ, IBIDNO, ZR(JPHYD))            

         ENDIF

C 5.3 CALCUL DE LA GRANDEUR EQUIVALENTE AU SENS DU CRITERE CHOISI :
C     DOMM_MAXI, FATEMI ET SOCIE (ELASTIQUE OU PLASTIQUE), DANG VAN

         CALL JERAZO('&&AVGRNO.GDR_EQUI', NBVEC*NBORDR, 1)

         CALL AVCRIT(NBVEC, NBORDR, VALA, COEFPA, ZI(INCYC),
     &               ZR(IVMIN), ZR(IVMAX), ZI(IOMIN), ZI(IOMAX),
     &               NOMCRI, ZR(JSIGN), ZR(JPHYD), ZR(JGDEQ))

C 6. CALCUL DU DOMMAGE ELEMENTAIRE DE WOHLER

         CALL JERAZO('&&AVGRNO.NCYC_RUPT', NBVEC*NBORDR, 1)
         CALL JERAZO('&&AVGRNO.DOMM_ELEM', NBVEC*NBORDR, 1)

            CALL AVDOWH(NBVEC, NBORDR, NOMMAT, NOMCRI, ZI(INCYC),
     &                  ZR(JGDEQ), ZR(JDOEL), ZR(JNRUP))

C 7. CALCUL DU DOMMAGE TOTAL (CUMUL)

         CALL JERAZO('&&AVGRNO.DOMM_TOT', NBVEC, 1)

         CALL AVDOMT(NBVEC, NBORDR, ZI(INCYC), ZR(JDOEL), ZR(JDOTO))

C 8. CALCUL DU CUMUL DE DOMMAGE MAXIMAL ET VECTEUR NORMAL ASSOCIE

         CALL AVCDMX(NBVEC, ZR(JDOTO), CUDOMX, VNORMX)

C 9. PREMIER RAFFINEMENT CONCERNANT LA DETERMINATION DU VECTEUR NORMAL
C    CORRESPONDANT AU MAX DES CUMULS DE DOMMAGE.

         NXM = ZR(JVECTN + (VNORMX-1)*3)
         NYM = ZR(JVECTN + (VNORMX-1)*3 + 1)
         NZM = ZR(JVECTN + (VNORMX-1)*3 + 2)
         GAMMAM = ATAN2(SQRT(ABS(1.0D0-NZM**2)),NZM)
         IF (GAMMAM .LT. 0.0D0) THEN
            GAMMAM = GAMMAM + PI
         ENDIF

         IF ((ABS(NYM) .LT. EPSILO) .AND.
     &       (ABS(NXM) .LT. EPSILO)) THEN
           PHIM = 0.0D0
         ELSE
           PHIM = ATAN2(ABS(NYM),NXM)
         ENDIF
         IF (PHIM .LT. 0.0D0) THEN
           PHIM = PHIM + PI
         ENDIF

         IF (ABS(GAMMAM) .LT. EPSILO) THEN
            GAMMA = 5.0D0*(PI/180.0D0)
            DPHI2 = 60.0D0*(PI/180.0D0)
            IDEB = 1
            IFIN = 6
            N = 0
            K = 1
            DIM = 27
            PHI0 = 0.0D0

            CALL VECNUV(IDEB, IFIN, GAMMA, PHI0, DPHI2, N, K, DIM,
     &                  ZR(JVECN2), ZR(JVECU2), ZR(JVECV2))
            GAMMA = 0.0D0
            PHI0 = PI
            IDEB = 1
            IFIN = 1
            K = 1

            CALL VECNUV(IDEB, IFIN, GAMMA, PHI0, DPHI2, N, K, DIM,
     &                  ZR(JVECN2), ZR(JVECU2), ZR(JVECV2))

C 9.1 PROJECTION DE L'HISTORIQUE DU CISAILLEMENT SUR UN PLAN

            NBVEC = 7
            CALL JERAZO( '&&AVGRNO.VECTNO2', 18*NBORDR, 1 )
            CALL PROPLA(NBVEC, ZR(JVECN2), ZR(JVECU2), ZR(JVECV2),
     &                  NBORDR, KWORK, SOMNOW, VWORK, TDISP, TSPAQ,
     &                  IBIDNO, NOMCRI, FATSOC, ZR(JVNO2))
         ELSE
            DGAM2 = 2.0D0*(PI/180.0D0)
            DPHI2 = DGAM2/SIN(GAMMAM)
            N = 0
            K = 2
            DIM = 27
            IDEB = 1
            IFIN = 3
            DO 430 J=1, 3
               GAMMA = GAMMAM + (J-K)*DGAM2
               CALL VECNUV(IDEB, IFIN, GAMMA, PHIM, DPHI2, N, K, DIM,
     &                     ZR(JVECN2), ZR(JVECU2), ZR(JVECV2))
 430        CONTINUE

            NBVEC = 9
            CALL JERAZO( '&&AVGRNO.VECTNO2', 18*NBORDR, 1 )
            CALL PROPLA(NBVEC, ZR(JVECN2), ZR(JVECU2), ZR(JVECV2),
     &                  NBORDR, KWORK, SOMNOW, VWORK, TDISP, TSPAQ,
     &                  IBIDNO, NOMCRI, FATSOC, ZR(JVNO2))
         ENDIF

         CALL JERAZO('&&AVGRNO.AXE', NBVEC*NBORDR, 1)
         CALL JERAZO('&&AVGRNO.FLAG', NBVEC, 1)
         CALL JERAZO('&&AVGRNO.MINMAX', 4*NBVEC, 1)

C 9.2 ENCADREMENT DES POINTS DANS LE PLAN

         CALL AVENCA(ZR(JVNO2), NBVEC, NBORDR, LSIG0, ZI(JFLAG),
     &               ZR(JMIMA))

C 9.3 PROJECTION DE L'HISTORIQUE DE CHARGEMENT SUR UN OU DEUX AXES

         CALL PROJAX(ZR(JVNO2), NBVEC, NBORDR, PROAXE, ZI(JFLAG),
     &               ZR(JMIMA), ZR(JAXE))

C 9.4 COMPTAGE RAINFLOW (NORME AFNOR + POSTDAM)

C 9.4.1 PREMIER FILTRAGE DES PICS DE LA FONCTION

         CALL JERAZO('&&AVGRNO.NB_POIN', NBVEC, 1)
         CALL JERAZO('&&AVGRNO.VAL_POIN', NBVEC*NBORDR, 1)
         CALL JERAZO('&&AVGRNO.NUME_ORDR', NBVEC*NBORDR, 1)

         CALL AVPEAK(ZR(JAXE), NBVEC, NBORDR, PSEUIL, ZI(JFLAG),
     &               ZI(JNPOIN), ZR(JVPOIN), ZI(JVORDR))

C 9.4.2 REARANGEMENT ET EXTRACTION DES PICS

         CALL JERAZO('&&AVGRNO.POIN_TRAV', NBORDR+2, 1)
         CALL JERAZO('&&AVGRNO.ORDR_TRAV', 2*(NBORDR+2), 1)

         CALL JERAZO('&&AVGRNO.NB_PIC', NBVEC, 1)
         CALL JERAZO('&&AVGRNO.VAL_PICS', NBVEC*(NBORDR+2), 1)
         CALL JERAZO('&&AVGRNO.NORD_PIC', NBVEC*(NBORDR+2), 1)

         CALL AVPIC2(METHOD, NBVEC, NBORDR, ZR(IVTRAV),
     &               ZI(INTRAV), ZI(JNPOIN), ZR(JVPOIN),
     &               ZI(JVORDR), ZI(INPIC), ZR(IVPIC), ZI(IORPIC))

C 9.4.3 COMPTAGE RAINFLOW

         CALL JERAZO('&&AVGRNO.NB_CYCL', NBVEC, 1)
         CALL JERAZO('&&AVGRNO.VAL_MIN', NBVEC*(NBORDR+2), 1)
         CALL JERAZO('&&AVGRNO.VAL_MAX', NBVEC*(NBORDR+2), 1)
         CALL JERAZO('&&AVGRNO.NORD_MIN', NBVEC*(NBORDR+2), 1)
         CALL JERAZO('&&AVGRNO.NORD_MAX', NBVEC*(NBORDR+2), 1)

         CALL AVRAIN(NBVEC, NBORDR, ZI(INTRAV), ZI(INPIC),
     &               ZR(IVPIC), ZI(IORPIC), FATSOC, ZI(INCYC),
     &               ZR(IVMIN), ZR(IVMAX), ZI(IOMIN), ZI(IOMAX))

C 9.5 CALCUL DE LA CONTRAINTE EQUIVALENTE AU SENS DES CRITERES

         CALL JERAZO('&&AVGRNO.SIG_NORM', NBVEC*NBORDR, 1)
         CALL JERAZO('&&AVGRNO.PRES_HYDR', NBORDR, 1)

         IF (( NOMCRI(1:9) .EQ. 'DOMM_MAXI' ) .OR.
     &       ( NOMCRI(1:12) .EQ. 'FATEMI_SOCIE' )) THEN

C 9.5.1 CALCUL DE LA CONTRAINTE NORMALE

            CALL JXVERI('MESSAGE','VERI18')

            CALL AVSIGN(NBVEC, NBORDR, ZR(JVECN2),
     &                  VWORK, TDISP, KWORK, 
     &                  SOMNOW, TSPAQ, IBIDNO, NOMCRI, ZR(JSIGN))

         ELSEIF ( NOMCRI(1:16) .EQ. 'DANG_VAN_MODI_AV' ) THEN

C 9.5.2 CALCUL DE LA PRESSION HYDROSTATIQUE

            CALL JXVERI('MESSAGE','VERI18')

            CALL AVPHYD(NBORDR, VWORK, TDISP, KWORK, SOMNOW,
     &                  TSPAQ, IBIDNO, ZR(JPHYD))            

         ENDIF

C 9.5.3 CALCUL DE LA GRANDEUR EQUIVALENTE AU SENS DU CRITERE CHOISI :
C       DOMM_MAXI, FATEMI ET SOCIE (ELASTIQUE OU PLASTIQUE), DANG VAN

         CALL JERAZO('&&AVGRNO.GDR_EQUI', NBVEC*NBORDR, 1)

         CALL AVCRIT(NBVEC, NBORDR, VALA, COEFPA, ZI(INCYC),
     &               ZR(IVMIN), ZR(IVMAX), ZI(IOMIN), ZI(IOMAX),
     &               NOMCRI, ZR(JSIGN), ZR(JPHYD), ZR(JGDEQ))

C 9.6 CALCUL DU DOMMAGE ELEMENTAIRE DE WOHLER

         CALL JERAZO('&&AVGRNO.NCYC_RUPT', NBVEC*NBORDR, 1)
         CALL JERAZO('&&AVGRNO.DOMM_ELEM', NBVEC*NBORDR, 1)

            CALL AVDOWH(NBVEC, NBORDR, NOMMAT, NOMCRI, ZI(INCYC),
     &                  ZR(JGDEQ), ZR(JDOEL), ZR(JNRUP))

C 9.7 CALCUL DU DOMMAGE TOTAL (CUMUL)

         CALL JERAZO('&&AVGRNO.DOMM_TOT', NBVEC, 1)

         CALL AVDOMT(NBVEC, NBORDR, ZI(INCYC), ZR(JDOEL),
     &               ZR(JDOTO))

C 9.8 CALCUL DU CUMUL DOMMAGE MAXIMAL ET VECTEUR NORMAL ASSOCIE

         CALL AVCDMX(NBVEC, ZR(JDOTO), CUDOMX, VNORMX)

C 10. SECOND RAFFINEMENT CONCERNANT LA DETERMINATION DU VECTEUR NORMAL
C     CORRESPONDANT AU MAX DES CUMULS DE DOMMAGE.

         NXM = ZR(JVECN2 + (VNORMX-1)*3)
         NYM = ZR(JVECN2 + (VNORMX-1)*3 + 1)
         NZM = ZR(JVECN2 + (VNORMX-1)*3 + 2)
         GAMMAM = ATAN2(SQRT(ABS(1.0D0-NZM**2)),NZM)
         IF (GAMMAM .LT. 0.0D0) THEN
            GAMMAM = GAMMAM + PI
         ENDIF

         IF ((ABS(NYM) .LT. EPSILO) .AND.
     &       (ABS(NXM) .LT. EPSILO)) THEN
           PHIM = 0.0D0
         ELSE
           PHIM = ATAN2(ABS(NYM),NXM)
         ENDIF
         IF (PHIM .LT. 0.0D0) THEN
           PHIM = PHIM + PI
         ENDIF

         IF (ABS(GAMMAM) .LT. EPSILO) THEN
            GAMMA = 5.0D0*(PI/180.0D0)
            DPHI2 = 60.0D0*(PI/180.0D0)
            IDEB = 1
            IFIN = 6
            N = 0
            K = 1
            DIM = 27
            PHI0 = 0.0D0
            CALL VECNUV(IDEB, IFIN, GAMMA, PHI0, DPHI2, N, K, DIM,
     &                  ZR(JVECN1), ZR(JVECU1), ZR(JVECV1))

            GAMMA = 0.0D0
            PHI0 = PI
            IDEB = 1
            IFIN = 1
            K = 1
            CALL VECNUV(IDEB, IFIN, GAMMA, PHI0, DPHI2, N, K, DIM,
     &                  ZR(JVECN1), ZR(JVECU1), ZR(JVECV1))

C 10.1 PROJECTION DE L'HISTORIQUE DU CISAILLEMENT SUR UN PLAN

            NBVEC = 7
            CALL JERAZO( '&&AVGRNO.VECTNO1', 18*NBORDR, 1 )
            CALL PROPLA(NBVEC, ZR(JVECN1), ZR(JVECU1), ZR(JVECV1),
     &                  NBORDR, KWORK, SOMNOW, VWORK, TDISP, TSPAQ,
     &                  IBIDNO, NOMCRI, FATSOC, ZR(JVNO1))
         ELSE
            DGAM2 = 1.0D0*(PI/180.0D0)
            DPHI2 = DGAM2/SIN(GAMMAM)
            N = 0
            K = 2
            DIM = 27
            IDEB = 1
            IFIN = 3
            DO 440 J=1, 3
               GAMMA = GAMMAM + (J-K)*DGAM2
               CALL VECNUV(IDEB, IFIN, GAMMA, PHIM, DPHI2, N, K, DIM,
     &                     ZR(JVECN1), ZR(JVECU1), ZR(JVECV1))
 440        CONTINUE

            NBVEC = 9
            CALL JERAZO( '&&AVGRNO.VECTNO1', 18*NBORDR, 1 )
            CALL PROPLA(NBVEC, ZR(JVECN1), ZR(JVECU1), ZR(JVECV1),
     &                  NBORDR, KWORK, SOMNOW, VWORK, TDISP, TSPAQ,
     &                  IBIDNO, NOMCRI, FATSOC, ZR(JVNO1))
         ENDIF

         CALL JERAZO('&&AVGRNO.AXE', NBVEC*NBORDR, 1)
         CALL JERAZO('&&AVGRNO.FLAG', NBVEC, 1)
         CALL JERAZO('&&AVGRNO.MINMAX', 4*NBVEC, 1)

C 10.2 ENCADREMENT DES POINTS DANS LE PLAN

         CALL AVENCA(ZR(JVNO1), NBVEC, NBORDR, LSIG0, ZI(JFLAG),
     &               ZR(JMIMA))

C 10.3 PROJECTION DE L'HISTORIQUE DE CHARGEMENT SUR UN OU DEUX AXES

         CALL PROJAX(ZR(JVNO1), NBVEC, NBORDR, PROAXE, ZI(JFLAG),
     &               ZR(JMIMA), ZR(JAXE))

C 10.4 COMPTAGE RAINFLOW (NORME AFNOR + POSTDAM)

C 10.4.1 PREMIER FILTRAGE DES PICS DE LA FONCTION

         CALL JERAZO('&&AVGRNO.NB_POIN', NBVEC, 1)
         CALL JERAZO('&&AVGRNO.VAL_POIN', NBVEC*NBORDR, 1)
         CALL JERAZO('&&AVGRNO.NUME_ORDR', NBVEC*NBORDR, 1)

         CALL AVPEAK(ZR(JAXE), NBVEC, NBORDR, PSEUIL, ZI(JFLAG),
     &               ZI(JNPOIN), ZR(JVPOIN), ZI(JVORDR))

C 10.4.2 REARANGEMENT ET EXTRACTION DES PICS

         CALL JERAZO('&&AVGRNO.POIN_TRAV', NBORDR+2, 1)
         CALL JERAZO('&&AVGRNO.ORDR_TRAV', 2*(NBORDR+2), 1)

         CALL JERAZO('&&AVGRNO.NB_PIC', NBVEC, 1)
         CALL JERAZO('&&AVGRNO.VAL_PICS', NBVEC*(NBORDR+2), 1)
         CALL JERAZO('&&AVGRNO.NORD_PIC', NBVEC*(NBORDR+2), 1)

         CALL AVPIC2(METHOD, NBVEC, NBORDR, ZR(IVTRAV),
     &               ZI(INTRAV), ZI(JNPOIN), ZR(JVPOIN),
     &               ZI(JVORDR), ZI(INPIC), ZR(IVPIC), ZI(IORPIC))

C 10.4.3 COMPTAGE RAINFLOW

         CALL JERAZO('&&AVGRNO.NB_CYCL', NBVEC, 1)
         CALL JERAZO('&&AVGRNO.VAL_MIN', NBVEC*(NBORDR+2), 1)
         CALL JERAZO('&&AVGRNO.VAL_MAX', NBVEC*(NBORDR+2), 1)
         CALL JERAZO('&&AVGRNO.NORD_MIN', NBVEC*(NBORDR+2), 1)
         CALL JERAZO('&&AVGRNO.NORD_MAX', NBVEC*(NBORDR+2), 1)

         CALL AVRAIN(NBVEC, NBORDR, ZI(INTRAV), ZI(INPIC),
     &               ZR(IVPIC), ZI(IORPIC), FATSOC, ZI(INCYC),
     &               ZR(IVMIN), ZR(IVMAX), ZI(IOMIN), ZI(IOMAX))

C 10.5 CALCUL DE LA CONTRAINTE EQUIVALENTE AU SENS DES CRITERES

         CALL JERAZO('&&AVGRNO.SIG_NORM', NBVEC*NBORDR, 1)
         CALL JERAZO('&&AVGRNO.PRES_HYDR', NBORDR, 1)

         IF (( NOMCRI(1:9) .EQ. 'DOMM_MAXI' ) .OR.
     &       ( NOMCRI(1:12) .EQ. 'FATEMI_SOCIE' )) THEN

C 10.5.1 CALCUL DE LA CONTRAINTE NORMALE

            CALL JXVERI('MESSAGE','VERI18')

            CALL AVSIGN(NBVEC, NBORDR, ZR(JVECN1),
     &                  VWORK, TDISP, KWORK, 
     &                  SOMNOW, TSPAQ, IBIDNO, NOMCRI, ZR(JSIGN))

         ELSEIF ( NOMCRI(1:16) .EQ. 'DANG_VAN_MODI_AV' ) THEN

C 10.5.2 CALCUL DE LA PRESSION HYDROSTATIQUE

            CALL JXVERI('MESSAGE','VERI18')

            CALL AVPHYD(NBORDR, VWORK, TDISP, KWORK, SOMNOW,
     &                  TSPAQ, IBIDNO, ZR(JPHYD))            

         ENDIF

C 10.5.3 CALCUL DE LA GRANDEUR EQUIVALENTE AU SENS DU CRITERE CHOISI :
C        DOMM_MAXI, FATEMI ET SOCIE (ELASTIQUE OU PLASTIQUE), DANG VAN

         CALL JERAZO('&&AVGRNO.GDR_EQUI', NBVEC*NBORDR, 1)

         CALL AVCRIT(NBVEC, NBORDR, VALA, COEFPA, ZI(INCYC),
     &               ZR(IVMIN), ZR(IVMAX), ZI(IOMIN), ZI(IOMAX),
     &               NOMCRI, ZR(JSIGN), ZR(JPHYD), ZR(JGDEQ))

C 10.6 CALCUL DU DOMMAGE ELEMENTAIRE DE WOHLER

         CALL JERAZO('&&AVGRNO.NCYC_RUPT', NBVEC*NBORDR, 1)
         CALL JERAZO('&&AVGRNO.DOMM_ELEM', NBVEC*NBORDR, 1)

            CALL AVDOWH(NBVEC, NBORDR, NOMMAT, NOMCRI, ZI(INCYC),
     &                  ZR(JGDEQ), ZR(JDOEL), ZR(JNRUP))

C 10.7 CALCUL DU DOMMAGE TOTAL (CUMUL)

         CALL JERAZO('&&AVGRNO.DOMM_TOT', NBVEC, 1)

         CALL AVDOMT(NBVEC, NBORDR, ZI(INCYC), ZR(JDOEL),
     &               ZR(JDOTO))

C 10.8 CALCUL DU CUMUL DOMMAGE MAXIMAL ET VECTEUR NORMAL ASSOCIE

         CALL AVCDMX(NBVEC, ZR(JDOTO), CUDOMX, VNORMX)

         NXM = ZR(JVECN1 + (VNORMX-1)*3)
         NYM = ZR(JVECN1 + (VNORMX-1)*3 + 1)
         NZM = ZR(JVECN1 + (VNORMX-1)*3 + 2)

C 11. CONSTRUCTION D'UN CHAM_ELEM SIMPLE PUIS D'UN CHAM_ELEM CONTENANT
C     POUR CHAQUE POINT DE GAUSS DE CHAQUE MAILLE LE DOMMAGE_MAX ET LE
C     VECTEUR NORMAL ASSOCIE.

 555     CONTINUE

         DO 600 ICMP=1, 22
            VRESU(ICMP) = 0.0D0
 600     CONTINUE
         VRESU(2) = NXM
         VRESU(3) = NYM
         VRESU(4) = NZM
         VRESU(11) = CUDOMX

C 12. AFFECTATION DES RESULTATS DANS UN CHAM_ELEM SIMPLE

         DO 610 ICMP=1, 22
            JAD = 22*(NUNOE-1) + ICMP
            ZL(JCNRL - 1 + JAD) = .TRUE.
            ZR(JCNRV - 1 + JAD) = VRESU(ICMP)
 610     CONTINUE

 400  CONTINUE

C MENAGE

      CALL DETRSD('CHAM_ELEM_S',CESMAT)

      CALL JEDETR('&&AVGRNO.VECT_NORMA')
      CALL JEDETR('&&AVGRNO.VECT_TANGU')
      CALL JEDETR('&&AVGRNO.VECT_TANGV')
      CALL JEDETR('&&AVGRNO.VECT_NORMA1')
      CALL JEDETR('&&AVGRNO.VECT_TANGU1')
      CALL JEDETR('&&AVGRNO.VECT_TANGV1')
      CALL JEDETR('&&AVGRNO.VECT_NORMA2')
      CALL JEDETR('&&AVGRNO.VECT_TANGU2')
      CALL JEDETR('&&AVGRNO.VECT_TANGV2')
      CALL JEDETR('&&AVGRNO.VECTNO1')
      CALL JEDETR('&&AVGRNO.VECTNO2')
      CALL JEDETR('&&AVGRNO.AXE')
      CALL JEDETR('&&AVGRNO.FLAG')
      CALL JEDETR('&&AVGRNO.MINMAX')
      CALL JEDETR('&&AVGRNO.NB_POIN')
      CALL JEDETR('&&AVGRNO.VAL_POIN')
      CALL JEDETR('&&AVGRNO.NUME_ORDR')
      CALL JEDETR('&&AVGRNO.POIN_TRAV')
      CALL JEDETR('&&AVGRNO.ORDR_TRAV')
      CALL JEDETR('&&AVGRNO.NB_PIC')
      CALL JEDETR('&&AVGRNO.VAL_PICS')
      CALL JEDETR('&&AVGRNO.NORD_PIC')
      CALL JEDETR('&&AVGRNO.NB_CYCL')
      CALL JEDETR('&&AVGRNO.VAL_MIN')
      CALL JEDETR('&&AVGRNO.VAL_MAX')
      CALL JEDETR('&&AVGRNO.NORD_MIN')
      CALL JEDETR('&&AVGRNO.NORD_MAX')
      CALL JEDETR('&&AVGRNO.SIG_NORM')
      CALL JEDETR('&&AVGRNO.PRES_HYDR')
      CALL JEDETR('&&AVGRNO.GDR_EQUI')
      CALL JEDETR('&&AVGRNO.NCYC_RUPT')
      CALL JEDETR('&&AVGRNO.DOMM_ELEM')
      CALL JEDETR('&&AVGRNO.DOMM_TOT')
      CALL JEDETR('&&AVGRNO.VECTNO')
      CALL JEDETR('&&AVGRNO.CNCINV')
C
      CALL JEDEMA()
      END
