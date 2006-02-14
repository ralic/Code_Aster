      SUBROUTINE NMCONV (CNRESI, CNDIRI, CNFEXT, CNVCFO, PARCRI,
     &                   ITERAT, ETA   , CONV  , LICCVG, ITEMAX,
     &                   CONVER, ECHLDC, ECHEQU, ECHCON, ECHPIL,
     &                   FINPAS, CRITNL, NUMINS, FOINER, PARTPS,
     &                   PARMET, NEQ,    DEPDEL, AUTOC1, AUTOC2,
     &                   VECONT, LREAC,  CNVFRE, MAILLA, CNVCF1,
     &                   NUMEDD, DEFICO, RESOCO, IMPRCO, ZFON  ,
     &                   FONACT, MAXREL)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/02/2006   AUTEUR MABBAS M.ABBAS 
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
C TOLE CRP_20
C RESPONSABLE MABBAS M.ABBAS
C ======================================================================
      IMPLICIT     NONE
      INTEGER      ZFON
      LOGICAL      FONACT(ZFON)
      LOGICAL      ITEMAX, CONVER, ECHLDC, FINPAS, ECHCON(2), ECHEQU
      LOGICAL      LREAC(2),MAXREL,ECHPIL
      INTEGER      ITERAT, LICCVG(*), NUMINS, NEQ, VECONT(2)
      REAL*8       ETA, CONV(*), PARCRI(*), PARMET(*)
      CHARACTER*19 CRITNL, CNRESI, CNDIRI, CNFEXT, CNVCFO, CNVFRE
      CHARACTER*19 FOINER, PARTPS, AUTOC1, AUTOC2, CNVCF1
      CHARACTER*24 DEPDEL
      CHARACTER*8  MAILLA
      CHARACTER*24 NUMEDD
      CHARACTER*24 RESOCO
      CHARACTER*24 DEFICO
      CHARACTER*24 IMPRCO

C ----------------------------------------------------------------------
C
C     COMMANDE STAT_NON_LINE : VERIFICATION DES CRITERES D'ARRET
C
C ----------------------------------------------------------------------
C IN  CNRESI  : FINT + BT.LAMBDA
C IN  CNDIRI  : BT.LAMBDA
C IN  CNFEXT  : RESULTANTE DES EFFORTS EXTERIEURS
C IN  CNVCFO  : FORCE DE REFERENCE DES VARI_COM
C IN  PARCRI  : CRITERES DE CONVERGENCE
C                1 : ITER_GLOB_MAXI
C                2 : RESI_GLOB_RELA
C                3 : RESI_GLOB_MAXI
C                4 : ARRET
C                5 : ITER_GLOB_ELAS
C                6 : RESI_REFE_RELA
C               10 : RESI_PRIM_ABSO (LAGRANGIEN)
C               11 : RESI_DUAL_ABSO (LAGRANGIEN)
C IN  ITERAT  : NUMERO D'ITERATION
C IN  ETA     : COEFFICIENT DE PILOTAGE
C IN  CONV    : INFORMATIONS SUR LA CONVERGENCE DU CALCUL
C                1 - RESI_DUAL_ABSO      (LAGRANGIEN AUGMENTE)
C                2 - RESI_PRIM_ABSO      (LAGRANGIEN AUGMENTE)
C                3 - NOMBRE D'ITERATIONS DUAL (LAGRANGIEN AUGMENTE)
C                4 - NUMERO ITERATION BFGS (LAGRANGIEN AUGMENTE)
C               10 - NOMBRE D'ITERATIONS (RECHERCHE LINEAIRE)
C               11 - RHO                 (RECHERCHE LINEAIRE)
C               20 - RESI_GLOB_RELA
C               21 - RESI_GLOB_MAXI
C IN  LICCVG : CODES RETOURS
C              (1) : PILOTAGE
C                  =  0 CONVERGENCE
C                  =  1 PAS DE CONVERGENCE
C                  = -1 BORNE ATTEINTE
C              (2) : INTEGRATION DE LA LOI DE COMPORTEMENT
C                  = 0 OK
C                  = 1 ECHEC DANS L'INTEGRATION : PAS DE RESULTATS
C                  = 3 SIZZ NON NUL (DEBORST) ON CONTINUE A ITERER
C              (3) : TRAITEMENT DU CONTACT UNILATERAL EN GD. DEPL.
C                  = 0 OK
C                  = 1 ECHEC DANS LE TRAITEMENT DU CONTACT
C              (4) : TRAITEMENT DU CONTACT UNILATERAL EN GD. DEPL.
C                  = 0 OK
C                  = 1 MATRICE DE CONTACT SINGULIERE
C              (5) : MATRICE DU SYSTEME (MATASS)
C                  = 0 OK
C                  = 1 MATRICE SINGULIERE
C IN  FOINER  : FORCES D'INERTIE EN DYNA
C IN  PARTPS  : LISTE D'INSTANT
C IN  PARMET  : PARAMETRES DE LA METHODE DE RESOLUTION 
C                       3 - PAS_MINI_ELAS
C IN  ZFON    : LONGUEUR MAXI DU VECTEUR FONACT
C IN  FONACT  : FONCTIONNALITES ACTIVEES
C            FONACT(1):  RECHERCHE LINEAIRE
C            FONACT(2):  PILOTAGE
C            FONACT(3):  LOIS NON LOCALES
C            FONACT(4):  CONTACT DISCRET
C            FONACT(5):  CONTACT CONTINU
C            FONACT(6):  METHODE XFEM
C            FONACT(7):  ALGORITHME DE DE BORST
C            FONACT(8):  CONVERGENCE PAR RESIDU DE REFERENCE
C            FONACT(9):  METHODE XFEM AVEC CONTACT
C            FONACT(10): CONTACT/FROTTEMENT CONTINU
C OUT  ITEMAX : .TRUE. SI ITERATION MAXIMUM ATTEINTE
C OUT  CONVER : .TRUE. SI CONVERGENCE REALISEE
C OUT  ECHLDC : .TRUE. SI ECHEC INTEGRATION LOI DE COMPORTEMENT
C OUT  ECHCON : .TRUE. SI ECHEC TRAITEMENT DU CONTACT UNILATERAL
C OUT  ECHEQU : .TRUE. SI MATASS SINGULIERE
C OUT  ECHPIL : .TRUE. SI ECHEC PILOTAGE
C OUT  FINPAS : .TRUE. SI ON NE FAIT PLUS D'AUTRES PAS DE TEMPS
C OUT  MAXREL : .TRUE. SI CRITERE RESI_GLOB_RELA ET CHARGEMENT = 0,
C                             ON UTILISE RESI_GLOB_MAXI
C IN   MAILLA : NOM DU MAILLAGE
C IN   DEFICO : SD DE TRAITEMENT DU CONTACT (ISSU D'AFFE_CHAR_MECA)
C IN   RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN   IMPRCO : SD AFFICHAGE
C IN   NUMEDD : NUMEROTATION NUME_DDL
C IN/JXOUT CRITNL : SYNTHESE DES RESULTATS DE CONVERGENCE POUR 
C                   ARCHIVAGE 
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      LOGICAL      ERROR
      LOGICAL      CREFE,CINIT
      INTEGER      LONCH,NOCC
      INTEGER      JCRR
      INTEGER      IBID
      INTEGER      TYPALC
      REAL*8       R8VIDE,R8BID
      REAL*8       INSTAM,INSTAP,DIINST,PASMIN
      CHARACTER*8  K8BID
      CHARACTER*16 GEONOE
      REAL*8       GEOVAL
      CHARACTER*16 K16BID
      CHARACTER*24 IMPCNT,IMPCNL,IMPCNV,IMPCNA
      INTEGER      JIMPCT,JIMPCL,JIMPCV,JIMPCA
      REAL*8       VRELA,VMAXI,VREFE,VRESI,VCHAR,VINIT
      INTEGER      IRELA,IMAXI,IREFE,IRESI,ICHAR,IINIT
      LOGICAL      CTCFIX,CTCGEO,CTCCVG
      INTEGER      CTCITE
      LOGICAL      CBORST,BORCVG
      INTEGER      CTCINT(2)
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      FINPAS = .FALSE.
      ITEMAX = .FALSE.
      ERROR  = .FALSE.
      CBORST = LICCVG(2).EQ.3
      BORCVG = .TRUE.
      CTCCVG = .FALSE.
      IMPCNT = IMPRCO(1:14)//'CONV.TYPE'
      IMPCNL = IMPRCO(1:14)//'CONV.LIEU'
      IMPCNV = IMPRCO(1:14)//'CONV.VAL'
      IMPCNA = IMPRCO(1:14)//'CONV.ACT'
      CALL JEVEUO(IMPCNT,'E',JIMPCT)
      CALL JEVEUO(IMPCNL,'E',JIMPCL)
      CALL JEVEUO(IMPCNV,'E',JIMPCV)
      CALL JEVEUO(IMPCNA,'E',JIMPCA)
C
C --- NOUVEL INSTANT
C
      INSTAM = DIINST(PARTPS, NUMINS-1)
      INSTAP = DIINST(PARTPS, NUMINS  )
C
C ======================================================================
C
C    TRAITEMENT DES CODES RETOUR D'ERREUR
C
C ======================================================================
C
C --- LOI DE COMPORTEMENT
C 
      ECHLDC = (LICCVG(2).EQ.1)
      IF (ECHLDC) THEN
        ERROR  = .TRUE.
        CALL NMIMPR('IMPR','ERREUR','ECHEC_LDC',0.D0,1)
      END IF
C
C --- PILOTAGE
C
      ECHPIL = LICCVG(1) .EQ. 1
      IF (ECHPIL) THEN
        ERROR  = .TRUE.
        CALL NMIMPR('IMPR','ERREUR','ECHEC_PIL',0.D0,1)
      END IF
C
C --- MATRICE DU SYSTEME
C
      ECHEQU = LICCVG(5).NE.0
      IF (ECHEQU) THEN
        ERROR  = .TRUE.
        CALL NMIMPR('IMPR','ERREUR','MATR_SING',0.D0,0)
      END IF
C
C --- CONTACT
C
      ECHCON(1) = LICCVG(3) .NE. 0
      IF (ECHCON(1)) THEN
        ERROR  = .TRUE.
        CALL NMIMPR('IMPR','ERREUR','CONT_ERR',0.D0,1)
      END IF

      ECHCON(2) = LICCVG(4) .NE. 0
      IF (ECHCON(2)) THEN
        ERROR  = .TRUE.
        CALL NMIMPR('IMPR','ERREUR','CONT_SING',0.D0,0)
      END IF

      IF (ERROR) THEN
        CONVER = .FALSE.
        GOTO 9999
      ENDIF
C
C ======================================================================
C
C    CALCUL DES RESIDUS
C
C ======================================================================
C
C --- LONGUEUR DES VECTEURS RESIDUS ET CHARGEMENT
C
      CALL JELIRA(CNRESI//'.VALE','LONMAX',LONCH,K8BID)
C
C --- DOIT-ON VERIFIER UN RESI_GLOB_REFE ?
C
      CREFE = PARCRI(6).NE.R8VIDE()
C
C --- DOIT-ON VERIFIER LES VARIABLES DE COMMANDE INITIALES ?
C
      CALL GETFAC('ETAT_INIT',NOCC)
      CINIT = (NUMINS.EQ.1).AND.(NOCC.EQ.0)
C
C --- CALCUL DES RESIDUS ET DES CHARGEMENTS
C
      CALL NMRESI(CNRESI,CNDIRI,CNFEXT,CNVCFO,
     &            CNVFRE,FOINER,CNVCF1,
     &            LONCH,CREFE,CINIT,
     &            VRELA,VMAXI,VCHAR,VRESI,VREFE,VINIT,
     &            IRELA,IMAXI,ICHAR,IRESI,IREFE,IINIT)

C
C --- ECRITURE DES INFOS SUR LES RESIDUS POUR AFFICHAGE
C
      CALL IMPCMP(IRELA,NUMEDD,ZK16(JIMPCL-1+1))
      ZK16(JIMPCT-1+1) = 'RESI_GLOB_RELA'
      ZR(JIMPCV-1+1)   = VRELA

      CALL IMPCMP(IMAXI,NUMEDD,ZK16(JIMPCL-1+2))
      ZK16(JIMPCT-1+2) = 'RESI_GLOB_MAXI'
      ZR(JIMPCV-1+2)   = VMAXI

      CALL IMPCMP(IREFE,NUMEDD,ZK16(JIMPCL-1+3))
      ZK16(JIMPCT-1+3) = 'RESI_GLOB_REFE'
      ZR(JIMPCV-1+3)   = VREFE
 
      ZK16(JIMPCL-1+4) = 'INCONNU        '
      ZK16(JIMPCT-1+4) = 'RESI_DUAL_ABSO'
      ZR(JIMPCV-1+4)   = CONV(1)
   
      ZK16(JIMPCL-1+5) = 'INCONNU        ' 
      ZK16(JIMPCT-1+5) = 'RESI_PRIM_ABSO'
      ZR(JIMPCV-1+5)   = CONV(2)
C
C --- SAUVEGARDES RESIDUS
C
      CONV(20) = VRELA
      CONV(21) = VMAXI
C
C --- VERIFICATION QUE LES VARIABLES DE COMMANDE INITIALES CONDUISENT
C --- A DES FORCES NODALES NULLES
C
      IF (CINIT) THEN
         IF (VCHAR.GT.PARCRI(2)) THEN
           VINIT = VINIT/VCHAR
         ENDIF
         IF (VINIT.GT.PARCRI(2)) THEN
            CALL UTMESS('A','OP0070',
     &                  'LES VARIABLES DE COMMANDES INITIALES'//
     &                  ' INDUISENT DES CONTRAINTES INCOMPATIBLES')
         ENDIF
      ENDIF
C
C ======================================================================
C
C    AFFICHAGES DANS LE TABLEAU DE CONVERGENCE
C
C ======================================================================
C
C --- INSTANT
C
      CALL IMPSDR(IMPRCO(1:14),
     &            'INCR_TPS ',K16BID,INSTAP,IBID)
C
C --- NUMERO ITERATION NEWTON
C
      CALL IMPSDR(IMPRCO(1:14),
     &            'ITER_NEWT',K16BID,R8BID,ITERAT)
C
C --- RESIDUS
C
      CALL IMPSDR(IMPRCO(1:14),
     &            'RESI_RELA',K16BID,VRELA,IBID)
      CALL IMPSDR(IMPRCO(1:14),
     &            'RELA_NOEU',ZK16(JIMPCL-1+1),R8BID,IBID)
      CALL IMPSDR(IMPRCO(1:14),
     &            'RESI_MAXI',K16BID,VMAXI,IBID)
      CALL IMPSDR(IMPRCO(1:14),
     &            'MAXI_NOEU',ZK16(JIMPCL-1+2),R8BID,IBID)
      CALL IMPSDR(IMPRCO(1:14),
     &            'RESI_REFE',K16BID,VREFE,IBID)
      CALL IMPSDR(IMPRCO(1:14),
     &            'REFE_NOEU',ZK16(JIMPCL-1+3),R8BID,IBID)
C
C --- CRITERES DU LAGRANGIEN AUGMENTE
C
      CALL IMPSDR(IMPRCO(1:14),
     &            'LAGR_ECAR',K16BID,CONV(1),IBID)
      CALL IMPSDR(IMPRCO(1:14),
     &            'LAGR_INCR',K16BID,CONV(2),IBID)
      CALL IMPSDR(IMPRCO(1:14),
     &            'LAGR_ITER',K16BID,R8BID,INT(CONV(3)))
C
C --- CRITERES RECHERCHE LINEAIRE
C
      IF (ITERAT.EQ.0) THEN 
        CONV(10) = 0
      ENDIF
      CALL IMPSDR(IMPRCO(1:14),
     &            'RELI_ITER',K16BID,R8BID,INT(CONV(10)))

      IF (NINT(CONV(10)).EQ.0) THEN
        CALL IMPSDR(IMPRCO(1:14),
     &              'RELI_COEF',K16BID,1.D0,IBID)
      ELSE
        CALL IMPSDR(IMPRCO(1:14),
     &              'RELI_COEF',K16BID,CONV(11),IBID)
      ENDIF
C
C --- CRITERES PILOTAGE
C
      CALL IMPSDR(IMPRCO(1:14),
     &            'PILO_PARA',K16BID,ETA,IBID)
C
C ======================================================================
C
C    EXAMEN DE LA CONVERGENCE
C
C ======================================================================
C
C --- LE PILOTAGE A ATTEINT LES BORNES
C
      IF (LICCVG(1) .EQ. -1) THEN
        CALL IMPSDM(IMPRCO(1:14),'PILO_PARA','B')
        FINPAS  = .TRUE.
      END IF
C 
C --- EXAMEN DU NOMBRE D'ITERATIONS
C 
      PASMIN = PARMET(3)
      IF (ABS(INSTAP-INSTAM) .LT. PASMIN) THEN
        ITEMAX = ITERAT .GE. PARCRI(5)
      ELSE
        ITEMAX = ITERAT .GE. PARCRI(1)
      ENDIF
C
C --- COLONNES A TESTER POUR LE CRITERE D'ARRET
C
C     CVGTST(1): TEST SUR RESI_GLOB_RELA
C     CVGTST(2): TEST SUR RESI_GLOB_MAXI
C     CVGTST(3): TEST SUR RESI_REFE_RELA
C     CVGTST(4): TEST SUR RESI_DUAL_ABSO
C     CVGTST(5): TEST SUR RESI_PRIM_ABSO
C
      ZL(JIMPCA-1+1) = (PARCRI(2)  .NE. R8VIDE())
      ZL(JIMPCA-1+2) = (PARCRI(3)  .NE. R8VIDE())
      ZL(JIMPCA-1+3) = (PARCRI(6)  .NE. R8VIDE())
      ZL(JIMPCA-1+4) = (PARCRI(11) .NE. R8VIDE())
      ZL(JIMPCA-1+5) = (PARCRI(10) .NE. R8VIDE())
C
C --- CRITERES D'ARRET
C
      CONVER = .TRUE.
      IF (ZL(JIMPCA-1+1)) THEN
        IF ((VRELA.GT.PARCRI(2)).OR.(VRELA.LT.0.D0)) THEN
           CONVER = .FALSE.
           CALL IMPSDM(IMPRCO(1:14),'RESI_RELA','X')
        ELSE
           CALL IMPSDM(IMPRCO(1:14),'RESI_RELA',' ')
        ENDIF
      ENDIF
      IF (ZL(JIMPCA-1+2)) THEN
        IF ((VMAXI.GT.PARCRI(3)).OR.(VMAXI.LT.0.D0)) THEN
           CONVER = .FALSE.
           CALL IMPSDM(IMPRCO(1:14),'RESI_MAXI','X')
        ELSE
           CALL IMPSDM(IMPRCO(1:14),'RESI_MAXI',' ')
        ENDIF
      ENDIF
      IF (ZL(JIMPCA-1+3)) THEN
        IF ((VREFE.GT.PARCRI(6)).OR.(VREFE.LT.0.D0)) THEN
           CONVER = .FALSE.
           CALL IMPSDM(IMPRCO(1:14),'RESI_REFE','X')
        ELSE
           CALL IMPSDM(IMPRCO(1:14),'RESI_REFE',' ')
        ENDIF
      ENDIF
      IF (ZL(JIMPCA-1+4)) THEN
        IF ((CONV(1).GT.PARCRI(11)).OR.(CONV(1).LT.0.D0)) THEN
           CONVER = .FALSE.
           CALL IMPSDM(IMPRCO(1:14),'LAGR_ECAR','X')
        ELSE
           CALL IMPSDM(IMPRCO(1:14),'LAGR_ECAR',' ')
        ENDIF
      ENDIF
      IF (ZL(JIMPCA-1+5)) THEN
        IF ((CONV(2).GT.PARCRI(10)).OR.(CONV(2).LT.0.D0)) THEN
           CONVER = .FALSE.
           CALL IMPSDM(IMPRCO(1:14),'LAGR_INCR','X')
        ELSE
           CALL IMPSDM(IMPRCO(1:14),'LAGR_INCR',' ')
        ENDIF
      ENDIF
C
C --- SI CRITERE RESI_GLOB_RELA ET CHARGEMENT = 0, 
C --- ON UTILISE RESI_GLOB_MAXI
C
      MAXREL = .FALSE.
      IF (ZL(JIMPCA-1+1) .AND. (NUMINS .GT. 1)) THEN
         CALL JEVEUO(CRITNL //'.CRTR','L',JCRR)
         IF (VCHAR .LT. (1.D-6 * ZR(JCRR+5))) THEN
           CALL IMPSDM(IMPRCO(1:14),'RESI_RELA',' ')
           CALL IMPSDM(IMPRCO(1:14),'RESI_MAXI','X')
           IF (VRESI .LT. ZR(JCRR+6)) THEN
               CALL IMPSDM(IMPRCO(1:14),'RESI_RELA',' ')
               CALL IMPSDM(IMPRCO(1:14),'RESI_MAXI',' ')
               CONVER = .TRUE.
               MAXREL = .TRUE.
               CALL UTMESS('I','NMCONV',
     &                     'CONVERGENCE ATTEINTE AVEC RESI_GLOB_MAXI'//
     &                     ' POUR CAUSE DE CHARGEMENT PRESQUE NUL')
           ENDIF
         ENDIF
      ENDIF
C
C ======================================================================
C --- CONVERGENCE ADAPTEE AU CONTACT DISCRET
C ======================================================================
C
C
C --- TYPE DE CONTACT
C
      CALL CFDISC(DEFICO,'              ',TYPALC,IBID,IBID,IBID)

      IF (FONACT(4).AND.(TYPALC.NE.5)) THEN
C
C --- NOMBRE ITERATIONS INTERNES DE CONTACT POUR L'ITERATION COURANTE
C
         CALL CFITER(RESOCO,'L','ITER',CTCITE,R8BID)

         CALL IMPSDR(IMPRCO(1:14),
     &               'CTCD_ITER',K16BID,R8BID,CTCITE)
C
C --- TRAITEMENT DE LA REACTUALISATION GEOMETRIQUE POUR LE CONTACT
C
         IF (CONVER) THEN
          
           CTCFIX = LREAC(2)
         
           CALL CFCONV(MAILLA,NEQ,DEPDEL,AUTOC1,AUTOC2,
     &                 VECONT,CTCGEO,CTCFIX,
     &                 GEONOE,GEOVAL)

           IF (CTCGEO) THEN
C
C --- REACTUALISATION GEOMETRIQUE A FAIRE (-> NON CVG DU CONTACT)
C
             CTCCVG = .FALSE.
             CALL IMPSDR(IMPRCO(1:14),
     &                   'CTCD_INFO',' ALGO/REAC_GEOM  ',R8BID,IBID)
             CALL IMPSDR(IMPRCO(1:14),
     &                   'CTCD_NOEU',GEONOE,R8BID,IBID)
             CALL IMPSDR(IMPRCO(1:14),
     &                   'CTCD_GEOM',' ',GEOVAL,IBID)
           ELSE
C
C --- PAS DE REACTUALISATION GEOMETRIQUE A FAIRE
C
             IF (CTCFIX) THEN
C
C --- MAIS ATTENTE POINT FIXE (-> NON CVG DU CONTACT)
C
               CTCCVG = .FALSE.
               CALL IMPSDR(IMPRCO(1:14),
     &               'CTCD_INFO',' ATT_PT_FIXE    ',R8BID,IBID)
             ELSE
               CTCCVG = .TRUE.
               IF (ITERAT.EQ.0) THEN
                 CALL IMPSDR(IMPRCO(1:14),
     &                   'CTCD_INFO',' INIT_GEOM/ALGO  ',R8BID,IBID)  
               ELSE
                 CALL IMPSDR(IMPRCO(1:14),
     &               'CTCD_INFO',' ALGO.          ',R8BID,IBID)
               ENDIF
             ENDIF
             CALL IMPSDR(IMPRCO(1:14),
     &                   'CTCD_NOEU','                ',R8BID,IBID)
             CALL IMPSDR(IMPRCO(1:14),
     &                   'CTCD_GEOM',' ',R8VIDE(),IBID)
           ENDIF
         ELSE
           CTCCVG = .FALSE.
           CTCGEO = .FALSE.
           CTCFIX = .FALSE.
           IF (ITERAT.EQ.0) THEN
             CALL IMPSDR(IMPRCO(1:14),
     &                   'CTCD_INFO',' INIT_GEOM/ALGO  ',R8BID,IBID)   
           ELSE
             CALL IMPSDR(IMPRCO(1:14),
     &                   'CTCD_INFO',' ALGO.          ',R8BID,IBID)
           ENDIF
         ENDIF

         IF (CTCCVG) THEN
            CALL IMPSDM(IMPRCO(1:14),'CTCD_ITER',' ')
         ELSE
            CALL IMPSDM(IMPRCO(1:14),'CTCD_ITER','X')
         ENDIF
         
         LREAC(1) = CTCGEO
         LREAC(2) = CTCFIX
         
      ELSE
        CTCCVG = .TRUE.
      ENDIF
      
C
C ======================================================================
C --- CONVERGENCE ADAPTEE A LA METHODE DE DE BORST 
C ======================================================================
C
      IF (CBORST) THEN
         IF (CONVER) THEN
           CALL IMPSDR(IMPRCO(1:14),
     &                 'ITER_DEBO',' DE BORST...    ',R8BID,IBID)
           BORCVG = .FALSE.
         ELSE
           CALL IMPSDR(IMPRCO(1:14),
     &                 'ITER_DEBO','                ',R8BID,IBID)
         ENDIF
      ENDIF
C
C ======================================================================
C --- CONVERGENCE FINALE 
C ======================================================================
C
      CONVER = CONVER.AND.CTCCVG.AND.BORCVG
      IF (CONVER) THEN
         CALL IMPSDM(IMPRCO(1:14),'ITER_NEWT',' ')
      ELSE
         CALL IMPSDM(IMPRCO(1:14),'ITER_NEWT','X')
      ENDIF
C
C --- AFFICHAGE TABLEAU CONVERGENCE
C --- SAUF POUR METHODE CONTINUE: VOIR NMTBLE
C
      IF (.NOT.FONACT(5)) THEN
        CALL NMIMPR('IMPR','ETAT_CONV',' ',0.D0,0)      
      ENDIF
      IF (FONACT(5).AND.(.NOT.(CONVER))) THEN
        CALL NMIMPR('IMPR','ETAT_CONV',' ',0.D0,0)      
      ENDIF
C
C --- AFFICHAGE INFORMATIONS FINALES DE CONVERGENCE
C --- SAUF POUR METHODE CONTINUE: VOIR NMTBLE
C
      IF (CONVER) THEN
         IF (.NOT.FONACT(5)) THEN
C
C --- TYPE DE CONVERGENCE
C
           IF (MAXREL) THEN
             CALL NMIMPR('IMPR','MAXI_RELA',' ',0.D0,0)
           ELSE
             CALL NMIMPR('IMPR','CONV_OK',' ',0.D0,0)
           ENDIF
C
C --- RECAPITULATIF CRITERES CONVERGENCE - AFFICHAGE RESIDUS
C
           CALL NMIMPR('IMPR','CONV_RECA',' ',0.D0,0)
C
C --- INFOS DE CONVERGENCE CONTACT DISCRET
C
           IF (FONACT(4).AND.(TYPALC.NE.5)) THEN
C
C --- NOMBRE ITERATIONS INTERNES DE CONTACT POUR LE PAS DE TEMPS COURANT
C
             CALL CFITER(RESOCO,'L','ITEP',CTCITE,R8BID)
C --- ON ENLEVE LA PREMIERE RECHERCHE D'APPARIEMENT PUISQU'IL NE
C --- S'AGIT PAS D'UNE _RE_ ACTUALISATION GEOMETRIQUE
             CTCINT(1) = VECONT(2) - 1
             CTCINT(2) = CTCITE

             CALL NMIMPR('IMPR','CONV_CONT',GEONOE,GEOVAL,CTCINT)
           
           ENDIF

         ENDIF
      ELSE
         IF (ITEMAX) THEN
            CALL NMIMPR('IMPR','ERREUR','ITER_MAXI',0.D0,0)
         ENDIF
      ENDIF
C
C ======================================================================
C --- SAUVEGARDES INFOS CONVERGENCE 
C ======================================================================
C
C ======================================================================
C --- PREPARATION DES PARAMETRES ARCHIVES 
C ---   NOMBRE ITERATIONS NEWTON
C ---   NOMBRE ITERATIONS RECHERCHE LINEAIRE
C ---   RESI_GLOB_RELA
C ---   RESI_GLOB_MAXI
C ---   PARAMETRE DE PILOTAGE ETA
C ======================================================================
      CALL JEVEUO(CRITNL //'.CRTR','E',JCRR)
      ZR(JCRR+0) = ITERAT
      ZR(JCRR+1) = CONV(10)
      ZR(JCRR+2) = VRELA
      ZR(JCRR+3) = VMAXI
      ZR(JCRR+4) = ETA
      IF ((NUMINS.EQ.1) .AND. (ITERAT.EQ.0)) THEN 
         ZR(JCRR+5) = VCHAR
      ELSE
         IF ((CONVER).AND.(.NOT.MAXREL)) THEN
           ZR(JCRR+5) = MIN(VCHAR, ZR(JCRR+5))
         ENDIF
      ENDIF
      IF (CONVER) THEN
        ZR(JCRR+6) = VRESI
      ENDIF
C ======================================================================
 9999 CONTINUE
      CALL JEDEMA()
      END
