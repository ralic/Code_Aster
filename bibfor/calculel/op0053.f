      SUBROUTINE OP0053 ( IER )
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 03/07/2002   AUTEUR CIBHHPD D.NUNEZ 
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
C-----------------------------------------------------------------------
C
C      OPERATEUR :     CALC_G_THETA_T
C
C   -------------------------------------------------------------------
C     SUBROUTINES APPELLEES:
C       JEVEUX: JEMARQ,JEVEUO,RSUTNU,JECREO,JEECRA,DISMOI,RSEXCH,
C               RSEXC2,TBCRSD,TBAJPA,RSADPA,WKVECT,TBEXVE,DETRSD,
C               JEDETR,JEDETC,JEDEMA.
C       MESSAGE: INFMAJ,UTMESS,UTDEBM,UTIMPI,UTIMPK,UTFINM.
C       PARAMETRE: GETRES,GETVID,GETVTX,GETVR8,GETFAC,GETVIS.
C       DIVERS: RCMFMC,NMDORC,MEBILG,MEMAXG,MECALG,MLAGRG,MEFICG,
C               TITRE.
C
C     FONCTIONS INTRINSEQUES:
C       AUCUNE.
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       11/12/00 (OB): TOILETTAGE FORTRAN, INTRODUCTION DE L'OPTION
C                      'CALC_DG' CALCULANT LA DERIVEE DE G PAR RAPPORT
C                      A UNE VARIATION DE DOMAINE.
C----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INTEGER IER
            
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

C DECLARATION VARIABLES LOCALES      
      CHARACTER*6 NOMPRO
      PARAMETER ( NOMPRO = 'OP0O53' )
C
      INTEGER      I,J,ICHA,IBID,IORD,IRET,IVEC,IPROPA,IFOND
      INTEGER      JINST,LONVEC,NBPRUP,IORD1,IORD2
      INTEGER      NBINST,NC,NCHA,NDEP,NP,NRES,NBVAL,N1,IG,IBOR,NBORN
      INTEGER      NBCO

      REAL*8       TIME,ALPHA,PREC,RBID

      CHARACTER*8  MODELE, MATERI, TYPRUP(6)
      CHARACTER*8  RESU,RESULT,FOND,SYMECH,K8BI1,K8BID,CRIT
      CHARACTER*8 RESUSE, NOPASE
      CHARACTER*16 TYPE, OPER, OPTION, NOPRUP(6),OPTIO2
      CHARACTER*24 MATE,COMPOR,DEPLA,VECORD,VCHAR,THETA,SDTHET,
     &             DEPLA1,DEPLA2,DLAGDE

      LOGICAL      EXITIM

      CALL JEMARQ()
      CALL INFMAJ
      CALL GETRES(RESULT,TYPE,OPER)

C=======================================================================
C- MODELE, CHAM_MATER ET RELATION DE COMPORTEMENT --------------------
C=======================================================================

      CALL GETVID(' ','MODELE'    ,0,1,1,MODELE,IBID)
      CALL GETVID(' ','CHAM_MATER',0,1,1,MATERI,IBID)
      CALL GETVTX(' ','OPTION'    ,0,1,1,OPTION,IBID)
      CALL RCMFMC(MATERI,MATE)
      CALL NMDORC(MODELE,COMPOR)
C      CALL JEVEUO(COMPOR(1:19)//'.VALV','L',ICOMP)
C      IF ((ZK16(ICOMP).NE.'ELAS').AND.(OPTION.NE.'CALC_G')) THEN
C        CALL UTMESS('F',OPER,'ELASTICITE OBLIGATOIRE AVEC '//OPTION)
C      ENDIF

C=======================================================================
C - RECUPERATION DU DEPLACEMENT A PARTIR DU MOT CLE DEPL OU EXTRACTION-
C - D'UN OU PLUSIEURS DEPLACEMENTS A PARTIR D'UN RESULTAT  ------------
C=======================================================================

      CALL GETVID(' ','DEPL',0,1,1,DEPLA,NDEP)
      IF(NDEP.NE.0) THEN
        LONVEC = 1
        IORD = 0
        CALL GETVR8(' ','INST',0,1,0,RBID,NBINST)
        IF (NBINST.EQ.0) THEN
          EXITIM = .FALSE.
          TIME  = 0.D0
        ELSE
          NBINST = -NBINST
          IF(NBINST.GT.1) THEN
            CALL UTMESS('F',OPER,'LA LISTE D''INSTANTS NE DOIT'
     &        //'COMPORTER QU''UN SEUL INSTANT AVEC LE MOT-CLE DEPL')
          ENDIF
          CALL GETVR8(' ','INST',0,1,NBINST,TIME,IBID)
          EXITIM = .TRUE.
        ENDIF
      ENDIF

      CALL GETVID (' ','RESULTAT',0,1,1,RESU,NRES)
      IF (NRES.NE.0) THEN
        VECORD = '&&'//NOMPRO//'.VECTORDR'
        CALL GETVR8(' ','PRECISION',0,1,1,PREC,NP)
        CALL GETVTX(' ','CRITERE'  ,0,1,1,CRIT,NC)
        CALL RSUTNU ( RESU, ' ', 0, VECORD, LONVEC, PREC, CRIT, IER )
        IF(IER.NE.0) THEN
          CALL UTMESS('F',OPER,'PROBLEME A LA RECUPERATION D''UN CHAMP')
        ENDIF
        CALL JEVEUO ( VECORD, 'L', IVEC )
      ENDIF

C=======================================================================
C ---  CHARGE ---------------------------------------------------------
C=======================================================================

      CALL GETVID (' ','CHARGE',0,1,0,K8BID,NCHA)
      NCHA  = -NCHA
      VCHAR = '&&'//NOMPRO//'.CHARGES'
      CALL JECREO(VCHAR,'V V K8')
      CALL JEECRA(VCHAR,'LONMAX',MAX(1,NCHA),' ')
      CALL JEVEUO(VCHAR,'E',ICHA)
      CALL GETVID(' ','CHARGE',0,1,NCHA,ZK8(ICHA),IBID)
      IF (NCHA.NE.0) THEN
        CALL DISMOI('F','NOM_MODELE',ZK8(ICHA),'CHARGE',IBID,K8BI1,IER)
        IF (K8BI1.NE.MODELE) THEN
          CALL UTMESS('F',OPER,'LES CHARGES NE S''APPUIENT PAS'
     &                       //' SUR LE MODELE DONNE EN ARGUMENT')
        ENDIF
        DO 100 I = 1,NCHA
          CALL DISMOI('F','NOM_MODELE',ZK8(ICHA-1+I),'CHARGE',IBID,
     &                 K8BID,IER)
          IF (K8BID.NE.K8BI1) THEN
            CALL UTMESS('F',OPER,'LES CHARGES NE '
     &                 // 'S''APPUIENT PAS TOUTES SUR LE MEME MODELE')
          ENDIF
100     CONTINUE

      ENDIF

C=======================================================================
C ---  THETA, SYMETRIE DU CHARGEMENT, FOND DE FISSURE -----------
C=======================================================================

      CALL GETVID(' ','THETA'    ,0,1,1,SDTHET,IBID)
      CALL GETVTX(' ','SYME_CHAR',0,1,1,SYMECH,IBID)
      CALL GETVID(' ','FOND'     ,0,1,1,FOND  ,IFOND)
      IF (IFOND.NE.0) THEN
          CALL UTDEBM('A','CALC_G_THETA_T','LE MOT ')
          CALL UTIMPK('S','CLE ',1,'FOND')
          CALL UTIMPK('S',' EST APPELE A DISPARAITRE EN 6.4 ET SERA'//
     +                     ' REMPLACE PAR ',1,'FOND_FISS')
          CALL UTFINM()
        ELSE  
          CALL GETVID ( ' ', 'FOND_FISS', 0,1,1, FOND, IFOND)
        ENDIF
      IF ((OPTION .EQ. 'CALC_K_G').AND.(IFOND.EQ.0)) THEN
        CALL UTMESS('F', OPER,'FOND OBLIGATOIRE AVEC OPTION CALC_K_G')
      ENDIF

C=======================================================================
C---  POUR L'OPTION CALC_G_LAGR RECUPERATION DE LA PROPAGATION ALPHA
C=======================================================================

      CALL GETVR8(' ','PROPAGATION',0,1,1,ALPHA ,IPROPA)
      IF (IPROPA.EQ.0) ALPHA=0.D0
      IF ((OPTION.NE.'CALC_G_LAGR').AND.(IPROPA.NE.0)) THEN
        CALL UTMESS('F', OPER,'MOT CLE PROPAGATION UTILISE'
     &                 //'SEULEMENT AVEC L''OPTION CALC_G_LAGR')
      ENDIF

C=======================================================================
C --- RECUPERATION DU CHAMNO DE THETA DE LA S.D. SDTHET DE TYPE
C --- THETA_GEOM
C=======================================================================

      CALL RSEXCH(SDTHET,'THETA',0,THETA,IRET)
      IF (IRET.GT.0) THEN
        CALL UTMESS('F',NOMPRO,'LE CHAMP DE THETA EST INEXISTANT '//
     &              'DANS LA STRUCTURE DE DONNEES '//SDTHET//' DE '//
     &              'TYPE THETA_GEOM .')
      ENDIF
      
C=======================================================================
C RECUPERATION DU CHAMNO DE THETA SENSIBILITE DE LA S.D. SDTHET
C=======================================================================

      IF (OPTION.EQ.'CALC_DG') THEN
C
        CALL GETVID ('SENSIBILITE','THETA',1,1,1,NOPASE,IBID)
        IF (IBID.EQ.0) THEN
          CALL UTMESS('F',NOMPRO,'LE MOT-CLE THETA DU MOT-CLE '//
     &    'FACTEUR SENSIBILITE EST ABSENT DU FICHIER '//
     &    'DE COMMANDE POUR L''OPERATEUR CALC_G_THETA_T !')
        ENDIF
C
        CALL PSRENC ( RESU, NOPASE, RESUSE, IRET )
        IF ( IRET.NE.0 ) THEN
          CALL UTMESS ('F', NOMPRO,
     >  'IMPOSSIBLE DE TROUVER LE RESULTAT DERIVE ASSOCIE AU RESULTAT '
     >  //RESU//' ET AU PARAMETRE SENSIBLE '//NOPASE)
        ENDIF
C
      ENDIF
      
C=======================================================================
C ---  CREATION DE TABLE ET CALCUL ----------------------------------
C=======================================================================

      IF (OPTION.EQ.'CALC_G'.OR.OPTION.EQ.'CALC_G_LAGR') THEN
        IF (NDEP .NE. 0 ) THEN
          NBPRUP = 1
          NOPRUP(1) = 'G'
          TYPRUP(1) = 'R'
        ELSE
          NBPRUP = 3
          NOPRUP(1) = 'NUME_ORDRE'
          TYPRUP(1) = 'I'
          NOPRUP(2) = 'INST'
          TYPRUP(2) = 'R'
          NOPRUP(3) = 'G'
          TYPRUP(3) = 'R'
        ENDIF
      ELSEIF ( OPTION .EQ. 'CALC_DG'    ) THEN
        IF ( NDEP .EQ. 0 ) THEN
          NBPRUP = 4
          NOPRUP(1) = 'NUME_ORDRE'
          TYPRUP(1) = 'I'
          NOPRUP(2) = 'INST'
          TYPRUP(2) = 'R'
          NOPRUP(3) = 'G'
          TYPRUP(3) = 'R'
          NOPRUP(4) = 'DG'
          TYPRUP(4) = 'R'
        ELSE
          CALL UTMESS('F',NOMPRO,'L''OPTION '//OPTION//
     &         ' N''A PAS ETE ETENDUE AU MOT-CLE DEPLACEMENT !')
        ENDIF
      ELSEIF ( OPTION .EQ. 'CALC_K_G'    ) THEN
        IF ( NDEP .NE. 0 ) THEN
          NBPRUP = 4
          NOPRUP(1) = 'G'
          TYPRUP(1) = 'R'
          NOPRUP(2) = 'K1'
          TYPRUP(2) = 'R'
          NOPRUP(3) = 'K2'
          TYPRUP(3) = 'R'
          NOPRUP(4) = 'G_IRWIN'
          TYPRUP(4) = 'R'
        ELSE
          NBPRUP = 6
          NOPRUP(1) = 'NUME_ORDRE'
          TYPRUP(1) = 'I'
          NOPRUP(2) = 'INST'
          TYPRUP(2) = 'R'
          NOPRUP(3) = 'G'
          TYPRUP(3) = 'R'
          NOPRUP(4) = 'K1'
          TYPRUP(4) = 'R'
          NOPRUP(5) = 'K2'
          TYPRUP(5) = 'R'
          NOPRUP(6) = 'G_IRWIN'
          TYPRUP(6) = 'R'
        ENDIF
      ELSEIF ( OPTION .EQ. 'G_BILINEAIRE'
     &            .OR.OPTION .EQ. 'CALC_G_MAX') THEN
        NBPRUP = 3
        NOPRUP(1) = 'NUME_CMP_I'
        TYPRUP(1) = 'I'
        NOPRUP(2) = 'NUME_CMP_J'
        TYPRUP(2) = 'I'
        NOPRUP(3) = 'G_BILIN'
        TYPRUP(3) = 'R'
      ENDIF
      CALL TBCRSD ( RESULT, 'G' )
      CALL TBAJPA ( RESULT, NBPRUP, NOPRUP, TYPRUP )

      IF (OPTION .EQ.'G_BILINEAIRE'.OR. OPTION .EQ.'CALC_G_MAX') THEN

C=======================================================================
C   CALCUL DE LA FORME BILINEAIRE DU TAUX DE RESTITUTION
C=======================================================================

        DO 150 I = 1 , LONVEC
          DO 160 J = 1,I
            IF (NRES.NE.0) THEN
              IORD1 = ZI(IVEC-1+I)
              CALL RSEXCH(RESU,'DEPL',IORD1,DEPLA1,IRET)
              IORD2 = ZI(IVEC-1+J)
              CALL RSEXCH(RESU,'DEPL',IORD2,DEPLA2,IRET)
              IF(IRET.NE.0) THEN
                CALL UTMESS('F',OPER,'ACCES IMPOSSIBLE AU DEPLACEMENT')
              ENDIF
              CALL RSADPA(RESU,'L',1,'INST',IORD1,0,JINST,K8BID)
              TIME  = ZR(JINST)
              EXITIM = .TRUE.
            ELSE
              DEPLA1 = DEPLA
              DEPLA2 = DEPLA
            ENDIF
            OPTIO2 = 'CALC_G_BILI'
            CALL MEBILG (OPTIO2,RESULT,MODELE,DEPLA1,DEPLA2,THETA,MATE,
     &                   NCHA,ZK8(ICHA),SYMECH,EXITIM,TIME,I,J,NBPRUP,
     &                   NOPRUP )
 160        CONTINUE
 150      CONTINUE
 
          IF (OPTION .EQ.'CALC_G_MAX') THEN

C==============================================================
C   MAXIMISATION DU G SOUS CONTRAINTES BORNES
C==============================================================

            CALL GETFAC ('BORNES', NBORN )
            IF (NBORN.NE.0) THEN
              NBCO = 2*NBORN
              CALL WKVECT('&&'//NOMPRO//'.COUPLES_BORNES'
     &                     ,'V V R8',NBCO,IBOR)
              DO 170 I=1, NBORN
                CALL GETVIS('BORNES','NUME_ORDRE',I,1,1,IORD,N1)
                CALL GETVR8('BORNES','VALE_MIN',I,1,1,
     &                      ZR(IBOR+2*(IORD-1)),N1)
                CALL GETVR8('BORNES','VALE_MAX',I,1,1,
     &                      ZR(IBOR+2*(IORD-1)+1),N1)
 170          CONTINUE
              CALL TBEXVE(RESULT,'G_BILIN',
     &                          '&&'//NOMPRO//'.GBILIN','V',NBVAL,K8BID)
              CALL JEVEUO('&&'//NOMPRO//'.GBILIN','L',IG)
              CALL DETRSD('TABLE',RESULT)
              CALL MEMAXG(NBCO,ZR(IBOR),ZR(IG),LONVEC,RESULT)
            ELSE
              CALL UTMESS('F',OPER,'MOT-CLEF <BORNES> OBLIGATOIRE'//
     &                    ' AVEC CETTE OPTION !')
            ENDIF
          ENDIF

C==============================================================
C   FIN DU CALCUL DE LA FORME BILINEAIRE DU TAUX DE RESTITUTION
C==============================================================

      ELSE

C==============================================================
C   CALCUL DE G, G_LAGR, K_G ET DG
C==============================================================

        DO 220 I = 1 , LONVEC
          IF(NRES.NE.0) THEN
            IORD = ZI(IVEC-1+I)
            CALL RSEXCH(RESU,'DEPL',IORD,DEPLA,IRET)
            IF(IRET.NE.0) THEN
              CALL UTMESS('F',OPER,'ACCES IMPOSSIBLE AU DEPLACEMENT')
            ENDIF
            CALL RSADPA(RESU,'L',1,'INST',IORD,0,JINST,K8BID)
            TIME  = ZR(JINST)
            EXITIM = .TRUE.
            
C RECUPERATION DES CHAMNO DE DERIVEE LAGRANGIENNE DE DEPLACEMENT
C DANS LA SD RESULTAT DERIVE DE TYPE EVOL_ELAS.
            IF (OPTION.EQ.'CALC_DG') THEN
              CALL RSEXC2(1,1,RESUSE,'DEPL',IORD,DLAGDE,OPTION,IRET)
              IF (IRET.GT.0) THEN
                CALL UTDEBM('F',NOMPRO,'LA DERIVEE LAGRANGIENNE')
                CALL UTIMPI('L','DU DEPLACEMENT D''OCCURRENCE N ',1,
     &            IORD)
                CALL UTIMPK('L','EST INEXISTANT DANS LA SD ',1,RESU)
                CALL UTIMPK('L','DERIVEE PAR RAPPORT A ',1,NOPASE)
                CALL UTFINM()            
              ENDIF
            ENDIF

          ENDIF
          IF (OPTION.EQ.'CALC_G' .OR. OPTION.EQ.'CALC_DG') THEN
            CALL MECALG (OPTION,RESULT,MODELE,DEPLA,THETA,MATE,NCHA,
     &                   ZK8(ICHA),SYMECH,COMPOR,EXITIM,TIME,IORD,
     &                   NBPRUP,NOPRUP,NOPASE,DLAGDE)

          ELSE IF (OPTION .EQ.'CALC_G_LAGR') THEN
            CALL MLAGRG (OPTION,RESULT,MODELE,DEPLA,THETA,ALPHA,MATE,
     &                   NCHA,ZK8(ICHA),SYMECH,EXITIM,TIME,IORD,
     &                   NBPRUP, NOPRUP )
          ELSE IF (OPTION .EQ.'CALC_K_G') THEN
            CALL MEFICG (OPTION,RESULT,MODELE,DEPLA,THETA,MATE,NCHA,
     &                   ZK8(ICHA),SYMECH,FOND,EXITIM,TIME,IORD,
     &                   NBPRUP, NOPRUP )
          ENDIF
220     CONTINUE

      ENDIF

      CALL TITRE

      CALL JEDETR('&&'//NOMPRO//'.CHARGES')
      CALL JEDETC('G','&&NMDORC',1)
      CALL JEDETC('V','.CODI',20)
      CALL JEDETC('V','.MATE_CODE',9)
      CALL JEDETC('V','&&',1)
      CALL JEDEMA()
      END
