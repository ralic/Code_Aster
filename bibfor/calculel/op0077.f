      SUBROUTINE OP0077(IER)
      IMPLICIT NONE
C ---------------------------------------------------------------------
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

C      OPERATEUR :     CALC_G_LOCAL_T

C ---------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX --------------------

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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX --------------------

      INTEGER NCHA,IER,ICHA,IBID,IERD,IN,IADRMA
      INTEGER IORD,IORD1,IORD2,NRES,LONVEC,NP,NC,NDEG,IVEC,IRET
      INTEGER LNOFF,NDEP,JINST,ICOMP,ITHETA,IPROPA,IFOND
      INTEGER I,J,NBINST,NBRE,NBPRUP,IADNUM,NBORN,NBCO
      INTEGER NBVAL,IBOR,N1,IG,LNOEU,LABSCU

      REAL*8 PREC,ALPHA,TIME,RBID

      CHARACTER*1 K1BID
      CHARACTER*4 TYPRUP(6)
      CHARACTER*8 NOMA,MODELE,SYMECH,K8B,K8BID
      CHARACTER*8 RESU,FOND,K8BI1,CRIT,THETAI,RESULT
      CHARACTER*16 TYPE,OPER,OPTION,NOPRUP(6),OPTIO2
      CHARACTER*24 CHFOND,NOMNO,COORN,OBJMA,CHDEPL,VECORD,THETLG,VCHAR
      CHARACTER*24 TRAV1,TRAV2,TRAV3,STOK4,LISSTH,LISSG,SDTHET
      CHARACTER*24 MATERI,COMPOR,MATE,DEPLA1,DEPLA2

      LOGICAL THLAGR,GLAGR,EXTIM,MILIEU,CONNEX

C ----------------------------------------------------------------------

      CALL JEMARQ()
      CALL INFMAJ
      THLAGR = .FALSE.
      GLAGR = .FALSE.

      CALL GETRES(RESULT,TYPE,OPER)

C- MODELE, CHAM_MATER ET RELATION DE COMPORTEMENT --------------------

      CALL GETVID(' ','MODELE',0,1,1,MODELE,IBID)
      CALL GETVID(' ','CHAM_MATER',0,1,1,MATERI,IBID)
      CALL GETVTX(' ','OPTION',0,1,1,OPTION,IBID)
      CALL RCMFMC(MATERI,MATE)
      CALL NMDORC(MODELE,COMPOR)
C      CALL JEVEUO(COMPOR(1:19)//'.VALV','L',ICOMP)
C      IF ((ZK16(ICOMP).NE.'ELAS') .AND. (OPTION.NE.'CALC_G')) THEN
C        CALL UTMESS('F',OPER,'ELASTICITE OBLIGATOIRE AVEC '//OPTION)
C      END IF

C- FOND DE FISSURE : POUR AFFECTATION DE R_INF, R_SUP ET MODULE DE THETA

      CALL GETVID(' ','FOND',0,1,1,FOND,IFOND)
      IF (IFOND.NE.0) THEN
          CALL UTDEBM('A','CALC_G_LOCAL_T','LE MOT ')
          CALL UTIMPK('S','CLE ',1,'FOND')
          CALL UTIMPK('S',' EST APPELE A DISPARAITRE EN 6.4 ET SERA'//
     +                     ' REMPLACE PAR ',1,'FOND_FISS')
          CALL UTFINM()
        ELSE  
          CALL GETVID ( ' ', 'FOND_FISS', 1,1,1, FOND, IFOND )
        ENDIF
      CHFOND = FOND//'.FOND      .NOEU'
      CALL JELIRA(CHFOND,'LONMAX',LNOFF,K1BID)

C- METHODE DE DECOMPOSITION DE THETA ET G : LAGRANGE OU LEGENDRE ------

      CALL GETVTX(' ','LISSAGE_THETA',0,1,1,LISSTH,IBID)
      CALL GETVTX(' ','LISSAGE_G',0,1,1,LISSG,IBID)
      CALL GETVIS(' ','DEGRE',0,1,1,NDEG,IBID)

      IF ((LISSTH.EQ.'LEGENDRE') .AND. (LISSG.EQ.'LEGENDRE')) THEN
        THLAGR = .FALSE.
        GLAGR = .FALSE.
        NBRE = NDEG
      ELSE IF ((LISSTH.EQ.'LAGRANGE') .AND. (LISSG.EQ.'LEGENDRE')) THEN
        THLAGR = .TRUE.
        GLAGR = .FALSE.
        NBRE = LNOFF
        IF ((OPTION .EQ. 'CALC_G_MAX') .OR.
     +      (OPTION .EQ. 'G_BILINEAIRE')) THEN
           CALL UTMESS('F','OP0077','CETTE COMBINAISON DE LISSAGE '//
     +       'N''EST PAS PROGRAMMEE POUR L''OPTION : '//OPTION//'.')
        END IF
        IF (NDEG.GT.LNOFF) THEN
          CALL UTMESS('F','OP0077','LE DEGRE DES POLYNOMES DE LEGENDRE'
     +                //
     +        ' DOIT ETRE INFERIEUR OU EGAL AU NOMBRE DE NOEUDS DU FOND'
     +                //' DE FISSURE AVEC LA METHODE THETA-LAGRANGE')
        END IF
      ELSE IF ((LISSTH.EQ.'LAGRANGE') .AND.
     +         ((LISSG.EQ.'LAGRANGE').OR. (LISSG.EQ.
     +         'LAGRANGE_NO_NO'))) THEN
        THLAGR = .TRUE.
        GLAGR = .TRUE.
        NBRE = LNOFF
      ELSE IF ((LISSTH.EQ.'LEGENDRE') .AND. (LISSG.NE.'LEGENDRE')) THEN
        CALL UTMESS('F','OP0077','LE LISSAGE DE G DOIT ETRE DE TYPE '//
     +            'LEGENDRE SI LE LISSAGE DE THETA EST DE TYPE LEGENDRE'
     +              )
      END IF
   10 CONTINUE

C - RECUPERATION DU DEPLACEMENT A PARTIR DU MOT CLE DEPL OU EXTRACTION-
C - D'UN OU PLUSIEURS DEPLACEMENTS A PARTIR D'UN RESULTAT  ------------

      CALL GETVID(' ','DEPL',0,1,1,CHDEPL,NDEP)
      IF (NDEP.NE.0) THEN
        LONVEC = 1
        CALL GETVR8(' ','INST',0,1,0,RBID,NBINST)
        IF (NBINST.EQ.0) THEN
          EXTIM = .FALSE.
          TIME = 0.D0
        ELSE
          NBINST = -NBINST
          IF (NBINST.GT.1) THEN
            CALL UTMESS('F',OPER,'LA LISTE D''INSTANTS NE DOIT'//
     +              'COMPORTER QU''UN SEUL INSTANT AVEC LE MOT-CLE DEPL'
     +                  )
          END IF
          CALL GETVR8(' ','INST',0,1,NBINST,TIME,IBID)
          EXTIM = .TRUE.
        END IF
        NBPRUP = 3
        NOPRUP(1) = 'NOEUD'
        TYPRUP(1) = 'K8'
        NOPRUP(2) = 'ABSC_CURV'
        TYPRUP(2) = 'R'
        NOPRUP(3) = 'G_LOCAL'
        TYPRUP(3) = 'R'
      END IF

      CALL GETVID(' ','RESULTAT',0,1,1,RESU,NRES)
      IF (NRES.NE.0) THEN
        VECORD = '&&OP0077.VECTORDR'
        CALL GETVR8(' ','PRECISION',1,1,1,PREC,NP)
        CALL GETVTX(' ','CRITERE',1,1,1,CRIT,NC)
        CALL RSUTNU(RESU,' ',0,VECORD,LONVEC,PREC,CRIT,IER)
        IF (IER.NE.0) THEN
          CALL UTMESS('F',OPER,'PROBLEME A LA RECUPERATION D''UN CHAMP')
        END IF
        CALL JEVEUO(VECORD,'L',IVEC)
        NBPRUP = 5
        NOPRUP(1) = 'NUME_ORDRE'
        TYPRUP(1) = 'I'
        NOPRUP(2) = 'INST'
        TYPRUP(2) = 'R'
        NOPRUP(3) = 'NOEUD'
        TYPRUP(3) = 'K8'
        NOPRUP(4) = 'ABSC_CURV'
        TYPRUP(4) = 'R'
        NOPRUP(5) = 'G_LOCAL'
        TYPRUP(5) = 'R'
      END IF

C ---  CHARGE ---------------------------------------------------------

      CALL GETVID(' ','CHARGE',0,1,0,K8B,NCHA)
      NCHA = -NCHA
      VCHAR = '&&OP0077.CHARGES'
      CALL JECREO(VCHAR,'V V K8')
      CALL JEECRA(VCHAR,'LONMAX',MAX(1,NCHA),' ')
      CALL JEVEUO(VCHAR,'E',ICHA)
      CALL GETVID(' ','CHARGE',0,1,NCHA,ZK8(ICHA),IBID)
      IF (NCHA.GT.0) THEN
        CALL DISMOI('F','NOM_MODELE',ZK8(ICHA),'CHARGE',IBID,K8BI1,IERD)
        IF (K8BI1.NE.MODELE) THEN
          CALL UTMESS('F',OPER,'LES CHARGES NE S''APPUIENT PAS'//
     +                ' SUR LE MODELE DONNE EN ARGUMENT')
        END IF
        DO 20 I = 1,NCHA
          CALL DISMOI('F','NOM_MODELE',ZK8(ICHA-1+I),'CHARGE',IBID,K8B,
     +                IERD)
          IF (K8B.NE.K8BI1) THEN
            CALL UTMESS('F',OPER,'LES CHARGES NE '//
     +                  'S''APPUIENT PAS TOUTES SUR LE MEME MODELE')
          END IF
   20   CONTINUE
      END IF

C---  POUR L'OPTION CALC_G_LGLO : PROPAGATION ALPHA ET CHAMP THETA

      CALL GETVID(' ','THETA',0,1,1,SDTHET,ITHETA)
      IF (ITHETA.EQ.0) THEN
        THETLG = ' '
      ELSE

C --- RECUPERATION DU CHAMNO DE THETA DE LA S.D. SDTHET DE TYPE
C --- THETA_GEOM

        CALL RSEXCH(SDTHET,'THETA',0,THETLG,IRET)
        IF (IRET.GT.0) THEN
          CALL UTMESS('F','OP0077','LE CHAMP DE THETA EST INEXISTANT '//
     +                'DANS LA STRUCTURE DE DONNEES '//SDTHET//' DE '//
     +                'TYPE THETA_GEOM .')
        END IF
      END IF
      IF ((OPTION.EQ.'CALC_G_LGLO') .AND. (ITHETA.EQ.0)) THEN
        CALL UTMESS('F',OPER,'CHAMP THETA OBLIGATOIRE AVEC '//OPTION)
      ELSE IF ((OPTION.EQ.'CALC_G') .AND. (ITHETA.NE.0)) THEN
        CALL UTMESS('F',OPER,'CHAMP THETA CALCULE AUTOMATIQUEMENT')
      END IF
      CALL GETVR8(' ','PROPAGATION',0,1,1,ALPHA,IPROPA)
      IF (IPROPA.EQ.0) ALPHA = 0.D0
      IF ((OPTION.NE.'CALC_G_LGLO') .AND. (IPROPA.NE.0)) THEN
        CALL UTMESS('F',OPER,'MOT CLE PROPAGATION UTILISE '//
     +              'SEULEMENT AVEC L''OPTION CALC_G_LGLO')
      END IF

C --- SYMETRIE DU CHARGEMENT ET IMPRESSION --------------------------

      CALL GETVTX(' ','SYME_CHAR',0,1,1,SYMECH,IBID)

C - FOND DE FISSURE : AFFECTATION DE R_INF, R_SUP ET MODULE DE THETA
C   RECUPERATION INFO SUR CONNEXITE DU FOND DE FISSURE

      CALL JEVEUO(CHFOND,'L',IADNUM)
      IF (ZK8(IADNUM+1-1).EQ.ZK8(IADNUM+LNOFF-1)) THEN
        CONNEX = .TRUE.
      ELSE
        CONNEX = .FALSE.
      END IF
      IF ((CONNEX.AND. (.NOT.THLAGR)) .OR.
     +    (CONNEX.AND. (.NOT.GLAGR))) THEN
        CALL UTMESS('F','OP0077','L USAGE DES POLYNOMES DE LEGENDRE'//
     +        ' DANS LE CAS D UN FOND DE FISSURE CLOS EST INTERDIT     '
     +              )
      END IF
      OBJMA = MODELE//'.MODELE    .NOMA'
      THETAI = '&&THETA '
      CALL JEVEUO(OBJMA,'L',IADRMA)
      NOMA = ZK8(IADRMA)
      NOMNO = NOMA//'.NOMNOE'
      IF ((OPTION.EQ.'CALC_G') .OR. 
     +    (OPTION.EQ.'G_BILINEAIRE') .OR.
     +    (OPTION.EQ.'CALC_G_MAX')) THEN
        COORN = NOMA//'.COORDO    .VALE'
      ELSE
        CALL VTGPLD(NOMA//'.COORDO    ',ALPHA,THETLG,'V','&&GMETH1.G2')
        COORN = '&&GMETH1.G2        '//'.VALE'
      END IF
      CALL GVERI2(NOMA,CHFOND,LNOFF,NOMNO,COORN,TRAV1,TRAV2,TRAV3,
     +            THLAGR,NDEG)
      CALL GCOUR2(THETAI,NOMA,MODELE,NOMNO,COORN,LNOFF,TRAV1,TRAV2,
     +            TRAV3,CHFOND,FOND,.FALSE.,CONNEX,STOK4,THLAGR,NDEG,
     +            MILIEU)
      CALL GIMPT2(THETAI,NBRE,TRAV1,TRAV2,TRAV3,CHFOND,STOK4,LNOFF,0)

      CALL JEEXIN(TRAV1,IRET)
      IF (IRET.NE.0) CALL JEDETR(TRAV1)
      CALL JEEXIN(TRAV2,IRET)
      IF (IRET.NE.0) CALL JEDETR(TRAV2)
      CALL JEEXIN(TRAV3,IRET)
      IF (IRET.NE.0) CALL JEDETR(TRAV3)
      CALL JEEXIN(STOK4,IRET)
      IF (IRET.NE.0) CALL JEDETR(STOK4)

C - CREATION DE TABLE -----------------------------------------------

      IF ( OPTION .EQ. 'G_BILINEAIRE' .OR.
     +     OPTION .EQ. 'CALC_G_MAX' ) THEN
         NBPRUP = 6
         NOPRUP(1) = 'INST'
         TYPRUP(1) = 'R'
         NOPRUP(2) = 'NUME_CMP_I'
         TYPRUP(2) = 'I'
         NOPRUP(3) = 'NUME_CMP_J'
         TYPRUP(3) = 'I'
         NOPRUP(4) = 'NOEUD'
         TYPRUP(4) = 'K8'
         NOPRUP(5) = 'ABSC_CURV'
         TYPRUP(5) = 'R'
         NOPRUP(6) = 'G_BILI_LOCAL'
         TYPRUP(6) = 'R'
      ENDIF

      CALL TBCRSD(RESULT,'G')
      CALL TBAJPA(RESULT,NBPRUP,NOPRUP,TYPRUP)

      IF ( OPTION .EQ. 'G_BILINEAIRE' .OR.
     +     OPTION .EQ. 'CALC_G_MAX' ) THEN

C=======================================================================
C  CALCUL DE LA FORME BILINEAIRE "LOCALE" DU TAUX DE RESTITUTION  
C  D'ENERGIE G
C=======================================================================

         DO 110 I=1, LONVEC
            DO 120 J=1, I
               IF ( NRES .NE. 0 ) THEN
                  IORD1 = ZI(IVEC-1+I)
                  CALL RSEXCH(RESU,'DEPL',IORD1,DEPLA1,IRET)
                  IORD2 = ZI(IVEC-1+J)
                  CALL RSEXCH(RESU,'DEPL',IORD2,DEPLA2,IRET)
                  IF ( IRET .NE. 0 ) THEN
                     CALL UTMESS('F',OPER,'ACCES IMPOSSIBLE AUX '//
     +                                    'DEPLACEMENTS')
                  ENDIF
                  CALL RSADPA(RESU,'L',1,'INST',IORD1,0,JINST,K8BID)
                  TIME = ZR(JINST)
                  EXTIM = .TRUE.
               ELSE
                  DEPLA1 = CHDEPL
                  DEPLA2 = CHDEPL
               ENDIF
               OPTIO2 = 'CALC_G_BILI'
               CALL MBILGL(OPTIO2,RESULT,MODELE,DEPLA1,DEPLA2,THETAI,
     +                     MATE,NCHA,ZK8(ICHA),SYMECH,CHFOND,LNOFF,
     +                     IORD1,IORD2,NDEG,THLAGR,GLAGR,MILIEU,EXTIM,
     +                     TIME,I,J,NBPRUP,NOPRUP)
  120       CONTINUE
  110    CONTINUE
C
C
         IF (OPTION .EQ.'CALC_G_MAX') THEN

C=======================================================================
C  MAXIMISATION DE G LOCAL SOUS CONTRAINTES BORNES (GMAX LOCAL) 
C=======================================================================

            CALL GETFAC ('BORNES', NBORN )
            IF (NBORN.NE.0) THEN
              NBCO = 2*NBORN
              CALL WKVECT('&&OP0077.COUPLES_BORNES','V V R8',NBCO,IBOR)
              DO 170 I=1, NBORN
                CALL GETVIS('BORNES','NUME_ORDRE',I,1,1,IORD,N1)
                CALL GETVR8('BORNES','VALE_MIN',I,1,1,
     +                      ZR(IBOR+2*(IORD-1)),N1)
                CALL GETVR8('BORNES','VALE_MAX',I,1,1,
     +                      ZR(IBOR+2*(IORD-1)+1),N1)
 170          CONTINUE
              CALL TBEXVE(RESULT,'G_BILI_LOCAL',
     +                               '&&OP0077.GBILIN','V',NBVAL,K8BID)
              CALL JEVEUO('&&OP0077.GBILIN','L',IG)
              CALL TBEXVE(RESULT,'NOEUD',
     +                               '&&OP0077.NOEUD','V',NBVAL,K8BID)
              CALL JEVEUO('&&OP0077.NOEUD','L',LNOEU)
              CALL TBEXVE(RESULT,'ABSC_CURV',
     +                               '&&OP0077.ABSCUR','V',NBVAL,K8BID)
              CALL JEVEUO('&&OP0077.ABSCUR','L',LABSCU)
C
              CALL DETRSD('TABLE',RESULT)
              CALL MMAXGL(NBCO,ZR(IBOR),ZR(IG),ZK8(LNOEU),
     +                    ZR(LABSCU),LONVEC,LNOFF,RESULT)
            ELSE
              CALL UTMESS('F',OPER,'MOT-CLEF <BORNES> OBLIGATOIRE'//
     +                    ' AVEC CETTE OPTION !')
            ENDIF
          ENDIF

C=======================================================================
C  FIN DU CALCUL DE LA FORME BILINEAIRE "LOCALE" DE G  
C  ET DE SA MAXIMISATION GMAX
C=======================================================================

      ELSE

C=======================================================================
C  CALCUL DU TAUX DE RESTITUTION LOCAL D'ENERGIE 
C=======================================================================

         DO 30 I = 1,LONVEC
           IF (NRES.NE.0) THEN
             IORD = ZI(IVEC-1+I)
             CALL RSEXCH(RESU,'DEPL',IORD,CHDEPL,IRET)
             IF (IRET.NE.0) THEN
               CALL UTDEBM('F',OPER,'ACCES IMPOSSIBLE ')
               CALL UTIMPK('L',' CHAMP : ',1,'DEPL')
               CALL UTIMPI('S',', NUME_ORDRE : ',1,IORD)
               CALL UTFINM()
             END IF
             CALL RSADPA(RESU,'L',1,'INST',IORD,0,JINST,K8B)
             TIME = ZR(JINST)
             EXTIM = .TRUE.
           END IF
           CALL MECAGL(OPTION,RESULT,MODELE,CHDEPL,THETAI,MATE,COMPOR,
     +              NCHA,ZK8(ICHA),SYMECH,CHFOND,LNOFF,IORD,NDEG,THLAGR,
     +              GLAGR,MILIEU,THETLG,ALPHA,EXTIM,TIME,NBPRUP,NOPRUP)
   30 CONTINUE

      ENDIF

      CALL TITRE

      CALL JEDETR('&&OP0077.CHARGES')
      CALL JEDETR('&&OP0077.VECTORDR')
      CALL JEDETC(' ',THETAI,1)
      CALL JEDETC('V','.CODI',20)
      CALL JEDETC('V','.MATE_CODE',9)
      CALL JEDETC('G','&&NMDORC',1)
      CALL JEDETC('V','1',1)
      CALL JEDETC('V','2',1)
      CALL JEDETC('V','3',1)
      CALL JEDETC('V','4',1)
      CALL JEDETC('V','5',1)
      CALL JEDETC('V','6',1)
      CALL JEDETC('V','7',1)
      CALL JEDETC('V','8',1)
      CALL JEDETC('V','9',1)
      CALL JEDETC('V','&&',1)


      CALL JEDEMA()
      END
