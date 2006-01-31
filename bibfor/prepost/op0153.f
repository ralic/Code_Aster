      SUBROUTINE OP0153 ( IERR )
      IMPLICIT  REAL*8  ( A-H,O-Z )
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 30/01/2006   AUTEUR LEBOUVIE F.LEBOUVIER 
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
C
C     OPERATEUR  "POST_USURE"
C
C ----------------------------------------------------------------------
C     ---- DEBUT DES COMMUNS JEVEUX ------------------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ---- FIN DES COMMUNS JEVEUX --------------------------------------
      PARAMETER    ( NBPAR = 16, NBPAR2 = 12, NBPMR = 5 )
      REAL*8       PMOYE, INSDEB, EPSIL, DINST   
      CHARACTER*8  K8B
      CHARACTER*8  TYPAR(NBPAR), TYPPMR(NBPMR)
      REAL*8       VALER(NBPAR)
      CHARACTER*16 NOPAR(NBPAR), NOMPMR(NBPMR), NOPAR2(NBPAR2)
      CHARACTER*12 NOMVAR
      CHARACTER*16 CONCEP, NOMCMD, VALEK(2)
      CHARACTER*19 RESU, LINST, KFORN, KVGLI
      CHARACTER*19 TABPUS,NOMTA,NEWTAB
      CHARACTER*24 TYPE
      COMPLEX*16   C16B
      DATA NOPAR  / 'PUIS_USUR_GLOBAL' ,
     &              'INST' , 'DUREE' , 'ORIG_INST' ,
     &              'V_USUR_TUBE' , 'V_USUR_OBST' , 'P_USUR_TUBE' ,
     &              'SECTEUR' , 'ANGLE_DEBUT' , 'ANGLE_FIN' ,
     &              'V_USUR_TUBE_SECT' , 'V_USUR_OBST_SECT' ,
     &              'P_USUR_TUBE_SECT' , 'P_USUR_OBST_SECT' ,
     &              'V_USUR_TUBE_CUMU' , 'V_USUR_OBST_CUMU' /
      DATA TYPAR  /'R','R','R','R','R','R','R','I','R','R','R','R','R',
     &             'R','R','R'/
      DATA NOPAR2 / 'INST' , 'DUREE' , 'ORIG_INST' ,
     &              'SECTEUR' , 'ANGLE_DEBUT' , 'ANGLE_FIN' ,
     &              'V_USUR_TUBE_SECT' , 'V_USUR_OBST_SECT' ,
     &              'P_USUR_TUBE_SECT' , 'P_USUR_OBST_SECT' ,
     &              'V_USUR_TUBE_CUMU' , 'V_USUR_OBST_CUMU' /
      DATA NOMPMR /'PUIS_USUR_GLOBAL' ,
     &             'INST','V_USUR_TUBE','V_USUR_OBST','P_USUR_TUBE'/
      DATA TYPPMR /'R','R','R','R','R'/
C     ------------------------------------------------------------------
C 
      CALL JEMARQ()
      CALL INFMAJ()
C
      INDIC = 0
      IFIRES = IUNIFI('RESULTAT')
      KFORN = '&&OP0153.FORC_N'
      KVGLI = '&&OP0153.VITE_G'
C
      CALL GETRES ( RESU, CONCEP, NOMCMD )
C
C     ------------------------------------------------------------------
C            REMPLACEMENT DU TUBE PERCE PAR UN TUBE NEUF
C     ------------------------------------------------------------------
C
      DINST = 0.D0
      CALL GETVTX ( ' ', 'TUBE_NEUF', 1,1,1, K8B, NTN )
      IF ( NTN .NE. 0 ) THEN
         CALL EXISD ( 'TABLE', RESU, IRET )
         IF ( IRET .EQ. 0 ) THEN
            CALL UTMESS('F','USURE','"TUBE_NEUF" N''A DE SENS QUE '//
     +                             'POUR UNE TABLE D''USURE EXISTANTE')
         ENDIF
         CALL GETVID ( ' ', 'TABL_USURE', 1,1,1, K8B, N1 )
         IF ( K8B .NE. RESU(1:8) ) THEN
            CALL UTMESS('F','USURE','"TUBE_NEUF" N''A DE SENS QUE '//
     +                             'POUR UNE TABLE D''USURE EXISTANTE')
         ENDIF
         CALL GETVR8 ( ' ', 'INST', 1,1,1, DINST, NIS )
         IF ( NIS .EQ. 0 ) THEN
            CALL TBEXV1 ( RESU, 'INST', '&&OP0153.INST','V',NBV,K8B)
            CALL JEVEUO ('&&OP0153.INST', 'L', JINST )
            DINST = ZR(JINST+NBV-1) 
         ENDIF
         CALL TBEXV1 ( RESU, 'SECTEUR', '&&OP0153.SECT','V',NBV,K8B)
         CALL JEVEUO ('&&OP0153.SECT', 'L', JSECT )
         NBSECT = ZI(JSECT+NBV-1)
         CALL JEDETR ( '&&OP0153.SECT' ) 
         CALL MOTUBN ( RESU, DINST, NBSECT )
         GOTO 888
      ENDIF
C
C     ------------------------------------------------------------------
      CALL GETVIS(' ','INFO',1,1,1,INFO,N0)
      IF (INFO.GT.1) THEN
        WRITE(IFIRES,1000)
        WRITE(IFIRES,*)
        WRITE(IFIRES,*) RESU
      ENDIF
C
C     --- CALCUL DE LA PUISSANCE D'USURE ---
      CALL USUPUS ( PUUSUR, KFORN, KVGLI, NBPT)
      CALL JEEXIN(KFORN,IRET)
      IF (IRET.GT.0) THEN
        CALL JEVEUO(KFORN,'E',JFN)
        CALL JEVEUO(KVGLI,'E',JVG)
      ENDIF
C
C     --- RECUPERATION DES INSTANTS DE CALCUL ---
      CALL GETVR8(' ','INST',1,1,0,R8B,NI1)
      IF ( NI1 .NE. 0 ) THEN
         NBINST = -NI1
         CALL WKVECT('&&OP0153.INSTANT','V V R',NBINST,JINST)
         CALL GETVR8(' ','INST',1,1,NBINST,ZR(JINST),N1)
      ELSE
         CALL GETVID(' ','LIST_INST',1,1,1,LINST,N1)
         CALL JELIRA(LINST//'.VALE','LONUTI',NBINST,K8B)
         CALL JEVEUO(LINST//'.VALE','L',JINST)
      ENDIF
      CALL WKVECT('&&OP0153.INSTAN2','V V R',NBINST,JINS2)
      DO 10 I = 0 , NBINST-1
         ZR(JINS2+I) = ZR(JINST+I)
 10   CONTINUE
      CALL GETVR8(' ','COEF_INST',1,1,1,COINST,N1)
      IF ( N1 .NE. 0 ) THEN
         DO 12 I = 0 , NBINST-1
            ZR(JINS2+I) = ZR(JINS2+I) * COINST
 12      CONTINUE
      ENDIF
C
      CALL WKVECT('&&OP0153.USURE_TUBE','V V R',NBINST,JUSUT)
      CALL WKVECT('&&OP0153.USURE_OBST','V V R',NBINST,JUSUO)
      CALL WKVECT('&&OP0153.PRONF_TUBE','V V R',NBINST,JPRUT)
C
      CALL GETFAC ( 'SECTEUR', NBSECT )
      IF ( NBSECT .NE. 0 ) THEN
         NBPAIR = NBSECT+1
         NBTOTA = NBSECT*NBINST
         CALL WKVECT('&&OP0153.ANGT'  , 'V V R'  ,NBPAIR, IDANGT )
         CALL WKVECT('&&OP0153.VUSTUB', 'V V R'  ,NBTOTA, IVUSTU )
         CALL WKVECT('&&OP0153.VUSOB' , 'V V R'  ,NBTOTA, IVUSOB )
         CALL WKVECT('&&OP0153.PRFUST', 'V V R'  ,NBTOTA, IPRFUT )
         CALL WKVECT('&&OP0153.PRFUSO', 'V V R'  ,NBTOTA, IPRFUO )
         CALL WKVECT('&&OP0153.VUST'  , 'V V R'  ,NBSECT, IVUST  )
         CALL WKVECT('&&OP0153.VUSO'  , 'V V R'  ,NBSECT, IVUSO  )
         CALL WKVECT('&&OP0153.PUS'   , 'V V R'  ,NBSECT, IPUS   )
         CALL WKVECT('&&OP0153.POUPRE', 'V V R'  ,NBSECT, IPOUPR )
         CALL WKVECT('&&OP0153.POURPU', 'V V R'  ,NBSECT, IPOURP )
         CALL WKVECT('&&OP0153.VCTU'  , 'V V R'  ,NBSECT, IDVCTU )
         CALL WKVECT('&&OP0153.VCOB'  , 'V V R'  ,NBSECT, IDVCOB )
         CALL WKVECT('&&OP0153.COTU'  , 'V V K16',NBSECT, IDCOTU )
         EPSIL = 1.D-4
         DO 14 I = 1 , NBSECT
            IF ( I .EQ. 1 ) THEN
               CALL GETVR8('SECTEUR','ANGL_INIT',1,1,1,ZR(IDANGT),NA)
C
C              LES ANGLES SONT CROISSANTS ENTRE -180. ET +180. :
C              -----------------------------------------------
               IF ( (ZR(IDANGT).LT.(-180.D0-EPSIL)) .OR.
     +              (ZR(IDANGT).GT.(-180.D0+EPSIL)) ) THEN
                  CALL UTMESS('F','OP0153','ANGLE INITIAL DIFFERENT '//
     +                                    'DE -180. DEGRES.')
               ENDIF
            ENDIF
            CALL GETVR8('SECTEUR','ANGL_FIN',I,1,1,ZR(IDANGT+I),NA)
            IF ( ZR(IDANGT+I) .LT. ZR(IDANGT+I-1) ) THEN
         CALL UTMESS('F','OP0153','LES ANGLES NE SONT PAS CROISSANTS.')
            ENDIF
            IF ( I .EQ. NBSECT ) THEN
               IF ( (ZR(IDANGT+I).LT.(180.D0-EPSIL)) .OR.
     +              (ZR(IDANGT+I).GT.(180.D0+EPSIL)) ) THEN
                  CALL UTMESS('F','USURE','ANGLE FINAL DIFFERENT '//
     +                                    'DE 180. DEGRES.')
               ENDIF
            ENDIF
            CALL GETVR8('SECTEUR','COEF_USUR_MOBILE',I,1,1,
     +                                             ZR(IDVCTU+I-1),N5)
            CALL GETVR8('SECTEUR','COEF_USUR_OBST'  ,I,1,1,
     +                                             ZR(IDVCOB+I-1),N5)
            CALL GETVTX('SECTEUR','CONTACT'         ,I,1,1,
     +                                             ZK16(IDCOTU+I-1),N5)
 14      CONTINUE
      ELSE
         INDIC = 1
      ENDIF
C
      IF ( PUUSUR .LE. R8PREM() ) GOTO 777
C
C     --- CALCUL DU VOLUME D'USURE TUBE ---
      ITUBE = 1
C
      IF (INDIC.EQ.0) THEN
         CALL USUVU2 ( PUUSUR, ZR(JUSUT), NBINST, ZR(JINS2), ITUBE,
     +                 NBPT, NBSECT, ZR(IDVCTU), ZR(IDANGT), ZR(JFN),
     +                 ZR(JVG), IRET, ZR(IVUSTU), ZR(IVUSOB),
     +                 ZR(IPUS), PMOYE, ZR(IPOURP), ZR(IPOUPR) )
      ELSEIF (INDIC.EQ.1) THEN
         CALL USUVUS ( PUUSUR, ZR(JUSUT), NBINST, ZR(JINS2), ITUBE,
     +                 NBPT, ZR(JFN), ZR(JVG), IRET )
      ENDIF
      IF ( IRET .NE. 0 ) GOTO 9999
C
C     --- CALCUL DU VOLUME D'USURE OBSTABLE ---
      IOBST = 2
C
      IF (INDIC.EQ.0) THEN
         CALL USUVU2 ( PUUSUR, ZR(JUSUO), NBINST, ZR(JINS2), IOBST,
     +                 NBPT, NBSECT, ZR(IDVCOB), ZR(IDANGT), ZR(JFN),
     +                 ZR(JVG), IRET, ZR(IVUSTU), ZR(IVUSOB),
     +                 ZR(IPUS), PMOYE, ZR(IPOURP), ZR(IPOUPR) )
      ELSEIF (INDIC.EQ.1) THEN
         CALL USUVUS ( PUUSUR, ZR(JUSUO), NBINST, ZR(JINS2), IOBST,
     +                 NBPT, ZR(JFN), ZR(JVG), IRET )
      ENDIF
      IF ( IRET .NE. 0 ) GOTO 9999
C
      IF (INDIC.EQ.0) THEN
         CALL GETVR8(' ','LARGEUR_OBST',1,1,1,HAUT,N1)
         IF (N1.LE.0) THEN
            HAUT=0.011D0
         ENDIF
         IF (INFO.GT.1) WRITE(IFIRES,1130)
         CALL GETVR8(' ','RAYON_MOBILE',1,1,1,RAYOT,N1)
         IF (N1.EQ.0) THEN
           CALL UTMESS('F','USURE','RAYON MOBILE OBLIGATOIRE AVEC
     +      SECTEUR.')
         ENDIF
         DO 24 I=1,NBSECT
            DO 22 K=1,NBINST
               ZR(IPRFUT+(K-1)*NBSECT+I-1) = RAYOT - 
     +          SQRT(RAYOT*RAYOT-2.D0*ZR(IVUSTU+(K-1)*NBSECT+I-1)/
     +         (HAUT*(ZR(IDANGT+I)-ZR(IDANGT+I-1))) )
 22         CONTINUE
 24      CONTINUE
         CALL GETVR8(' ','RAYON_OBST',1,1,1,RAYOO,N1)
         IF (N1.EQ.0) THEN
           CALL UTMESS('F','USURE','RAYON OBSTACLE OBLIGATOIRE AVEC
     +      SECTEUR.')
         ENDIF
         DO 20 I=1,NBSECT
            DO 23 K=1,NBINST
               ZR(IPRFUO+(K-1)*NBSECT+I-1) = RAYOO - 
     +          SQRT(RAYOO*RAYOO-2.D0*ZR(IVUSOB+(K-1)*NBSECT+I-1)/
     +         (HAUT*(ZR(IDANGT+I)-ZR(IDANGT+I-1))) )
 23      CONTINUE
 20      CONTINUE
      ENDIF
C
C      --- IMPRESSIONS DES RESULTATS ---
C
C
      IF (INDIC.NE.0) GOTO 666
      DO 25 I=1,NBSECT
        IF (INFO.GT.1) THEN
         WRITE(IFIRES,*)
         WRITE(IFIRES,*)
         WRITE(IFIRES,1090) 'SECTEUR : ',ZR(IDANGT+I-1),' / ',
     +    ZR(IDANGT+I)
         WRITE(IFIRES,*)
         WRITE(IFIRES,1120) 'TYPE DE CONTACT     ',':',
     +    ZK16(IDCOTU+I-1)
         WRITE(IFIRES,1040) 'COEF USURE TUBE     ',':',
     +    ZR(IDVCTU+I-1)
         WRITE(IFIRES,1040) 'COEF USURE OBSTACLE ',':',
     +    ZR(IDVCOB+I-1)
         WRITE(IFIRES,1030) 'PRESENCE DU CRAYON  ',':',
     +                       ZR(IPOUPR+I-1)*100.D0,'%'
         WRITE(IFIRES,1060) 'PUISSANCE D USURE   ',':',
     +    ZR(IPUS+I-1),'W'
         WRITE(IFIRES,1030) '% PU DANS CE SECTEUR',':',
     +    ZR(IPOURP+I-1),'%'
         WRITE(IFIRES,*)
         WRITE(IFIRES,1010) 'ANNEES','V_USUR_TUBE','V_USUR_OBST',
     +                     'P_USUR_TUBE','P_USUR_OBST'
        ENDIF
       DO 27 K=1,NBINST
        IF (INFO.GT.1) WRITE(IFIRES,1080) (ZR(JINS2+K-1) / COINST),
     &     ZR(IVUSTU+(K-1)*NBSECT+I-1),ZR(IVUSOB+(K-1)*NBSECT+I-1),
     &     ZR(IPRFUT+(K-1)*NBSECT+I-1),ZR(IPRFUO+(K-1)*NBSECT+I-1)
 27    CONTINUE
 25   CONTINUE
 666  CONTINUE
C     --- CALCUL DE PROFONDEUR D'USURE ---
      IF (INDIC.EQ.1)
     &  CALL USUPRU ( ZR(JUSUT), ZR(JUSUO), NBINST, ZR(JPRUT))
 777  CONTINUE
C
      IF ( INDIC .EQ. 1 ) THEN
C        --- CREATION DE LA TABLE ---
         CALL TBCRSD(RESU,'G')
         CALL TBAJPA(RESU,NBPMR,NOMPMR,TYPPMR)
         CALL TBAJLI(RESU,1,'PUIS_USUR_GLOBAL',IBID,PUUSUR,C16B,K8B,0)
         DO 30 K=1,NBINST
            VALER(1) = ZR(JINS2+K-1) / COINST
            VALER(2) = ZR(JUSUT+K-1)
            VALER(3) = ZR(JUSUO+K-1)
            VALER(4) = ZR(JPRUT+K-1)
            CALL TBAJLI ( RESU, 4,NOMPMR(2), IBID,VALER,C16B,K8B, 0)  
 30      CONTINUE
         GOTO 888
      ENDIF
C
C     REPRISE EVENTUELLE ET STOCKAGE DE LA TABLE POST_USURE :
C     -----------------------------------------------------
C
      CALL GETVID ( 'ETAT_INIT', 'TABL_USURE', 1,1,1, TABPUS, NPU )
      IF ( NPU .EQ. 0 ) THEN
         DINST = 0.D0
         CALL TBCRSD(RESU,'G')
         CALL TBAJPA(RESU,NBPAR,NOPAR,TYPAR)
      ELSE
         IF (TABPUS.NE.RESU) THEN
            CALL UTMESS('F','OP0153','LA TABLE USURE EN SORTIE EST '//
     &                             'DIFFERENTE DE CELLE EN ENTREE')
         ENDIF
C   ON REPREND UNE TABLE EXISTANTE
         NOMTA = TABPUS
         CALL TBEXP2(NOMTA,'INST')
         CALL TBEXP2(NOMTA,'SECTEUR')
         CALL TBEXP2(NOMTA,'V_USUR_OBST_CUMU')
         CALL TBEXP2(NOMTA,'V_USUR_TUBE_CUMU')
         CALL TBEXVE(NOMTA,'INST','&&OP0153.INS3','V',NBVPU,TYPE)
         CALL JEVEUO('&&OP0153.INS3','L',JINST3)
         INSDEB = ZR(JINST3+NBVPU-1)
         CALL TBEXVE(NOMTA,'SECTEUR','&&OP0153.SECT','V',NBVPU,TYPE)
         CALL JEVEUO('&&OP0153.SECT','L',JSECT)
         NBSEC2 = ZI(JSECT+NBVPU-1)
         IF (NBSEC2.NE.NBSECT) THEN
          CALL UTMESS('F','OP0153','LE NOMBRE DE SECTEURS EN SORTIE '//
     &               'EST DIFFERENT DE CELUI EN ENTREE')
         ENDIF
         CALL GETVR8 ( 'ETAT_INIT', 'INST_INIT', 1,1,1, DINST, NIS )
         IF (NIS.EQ.0) THEN
            DINST = INSDEB
         ELSEIF (DINST.GT.INSDEB) THEN
            DINST = INSDEB
         ELSE
            NEWTAB = '&&OP0153.NEWTAB'
            CALL TBEXTB(NOMTA,'V',NEWTAB,1,'INST','LE',IBID,DINST,C16B,
     &                  K8B,1.D-03,'RELA')
            CALL JEDETC(' ',NOMTA(1:8),1)
            CALL TBEXTB(NEWTAB,'G',NOMTA,1,'INST','LE',IBID,DINST,C16B,
     &                  K8B,1.D-03,'RELA')
            CALL TBEXVE(NOMTA,'INST','&&OP0153.INS5','V',NBVPU,TYPE)
            CALL JEVEUO('&&OP0153.INS5','L',JINST5)
            DINST = ZR(JINST5+NBVPU-1)
         ENDIF   
C      
C        DETERMINATION PAR SECTEUR DES VOLUS PAR TUBE ET OBST A DINST
C        ------------------------------------------------------------
C
         VALEK(1) = 'INST'
         VALEK(2) = 'SECTEUR'
         DO 1 I = 1 , NBSECT
            CALL TBLIVA(NOMTA,2,VALEK,I,DINST,C16B,K8B,'RELA',1.D-03,
     &         'V_USUR_TUBE_CUMU',K8B,IBID,ZR(IVUST+I-1),C16B,K8B,IRE1)
            CALL TBLIVA(NOMTA,2,VALEK,I,DINST,C16B,K8B,'RELA',1.D-03,
     &         'V_USUR_OBST_CUMU',K8B,IBID,ZR(IVUSO+I-1),C16B,K8B,IRE2)
            IF ((IRE1+IRE2).GT.0) THEN
               CALL UTMESS('F','OP0153','PROBLEME EXTRACTION POUR '//
     &                                'LA TABLE '//NOMTA)
            ENDIF
 1       CONTINUE
      ENDIF
C
      CALL TBAJLI(RESU,1,'PUIS_USUR_GLOBAL',IBID,PUUSUR,C16B,K8B,0)
C
      DO 26 K=1,NBINST
C        -INST-
         VALER(1) = ZR(JINS2+K-1) / COINST + DINST
C        -DUREE-
         VALER(2) = ZR(JINS2+K-1) / COINST
C        -ORIG_INST-
         VALER(3) = DINST 
C        -V_USUR_TUBE-
         VALER(4) = ZR(JUSUT+K-1)
C        -V_USUR_OBST-
         VALER(5) = ZR(JUSUO+K-1)
C        -P_USUR_TUBE-
         VALER(6) = ZR(JPRUT+K-1)
         CALL TBAJLI ( RESU, 6,NOPAR(2), IBID,VALER,C16B,K8B, 0)  
         DO 28 I=1,NBSECT
C           -ANGLE_DEBUT-
            VALER(4) = ZR(IDANGT+I-1) 
C           -ANGLE_FIN-
            VALER(5) = ZR(IDANGT+I) 
C           -V_USUR_TUBE_SECT-
            VALER(6) = ZR(IVUSTU+(K-1)*NBSECT+I-1)
C           -V_USUR_OBST_SECT-
            VALER(7) = ZR(IVUSOB+(K-1)*NBSECT+I-1)
C           -P_USUR_TUBE_SECT-
            VALER(8) = ZR(IPRFUT+(K-1)*NBSECT+I-1)
C           -P_USUR_OBST_SECT-
            VALER(9) = ZR(IPRFUO+(K-1)*NBSECT+I-1)
C           -V_USUR_TUBE_CUMU-
            VALER(10) = ZR(IVUSTU+(K-1)*NBSECT+I-1) + ZR(IVUST+I-1)
C           -V_USUR_OBST_CUMU-
            VALER(11) = ZR(IVUSOB+(K-1)*NBSECT+I-1) + ZR(IVUSO+I-1)
            CALL TBAJLI ( RESU, NBPAR2,NOPAR2, I,VALER,C16B,K8B, 0)
 28      CONTINUE
 26   CONTINUE
      IF (NBSECT.NE.0.AND.INFO.GT.1) THEN
        WRITE(IFIRES,*)
        WRITE(IFIRES,*) 'PUISSANCE D USURE MOYENNE'
        WRITE(IFIRES,1020) PMOYE,'W'
      ENDIF
C
 888  CONTINUE
C
      CALL TITRE
C
 1000 FORMAT(/,80('-'))
 1010 FORMAT(A11,2X,A15,2X,A15,2X,A15,2X,A15)
 1020 FORMAT(E11.5,1X,A1)
 1030 FORMAT(A20,1X,A1,1X,F6.2,1X,A1) 
 1040 FORMAT(A20,1X,A1,1X,E10.4)
 1060 FORMAT(A20,1X,A1,1X,E11.5,1X,A1)
 1080 FORMAT(1P E11.5,2X,E15.9,2X,E15.9,2X,E15.9,2X,E15.9)
 1090 FORMAT(A10,1X,F7.2,A3,F7.2)
 1110 FORMAT(A11,3X,A15)
 1100 FORMAT(1P E11.5,3X,E15.9)
 1120 FORMAT(A20,1X,A1,1X,A14)
 1130 FORMAT(
     +'LES PROFONDEURS USEES PAR SECTEUR SONT DES APPROXIMATIONS')
C
 9999 CONTINUE
      CALL JEDEMA()
      END
