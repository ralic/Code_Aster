      SUBROUTINE OP0166 ( IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             IER
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/05/2002   AUTEUR DURAND C.DURAND 
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
C     COMMANDE:  PROJ_CHAMP
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
      CHARACTER*4   TYPE ,CDIM1,CDIM2
      CHARACTER*8   K8,K8B,NOMA1,NOMA2,EVO1,MODEL1,MODEL2
      CHARACTER*16  TYPRES, NOMCMD,CORRES
      CHARACTER*19  RESU, CHAM1, CHAM2, NUAGE1, NUAGE2,METHOD
      CHARACTER*19  LMA, LNO, LMA1, LNO1, LMA2, LNO2
      INTEGER       NDIM
      LOGICAL ELTF
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL GETRES( RESU , TYPRES , NOMCMD )
      CALL TITRE
      CALL GETVTX(' ','METHODE' ,1,1,1, METHOD, N3 )


C     -- QUELQUES VERIFS. :
C     ----------------------
      IF (METHOD.EQ.'ELEM') THEN
        ELTF=.TRUE.
C       -- POUR L'INSTANT 'ELTFXX' N'EST PERMIS QUE POUR LES EVOL_XXX
        CALL GETVID(' ','RESULTAT'       ,1,1,1, EVO1, N1 )
        IF (N1.EQ.0) CALL UTMESS('F','OP0106',
     &     'METHODE: ELEM AUTORISEE'
     &     //' SEULEMENT POUR LES RESULTATS EVOL_XXX.')

      ELSE IF (METHOD(1:6).EQ.'NUAGE_') THEN
        ELTF=.FALSE.
C       -- POUR L'INSTANT 'NUAGE_DEG_*' N'EST PERMIS QUE POUR LES CHAMPS
        CALL GETVID(' ','CHAM_NO'       ,1,1,1, CHAM1, N1 )
        IF (N1.EQ.0) CALL UTMESS('F','OP0106',
     &     'METHODE: NUAGE_DEG__* AUTORISEE'
     &     //' SEULEMENT POUR LES CHAMPS.')
      ELSE
        CALL UTMESS('F','OP0166','STOP 1')
      END IF


C     1.-- CAS METHODE: 'ELEM' :
C     ----------------------------
      IF (ELTF) THEN
        CALL PJEFTE()
      END IF


C     2. -- CAS METHODE: 'NUAGE_DEG_0/1' :
C     ---------------------------------
      IF (.NOT.ELTF) THEN
         CALL GETVID(' ','CHAM_NO'     ,1,1,1, CHAM1, N1 )
         CALL DISMOI('F', 'NOM_MAILLA', CHAM1, 'CHAMP', IBID, NOMA1, IE)

C        C'EST LE MAILLAGE MAILLA1 QUI IMPOSE LA DIMENSION D'ESPACE DES
C        DEUX NUAGES :
         NX=3
         CALL DISMOI('F','Z_CST' ,NOMA1,'MAILLAGE',IBID,K8,IE)
         IF (K8.EQ.'OUI') NX=2

         CALL GETVID(' ','CHAM_NO_REFE',1,1,1, CHAM2, N2 )
         CALL DISMOI('F', 'NOM_MAILLA', CHAM2, 'CHAMP', IBID, NOMA2, IE)
C
C
         CALL GETFAC ( 'VIS_A_VIS' , NBOCC )
         IF ( NBOCC .NE. 0 ) THEN
C
            CALL COPISD('CHAMP_GD','G',CHAM2,RESU)
            CALL JELIRA(RESU//'.VALE','LONMAX',NBEQUA,K8B)
            CALL JELIRA(RESU//'.VALE', 'TYPE' ,IBID  ,TYPE )
            CALL JEVEUO(RESU//'.VALE','E',KVALE)
            IF ( TYPE(1:1) .EQ. 'R' ) THEN
               DO 10 IEQ = 0 , NBEQUA-1
                  ZR(KVALE+IEQ) = 0.D0
 10            CONTINUE
            ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
               DO 12 IEQ = 0 , NBEQUA-1
                  ZC(KVALE+IEQ) = 0.D0
 12            CONTINUE
            ENDIF
C
            DO 100 IOC = 1 , NBOCC
C
              LMA1 = '&&LISTE_MA_1'
              LNO1 = '&&LISTE_NO_1'
              CALL UTNUAV ( NOMA1, 1, IOC, LMA1, LNO1 )
C
              LMA2 = '&&LISTE_MA_2'
              LNO2 = '&&LISTE_NO_2'
              CALL UTNUAV ( NOMA2, 2, IOC, LMA2, LNO2 )
C
              NUAGE1 = '&&NUAGE1'
              NUAGE2 = '&&NUAGE2'
              CALL CHPNUA ( NX,CHAM1 , LMA1 , LNO1 , NUAGE1 )
              CALL CHPNUA ( NX,RESU  , LMA2 , LNO2 , NUAGE2 )
C
              CALL PRONUA ( METHOD , NUAGE1 , NUAGE2 )
C
              CALL NUACHP ( NUAGE2 , LMA2 , LNO2 , RESU )
C
              CALL DETRSD ( 'NUAGE', NUAGE1 )
              CALL DETRSD ( 'NUAGE', NUAGE2 )
              CALL JEDETC ( 'V' , '&&LISTE_' , 1 )
C
 100       CONTINUE
C
         ELSE
C
            CALL COPISD('CHAMP_GD','G',CHAM2,RESU)
            CALL JELIRA(RESU//'.VALE','LONMAX',NBEQUA,K8B)
            CALL JELIRA(RESU//'.VALE', 'TYPE' ,IBID  ,TYPE )
            CALL JEVEUO(RESU//'.VALE','E',KVALE)
            IF ( TYPE(1:1) .EQ. 'R' ) THEN
               DO 20 IEQ = 0 , NBEQUA-1
                  ZR(KVALE+IEQ) = 0.D0
 20            CONTINUE
            ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
               DO 22 IEQ = 0 , NBEQUA-1
                  ZC(KVALE+IEQ) = 0.D0
 22            CONTINUE
            ENDIF
C
            LMA = ' '
            LNO = ' '
            NUAGE1 = '&&NUAGE1'
            NUAGE2 = '&&NUAGE2'
            CALL CHPNUA ( NX,CHAM1 , LMA , LNO , NUAGE1 )
            CALL CHPNUA ( NX,RESU  , LMA , LNO , NUAGE2 )
C
            CALL PRONUA ( METHOD , NUAGE1 , NUAGE2 )
C
            CALL NUACHP ( NUAGE2 , LMA , LNO , RESU )
C
            CALL DETRSD ( 'NUAGE', NUAGE1 )
            CALL DETRSD ( 'NUAGE', NUAGE2 )
C
         ENDIF
      ENDIF
C
      CALL JEDEMA()
      END
