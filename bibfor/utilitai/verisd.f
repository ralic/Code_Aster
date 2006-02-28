      SUBROUTINE VERISD(TYPESD,NOMSD)
      IMPLICIT NONE
      CHARACTER*(*) TYPESD,NOMSD
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 28/02/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE VABHHTS J.PELLET
C ----------------------------------------------------------------------
C  BUT : FAIRE UN ENSEMBLE DE VERIFICATIONS SUR UNE SD
C        => ERREUR <F> SI SD INCORRECTE
C  IN   TYPESD : TYPE DE LA STRUCTURE DE DONNEE A TESTER
C         /'CARTE'        /'CHAM_NO'      /'CHAM_ELEM'   /'RESUELEM'
C         /'CHAM_ELEM_S'  /'CHAM_NO_S'
C         /'TABLE'
C         /'RESULTAT'
C         /'FONCTION'
C         /'MODELE'     /'MODELE_GENE'
C         /'MAILLAGE'
C         /'NUME_DDL'   /'NUME_DDL_GENE'
C         /'MATRICE'    /'MATR_ASSE'  /'MATR_ASSE_GENE'
C         /'VECT_ASSE_GENE'
C         /'SOLVEUR'
C       NOMSD   : NOM DE LA STRUCTURE DE DONNEES A TESTER

C ----------------------------------------------------------------------
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
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------

      INTEGER JREFA,I1,I2,I3,IBID,JSLVK,JDESC
      INTEGER JSCDE,JSMDE,JNOMA,JREFE,JNSLV
      CHARACTER*4 DOCU1,KBID
      CHARACTER*8 CH8,METRES
      CHARACTER*14 CH14
      CHARACTER*16 TYP2SD
      CHARACTER*19 CH19
C -DEB------------------------------------------------------------------

      CALL JEMARQ()
      TYP2SD = TYPESD


      IF (TYP2SD.EQ.'MAILLAGE') THEN
C     ------------------------------
        CH8 = NOMSD
        CALL VERIOB(CH8//'.DIME','EXIS',0)
        CALL VERIOB(CH8//'.NOMNOE','EXIS',0)
C       -- JP: ON NE PEUT PAS VERIFIER .COORDO
C          CAR CELA ENTRAINE UNE BOUCLE INFINIE :
C          CHAM_NO -> MAILLAGE -> CHAM_NO ...
C       CALL VERIS2('CHAM_NO',CH8//'.COORDO')
        CALL JEEXIN(CH8//'.NOMMAI',I1)
        CALL JEEXIN(CH8//'.NOMACR',I2)
        CALL ASSERT(I1.GT.0 .OR. I2.GT.0)
        IF (I1.GT.0) THEN
           CALL VERIOB(CH8//'.NOMMAI','EXIS',0)
           CALL VERIOB(CH8//'.CONNEX','EXIS',0)
           CALL VERIOB(CH8//'.TYPMAIL','EXIS',0)
        ENDIF
        IF (I2.GT.0) THEN
           CALL VERIOB(CH8//'.NOMACR','EXIS',0)
           CALL VERIOB(CH8//'.PARA_R','EXIS',0)
           CALL VERIOB(CH8//'.SUPMAIL','EXIS',0)
        ENDIF


      ELSE IF (TYP2SD.EQ.'LIGREL') THEN
C     ------------------------------
        CH19 = NOMSD
        CALL VERIOB(CH19//'.NBNO','EXIS',0)
        CALL VERIOB(CH19//'.NOMA','EXIS',0)
        CALL JEVEUO(CH19//'.NOMA','L',JNOMA)
        CALL VERIS2('MAILLAGE',ZK8(JNOMA-1+1)(1:8))
        CALL JEEXIN(CH19//'.LIEL',I1)
        IF (I1.GT.0) THEN
           CALL VERIOB(CH19//'.LIEL','EXIS',0)
           CALL VERIOB(CH19//'.REPE','EXIS',0)
        ENDIF

      ELSE IF (TYP2SD.EQ.'MODELE') THEN
C     ------------------------------
        CH8 = NOMSD
        CALL VERIS2('LIGREL',CH8//'.MODELE')
        CALL JEEXIN(CH8//'.MAILLE',I1)
        CALL JEEXIN(CH8//'.SSSA',I2)
        IF (I1.GT.0) THEN
           CALL VERIOB(CH8//'.MAILLE','EXIS',0)
           CALL VERIOB(CH8//'.NOEUD','EXIS',0)
        ELSE
           CALL ASSERT(I2.GT.0)
        ENDIF

      ELSE IF (TYP2SD.EQ.'MODELE_GENE') THEN
C     -----------------------------------
        CH14 = NOMSD
        CALL VERIOB(CH14//'.MODG.DESC','EXIS',0)
        CALL VERIOB(CH14//'.MODG.LIDF','EXIS',0)
        CALL VERIOB(CH14//'.MODG.LIPR','EXIS',0)
        CALL VERIOB(CH14//'.MODG.LIMA','EXIS',0)
        CALL VERIOB(CH14//'.MODG.SSME','EXIS',0)
        CALL VERIOB(CH14//'.MODG.SSNO','EXIS',0)
        CALL VERIOB(CH14//'.MODG.SSOR','EXIS',0)
        CALL VERIOB(CH14//'.MODG.SSTR','EXIS',0)


      ELSE IF (TYP2SD.EQ.'CARTE') THEN
C     ------------------------------
        CH19 = NOMSD
        CALL VERIOB(CH19//'.NOMA','EXIS',0)
        CALL VERIOB(CH19//'.DESC','EXIS',0)
        CALL VERIOB(CH19//'.VALE','EXIS',0)


      ELSE IF (TYP2SD.EQ.'CHAM_NO') THEN
C     ------------------------------
        CH19 = NOMSD
        CALL VERIOB(CH19//'.DESC','EXIS',0)
        CALL JEVEUO(CH19//'.DESC','L',JDESC)

        CALL VERIOB(CH19//'.REFE','EXIS',0)
        CALL JEVEUO(CH19//'.REFE','L',JREFE)
        CALL VERIS2('MAILLAGE',ZK24(JREFE-1+1))
C       -- SI CHAM_NO "CONSTANT" : PAS DE PROF_CHNO:
        IF (ZI(JDESC-1+2).EQ.0) THEN
          CALL VERIS2('PROF_CHNO',ZK24(JREFE-1+2))
        ENDIF
        CALL VERIOB(CH19//'.VALE','EXIS',0)


      ELSE IF (TYP2SD.EQ.'CHAM_ELEM') THEN
C     ------------------------------
        CH19 = NOMSD
        CALL VERIOB(CH19//'.CELK','EXIS',0)
        CALL VERIOB(CH19//'.CELD','EXIS',0)
        CALL VERIOB(CH19//'.CELV','EXIS',0)


      ELSE IF (TYP2SD.EQ.'RESUELEM') THEN
C     ------------------------------
        CH19 = NOMSD
        CALL VERIOB(CH19//'.DESC','EXIS',0)
        CALL VERIOB(CH19//'.RESL','EXIS',0)
        CALL VERIOB(CH19//'.NOLI','EXIS',0)


      ELSE IF (TYP2SD.EQ.'CHAM_NO_S') THEN
C     ------------------------------------
        CH19 = NOMSD
        CALL VERIOB(CH19//'.CNSD','EXIS',0)
        CALL VERIOB(CH19//'.CNSV','EXIS',0)
        CALL VERIOB(CH19//'.CNSL','EXIS',0)


      ELSE IF (TYP2SD.EQ.'CHAM_ELEM_S') THEN
C     --------------------------------------
        CH19 = NOMSD
        CALL VERIOB(CH19//'.CESD','EXIS',0)
        CALL VERIOB(CH19//'.CESV','EXIS',0)
        CALL VERIOB(CH19//'.CESL','EXIS',0)


      ELSE IF (TYP2SD.EQ.'TABLE') THEN
C     --------------------------------
        CH19 = NOMSD
        CALL VERIOB(CH19//'.TBBA','EXIS',0)
        CALL VERIOB(CH19//'.TBNP','EXIS',0)
        CALL VERIOB(CH19//'.TBLP','EXIS',0)


      ELSE IF (TYP2SD.EQ.'RESULTAT') THEN
C     -----------------------------------
        CH19 = NOMSD
        CALL VERIOB(CH19//'.DESC','EXIS',0)
        CALL VERIOB(CH19//'.TACH','EXIS',0)
        CALL VERIOB(CH19//'.NOVA','EXIS',0)
        CALL VERIOB(CH19//'.TAVA','EXIS',0)
        CALL VERIOB(CH19//'.ORDR','EXIS',0)

      ELSE IF (TYP2SD.EQ.'LIGREL') THEN
C     -----------------------------------
        CH19 = NOMSD
        CALL VERIOB(CH19//'.LIEL','EXIS',0)
        CALL VERIOB(CH19//'.NOMA','EXIS',0)
        CALL VERIOB(CH19//'.NBNO','EXIS',0)


      ELSE IF (TYP2SD.EQ.'FONCTION') THEN
C     -----------------------------------
        CH19 = NOMSD
        CALL VERIOB(CH19//'.PROL','EXIS',0)


      ELSE IF (TYP2SD.EQ.'RESULTAT') THEN
C     -----------------------------------
        CH19 = NOMSD
        CALL VERIOB(CH19//'.TACH','EXIS',0)


      ELSE IF (TYP2SD.EQ.'MATRICE') THEN
C     -----------------------------------
        CH19 = NOMSD
        CALL VERIOB(CH19//'.REFA','EXIS',0)
        CALL VERIOB(CH19//'.REFA','LONMAX_EGAL',10)
C       -- MATR_ASSE OU MATR_ASSE_GENE ? :
        CALL JEVEUO(CH19//'.REFA','L',JREFA)
        IF (ZK24(JREFA-1+10).EQ.'NOEU') THEN
          CALL VERIS2('MATR_ASSE',CH19)
        ELSE IF (ZK24(JREFA-1+10).EQ.'GENE') THEN
          CALL VERIS2('MATR_ASSE_GENE',CH19)
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF


      ELSE IF (TYP2SD.EQ.'MATR_ASSE') THEN
C     -----------------------------------
        CH19 = NOMSD
        CALL VERIOB(CH19//'.REFA','EXIS',0)
        CALL VERIOB(CH19//'.REFA','LONMAX_EGAL',10)
        CALL JELIRA(CH19//'.REFA','DOCU',IBID,DOCU1)
        CALL ASSERT(DOCU1.EQ.' ')
        CALL JEVEUO(CH19//'.REFA','L',JREFA)
        CALL ASSERT(ZK24(JREFA-1+10).EQ.'NOEU')

        CALL VERIS2('MAILLAGE',ZK24(JREFA-1+1))
        CALL VERIS2('NUME_DDL',ZK24(JREFA-1+2))
        CALL ASSERT(ZK24(JREFA-1+3).EQ.' ')

        IF (ZK24(JREFA-1+5).EQ.' ') THEN
           CALL ASSERT(ZK24(JREFA-1+6).EQ.' ')
        ELSEIF (ZK24(JREFA-1+5).EQ.'FETI') THEN
           CALL ASSERT(ZK24(JREFA-1+6).NE.' ')
        ELSE
           CALL ASSERT(.FALSE.)
        ENDIF

        IF(ZK24(JREFA-1+7).NE.' ') THEN
          CALL VERIS2('SOLVEUR',ZK24(JREFA-1+7))
        ENDIF

        CALL ASSERT(ZK24(JREFA-1+8).EQ.' '.OR.
     &              ZK24(JREFA-1+8).EQ.'ASSE'.OR.
     &              ZK24(JREFA-1+8).EQ.'DECT'.OR.
     &              ZK24(JREFA-1+8).EQ.'DECP')

        CALL ASSERT(ZK24(JREFA-1+9).EQ.'MS'.OR.
     &              ZK24(JREFA-1+9).EQ.'MR')

C       EXCEP1) LA MATRICE DE CONTACT DE STAT_NON_LINE
C       EST TOUJOURS LIGNE DE CIEL :
        IF (CH19.EQ.'&&OP0070.RESOC.MATR') GOTO 10

C       EXCEP2) UNE MATRICE FETI N'A PAS DE .VALM :
        IF (ZK24(JREFA-1+5).EQ.'FETI') GOTO 10

        CALL VERIOB(CH19//'.VALM','EXIS',0)
        CALL JELIRA(CH19//'.VALM','DOCU',IBID,DOCU1)
        CALL JELIRA(CH19//'.VALM','NMAXOC',I1,KBID)
        CALL ASSERT(DOCU1.EQ.' ')
        IF (ZK24(JREFA-1+9).EQ.'MS') CALL ASSERT(I1.EQ.1)
        IF (ZK24(JREFA-1+9).EQ.'MR') CALL ASSERT(I1.EQ.2)


        IF (ZK24(JREFA-1+8).EQ.' '.OR.
     &      ZK24(JREFA-1+8).EQ.'ASSE') THEN
            CALL VERIOB(CH19//'.UALF','NOEXIS',0)
            CALL VERIOB(CH19//'.VALF','NOEXIS',0)
            CALL VERIOB(CH19//'.WALF','NOEXIS',0)
        ENDIF


      ELSE IF (TYP2SD.EQ.'MATR_ASSE_GENE') THEN
C     -----------------------------------
        CH19 = NOMSD
        CALL JEEXIN(CH19//'.DESC',I1)
        IF (I1.GT.0) THEN
          CALL VERIOB(CH19//'.DESC','EXIS',0)
          CALL VERIOB(CH19//'.DESC','LONMAX_EGAL',3)
          CALL JEVEUO(CH19//'.DESC','L',JDESC)
C         PARFOIS .DESC EST REMPLI, PARFOIS NON :
          IF (ZI(JDESC-1+1).EQ.2) THEN
            CALL ASSERT(ZI(JDESC-1+2).GT.0)
          ELSE
            CALL ASSERT(ZI(JDESC-1+1).EQ.0)
            CALL ASSERT(ZI(JDESC-1+2).EQ.0)
            CALL ASSERT(ZI(JDESC-1+3).EQ.0)
          ENDIF
        ENDIF

        CALL VERIOB(CH19//'.REFA','EXIS',0)
        CALL VERIOB(CH19//'.REFA','LONMAX_EGAL',10)
        CALL JELIRA(CH19//'.REFA','DOCU',IBID,DOCU1)
        CALL ASSERT(DOCU1.EQ.' ')
        CALL JEVEUO(CH19//'.REFA','L',JREFA)
        CALL ASSERT(ZK24(JREFA-1+10).EQ.'GENE')

C       CALL VERIS2('????',ZK24(JREFA-1+1))
        CALL VERIS2('NUME_DDL_GENE',ZK24(JREFA-1+2))

        CALL ASSERT(ZK24(JREFA-1+3).EQ.' ')
        CALL ASSERT(ZK24(JREFA-1+4).EQ.' ')
        CALL ASSERT(ZK24(JREFA-1+5).EQ.' ')
        CALL ASSERT(ZK24(JREFA-1+6).EQ.' ')

        IF(ZK24(JREFA-1+7).NE.' ') THEN
          CALL VERIS2('SOLVEUR',ZK24(JREFA-1+7))
        ENDIF

        CALL ASSERT(ZK24(JREFA-1+8).EQ.' '.OR.
     &              ZK24(JREFA-1+8).EQ.'ASSE'.OR.
     &              ZK24(JREFA-1+8).EQ.'DECT'.OR.
     &              ZK24(JREFA-1+8).EQ.'DECP')

        CALL ASSERT(ZK24(JREFA-1+9).EQ.'MS'.OR.
     &              ZK24(JREFA-1+9).EQ.'MR')


        CALL VERIOB(CH19//'.VALM','EXIS',0)
        CALL JELIRA(CH19//'.VALM','DOCU',IBID,DOCU1)
        CALL JELIRA(CH19//'.VALM','NMAXOC',I1,KBID)
        CALL ASSERT(DOCU1.EQ.' ')
        IF (ZK24(JREFA-1+9).EQ.'MS') CALL ASSERT(I1.EQ.1)
        IF (ZK24(JREFA-1+9).EQ.'MR') CALL ASSERT(I1.EQ.2)


        IF (ZK24(JREFA-1+8).EQ.' '.OR.
     &      ZK24(JREFA-1+8).EQ.'ASSE') THEN
            CALL VERIOB(CH19//'.UALF','NOEXIS',0)
            CALL VERIOB(CH19//'.VALF','NOEXIS',0)
            CALL VERIOB(CH19//'.WALF','NOEXIS',0)
        ENDIF


      ELSE IF (TYP2SD.EQ.'SOLVEUR') THEN
C     -----------------------------------
        CH19 = NOMSD
        CALL VERIOB(CH19//'.SLVK','EXIS',0)
        CALL VERIOB(CH19//'.SLVK','LONMAX_EGAL',11)
        CALL JEVEUO(CH19//'.SLVK','L',JSLVK)
        METRES=ZK24(JSLVK-1+1)
        CALL ASSERT(METRES.EQ.'FETI' .OR.
     &     METRES.EQ.'GCPC' .OR. METRES.EQ.'LDLT' .OR.
     &     METRES.EQ.'MULT_FRO' .OR. METRES.EQ.'MUMPS')
        CALL VERIOB(CH19//'.SLVI','EXIS',0)
        CALL VERIOB(CH19//'.SLVI','LONMAX_EGAL',6)
        CALL VERIOB(CH19//'.SLVR','EXIS',0)
        CALL VERIOB(CH19//'.SLVR','LONMAX_EGAL',4)


      ELSE IF (TYP2SD.EQ.'PROF_GENE') THEN
C     -----------------------------------
        CH19 = NOMSD

C       -- SOIT PROF_GENE EXISTE SOIT NON :
        CALL JEEXIN(CH19//'.NEQU',I1)
        IF (I1.GT.0) THEN
          CALL VERIOB(CH19//'.NEQU','EXIS',0)
          CALL VERIOB(CH19//'.DESC','EXIS',0)
          CALL VERIOB(CH19//'.REFN','EXIS',0)
          CALL VERIOB(CH19//'.DEEQ','EXIS',0)
          CALL VERIOB(CH19//'.LILI','EXIS',0)
          CALL VERIOB(CH19//'.NUEQ','EXIS',0)
          CALL VERIOB(CH19//'.PRNO','EXIS',0)
          CALL VERIOB(CH19//'.ORIG','EXIS',0)
        ELSE
          CALL VERIOB(CH19//'.NEQU','NOEXIS',0)
          CALL VERIOB(CH19//'.DESC','NOEXIS',0)
          CALL VERIOB(CH19//'.REFN','NOEXIS',0)
          CALL VERIOB(CH19//'.DEEQ','NOEXIS',0)
          CALL VERIOB(CH19//'.LILI','NOEXIS',0)
          CALL VERIOB(CH19//'.NUEQ','NOEXIS',0)
          CALL VERIOB(CH19//'.PRNO','NOEXIS',0)
          CALL VERIOB(CH19//'.ORIG','NOEXIS',0)
        ENDIF


      ELSE IF (TYP2SD.EQ.'PROF_CHNO') THEN
C     -----------------------------------
        CH19 = NOMSD
        CALL VERIOB(CH19//'.DEEQ','EXIS',0)
        CALL VERIOB(CH19//'.LILI','EXIS',0)
C       CALL VERIOB(CH19//'.LPRN','EXIS',0)
        CALL VERIOB(CH19//'.NUEQ','EXIS',0)
        CALL VERIOB(CH19//'.PRNO','EXIS',0)


      ELSE IF (TYP2SD.EQ.'NUME_EQUA') THEN
C     -----------------------------------
        CH19 = NOMSD

C       -- SOIT NUME_EQUA EXISTE SOIT NON :
        CALL JEEXIN(CH19//'.NEQU',I1)
        IF (I1.GT.0) THEN
          CALL VERIOB(CH19//'.NEQU','EXIS',0)
          CALL VERIOB(CH19//'.REFN','EXIS',0)
          CALL VERIS2('PROF_CHNO',CH19)
          CALL VERIOB(CH19//'.DELG','EXIS',0)
        ELSE
          CALL VERIOB(CH19//'.NEQU','NOEXIS',0)
          CALL VERIOB(CH19//'.REFN','NOEXIS',0)
          CALL VERIOB(CH19//'.DELG','NOEXIS',0)
          CALL VERIOB(CH19//'.PRNO','NOEXIS',0)
          CALL VERIOB(CH19//'.DEEQ','NOEXIS',0)
        ENDIF


      ELSE IF (TYP2SD.EQ.'NUME_DDL') THEN
C     -----------------------------------
        CH14 = NOMSD
        CALL VERIS2('NUME_EQUA',CH14//'.NUME')
        CALL VERIOB(CH14//'.NSLV','EXIS',0)
        CALL VERIOB(CH14//'.NSLV','LONMAX_EGAL',1)
        CALL JEVEUO(CH14//'.NSLV','L',JNSLV)
        CALL VERIS2('SOLVEUR',ZK24(JNSLV))

C       -- SI FETI, IL N'Y A PAS DE STOCKAGE :
        CALL JEVEUO(ZK24(JNSLV)(1:19)//'.SLVK','L',JSLVK)
        IF (ZK24(JSLVK).NE.'FETI') CALL VERIS2('STOCKAGE',CH14)


      ELSE IF (TYP2SD.EQ.'NUME_DDL_GENE') THEN
C     -----------------------------------
        CH14 = NOMSD
        CALL VERIS2('STOCKAGE',CH14)
        CALL VERIS2('PROF_GENE',CH14//'.NUME')
        CALL VERIOB(CH14//'.NSLV','EXIS',0)
        CALL VERIOB(CH14//'.NSLV','LONMAX_EGAL',1)
        CALL JEVEUO(CH14//'.NSLV','L',JNSLV)
        CALL VERIS2('SOLVEUR',ZK24(JNSLV))


      ELSE IF (TYP2SD.EQ.'STOCKAGE') THEN
C     -----------------------------------
        CH14 = NOMSD
        CALL JEEXIN(CH14//'.SMOS.SMDI',I1)
        CALL JEEXIN(CH14//'.SLCS.SCDI',I2)
        CALL JEEXIN(CH14//'.MLTF.ADNT',I3)
        CALL ASSERT(I1.GT.0)
        CALL VERIS2('STOC_MORSE',CH14//'.SMOS')
        IF (I2.GT.0) CALL VERIS2('STOC_LCIEL',CH14//'.SLCS')
        IF (I3.GT.0) CALL VERIS2('STOC_MLTF', CH14//'.MLTF')


      ELSE IF (TYP2SD.EQ.'STOC_LCIEL') THEN
C     -----------------------------------
        CH19 = NOMSD
        CALL VERIOB(CH19//'.SCBL','EXIS',0)
        CALL VERIOB(CH19//'.SCDI','EXIS',0)

        CALL VERIOB(CH19//'.SCDE','EXIS',0)
        CALL VERIOB(CH19//'.SCDE','LONMAX_EGAL',6)
        CALL JEVEUO(CH19//'.SCDE','L',JSCDE)
        CALL ASSERT(ZI(JSCDE-1+1).GT.0)
        CALL ASSERT(ZI(JSCDE-1+2).GT.0)
        CALL ASSERT(ZI(JSCDE-1+3).GT.0)
        CALL ASSERT(ZI(JSCDE-1+4).GT.0)
        CALL ASSERT(ZI(JSCDE-1+5).EQ.0)
        CALL ASSERT(ZI(JSCDE-1+6).EQ.0)

        CALL VERIOB(CH19//'.SCHC','EXIS',0)
        CALL VERIOB(CH19//'.SCIB','EXIS',0)


      ELSE IF (TYP2SD.EQ.'STOC_MORSE') THEN
C     -----------------------------------
        CH19 = NOMSD
        CALL VERIOB(CH19//'.SMBL','NOEXIS',0)
        CALL VERIOB(CH19//'.SMDI','EXIS',0)

        CALL VERIOB(CH19//'.SMDE','EXIS',0)
        CALL VERIOB(CH19//'.SMDE','LONMAX_EGAL',6)
        CALL JEVEUO(CH19//'.SMDE','L',JSMDE)
        CALL ASSERT(ZI(JSMDE-1+1).GT.0)
        CALL ASSERT(ZI(JSMDE-1+2).GT.0)
        CALL ASSERT(ZI(JSMDE-1+3).EQ.1)
        CALL ASSERT(ZI(JSMDE-1+4).EQ.0)
        CALL ASSERT(ZI(JSMDE-1+5).EQ.0)
        CALL ASSERT(ZI(JSMDE-1+6).EQ.0)

        CALL VERIOB(CH19//'.SMHC','EXIS',0)
        CALL VERIOB(CH19//'.SMIB','NOEXIS',0)


      ELSE IF (TYP2SD.EQ.'STOC_MLTF') THEN
C     -----------------------------------
        CH19 = NOMSD
        CALL VERIOB(CH19//'.ADNT','EXIS',0)
        CALL VERIOB(CH19//'.GLOB','EXIS',0)
        CALL VERIOB(CH19//'.LOCL','EXIS',0)


      ELSE IF (TYP2SD.EQ.'VECT_ASSE_GENE') THEN
C     -----------------------------------
        CH19 = NOMSD
        CALL VERIOB(CH19//'.REFE','EXIS',0)
        CALL VERIOB(CH19//'.REFE','LONMAX_EGAL',2)
        CALL VERIOB(CH19//'.DESC','EXIS',0)
        CALL VERIOB(CH19//'.DESC','LONMAX_EGAL',3)
        CALL VERIOB(CH19//'.VALE','EXIS',0)


      ELSE
        CALL UTMESS('A','VERISD',' LE MOT CLE :'//TYP2SD//
     &              ' N EST PAS PREVU.')
      END IF

   10 CONTINUE
      CALL JEDEMA()
      END
