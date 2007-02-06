      SUBROUTINE VSDTRG(NOMZ,M80,NVU,NOBJ)

      IMPLICIT NONE
      CHARACTER*(*) NOMZ,M80
      INTEGER NVU,NOBJ

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 05/02/2007   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C BUT : VERISD/TRAN_GENE
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  --------------------------
C -DEB------------------------------------------------------------------

      INTEGER        IER,IRET,JDESC,JREFD,NBMODE,NBCHOC,NBREDE,VERIOB
      INTEGER        METH,NBSAUV,JICHO,JREDC,JSST,I,NBPTEM,IBID
      INTEGER        JFACC,JFVIT,JFDEP,LONG,NEXCIT
      CHARACTER*24   RIGGEN,MASGEN,AMOGEN,NUMGEN,NUMGE2,KBID,CONCEP
      CHARACTER*16   TYPCO
      CHARACTER*8    NOM


      CALL JEMARQ()
      
      NOM=NOMZ(1:8)

C---- 1.VERIFICATION DE L OBJET .DESC

      IER=VERIOB('O',NOM//'           .DESC','EXIS_V',0,' ')
      IER=VERIOB('O',NOM//'           .DESC','TYPE_I',0,' ')
      IER=VERIOB('O',NOM//'           .DESC','LONMAX',5,' ')

      CALL JEVEUO(NOM//'           .DESC','L',JDESC)


C      VERIFICATION SUPPLEMENTAIRES SUR LES VALEURS DU DESC
C      ET RECUPERATION DE DONNEES NECESSAIRE POUR VERIFICATION 
C      ULTERIEURE

      METH=ZI(JDESC)
      
      CALL ASSERT((METH.EQ.1).OR.(METH.EQ.2).OR.(METH.EQ.3))
      
      NBMODE=ZI(JDESC+1)
      NBCHOC=ZI(JDESC+2)
      NBREDE=ZI(JDESC+3)
      
C---- 2.VERIFICATION DE L OBJET .REFD

      IER=VERIOB('O',NOM//'           .REFD','EXIS_V',0,' ')
      IER=VERIOB('O',NOM//'           .REFD','TYPE_K24',0,' ')
      IER=VERIOB('O',NOM//'           .REFD','LONMAX',6,' ')
      
      CALL JEVEUO(NOM//'           .REFD','L',JREFD)
      
      RIGGEN=ZK24(JREFD)
      MASGEN=ZK24(JREFD+1)
      AMOGEN=ZK24(JREFD+2)
      NUMGEN=ZK24(JREFD+3)
      CALL ASSERT(ZK24(JREFD+4).EQ.' ')
      CONCEP=ZK24(JREFD+5)
      
      
C      LA ON DOIT VERIFIER SI LES MATRICES SONT VRAIMENT 
C      CE QU ELLES DOIVENT ETRE AINSI QUE LA BASE MODALE
C      REMARQUE ON PEUT CREER DES OBJET "VIDE". DANS CE CAS
C      ON NE VERIFIE EVIDEMENT PAS
      IF (RIGGEN.NE.' ') THEN
        CALL VERIJA('O','MATR_ASSE_GENE',RIGGEN,M80,NVU,NOBJ)
      ENDIF

      IF (MASGEN.NE.' ') THEN
      CALL VERIJA('O','MATR_ASSE_GENE',MASGEN,M80,NVU,NOBJ)
      ENDIF

      CALL VERIJA('F','MATR_ASSE_GENE',AMOGEN,M80,NVU,NOBJ)
      
      
      CALL JEEXIN(CONCEP(1:8)//'      .NUME.REFN',IRET)

      IF (IRET.NE.0) THEN
C  LA SD CONCEP EST EN FAIT UN NUME_DDL_GENE
          CALL VERIJA('O','NUME_DDL',CONCEP,M80,NVU,NOBJ)
          CALL ASSERT(NUMGEN.EQ.CONCEP)
      ELSE
C    SINON C EST UN SD_RESULTAT 
C   (SOIT BASE_MODALE, SOIT MODE_MECA SOIT MODE_GENE)
        CALL DISMOI('F','TYPE_RESU',CONCEP,'RESULTAT',IBID,TYPCO,IER)
        IF (TYPCO.EQ.'BASE_MODALE') THEN
          CALL VERIJA('O','BASE_MODALE',CONCEP,M80,NVU,NOBJ)
        ELSE IF (TYPCO(1:5).EQ.'MODE_') THEN
          CALL VERIJA('O','SD_RESULTAT_DYN',CONCEP,M80,NVU,NOBJ)
        ELSE
          CALL U2MESK('F','SDVERI_31',1,TYPCO)
        ENDIF
      ENDIF
        


      IF (NUMGEN.NE.' ') THEN
      CALL VERIJA('O','NUME_DDL',NUMGEN,M80,NVU,NOBJ)
      ENDIF
      
C---- 3.VERIFICATION DE L OBJET .DEPL

C     RECUPERATION DU NOMBRE DE PAS DE TEMPS SAUVEGARDE

      CALL JELIRA(NOM//'           .ORDR','LONMAX',NBSAUV,KBID)
      
      IER=VERIOB('O',NOM//'           .DEPL','EXIS_V',0,' ')
      IER=VERIOB('O',NOM//'           .DEPL','TYPE_R',0,' ')
      IER=VERIOB('O',NOM//'           .DEPL','LONMAX',
     &           NBSAUV*NBMODE,' ')

C---- 4.VERIFICATION DE L OBJET .VITE

      IER=VERIOB('O',NOM//'           .VITE','EXIS_V',0,' ')
      IER=VERIOB('O',NOM//'           .VITE','TYPE_R',0,' ')
      IER=VERIOB('O',NOM//'           .VITE','LONMAX',
     &           NBSAUV*NBMODE,' ')

C---- 5.VERIFICATION DE L OBJET .ACCE

      IER=VERIOB('O',NOM//'           .ACCE','EXIS_V',0,' ')
      IER=VERIOB('O',NOM//'           .ACCE','TYPE_R',0,' ')
      IER=VERIOB('O',NOM//'           .ACCE','LONMAX',
     &           NBSAUV*NBMODE,' ')

C---- 6.VERIFICATION DE L OBJET .DLOC

      IER=VERIOB('F',NOM//'           .DLOC','EXIS_V',0,' ')
      IER=VERIOB('F',NOM//'           .DLOC','TYPE_R',0,' ')
      IER=VERIOB('F',NOM//'           .DLOC','LONMAX',
     &           6*NBSAUV*NBCHOC,' ')

C---- 7.VERIFICATION DE L OBJET .FCHO

      IER=VERIOB('F',NOM//'           .FCHO','EXIS_V',0,' ')
      IER=VERIOB('F',NOM//'           .FCHO','TYPE_R',0,' ')
      IER=VERIOB('F',NOM//'           .FCHO','LONMAX',
     &           3*NBSAUV*NBCHOC,' ')

C---- 8.VERIFICATION DE L OBJET .FACC

      IER=VERIOB('F',NOM//'           .FACC','EXIS_V',0,' ')
      IER=VERIOB('F',NOM//'           .FACC','TYPE_K8',0,' ')
      IF (IER.EQ.1) THEN
        CALL JELIRA(NOM//'           .FACC','LONMAX',LONG,KBID)
        NEXCIT=LONG/2
        CALL JEVEUO(NOM//'           .FACC','L',JFACC)

C       L OBJET PEUT ETRE CREE A VIDE... DONC ON VERIFIE QUE
C       C EST UNE FONCTIONAVANT DE LANCER LA VERIFICATION DE L OBJET
        DO 10 I=1,NEXCIT
          IF (ZK8(JFACC-1+I).NE.' ') THEN
            CALL JEEXIN(ZK8(JFACC-1+I)//'           .NOVA',IRET)
C    CONCEPT DE TYPE FONCTION OU FORMULE
            IF (IRET.EQ.0) THEN
              CALL VERIJA('O','FONCTION',ZK8(JFACC-1+I),
     &                    M80,NVU,NOBJ)
            ELSE
              CALL VERIJA('O','FORMULE',ZK8(JFACC-1+I),
     &                    M80,NVU,NOBJ)
            ENDIF
          ENDIF
10      CONTINUE
      ENDIF
      
C---- 9.VERIFICATION DE L OBJET .FDEP

      IER=VERIOB('F',NOM//'           .FDEP','EXIS_V',0,' ')
      IER=VERIOB('F',NOM//'           .FDEP','TYPE_K8',0,' ')
      IF (IER.EQ.1) THEN
        CALL JELIRA(NOM//'           .FDEP','LONMAX',LONG,KBID)
        NEXCIT=LONG/2
        CALL JEVEUO(NOM//'           .FDEP','L',JFDEP)

C       L OBJET PEUT ETRE CREE A VIDE... DONC ON VERIFIE QUE
C       C EST UNE FONCTION AVANT DE LANCER LA VERIFICATION DE L OBJET
        DO 20 I=1,NEXCIT
          IF (ZK8(JFDEP-1+I).NE.' ') THEN
            CALL JEEXIN(ZK8(JFDEP-1+I)//'           .NOVA',IRET)
C    CONCEPT DE TYPE FONCTION OU FORMULE
            IF (IRET.EQ.0) THEN
              CALL VERIJA('O','FONCTION',ZK8(JFDEP-1+I),
     &                    M80,NVU,NOBJ)
            ELSE
              CALL VERIJA('O','FORMULE',ZK8(JFDEP-1+I),
     &                    M80,NVU,NOBJ)
            ENDIF
          ENDIF
20      CONTINUE
      ENDIF

C---- 10.VERIFICATION DE L OBJET .FVIT

      IER=VERIOB('F',NOM//'           .FVIT','EXIS_V',0,' ')
      IER=VERIOB('F',NOM//'           .FVIT','TYPE_K8',0,' ')

      IF (IER.EQ.1) THEN
        CALL JELIRA(NOM//'           .FVIT','LONMAX',LONG,KBID)
        NEXCIT=LONG/2
        CALL JEVEUO(NOM//'           .FVIT','L',JFVIT)

C       L OBJET PEUT ETRE CREE A VIDE... DONC ON VERIFIE QUE
C       C EST UNE FONCTION AVANT DE LANCER LA VERIFICATION DE L OBJET
        DO 30 I=1,NEXCIT
          IF (ZK8(JFVIT-1+I).NE.' ') THEN
            CALL JEEXIN(ZK8(JFVIT-1+I)//'           .NOVA',IRET)
C    CONCEPT DE TYPE FONCTION OU FORMULE
            IF (IRET.EQ.0) THEN
              CALL VERIJA('O','FONCTION',ZK8(JFVIT-1+I),
     &                    M80,NVU,NOBJ)
            ELSE
              CALL VERIJA('O','FORMULE',ZK8(JFVIT-1+I),
     &                    M80,NVU,NOBJ)
            ENDIF
          ENDIF
30      CONTINUE
      ENDIF

C---- 11.VERIFICATION DE L OBJET .ICHO

      IER=VERIOB('F',NOM//'           .ICHO','EXIS_V',0,' ')
      IER=VERIOB('F',NOM//'           .ICHO','TYPE_I',0,' ')
      IER=VERIOB('F',NOM//'           .ICHO','LONMAX',
     &           NBSAUV*NBCHOC,' ')

      IF (IER.EQ.1) THEN
        CALL JEVEUO(NOM//'           .ICHO','L',JICHO)

        DO 40 I=1,NBSAUV*NBCHOC
          CALL ASSERT((ZI(JICHO-1+I).EQ.0).OR.
     &                (ZI(JICHO-1+I).EQ.1))
40      CONTINUE
      ENDIF

C---- 12.VERIFICATION DE L OBJET .INST

      IER=VERIOB('O',NOM//'           .INST','EXIS_V',0,' ')
      IER=VERIOB('O',NOM//'           .INST','TYPE_R',0,' ')
      IER=VERIOB('O',NOM//'           .INST','LONMAX',NBSAUV,' ')

C---- 13.VERIFICATION DE L OBJET .INTI

      IER=VERIOB('F',NOM//'           .INTI','EXIS_V',0,' ')
      IER=VERIOB('F',NOM//'           .INTI','TYPE_K8',0,' ')
      IER=VERIOB('F',NOM//'           .INTI','LONMAX',NBCHOC,' ')

C---- 14.VERIFICATION DE L OBJET .IPSD

      IER=VERIOB('F',NOM//'           .IPSD','EXIS_V',0,' ')
      IER=VERIOB('F',NOM//'           .IPSD','TYPE_R',0,' ')

C---- 15.VERIFICATION DE L OBJET .ORDR

      IER=VERIOB('O',NOM//'           .ORDR','EXIS_V',0,' ')
      IER=VERIOB('O',NOM//'           .ORDR','TYPE_I',0,' ')
      IER=VERIOB('O',NOM//'           .ORDR','LONMAX',NBSAUV,' ')

C---- 16.VERIFICATION DE L OBJET .PTEM

      IER=VERIOB('O',NOM//'           .PTEM','EXIS_V',0,' ')
      IER=VERIOB('O',NOM//'           .PTEM','TYPE_R',0,' ')

      CALL JELIRA(NOM//'           .PTEM','LONMAX',NBPTEM,KBID)

C     ON AURAIT BIEN AIME COMPARE NBTEM ET NBSAUV MAIS IMPOSSIBLE CAR
C     LE CAS D UN REUSE SUR UNE SD TRAN_GENE N A PAS ETE PRIS
C     CORRECTEMENT EN COMPTE
C     QUID DU CAS OU UN DES REUSE UTILISE LA METHODE ADAPT  OU MEME
C     UN REUSE EN UTILISANT UN PAS DE TEMPS DIFFERENT A CHAQUE FOIS ?
C     L OBJET PTEM EST A MON AVIS BUGGE.

C      CALL ASSERT((NBPTEM.EQ.1).OR.(NBPTEM.EQ.NBSAUV))
      
C---- 17.VERIFICATION DE L OBJET .NCHO

      IER=VERIOB('F',NOM//'           .NCHO','EXIS_V',0,' ')
      IER=VERIOB('F',NOM//'           .NCHO','TYPE_K8',0,' ')
      IER=VERIOB('F',NOM//'           .NCHO','LONMAX',2*NBCHOC,' ')

C---- 18.VERIFICATION DE L OBJET .VCHO

      IER=VERIOB('F',NOM//'           .VCHO','EXIS_V',0,' ')
      IER=VERIOB('F',NOM//'           .VCHO','TYPE_R',0,' ')
      IER=VERIOB('F',NOM//'           .VCHO','LONMAX',
     &           3*NBCHOC*NBSAUV,' ')

C---- 19.VERIFICATION DE L OBJET .VINT

      IER=VERIOB('F',NOM//'           .VINT','EXIS_V',0,' ')
      IER=VERIOB('F',NOM//'           .VINT','TYPE_R',0,' ')
      IER=VERIOB('F',NOM//'           .VINT','LONMAX',
     &           NBCHOC*NBSAUV,' ')

C---- 20.VERIFICATION DE L OBJET .SST

      IER=VERIOB('F',NOM//'           .SST','EXIS_V',0,' ')
      IER=VERIOB('F',NOM//'           .SST','TYPE_K8',0,' ')
      IER=VERIOB('F',NOM//'           .SST','LONMAX',
     &           2*NBCHOC,' ')

C---- 21.VERIFICATION DE L OBJET .REDC

      IER=VERIOB('F',NOM//'           .REDC','EXIS_V',0,' ')
      IER=VERIOB('F',NOM//'           .REDC','TYPE_I',0,' ')
      IER=VERIOB('F',NOM//'           .REDC','LONMAX',
     &           NBSAUV*NBREDE,' ')
      

      IF (IER.EQ.1) THEN
        CALL JEVEUO(NOM//'           .REDC','L',JREDC)
        DO 50 I=1,2*NBCHOC      
          CALL ASSERT((ZI(JREDC-1+I).EQ.0).OR.(ZI(JREDC-1+I).EQ.1))
50      CONTINUE
      ENDIF

     
C---- 22.VERIFICATION DE L OBJET .REDD

      IER=VERIOB('F',NOM//'           .REDD','EXIS_V',0,' ')
      IER=VERIOB('F',NOM//'           .REDD','TYPE_R',0,' ')
      IER=VERIOB('F',NOM//'           .REDD','LONMAX',
     &           NBSAUV*NBREDE,' ')

C---- 23.VERIFICATION DE L OBJET .REDN

      IER=VERIOB('F',NOM//'           .REDN','EXIS_V',0,' ')
      IER=VERIOB('F',NOM//'           .REDN','TYPE_K24',0,' ')
      IER=VERIOB('F',NOM//'           .REDN','LONMAX',
     &           NBREDE,' ')
      

      CALL JEDEMA()

      END
