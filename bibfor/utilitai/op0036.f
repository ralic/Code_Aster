      SUBROUTINE OP0036 ( IER )
      IMPLICIT   NONE
      INTEGER             IER

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 06/10/2003   AUTEUR JMBHH01 J.M.PROIX 
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
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER IOCC,JF,IBID,NI,NR,NK,I,J,IR,JJJ,JP,NDIM,NDIM1,JT,JPROL 
      INTEGER NOCC,IL,NOCC2
      REAL*8  RBID,VR(2)     
      COMPLEX*16 CBID 
      CHARACTER*1 KBID
      CHARACTER*3 NTYP
      CHARACTER*8  RESULT,TYPARA(2)
      CHARACTER*16 CONCEP,NOMCMD,NPAR,NPAR1,NPARF(2)
      CHARACTER*19 NFCT
      CHARACTER*24 TRAV
      DATA TYPARA / 'R'       , 'R'       /   
C     ------------------------------------------------------------------

      CALL JEMARQ()

      IER=0

      CALL GETRES(RESULT,CONCEP,NOMCMD)
      CALL TBCRSD(RESULT,'G')
      CALL GETFAC('LISTE',NOCC)
      CALL GETFAC('FONCTION',NOCC2)
      TRAV='&&OP0036.VAL'

C     ==========
C --- CAS: LISTE 
C     ==========

      IF(NOCC.NE.0)THEN
         DO 10 IOCC=1,NOCC
            CALL GETVIS('LISTE','LISTE_I',IOCC,1,0,IBID,NI)
            CALL GETVR8('LISTE','LISTE_R',IOCC,1,0,RBID,NR)
            CALL GETVTX('LISTE','LISTE_K',IOCC,1,0,KBID,NK)
            CALL GETVID('LISTE','PARA',IOCC,1,1,NPAR,JP)

C           LISTE D'ENTIERS :
C           ---------------
            IF (NI.NE.0)THEN
               NI=-NI
               IF(IOCC.EQ.2 )THEN
                  IF(NI.NE.NDIM1)CALL UTMESS('F','OP0036','LES LISTES'//
     &                 ' DOIVENT CONTENIR LE MEME NOMBRE DE TERMES')
                  IF(NPAR.EQ.NPAR1)CALL UTMESS('F','OP0036','LES NOMS'//
     &                 ' DES PARAMETRES DOIVENT ETRE DIFFERENTS')
               ENDIF
               CALL TBAJPA(RESULT,1,NPAR,'I')
               CALL WKVECT(TRAV,'V V I',NI,JJJ)
               CALL GETVIS('LISTE','LISTE_I',IOCC,1,NI,ZI(JJJ),IR)
               DO 20 J=1,NI
               IL=(IOCC-1)*J
               CALL TBAJLI(RESULT,1,NPAR,ZI(JJJ+J-1),RBID,CBID,KBID,IL)
 20            CONTINUE       
               NDIM1=NI
               NPAR1=NPAR
            ENDIF

C           LISTE DE REELS :
C           --------------
            IF (NR.NE.0)THEN
               NR=-NR
               IF(IOCC.EQ.2 )THEN
                  IF(NR.NE.NDIM1)CALL UTMESS('F','OP0036','LES LISTES'//
     &                 ' DOIVENT CONTENIR LE MEME NOMBRE DE TERMES')
                  IF(NPAR.EQ.NPAR1)CALL UTMESS('F','OP0036','LES NOMS'//
     &                 ' DES PARAMETRES DOIVENT ETRE DIFFERENTS')
               ENDIF
               CALL TBAJPA(RESULT,1,NPAR,'R')
               CALL WKVECT(TRAV,'V V R',NR,JJJ)
               CALL GETVR8('LISTE','LISTE_R',IOCC,1,NR,ZR(JJJ),IR)
               DO 30 J=1,NR
               IL=(IOCC-1)*J
               CALL TBAJLI(RESULT,1,NPAR,IBID,ZR(JJJ+J-1),CBID,KBID,IL)
 30            CONTINUE
               NDIM1=NR
               NPAR1=NPAR
            ENDIF

C           LISTE DE CHAINE DE CARACTERES :
C           -----------------------------
            IF (NK.NE.0)THEN
               NK=-NK
               IF(IOCC.EQ.2 )THEN
                  IF(NK.NE.NDIM1)CALL UTMESS('F','OP0036','LES LISTES'//
     &                 ' DOIVENT CONTENIR LE MEME NOMBRE DE TERMES')
                  IF(NPAR.EQ.NPAR1)CALL UTMESS('F','OP0036','LES NOMS'//
     &                 ' DES PARAMETRES DOIVENT ETRE DIFFERENTS')
               ENDIF
               CALL GETVTX('LISTE','TYPE_K',IOCC,1,1,NTYP,JT)

C              CHAINES DE 8 CARACTERES
               IF(NTYP(2:2).EQ.'8')THEN
                  CALL TBAJPA(RESULT,1,NPAR,'K8')
                  CALL WKVECT(TRAV,'V V K8',NK,JJJ)
                  CALL GETVTX('LISTE','LISTE_K',IOCC,1,NK,ZK8(JJJ),IR)
                  DO 40 J=1,NK
                     IL=(IOCC-1)*J
                     CALL TBAJLI(RESULT,1,NPAR,IBID,RBID,CBID,
     &                    ZK8(JJJ+J-1),IL)
 40               CONTINUE

C              CHAINES DE 16 CARACTERES
               ELSEIF(NTYP(2:2).EQ.'1')THEN
                  CALL TBAJPA(RESULT,1,NPAR,'K16')
                  CALL WKVECT(TRAV,'V V K16',NK,JJJ)
                  CALL GETVTX('LISTE','LISTE_K',IOCC,1,NK,ZK16(JJJ),IR)
                  DO 50 J=1,NK
                     IL=(IOCC-1)*J
                     CALL TBAJLI(RESULT,1,NPAR,IBID,RBID,CBID,
     &                    ZK16(JJJ+J-1),IL)
 50               CONTINUE  

C              CHAINES DE 24 CARACTERES
               ELSEIF(NTYP(2:2).EQ.'2')THEN
                  CALL TBAJPA(RESULT,1,NPAR,'K24')
                  CALL WKVECT(TRAV,'V V K24',NK,JJJ)
                  CALL GETVTX('LISTE','LISTE_K',IOCC,1,NK,ZK24(JJJ),IR)
                  DO 60 J=1,NK
                     IL=(IOCC-1)*J
                     CALL TBAJLI(RESULT,1,NPAR,IBID,RBID,CBID,
     &                    ZK24(JJJ+J-1),IL)
 60               CONTINUE 
               ENDIF
               NDIM1=NK
               NPAR1=NPAR
            ENDIF
            CALL JEDETR(TRAV) 
 10      CONTINUE

C     ==============    
C --- CAS : FONCTION
C     ==============

      ELSEIF(NOCC2.NE.0)THEN
         CALL GETVID('FONCTION','FONCTION',1,1,1,NFCT,IR)
         CALL JEVEUO(NFCT//'.PROL','L',JPROL)
         IF(ZK16(JPROL).NE.'FONCTION' .AND.
     &      ZK16(JPROL).NE.'CONSTANT')
     &      CALL UTMESS('F','OP0036','FONCTION'//
     &        ' INCOMPATIBLE AVEC '//NOMCMD)  
         CALL GETVTX('FONCTION','PARA',1,1,2,NPARF,IR)
         IF(IR.EQ.0)THEN
            NPARF(1)=ZK16(JPROL+2)
            NPARF(2)=ZK16(JPROL+3) 
         ENDIF
         IF(NPARF(1).EQ.NPARF(2)) CALL UTMESS('F','OP0036',
     &   'LES NOMS DE CHAQUE PARAMETRE DOIVENT ETRE DIFFERENTS') 
         CALL TBAJPA(RESULT,2,NPARF,TYPARA)
         CALL JELIRA(NFCT//'.VALE','LONMAX',NDIM,KBID)
         CALL JEVEUO(NFCT//'.VALE','L',JJJ)
         DO 70 I=1,NDIM/2
C NON, LES ORDONNEES SONT A LA FIN       VR(2)=ZR(JJJ+2*I-1)
C            VR(1)=ZR(JJJ+2*I-2)
C            VR(2)=ZR(JJJ+2*I-1)
            VR(1)=ZR(JJJ-1+I)
            VR(2)=ZR(JJJ-1+NDIM/2+I)
            CALL TBAJLI(RESULT,2,NPARF,IBID,VR,CBID,KBID,0)
 70   CONTINUE
      ENDIF


      CALL JEDEMA()

      END
