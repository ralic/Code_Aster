      SUBROUTINE FETSKP()

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 19/07/2005   AUTEUR ASSIRE A.ASSIRE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_20
C TOLE CRP_4
C-----------------------------------------------------------------------
C    - FONCTION REALISEE: 
C       - CREATION DU GRAPHE D'ENTREE DU PARTITIONNEUR
C       - GERE LES POIDS SUR LES MAILLES
C       - GERE LE GROUPAGE DES MAILLES
C       - APPEL A METIS OU EXECUTION DE SCOTCH
C       - CREATION DE NOUVEAUX GROUPES DE MAILLES
C       - VERIFICATION DE LA CONNEXITE DES SOUS-DOMAINES
C----------------------------------------------------------------------
C RESPONSABLE ASSIRE A.ASSIRE

C CORPS DU PROGRAMME
      IMPLICIT NONE

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX --------------------
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
      INTEGER*4          ZI4
      COMMON  / I4VAJE / ZI4(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX --------------------

C DECLARATION VARIABLES LOCALES
      INTEGER       NBMANO,IDCOI,COI,NBMAMA,IDCO,MAXI,TEMP,TEMP1,SDB,
     &              MASD,NBMASD,IDMASD,ID,ID1,ID2,ISD,NBSD,RENUM1,
     &              RENUM,NUMNO2,IDNO,JNOM,MEM,NBRE,NBMABO,MAIL2,
     &              I,J,IMA,INO,IAGMA,NBGRMN,NBNO,NUMNO,NBBORD,LREP,
     &              NBNOTO,NBMATO,NUTYMA,JVG,JGG,MAIL,TYPMA,RENUM2,
     &              IFM,NIV,NBLIEN,IDNOEU,NMAX,NBPART,RENUM3,
     &              NBMA,NBGMA,NOMSDM,NUMSDM,CO,MABORD,NBNOEU,IDF,
     &              FLAGMA,FLAGSD,LISTE,PTR,NMAP,VELO,EDLO,POIDS,
     &              IOCC,NOCC,IDMA,IULM2,IULM1,ULNUME,ERR
      REAL*8        TMPS(6),T0
      CHARACTER*8   MA,K8BID,NOGMA,KTMP,NOM,TYPMA1,TYPMA2,MOD,SDBORD,
     &              VERIF,GRPEMA,KTMP2,METH,BORD
      CHARACTER*24  GRPMAV,GRPMA
      CHARACTER*32  JEXNOM,JEXNUM
      CHARACTER*128 REP
      
C CORPS DU PROGRAMME
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)
      
C ********************************************************************
C                       CREATION DU GRAPHE 
C ********************************************************************
      
      IF (NIV .GE. 2) THEN 
        CALL UTTCPU(18,'INIT',6,TMPS)
        CALL UTTCPU(18,'DEBUT',6,TMPS)
      ENDIF        

C ------- ON RECUPERE LES DONNEES D ENTREE ---------------------------
      
      CALL GETVID(' ','MAILLAGE',0,1,1,MA,ERR)
      CALL DISMOI('F','NB_NO_MAILLA',MA,'MAILLAGE',NBNOTO,K8BID,ERR)
      CALL DISMOI('F','NB_MA_MAILLA',MA,'MAILLAGE',NBMATO,K8BID,ERR)
      CALL WKVECT('RENUM1','V V I',NBMATO,RENUM1)
      CALL GETVID(' ','MODELE' ,0,1,1,MOD,ERR)
  
      IF ( ERR .EQ. 0 ) THEN
         WRITE(IFM,*)' -- AUCUN MODELE PRIS EN COMPTE'
         CALL WKVECT('RENUM ','V V I',NBMATO,RENUM)
         DO 90 IMA=1,NBMATO
            ZI(RENUM-1+IMA)=IMA
            ZI(RENUM1-1+IMA)=IMA
 90      CONTINUE
       ELSE      
         WRITE(IFM,*)' -- PRISE EN COMPTE DU MODELE :',MOD
         NBMATO=0 
         CALL JELIRA(MOD//'.MODELE    .LIEL','NMAXOC',MAXI,K8BID)
         DO 94 I=1,MAXI
            CALL JELIRA(JEXNUM(MOD//'.MODELE    .LIEL',I),'LONMAX'
     &                  ,NBRE,K8BID)
            NBMATO=NBMATO+NBRE-1
 94      CONTINUE 
         CALL WKVECT('RENUM ','V V I',NBMATO,RENUM)
         ID=1
         DO 93 I=1,MAXI
            CALL JELIRA(JEXNUM(MOD//'.MODELE    .LIEL',I),'LONMAX'
     &                  ,NMAX,K8BID)
            CALL JEVEUO(JEXNUM(MOD//'.MODELE    .LIEL',I),'L',MAIL)
            DO 91 IMA=1,NMAX-1
               ZI(RENUM-1+ID)=ZI(MAIL-1+IMA)
               ZI(RENUM1-1+ZI(MAIL-1+IMA))=ID
               ID=ID+1
 91         CONTINUE   
 93      CONTINUE 
      ENDIF

      WRITE(IFM,*)' -- NOMBRE DE MAILLES : ',NBMATO
      WRITE(IFM,*)' -- NOMBRE DE NOEUDS  : ',NBNOTO
      WRITE(IFM,*)' '

C ------------------------ JEVEUX ------------------------------------

      CALL WKVECT('NBMANO','V V I',NBNOTO,NBMANO)
      CALL WKVECT('IDCOI ','V V I',NBNOTO+1,IDCOI)
      CALL WKVECT('ID1   ','V V I',NBNOTO,ID1)
      CALL WKVECT('TYPMA ','V V K8',NBMATO,TYPMA)
      CALL WKVECT('IDNO  ','V V I',NBMATO,IDNO)
      CALL WKVECT('NBNO  ','V V I',NBMATO,NBNO)
      DO 3 INO=1,NBNOTO 
         ZI(NBMANO-1+INO)=0
         ZI(ID1-1+INO)=0
  3   CONTINUE 
 
C ------- ON RECUPERE LE NOMBRE DE MAILLES RELIEES A CHAQUE NOEUD ----
C ------- CREATION DU TABLEAU DES TYPES DE MAILLES -------------------
C ------- REMPLISSAGE DES TABLEAUX NBNO ET IDNOEU --------------------
C ------- LINEARISATION DES ELEMENTS ---------------------------------
 
      CALL JEVEUO (MA//'.TYPMAIL','L',NUTYMA)
       
      DO 1 IMA=1,NBMATO 
        MAIL=ZI(RENUM-1+IMA)
        NBRE=ZI(NUTYMA-1+MAIL)
        CALL JENUNO (JEXNUM('&CATA.TM.NOMTM',NBRE),NOM)
        CALL JEVEUO (JEXNUM(MA//'.CONNEX',MAIL),'L',IDNOEU)
        CALL JELIRA (JEXNUM(MA//'.CONNEX',MAIL),'LONMAX',NBNOEU,
     +               K8BID)
        ZI(IDNO-1+IMA)=IDNOEU

C      ------- ON LINEARISE LES ELEMENTS -------  
        IF ( NOM .EQ. 'SEG3    ') THEN 
            ZK8(TYPMA-1+IMA)= 'SEG2    ' 
            ZI(NBNO-1+IMA)=2
        ELSEIF ( NOM .EQ. 'TRIA6   ') THEN  
            ZK8(TYPMA-1+IMA)= 'TRIA3   '
            ZI(NBNO-1+IMA)=3
        ELSEIF ( NOM .EQ. 'QUAD8   ') THEN  
            ZK8(TYPMA-1+IMA)= 'QUAD4   '
            ZI(NBNO-1+IMA)=4
        ELSEIF ( NOM .EQ. 'QUAD9   ') THEN  
            ZK8(TYPMA-1+IMA)= 'QUAD4   '
            ZI(NBNO-1+IMA)=4
        ELSEIF ( NOM .EQ. 'TETRA10 ') THEN  
            ZK8(TYPMA-1+IMA)= 'TETRA4  '
            ZI(NBNO-1+IMA)=4
        ELSEIF ( NOM .EQ. 'PENTA15 ') THEN  
            ZK8(TYPMA-1+IMA)= 'PENTA6  '
            ZI(NBNO-1+IMA)=6
        ELSEIF ( NOM .EQ. 'HEXA20  ') THEN  
            ZK8(TYPMA-1+IMA)= 'HEXA8   '
            ZI(NBNO-1+IMA)=8
        ELSEIF ( NOM .EQ. 'HEXA27  ') THEN
            ZK8(TYPMA-1+IMA)= 'HEXA8   '
            ZI(NBNO-1+IMA)=8
        ELSE 
           ZK8(TYPMA-1+IMA)= NOM
           ZI(NBNO-1+IMA)=NBNOEU
        ENDIF
C      ------- FIN DE LA LINEARISATION -------   

        DO 2 INO=1,ZI(NBNO-1+IMA)
          NUMNO = ZI(IDNOEU-1+INO)
          ZI(NBMANO-1+NUMNO)=ZI(NBMANO-1+NUMNO)+1
  2     CONTINUE     
  1   CONTINUE
       
C ------- ON CREE LE TABLEAU D'INDEX POUR COI ------------------------
  
      ZI(IDCOI)=1
      DO 4 INO=2,NBNOTO+1
       ZI(IDCOI-1+INO)=ZI(IDCOI-1+INO-1)+ZI(NBMANO-1+INO-1)
  4   CONTINUE

C ------- ON CREE LE TABLEAU DE CONNECTIVITE INVERSE ( COI ) ---------
     
      CALL WKVECT('COI   ','V V I',ZI(IDCOI-1+NBNOTO+1)-1,COI)
      
      DO 5 IMA=1,NBMATO
        IDNOEU=ZI(IDNO-1+IMA)
        NBNOEU=ZI(NBNO-1+IMA)
        DO 6 INO=1,NBNOEU
           NUMNO=ZI(IDNOEU-1+INO)
           ID=ZI(IDCOI-1+NUMNO)+ZI(ID1-1+NUMNO)
           ZI(COI-1+ID)=IMA
           ZI(ID1-1+NUMNO)=ZI(ID1-1+NUMNO)+1
  6     CONTINUE       
  5   CONTINUE       

      CALL JEDETR('ID1   ')

C ------- ON REMPLIT LE TABLEAU NOMBRE DE MAILLES PAR MAILLE (NBMAMA) 
      
      CALL WKVECT('NBMAMA','V V I',NBMATO,NBMAMA) 
      DO 33 IMA=1,NBMATO
         ZI(NBMAMA-1+IMA)=0
 33   CONTINUE
      
      DO 8 INO=1,NBNOTO
         IF (ZI(NBMANO-1+INO).GT.1) THEN
            DO 9 I=ZI(IDCOI-1+INO),ZI(IDCOI-1+INO+1)-1
               NBRE=ZI(IDCOI-1+INO+1)-1-ZI(IDCOI+INO-1)
               MAIL=ZI(COI-1+I)
               ZI(NBMAMA-1+MAIL)=ZI(NBMAMA-1+MAIL)+NBRE
  9         CONTINUE
         ENDIF  
  8   CONTINUE 
      
      CALL JEDETR('NBMANO')

C ------- ON REMPLIT L'INDEX DU TABLEAU DES CONNECTIVITES (CO) ------- 
      
      CALL WKVECT('IDCO  ','V V S',NBMATO+1,IDCO)

      MAXI=0
      ZI4(IDCO)=1
      DO 10 IMA=2,NBMATO+1
        IF (ZI(NBMAMA-1+IMA-1) .GT. MAXI) MAXI=ZI(NBMAMA-1+IMA-1)
        ZI4(IDCO-1+IMA)=ZI4(IDCO-1+IMA-1)+ZI(NBMAMA-1+IMA-1)
 10   CONTINUE
      
      IF (NIV .GE. 2) THEN 
         CALL UTTCPU(18,'FIN',6,TMPS)
         WRITE(IFM,*)'CONNECTIVITE INVERSE :',TMPS(3)
         T0=TMPS(3)
      ENDIF
      
C ------------------------ JEVEUX ------------------------------------

      CALL JEDETR('NBMAMA')
      CALL WKVECT('TEMP  ','V V I',MAXI,TEMP)
      CALL WKVECT('TEMP1 ','V V I',MAXI,TEMP1)
      CALL WKVECT('MABORD','V V I',NBMATO,MABORD)      
      DO 16 IMA=1,NBMATO
         ZI(MABORD-1+IMA) = 0
 16   CONTINUE 

C ------- ENLEVE T ON LES MAILLES DE BORDS ? -------------------------

      CALL GETVTX('        ','TRAITER_BORDS' ,0,1,1,BORD,ERR)
      IF (NIV .GE. 2 ) THEN
         WRITE(IFM,*)'TRAITE T-ON LES BORDS ? : ',BORD
         CALL UTTCPU(18,'DEBUT',6,TMPS)
      ENDIF   

C ------- ON CHERCHE LES MAILLES DE BORDS ----------------------------
       
      NBBORD=0
      DO 11 IMA=1,NBMATO
         NMAX=0
         MAXI=0
         IDNOEU=ZI(IDNO-1+IMA)
         DO 12 INO=1,ZI(NBNO-1+IMA)
            NUMNO=ZI(IDNOEU-1+INO)
            DO 13 I=ZI(IDCOI-1+NUMNO),ZI(IDCOI-1+NUMNO+1)-1
               MAIL=ZI(COI-1+I)
               IF (MAIL .EQ. IMA) GOTO 20
               DO 14 J=1,NMAX
                  IF (ZI(TEMP-1+J) .EQ. MAIL ) THEN
                      ZI(TEMP1-1+J)=ZI(TEMP1-1+J)+1
                     GOTO 20
                  ENDIF
 14            CONTINUE                    
               ZI(TEMP+NMAX)=MAIL
               ZI(TEMP1+NMAX)=1
               NMAX=NMAX+1
 20            CONTINUE
 13         CONTINUE
 12      CONTINUE 
         DO 15 J=1,NMAX
            IF ( ZI(NBNO-1+IMA) .EQ. ZI(TEMP1-1+J) ) THEN
               IF (ZI(MABORD-1+IMA) .EQ. 0) THEN 
                  NBBORD=NBBORD+1
                  MAXI=ZI(TEMP1-1+J)
               ENDIF   
               IF (ZI(TEMP1-1+J) .GE. MAXI ) THEN
                  ZI(MABORD-1+IMA) = ZI(TEMP-1+J)
               ENDIF   
            ENDIF
 15      CONTINUE      
 11   CONTINUE      
      
      IF (NIV .GE. 2) THEN
         CALL UTTCPU(18,'FIN',6,TMPS)
         WRITE(IFM,*)'MAILLE DE BORD :',TMPS(3)-T0
         T0=TMPS(3)   
         WRITE(IFM,*)'LE MAILLAGE CONTIENT ',NBBORD,' MAILLES DE BORDS'
      ENDIF
      
C ------ ON ENLEVE LES MAILLES DE BORDS ------------------------------
      
      CALL WKVECT('RENUM2','V V I',NBMATO,RENUM2)
      CALL WKVECT('RENUM3','V V I',NBMATO,RENUM3)
      NBMABO=NBMATO   
      IF (BORD .EQ. 'OUI     ') THEN
         NBMATO=NBMABO-NBBORD
         ID=1
         IDF=NBMATO+1
         DO 49 IMA=1,NBMABO
            IF ( ZI(MABORD-1+IMA) .NE. 0 ) THEN
               ZI(RENUM2-1+IDF)=IMA
               ZI(RENUM3-1+IMA)=IDF
               IDF=IDF+1
            ELSE
               ZI(RENUM2-1+ID)=IMA
               ZI(RENUM3-1+IMA)=ID
               ID=ID+1
            ENDIF
 49      CONTINUE
      ELSE
         DO 52 IMA=1,NBMATO
            ZI(RENUM2-1+IMA)=IMA
            ZI(RENUM3-1+IMA)=IMA
 52      CONTINUE
      ENDIF
C ------------------------ JEVEUX ------------------------------------
      
      CALL WKVECT('NBMAMA','V V I',NBMATO,NBMAMA)
      CALL WKVECT('ID1   ','V V I',NBMATO,ID1)
      DO 98 IMA=1,NBMATO
         ZI(NBMAMA-1+IMA)=0
         ZI(ID1-1+IMA)=0
 98   CONTINUE    

C ------ ON COMPTE LES LIENS -----------------------------------------
      
      IF (NIV .GE. 2) CALL UTTCPU(18,'DEBUT',6,TMPS)
     
      NBLIEN=0
      DO 17 IMA=1,NBMATO
         NMAX=0
         IDNOEU=ZI(IDNO-1+ZI(RENUM2-1+IMA))
         TYPMA1=ZK8(TYPMA-1+ZI(RENUM2-1+IMA))
         DO 19 INO=1,ZI(NBNO-1+ZI(RENUM2-1+IMA))
            NUMNO=ZI(IDNOEU-1+INO)
            DO 21 I=ZI(IDCOI-1+NUMNO),ZI(IDCOI-1+NUMNO+1)-1 
               MAIL=ZI(COI-1+I)               
               IF ( BORD .EQ. 'OUI     ' ) THEN
                  IF ( ZI(MABORD-1+MAIL) .NE. 0) GOTO 23
               ENDIF
               MAIL=ZI(RENUM3-1+MAIL)
               IF (MAIL .LE. IMA) GOTO 23
               DO 22 J=1,NMAX
                  IF (ZI(TEMP-1+J) .EQ. MAIL ) THEN
                     ZI(TEMP1-1+J)=ZI(TEMP1-1+J)+1
                     GOTO 23
                  ENDIF
 22            CONTINUE                    
               ZI(TEMP+NMAX)=MAIL
               ZI(TEMP1+NMAX)=1
               NMAX=NMAX+1
 23            CONTINUE
 21         CONTINUE
 19      CONTINUE      
         
         DO 24 J=1,NMAX
            TYPMA2=ZK8(TYPMA-1+ZI(RENUM2-1+ZI(TEMP-1+J)))
            IF ( ZI(TEMP1-1+J) .EQ. 1 ) THEN
               IF ( TYPMA1 .EQ. 'POI1    ') GOTO 61
               IF ( TYPMA2 .EQ. 'POI1    ') GOTO 61
               IF ( ZI(MABORD+ZI(RENUM2-1+IMA)-1) .EQ. 0) THEN
                  IF ( TYPMA1 .EQ. 'SEG2    ') GOTO 61
               ENDIF
               IF ( ZI(MABORD+ZI(RENUM2-1+ZI(TEMP-1+J)-1)) .EQ. 0) THEN
                  IF ( TYPMA2 .EQ. 'SEG2    ') GOTO 61
               ENDIF
            ELSEIF ( ZI(TEMP1-1+J) .EQ. 2 ) THEN
               IF ( TYPMA1 .EQ. 'SEG2    ') GOTO 61
               IF ( TYPMA2 .EQ. 'SEG2    ') GOTO 61
               IF ( ZI(MABORD+ZI(RENUM2-1+IMA)-1) .EQ. 0) THEN
                  IF ( TYPMA1 .EQ. 'TRIA3   ') GOTO 61
                  IF ( TYPMA1 .EQ. 'QUAD4   ') GOTO 61
               ENDIF 
               IF ( ZI(MABORD+ZI(RENUM2-1+ZI(TEMP-1+J))-1) .EQ. 0) THEN
                  IF ( TYPMA2 .EQ. 'TRIA3   ') GOTO 61
                  IF ( TYPMA2 .EQ. 'QUAD4   ') GOTO 61
               ENDIF
           ELSEIF ( ZI(TEMP1-1+J) .EQ. 3 ) THEN
               IF ( TYPMA1 .EQ. 'TRIA3   ') THEN
                  IF ( TYPMA2 .EQ. 'TETRA4  ') GOTO 61
                  IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 61
               ELSEIF ( TYPMA1 .EQ. 'TETRA4  ') THEN  
                  IF ( TYPMA2 .EQ. 'TRIA3   ') GOTO 61
                  IF ( TYPMA2 .EQ. 'TETRA4  ') GOTO 61
                  IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 61
               ELSEIF ( TYPMA1 .EQ. 'PENTA6  ') THEN
                  IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 61
                  IF ( TYPMA2 .EQ. 'TRIA3   ') GOTO 61
                  IF ( TYPMA2 .EQ. 'TETRA4  ') GOTO 61
               ENDIF
            ELSEIF ( ZI(TEMP1-1+J) .EQ. 4 ) THEN
               IF ( TYPMA1 .EQ. 'QUAD4   ') THEN
                  IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 61
                  IF ( TYPMA2 .EQ. 'HEXA8   ') GOTO 61
               ELSEIF ( TYPMA1 .EQ. 'PENTA6  ') THEN
                  IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 61
                  IF ( TYPMA2 .EQ. 'HEXA8   ') GOTO 61
               ELSEIF ( TYPMA1 .EQ. 'HEXA8   ') THEN
                  IF ( TYPMA2 .EQ. 'HEXA8   ') GOTO 61
                  IF ( TYPMA2 .EQ. 'QUAD4   ') GOTO 61
                  IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 61
               ENDIF
            ENDIF 
            
            GOTO 24
            
 61         CONTINUE
            ZI(NBMAMA-1+IMA)=ZI(NBMAMA-1+IMA)+1
            ZI(NBMAMA-1+ZI(TEMP-1+J))=ZI(NBMAMA-1+ZI(TEMP-1+J))+1
            NBLIEN=NBLIEN+2

 24      CONTINUE        
 17   CONTINUE      

      IF ( NIV .GE. 2) THEN
        WRITE(IFM,*)'NOMBRE DE LIENS :',NBLIEN
        CALL UTTCPU(18,'FIN',6,TMPS)
        WRITE(IFM,*)'NOMBRE DE LIENS :',TMPS(3)-T0
        T0=TMPS(3)
      ENDIF
      
C ------- ON RE-REMPLIT IDCO -----------------------------------------

      ZI4(IDCO)=1
      DO 99 IMA=2,NBMATO+1
        ZI4(IDCO-1+IMA)=ZI4(IDCO-1+IMA-1)+ZI(NBMAMA-1+IMA-1)
 99   CONTINUE
      
      CALL JEDETR('NBMAMA')

C ------ CREATION DES CONNECTIVITES DES MAILLES ( CO ) ---------------
      
      IF ( NIV .GE. 2) CALL UTTCPU(18,'DEBUT',6,TMPS)   
      CALL WKVECT('CO    ','V V S',NBLIEN,CO)

      DO 25 IMA=1,NBMATO
         NMAX=0
         IDNOEU=ZI(IDNO-1+ZI(RENUM2-1+IMA))
         TYPMA1=ZK8(TYPMA-1+ZI(RENUM2-1+IMA)) 
         DO 26 INO=1,ZI(NBNO-1+ZI(RENUM2-1+IMA))
            NUMNO=ZI(IDNOEU-1+INO)
            DO 27 I=ZI(IDCOI-1+NUMNO),ZI(IDCOI-1+NUMNO+1)-1
               MAIL=ZI(COI-1+I)
               IF ( BORD .EQ. 'OUI     ' ) THEN
                  IF ( ZI(MABORD-1+MAIL) .NE. 0 ) GOTO 29
               ENDIF
               MAIL=ZI(RENUM3-1+MAIL)
               IF (MAIL .LE. IMA) GOTO 29
               DO 28 J=1,NMAX
                  IF (ZI(TEMP-1+J) .EQ. MAIL ) THEN
                     ZI(TEMP1-1+J)=ZI(TEMP1-1+J)+1
                     GOTO 29
                  ENDIF
 28            CONTINUE                    
               ZI(TEMP+NMAX)=MAIL
               ZI(TEMP1+NMAX)=1
               NMAX=NMAX+1
 29            CONTINUE
 27         CONTINUE
 26      CONTINUE      
         
         DO 30 J=1,NMAX
            TYPMA2=ZK8(TYPMA-1+ZI(RENUM2-1+ZI(TEMP-1+J)))
            IF ( ZI(TEMP1-1+J) .EQ. 1 ) THEN
               IF ( TYPMA1 .EQ. 'POI1    ') GOTO 31
               IF ( TYPMA2 .EQ. 'POI1    ') GOTO 31
               IF ( ZI(MABORD+ZI(RENUM2-1+IMA)-1) .EQ. 0) THEN
                  IF ( TYPMA1 .EQ. 'SEG2    ') GOTO 31
               ENDIF
               IF ( ZI(MABORD+ZI(RENUM2-1+ZI(TEMP-1+J))-1) .EQ. 0) THEN
                  IF ( TYPMA2 .EQ. 'SEG2    ') GOTO 31
               ENDIF
            ELSEIF ( ZI(TEMP1-1+J) .EQ. 2 ) THEN
               IF ( TYPMA1 .EQ. 'SEG2    ') GOTO 31
               IF ( TYPMA2 .EQ. 'SEG2    ') GOTO 31
               IF ( ZI(MABORD+ZI(RENUM2-1+IMA)-1) .EQ. 0) THEN
                  IF ( TYPMA1 .EQ. 'TRIA3   ') GOTO 31
                  IF ( TYPMA1 .EQ. 'QUAD4   ') GOTO 31
               ENDIF 
               IF ( ZI(MABORD+ZI(RENUM2-1+ZI(TEMP-1+J))-1) .EQ. 0) THEN
                  IF ( TYPMA2 .EQ. 'TRIA3   ') GOTO 31
                  IF ( TYPMA2 .EQ. 'QUAD4   ') GOTO 31
               ENDIF
            ELSEIF ( ZI(TEMP1-1+J) .EQ. 3 ) THEN
               IF ( TYPMA1 .EQ. 'TRIA3   ') THEN
                  IF ( TYPMA2 .EQ. 'TETRA4  ') GOTO 31
                  IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 31
               ELSEIF ( TYPMA1 .EQ. 'TETRA4  ') THEN
                  IF ( TYPMA2 .EQ. 'TETRA4  ') GOTO 31
                  IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 31
                  IF ( TYPMA2 .EQ. 'TRIA3   ') GOTO 31
               ELSEIF ( TYPMA1 .EQ. 'PENTA6  ') THEN
                   IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 31
                   IF ( TYPMA2 .EQ. 'TRIA3   ') GOTO 31
                   IF ( TYPMA2 .EQ. 'TETRA4  ') GOTO 31
               ENDIF 
            ELSEIF ( ZI(TEMP1-1+J) .EQ. 4 ) THEN
               IF ( TYPMA1 .EQ. 'QUAD4   ') THEN
                  IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 31
                  IF ( TYPMA2 .EQ. 'HEXA8   ') GOTO 31
               ELSEIF ( TYPMA1 .EQ. 'PENTA6  ') THEN
                  IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 31
                  IF ( TYPMA2 .EQ. 'HEXA8   ') GOTO 31
               ELSEIF ( TYPMA1 .EQ. 'HEXA8   ') THEN
                  IF ( TYPMA2 .EQ. 'HEXA8   ') GOTO 31
                  IF ( TYPMA2 .EQ. 'PENTA6  ') GOTO 31
                  IF ( TYPMA2 .EQ. 'QUAD4   ') GOTO 31 
               ENDIF   
            ENDIF 
            
            GOTO 30
                     
 31         CONTINUE
            ID=ZI4(IDCO-1+IMA)+ZI(ID1-1+IMA)
            ID2=ZI4(IDCO-1+ZI(TEMP-1+J))+ZI(ID1-1+ZI(TEMP-1+J))
            ZI4(CO-1+ID2)=IMA
            ZI4(CO-1+ID)=ZI(TEMP-1+J)
            ZI(ID1-1+IMA)=ZI(ID1-1+IMA)+1
            ZI(ID1-1+ZI(TEMP-1+J))=ZI(ID1-1+ZI(TEMP-1+J))+1
 
 30      CONTINUE        
 25   CONTINUE 
 
      IF (NIV .GE. 2) THEN
         CALL UTTCPU(18,'FIN',6,TMPS)
         WRITE(IFM,*)'CONNECTIVITE :',TMPS(3)-T0
         T0=TMPS(3)
      ENDIF

      IF (NIV .GE. 2) CALL UTTCPU(18,'DEBUT',6,TMPS)
      
C ------------------------ JEVEUX ------------------------------------

      CALL JEDETR('NBNO  ')
      CALL JEDETR('IDNO  ')
      CALL JEDETR('TYPMA ')
      CALL JEDETR('IDCOI ')  
      CALL JEDETR('ID1   ')
      CALL JEDETR('COI   ')
      CALL JEDETR('TEMP  ')
      CALL JEDETR('TEMP1 ')

C ------- ON RECUPERE LE NOMBRE DE SOUS DOMAINES ---------------------
 
       CALL GETVIS(' ','NB_PART',0,1,1,NBPART,ERR)
 
C ------- ON RECUPERE LE PARTITIONNEUR -------------------------------
 
      CALL GETVTX('        ','METHODE' ,0,1,1,METH,ERR)
      IF (NIV .GE. 2) THEN
        WRITE(IFM,*)' ** METHODE DE PARTIONNEMENT : ',METH  
      ENDIF
       
C ********************************************************************
C                       UTILISATION DE CONTRAINTES
C ********************************************************************

      CALL WKVECT('VELO  ','V V S',NBMATO,VELO)
      CALL WKVECT('EDLO  ','V V S',NBLIEN,EDLO)
      DO 114 I=1,NBMATO
        ZI4(VELO-1+I)=1
 114  CONTINUE  
      DO 115 I=1,NBLIEN
        ZI4(EDLO-1+I)=1
 115  CONTINUE

      CALL GETFAC('GROUPAGE',NOCC)
      DO 116 IOCC=1,NOCC
        CALL GETVID('GROUPAGE','GROUP_MA',IOCC,1,1,GRPEMA,ERR)
        CALL JEEXIN(GRPEMA,ERR)
        IF ( ERR .NE. 0 ) THEN
          WRITE(IFM,*)'PAS DE COLLECTION GROUPE DE MAILLES'
        ELSE
          CALL JELIRA(MA//'.GROUPEMA','NMAXOC',NBRE,K8BID)
          DO 118 J=1,NBRE
            CALL JENUNO(JEXNUM(MA//'.GROUPEMA',J),NOM)
            IF (NOM .EQ. GRPEMA) THEN
              CALL JELIRA(JEXNUM(MA//'.GROUPEMA',J),'LONMAX',NBMA,K8BID)
              CALL JEVEUO(JEXNUM(MA//'.GROUPEMA',J),'L',IDMA)
            ENDIF   
 118      CONTINUE 
        ENDIF
        WRITE(IFM,*)'  - GROUPAGE  :',GRPEMA
        WRITE(IFM,*)' '
        DO 204 IMA=1,NBMA-1
          MAIL=ZI(RENUM3-1+ZI(RENUM1-1+ZI(IDMA-1+IMA)))
          DO 212 I=IMA+1,NBMA
            MAIL2=ZI(RENUM3-1+ZI(RENUM1-1+ZI(IDMA-1+I)))
            DO 205 J=ZI4(IDCO-1+MAIL),ZI4(IDCO-1+MAIL+1)-1
              IF ( ZI4(CO-1+J) .EQ. MAIL2 ) ZI4(EDLO-1+J)=NBMATO+1
 205        CONTINUE
            DO 220 J=ZI4(IDCO-1+MAIL2),ZI4(IDCO-1+MAIL2+1)-1
              IF ( ZI4(CO-1+J) .EQ. MAIL ) ZI4(EDLO-1+J)=NBMATO+1
 220        CONTINUE
 212      CONTINUE
 204    CONTINUE 
 116  CONTINUE     

      CALL GETFAC('POIDS_MAILLES',NOCC)
      DO 117 IOCC=1,NOCC
        CALL GETVID('POIDS_MAILLES','GROUP_MA',IOCC,1,1,GRPEMA,ERR)
        CALL JEEXIN(GRPEMA,ERR)
        IF ( ERR .NE. 0 ) THEN
          WRITE(IFM,*)'PAS DE COLLECTION GROUPE DE MAILLES'
        ELSE
          CALL JELIRA(MA//'.GROUPEMA','NMAXOC',NBRE,K8BID)
          DO 207 J=1,NBRE
            CALL JENUNO(JEXNUM(MA//'.GROUPEMA',J),NOM)
            IF (NOM .EQ. GRPEMA) THEN
              CALL JELIRA(JEXNUM(MA//'.GROUPEMA',J),'LONMAX',NBMA,K8BID)
              CALL JEVEUO(JEXNUM(MA//'.GROUPEMA',J),'L',IDMA)
            ENDIF   
 207      CONTINUE 
        ENDIF        
        CALL GETVIS('POIDS_MAILLES','POIDS',IOCC,1,1,POIDS,ERR)
        WRITE(IFM,*)'  - POIDS_MAILLES :',GRPEMA 
        WRITE(IFM,*)'       AVEC UN POIDS DE : ',POIDS
        WRITE(IFM,*) ' '
        DO 203 IMA=1,NBMA
         MAIL=ZI(RENUM3-1+ZI(RENUM1-1+ZI(IDMA-1+IMA)))
         ZI4(VELO-1+MAIL)=POIDS
 203    CONTINUE
 117  CONTINUE     

C ------- ON IMPRIME LE GRAPH POUR METIS -----------------------------
     
      IF ( METH .NE. 'SCOTCH  ') THEN
        IULM1 = ULNUME ()
        IF ( IULM1 .EQ. -1 ) THEN
          CALL UTMESS('F','FETSKP',' ERREUR A L''APPEL DE METIS '//
     &                 'PLUS AUCUNE UNITE LOGIQUE LIBRE !')     
        ENDIF 
        CALL ULOPEN ( IULM1,' ', ' ', 'NEW', 'O' )
        WRITE(IULM1,'(I12,I12,I3)')NBMATO,NBLIEN/2,11
        DO 71 IMA=1,NBMATO
          WRITE(IULM1,'(50I8)')ZI4(VELO-1+IMA),
     &                         (ZI4(CO-1+I),ZI4(EDLO-1+I),
     &                         I=ZI4(IDCO-1+IMA),ZI4(IDCO-1+IMA+1)-1)
 71     CONTINUE     
        CALL ULOPEN (-IULM1,' ',' ',' ',' ')
      ENDIF
      
      IF ( NIV .GE. 2 ) THEN
         CALL UTTCPU(18,'FIN',6,TMPS)
         WRITE(IFM,*)' -- ECRITURE DU GRAPHE : ',TMPS(3)
      ENDIF

      CALL JEDETR('RENUM3')
      CALL JEDETR('RENUM1')

C ********************************************************************
C                       LANCEMENT DU LOGICIEL
C ********************************************************************

C          
C     ************** LANCEMENT DE SCOTCH
C
      IF ( METH .EQ. 'SCOTCH  ') THEN
        IF ( NIV .GE. 2 ) THEN 
          CALL UTTCPU(18,'INIT',6,TMPS)
          CALL UTTCPU(18,'DEBUT',6,TMPS)
        ENDIF
        WRITE(IFM,*) ' '
        WRITE(IFM,*) '***************** SCOTCH *****************'
        WRITE(IFM,*) ' '
        WRITE(IFM,*) ' ' 
        WRITE(IFM,*) ' * LE NOMBRE DE MAILLES    :',NBMATO
        WRITE(IFM,*) ' * LE NOMBRE DE CONNEXIONS :',NBLIEN
        WRITE(IFM,*) ' '
        WRITE(IFM,*) ' '
        CALL WKVECT('NMAP  ','V V S',NBMATO,NMAP)
        CALL FETSCO (NBMATO,NBLIEN,ZI4(CO),ZI4(IDCO),NBPART,ZI4(NMAP),
     &                ZI4(EDLO),ZI4(VELO),ERR)
C        IF ( ERR .NE. 0 ) THEN
C            CALL UTMESS('F','FETSKP','ERREUR DANS '//
C     &                            'L EXECUTION DE SCOTCH')
C        ENDIF 
        IF ( NIV .GE. 2 ) THEN
          CALL UTTCPU(18,'FIN',6,TMPS)
          WRITE(IFM,*) ' * TEMPS DE PARTITIONNEMENT  :',TMPS(3)
        ENDIF
        WRITE(IFM,*) '*************** FIN SCOTCH ***************'
        WRITE(IFM,*) ' '
C          
C     ************** LANCEMENT DE METIS
C
      ELSE 
        CALL WKVECT('JNOM  ','V V K80',3,JNOM)
        WRITE(KTMP,'(I4)') NBPART
        WRITE(KTMP2,'(I4)') IULM1
        CALL LXCADR(KTMP)
        CALL LXCADR(KTMP2)
        ZK80(JNOM+1)='fort.'//KTMP2
        ZK80(JNOM+2)=KTMP
        CALL GETVTX('        ','LOGICIEL' ,0,1,1,REP,ERR)
        IF ( ERR .EQ. 0 ) THEN
          CALL REPOUT (1,LREP,REP)
          IF ( METH .EQ. 'PMETIS  ') THEN
            ZK80(JNOM)=REP(1:LREP)//'pmetis'
          ELSEIF ( METH .EQ. 'KMETIS  ') THEN
            ZK80(JNOM)=REP(1:LREP)//'kmetis'
          ENDIF
        ELSE
          LREP=0
          DO 77 I=1,LEN(REP)
            IF (REP(I:I).NE.' ') LREP=LREP+1
 77       CONTINUE       
          ZK80(JNOM)=REP(1:LREP)
        ENDIF        
        CALL APLEXT(NIV,3,ZK80(JNOM),ERR) 
C        IF ( ERR .NE. 0 ) THEN
C          CALL UTMESS('F','FETSKP',' ERREUR A L''APPEL DE METIS')     
C        ENDIF    
        CALL JEDETR('JNOM  ')     
      ENDIF

      CALL JEDETR('EDLO  ')
      CALL JEDETR('VELO  ')
       
C ********************************************************************
C                    LECTURE DU FICHIER DU PARTITIONNEUR
C ********************************************************************
      
      IF (NIV .GE. 2) THEN
        CALL UTTCPU(18,'INIT',6,TMPS)
        CALL UTTCPU(18,'DEBUT',6,TMPS)
      ENDIF  

C ------------------------ JEVEUX ------------------------------------
      SDB=0
      IF ( BORD .EQ. 'OUI     ') THEN
         CALL GETVTX('        ','NOM_GROUP_MA_BORD',0,1,1,SDBORD,ERR)
         IF ( ERR .NE. 0) THEN 
            NBPART=2*NBPART
            SDB=1
         ENDIF   
      ENDIF
         
      CALL WKVECT('NUMSDM','V V I',NBMABO,NUMSDM)
      CALL WKVECT('NBMASD','V V I',NBPART,NBMASD)
      DO 34 ISD=1,NBPART
         ZI(NBMASD-1+ISD)=0
 34   CONTINUE        

C ------- LECTURE ----------------------------------------------------

      IF ( METH .NE. 'SCOTCH  ') THEN
        IULM2 = ULNUME ()
        IF ( IULM2 .EQ. -1 ) THEN
          CALL UTMESS('F','FETSKP',' ERREUR A L''APPEL DE METIS '//
     &                 'PLUS AUCUNE UNITE LOGIQUE LIBRE !')     
        ENDIF 
        CALL WKVECT('JNOM  ','V V K80',1,JNOM)
        LREP=0
        DO 177 I=1,LEN(KTMP2)
          IF (KTMP2(I:I).NE.' ') LREP=LREP+1
 177    CONTINUE
        ZK80(JNOM)='fort.'//KTMP2(1:LREP)//'.part.'//KTMP
        CALL ULOPEN ( IULM2,ZK80(JNOM),' ', 'OLD', 'O' )
        DO 35 IMA=1,NBMATO  
          READ(IULM2,'(I4)')ZI(NUMSDM-1+ZI(RENUM2-1+IMA))
          ZI(NBMASD+ZI(NUMSDM-1+ZI(RENUM2-1+IMA)))=
     &                     ZI(NBMASD+ZI(NUMSDM-1+ZI(RENUM2-1+IMA)))+1
 35     CONTINUE
        CALL ULOPEN (-IULM2,' ',' ',' ',' ')
        CALL JEDETR('JNOM  ')
      ELSE 
        DO 135 IMA=1,NBMATO  
          ZI(NUMSDM-1+ZI(RENUM2-1+IMA))=ZI4(NMAP-1+IMA)
          ZI(NBMASD+ZI(NUMSDM-1+ZI(RENUM2-1+IMA)))=
     &                     ZI(NBMASD+ZI(NUMSDM-1+ZI(RENUM2-1+IMA)))+1
 135    CONTINUE                
        CALL JEDETR('NMAP  ')
      ENDIF
      
C ------- ON REMET LES MAILLES DE BORDS ------------------------------

      IF ( BORD .EQ. 'OUI     ') THEN
         IF ( SDB .EQ. 0) THEN
            DO 55 IMA=1,NBBORD
               MAIL=ZI(RENUM2-1+NBMATO+IMA)
               IF ( ZI(MABORD+ZI(MABORD-1+MAIL)-1) .NE. 0 ) THEN
                  ZI(MABORD-1+MAIL)=ZI(MABORD+ZI(MABORD-1+MAIL)-1)
               ENDIF 
              
               ZI(NUMSDM-1+MAIL)=ZI(NUMSDM-1+ZI(MABORD-1+MAIL))
               ZI(NBMASD+ZI(NUMSDM-1+ZI(MABORD-1+MAIL)))=
     &               ZI(NBMASD+ZI(NUMSDM-1+ZI(MABORD-1+MAIL)))+1
 55         CONTINUE 
         ELSE
            DO 45 IMA=1,NBBORD
               MAIL=ZI(RENUM2-1+NBMATO+IMA)
               IF ( ZI(MABORD+ZI(MABORD-1+MAIL)-1) .NE. 0 ) THEN
                  ZI(MABORD-1+MAIL)=ZI(MABORD+ZI(MABORD-1+MAIL)-1)
               ENDIF 
              
               ZI(NUMSDM-1+MAIL)=ZI(NUMSDM-1+ZI(MABORD-1+MAIL))+NBPART/2
               ZI(NBMASD+NBPART/2+ZI(NUMSDM-1+ZI(MABORD-1+MAIL)))=
     &            ZI(NBMASD+NBPART/2+ZI(NUMSDM-1+ZI(MABORD-1+MAIL)))+1
 45         CONTINUE
         ENDIF
      ENDIF
        
      CALL JEDETR('RENUM2')
      CALL JEDETR('MABORD')
      
      IF (NIV.GE.2) THEN 
        CALL UTTCPU(18,'FIN',6,TMPS)
        WRITE(IFM,*)'--- LECTURE DU FICHIER DU PARTITIONNEUR :',TMPS(3)
      ENDIF

C ********************************************************************
C                      VERIFICATION DE LA CONNEXITE
C ********************************************************************
    
      IF (NIV.GE.2) THEN 
        CALL UTTCPU(18,'INIT',6,TMPS)
        CALL UTTCPU(18,'DEBUT',6,TMPS)
      ENDIF

      CALL GETVTX(' ','VERI_CONNEX',0,1,1,VERIF,ERR)
      
      IF (VERIF .EQ. 'OUI     ') THEN
        CALL WKVECT('FLAGMA','V V I',NBMATO,FLAGMA)
        CALL WKVECT('FLAGSD','V V I',NBMATO,FLAGSD)
        CALL WKVECT('LISTE ','V V I',NBMATO,LISTE)
        DO 7 IMA=1,NBMATO
          ZI(FLAGMA-1+IMA)=0
          ZI(FLAGSD-1+IMA)=0
 7      CONTINUE
 
        DO 110 IMA=1,NBMATO
          IF ( ZI(FLAGMA-1+IMA) .EQ. 0 ) THEN
            IF ( ZI(FLAGSD+ZI(NUMSDM-1+IMA)) .EQ. 0 ) THEN
              ZI(FLAGMA-1+IMA)=1
              ZI(FLAGSD+ZI(NUMSDM-1+IMA))=1
              ZI(LISTE)= IMA
              ID=0
              PTR=0
 113          CONTINUE
              DO 112 I=ZI4(IDCO-1+ZI(LISTE+ID)),
     &                      ZI4(IDCO-1+ZI(LISTE+ID)+1)-1
                IF (ZI(FLAGMA-1+ZI4(CO-1+I)).EQ.0) THEN
                  IF(ZI(NUMSDM-1+ZI4(CO-1+I)).EQ.ZI(NUMSDM-1+IMA))THEN
                    PTR=PTR+1
                    ZI(LISTE+PTR)=ZI4(CO-1+I)
                    ZI(FLAGMA-1+ZI4(CO-1+I))=1
                  ENDIF   
                ENDIF
 112          CONTINUE
              ID=ID+1
              IF( ID .LE. PTR ) GOTO 113
            ELSE
              CALL UTMESS('F','FETSKP',' PROBLEME CONNEXITE :'//
     &                 'UN SOUS DOMAINE EST NON CONNEXE')     
            ENDIF                             
          ENDIF
 110    CONTINUE 
        CALL JEDETR('LISTE ')
        CALL JEDETR('FLAGMA')
        CALL JEDETR('FLAGSD')
       ENDIF
      
      CALL JEDETR('CO    ')
      CALL JEDETR('IDCO  ')
           
      IF (NIV.GE.2) THEN 
        CALL UTTCPU(18,'FIN',6,TMPS)
        WRITE(IFM,*)'--- VERIFICATION DE LA CONNEXITE :',TMPS(3)
      ENDIF
     
C ********************************************************************
C                      CREATION DES GROUP_MA
C ********************************************************************
      
      NBMATO=NBMABO
      
      IF (NIV.GE.2) THEN
        CALL UTTCPU(18,'INIT',6,TMPS)
        CALL UTTCPU(18,'DEBUT',6,TMPS)
      ENDIF
      
C ------------------------ JEVEUX ------------------------------------
      
      CALL WKVECT('IDMASD','V V I',NBPART+1,IDMASD)
      CALL WKVECT('NOMSDM','V V K8',NBPART,NOMSDM)
      CALL WKVECT('MASD  ','V V I',NBMATO,MASD)
      CALL WKVECT('ID1   ','V V I',NBPART,ID1)
      DO 37 ISD=1,NBPART
         ZI(ID1-1+ISD)=0
 37   CONTINUE
 
C ------- On RECUPERE LE NOM DES SOUS DOMAINES -----------------------
      
      CALL GETVTX(' ','NOM_GROUP_MA' ,0,1,1,NOM,ERR)
      MAXI=0
      DO 50 I = 1,LEN(NOM)
        IF ( NOM(I:I) .NE. ' ' ) MAXI=MAXI+1
 50   CONTINUE
      
      DO 51 ISD = 1, NBPART
        WRITE(KTMP,'(I4)')ISD-1
        CALL LXCADR(KTMP)
        ZK8(NOMSDM-1+ISD) = NOM(1:MAXI)//KTMP
 51   CONTINUE
       
      IF ( SDB .EQ. 1) THEN
            MAXI=0
            DO 47 I = 1,LEN(SDBORD)
               IF ( SDBORD(I:I) .NE. ' ' ) MAXI=MAXI+1
 47         CONTINUE
            DO 66 ISD = NBPART/2+1, NBPART
               WRITE(KTMP,'(I4)')ISD-NBPART/2-1
               CALL LXCADR(KTMP)
               ZK8(NOMSDM-1+ISD) = SDBORD(1:MAXI)//KTMP
 66         CONTINUE            
      ENDIF
      
      IF ( NIV .GE. 2 ) THEN
         DO 56 I=1,NBPART  
            WRITE(IFM,*)'LE SOUS DOMAINE ',ZK8(NOMSDM-1+I),' CONTIENT '
     &                     ,ZI(NBMASD-1+I),' MAILLES '
 56      CONTINUE    
      ENDIF
               
C ------- CREATION DU TABLEAU DES PLACES DES GRPMA -------------------
      
      ZI(IDMASD)=1
      DO 32 ISD=2,NBPART+1
       ZI(IDMASD-1+ISD)=ZI(IDMASD-1+ISD-1)+ZI(NBMASD-1+ISD-1)
 32   CONTINUE

C ------- CREATION DU TABLEAU DONNANT LES MAILLES POUR CHAQUE GRMPA --
            
      DO 36 IMA=1,NBMATO
        NBRE=ZI(NUMSDM-1+IMA)+1
        ZI(MASD-1+ZI(IDMASD-1+NBRE)+ZI(ID1-1+NBRE))=ZI(RENUM-1+IMA)
        ZI(ID1-1+NBRE)=ZI(ID1-1+NBRE)+1
 36   CONTINUE
      
C ------- ON AGRANDIT LA COLLECTION GROUPE-MAILLE --------------------

      CALL JELIRA(MA//'.GROUPEMA','NMAXOC',NBGRMN,K8BID)

      GRPMA  = MA//'.GROUPEMA       '
      GRPMAV = '&&OP0029'//'.GROUPEMA       '
      CALL JEEXIN(GRPMA,ERR)
      IF ( ERR .EQ. 0  .AND.  NBPART .NE. 0 ) THEN
        CALL JECREC(GRPMA,'G V I','NOM','DISPERSE','VARIABLE',NBPART)
      ELSEIF ( ERR .EQ. 0  .AND.  NBPART .EQ. 0 ) THEN
      ELSE
         CALL JELIRA(GRPMA,'NOMUTI',NBGMA,K8BID)
         NBGRMN = NBGMA + NBPART
         CALL JEDUPO( GRPMA, 'V', GRPMAV, .FALSE. )
         CALL JEDETR ( GRPMA )
         CALL JECREC(GRPMA,'G V I','NOM','DISPERSE','VARIABLE',NBGRMN)
         DO 100 I = 1 , NBGMA
            CALL JENUNO(JEXNUM(GRPMAV,I),NOM)
            CALL JECROC(JEXNOM(GRPMA,NOM))
            CALL JEVEUO(JEXNUM(GRPMAV,I),'L',JVG)
            CALL JELIRA(JEXNUM(GRPMAV,I),'LONMAX',NBMA,K8BID)
            CALL JEECRA(JEXNOM(GRPMA,NOM),'LONMAX',NBMA,' ')
            CALL JEVEUO(JEXNOM(GRPMA,NOM),'E',JGG)
            DO 102 J = 0 , NBMA-1
               ZI(JGG+J) = ZI(JVG+J)
 102        CONTINUE
 100     CONTINUE
      ENDIF
      
C ------- ON RAJOUTE LES SD DANS LES GROUPES DE MAILLES --------------
         
      DO 38 ISD=1,NBPART
        NOGMA=ZK8(NOMSDM-1+ISD)
        NBMA=ZI(NBMASD-1+ISD)
        IF ( NBMA .NE. 0 )THEN
          CALL JECROC(JEXNOM(MA//'.GROUPEMA',NOGMA))
          CALL JEECRA(JEXNOM(MA//'.GROUPEMA',NOGMA),'LONMAX',NBMA,K8BID)
          CALL JEVEUO(JEXNOM(MA//'.GROUPEMA',NOGMA),'E',IAGMA)
          DO 41 IMA = 0,NBMA - 1
            ZI(IAGMA+IMA) = ZI(MASD+ZI(IDMASD-1+ISD)-1+IMA) 
 41       CONTINUE
        ENDIF   
 38   CONTINUE

C ------- DESTRUCTION DES TABLEAUX RESTANTS -------

      CALL JEDETR('IDMASD')
      CALL JEDETR('NOMSDM')
      CALL JEDETR('MASD  ')
      CALL JEDETR('ID1   ')
      CALL JEDETR('NBMASD')
      CALL JEDETR('NUMSDM')
      CALL JEDETR('RENUM ')
      
      IF ( NIV .GE. 2 ) THEN
        CALL UTTCPU(18,'FIN',6,TMPS)
        WRITE(IFM,*)'--- CREATION DES GRPMA :',TMPS(3)
      ENDIF
      
      CALL JEDEMA()
      END
