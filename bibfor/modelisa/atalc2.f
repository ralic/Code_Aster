      SUBROUTINE ATALC2(AZ,NUMEDZ,RTBLOC,ATAZ,BASEZ,NBLIG)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 02/11/2004   AUTEUR MABBAS M.ABBAS 
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
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)

C     ATALC2  --  LE BUT DE CETTE ROUTINE EST DE CREER LA MATR_ASSE
C                 DE NOM ATA.
C                 LE .VALE DE CETTE MATR_ASSE VA CONTENIR LES TERMES
C                 DU PRODUIT DE MATRICES AT*A.
C                 A EST 'CONCEPTUELLEMENT' UNE MATRICE RECTANGULAIRE
C                 DE 'HAUTEUR' P ET DE LARGEUR NEQ.
C                 A EST 'INFORMATIQUEMENT' UNE COLLECTION NUMEROTEE
C                 COMPORTANT P OBJETS QUI SONT DES VECTEURS DE REELS
C                 DE LONGUEUR NEQ.
C                 CHACUN DE CES VECTEURS EST UNE LIGNE DE LA MATRICE A.
C                 LA MATR_ASSE ATA VA DONC ETRE SYMETRIQUE ET A
C                 VALEURS REELLES. D'AUTRE PART ON VA LUI AFFECTER
C                 UN PROFIL EN LIGNE DE CIEL.

C   ARGUMENT        E/S  TYPE         ROLE
C    AZ              IN    K*     NOM DE LA COLLECTION DES VECTEURS
C                                 LIGNES (I.E. AZ EST LA MATRICE
C                                 RECTANGULAIRE POUR LAQUELLE ON VA
C                                 CALCULER LE PRODUIT AZ_T*AZ).
C    NUMEDZ         IN    K*      NOM DU NUME_DDL DECRIVANT LES
C                                 LIGNES DE LA MATRICE AZ
C    RTBLOC          IN    R8     TAILLE DES BLOCS DE LA MATR_ASSE.
C    ATAZ           OUT    K*     NOM DE LA MATR_ASSE SYMETRIQUE
C                                 A VALEURS REELLES DONT LE .VALE
C                                 CONTIENT LE PRODUIT AT*A.
C                                 LE PROFIL DE CETTE MATRICE EST
C                                 EN LIGNE DE CIEL.
C                                 CE PROFIL EST DETERMINE DANS LA
C                                 ROUTINE.
C    BASEZ           IN    K*     NOM DE LA BASE SUR LAQUELLE ON
C                                 CREE LA MATR_ASSE.
C.========================= DEBUT DES DECLARATIONS ====================
C ----- COMMUNS NORMALISES  JEVEUX
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8,MA
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C -----  ARGUMENTS
      CHARACTER*(*) AZ,NUMEDZ,ATAZ,BASEZ
C -----  VARIABLES LOCALES
      INTEGER IL1,IL2, COMPT, COMPT2
      CHARACTER*1 K1BID,BASE
      CHARACTER*14 NUMDDL,NUMEDD
      CHARACTER*19 PFCHNO,ATA
      CHARACTER*24 KHCOL,KADIA,KABLO,KIABL,KDESC,KREFA,KCONL,KVALE,
     +             KREFE,A

C.========================= DEBUT DU CODE EXECUTABLE ==================

      CALL JEMARQ()

C --- INITIALISATIONS :
C     ---------------
C
      BASE = BASEZ
      A = AZ
      ATA = ATAZ
      NUMEDD = NUMEDZ
      PFCHNO = NUMEDD//'.NUME'
      CALL GCNCON ( '_' , NUMDDL )
      CALL DETRSD('NUME_DDL',NUMDDL)
      CALL DETRSD('MATR_ASSE',ATA)

      UN = 1.0D0
      ZERO = 0.D0

C --- RECUPERATION DU NOMBRE D'OBJETS DE LA COLLECTION A (I.E.
C --- RECUPERATION DU NOMBRE DE VECTEURS LIGNES DE LA MATRICE
C --- RECTANGULAIRE A) :
C     ----------------
C      CALL JELIRA(A,'NMAXOC',NBLIG,K1BID)

C --- RECUPERATION DE LA LONGUEUR D'UN VECTEUR DE LA COLLECTION A
C --- (I.E. DU NOMBRE DE LIGNES DE LA MATRICE RECTANGULAIRE A) :
C     -------------------------------------------------------
      CALL JELIRA(A,'LONMAX',NEQ,K1BID)

C --- AFFECTATION DU PROF_CHNO DES CHAMNOS DONT LES .VALE SONT
C --- LES COLONNES DE LA MATRICE A, A CETTE MATRICE :
C     ---------------------------------------------
      CALL JEDUPO(PFCHNO//'.DEEQ',BASE,NUMDDL//'.NUME.DEEQ',.FALSE.)
      CALL JEDUPO(PFCHNO//'.DELG',BASE,NUMDDL//'.NUME.DELG',.FALSE.)
      CALL JEDUPO(PFCHNO//'.LILI',BASE,NUMDDL//'.NUME.LILI',.FALSE.)
      CALL JEDUPO(PFCHNO//'.LPRN',BASE,NUMDDL//'.NUME.LPRN',.FALSE.)
      CALL JEDUPO(PFCHNO//'.NUEQ',BASE,NUMDDL//'.NUME.NUEQ',.FALSE.)
      CALL JEDUPO(PFCHNO//'.PRNO',BASE,NUMDDL//'.NUME.PRNO',.TRUE.)
      CALL JEDUPO(PFCHNO//'.REFN',BASE,NUMDDL//'.NUME.REFN',.FALSE.)

C --- STOCKAGE DE LA COLLECTION A DANS UN VECTEUR DE TRAVAIL
C     (STOCKAGE EN LIGNE)
C     ---------------------------------------------------
C
      CALL WKVECT('&&ATALC2.A','V V R',NEQ*NBLIG,IDA)
      K = 1
      DO 10 I = 1, NBLIG
        CALL JEVEUO(JEXNUM(A,I),'L',IDLIGM)
        DO 20 J = 1,NEQ
          ZR(IDA+K-1) = ZR(IDLIGM+J-1)                    
          K = K + 1 
 20     CONTINUE 
        CALL JELIBE(JEXNUM(A,I))         
 10   CONTINUE
C     
C     INDCOL EST LE NOMBRE DE TERMES NON NULS PAR COLONNE DE A
C     IPREM EST L'INDICE DE LA PREMIERE COLONNE NON NULLE DE A
C
      CALL WKVECT('&&ATALC2.COL','V V R',NEQ*NBLIG,IDCOL)
      CALL WKVECT('&&ATALC2.TRV2','V V I',NEQ*NBLIG,IMCOL)      
      CALL WKVECT('&&ATALC2.ICOL','V V I',NEQ,ID2)    
      K = 1
      IDEC = 0
      DO 30 I = 1,NEQ
        INDCOL = 0
        DO 40 M = 1,NBLIG
           IF(ZR(IDA + (I-1)+NEQ*(M-1)).NE.ZERO) THEN 
             ZR(IDCOL + K -1) = ZR(IDA +(I-1)+NEQ*(M-1))
             ZI(IMCOL + K -1) = M
             K = K + 1
             INDCOL = INDCOL + 1        
           ENDIF     
   40   CONTINUE      
        IF (INDCOL .NE. 0 .AND. IDEC .EQ. 0) THEN
           IPREM = I
           IDEC = 1
        ENDIF
        ZI(ID2+I-1) = INDCOL
   30 CONTINUE                                    
C
C
C --- CREATION ET AFFECTATION DU TABLEAU .HCOL  
C     ========================================
      KHCOL = NUMDDL//'.SLCS.HCOL'
      CALL WKVECT(KHCOL,BASE//' V I',NEQ,IHCOL)
C 
C     TRAITEMENT DE LA PREMIERE COLONNE DE A
      K1 = 1              
      ZI(IHCOL +K1 -1) = 1
      K1 = K1 + 1
C
C     INDIC EST LE NOMBRE DE TERMES STOCKES PAR LIGNE DE ATA
C     ITOT EST LE NOMBRE TOTAL DE TERMES STOCKES    
      ICUM = 0
      ICUM = ZI(ID2 +1 -1) 
      ITOT = 1
      DO 50 I = 2 , NEQ
        INDIC = 0
        IREP = 0
        IF(ZI(ID2 +I -1).EQ. 0) THEN
           ZI(IHCOL +K1 -1) = 1
           K1 = K1 + 1
           ITOT = ITOT + 1
        ELSE           
           DO 60 J = IPREM , I 
              IBORJ = ZI(ID2 +J -1)
              IF(IBORJ .EQ. 0 .AND. IREP .EQ. 0) THEN
                  CONTINUE
              ELSE IF (IBORJ .EQ. 0 .AND. IREP .NE. 0) THEN
                 INDIC = INDIC + 1
              ELSE    
                 DO 70 L = 1 , ZI(ID2 +I -1)
                    L1 = ZI(IMCOL + ICUM + L -1)
                    IF((ZR(IDA+(L1-1)*NEQ+(J-1))).EQ.0) THEN
                      CONTINUE
                    ELSE                
                      IREP = 1         
                    ENDIF
  70             CONTINUE         
                 IF(IREP .NE. 0) THEN
                    INDIC = INDIC + 1
                 ENDIF   
              ENDIF              
  60       CONTINUE 
           ZI(IHCOL +K1 -1) = INDIC
           K1 = K1 + 1
           ITOT = ITOT +INDIC     
        ENDIF
        ICUM = ICUM + ZI(ID2 +I -1)      
  50  CONTINUE                                
C
C --- TAILLE D'UN BLOC :
C     ----------------
      ITBLOC = NINT(RTBLOC)*1024

C --- CREATION ET AFFECTATION DU TABLEAU .ADIA DES POSITIONS
C --- DES TERMES DIAGONAUX DANS LES BLOCS :
C     ===================================
      KADIA = NUMDDL//'.SLCS.ADIA'

      CALL WKVECT(KADIA,BASE//' V I',NEQ,IDADIA)
C
      NBLC = 1
      ZI(IDADIA) = ZI(IHCOL)
      NTBLC = ZI(IHCOL)

      DO 80 IEQUA = 2,NEQ
        NTBLC = NTBLC + ZI(IHCOL+IEQUA-1)
        IF (NTBLC.LE.ITBLOC) THEN
          ZI(IDADIA+IEQUA-1) = NTBLC
        ELSE
          NTBLC = ZI(IHCOL+IEQUA-1)
          ZI(IDADIA+IEQUA-1) = NTBLC
          NBLC = NBLC + 1
        END IF
   80 CONTINUE
C    
C
C --- CREATION DU TABLEAU DE TRAVAIL .VALE
C     ------------------------------------
      ITAIL = ITOT
      CALL WKVECT('&&ATALC2.VALE','V V R',ITAIL,IVALE)
      K = 1
      SOM = ZERO 
      IF (ZI(ID2 +1 -1) .EQ. 0) THEN
         SOM = ZERO
         ICUM = 0
      ELSE           
         DO 90 L =1,ZI(ID2 +1 -1) 
            SOM = SOM + ZR(IDCOL +L -1)*ZR(IDCOL +L -1) 
  90     CONTINUE
         ICUM = ZI(ID2 +1 -1) 
      ENDIF
      ZR(IVALE +K -1) = SOM
C
      K = K + 1
C      ICUM = 0
      DO 100 I = 2 , NEQ
        INDIC = 0
        IREP = 0
        IF(ZI(ID2 +I -1).EQ. 0) THEN
           ZR(IVALE +K -1) = ZERO
           K = K + 1
        ELSE           
           DO 110 J = IPREM , I 
              SOM = ZERO
              IBORJ = ZI(ID2 +J -1)
              IF(IBORJ .EQ. 0 .AND. IREP .EQ. 0) THEN
                 CONTINUE
              ELSE IF (IBORJ .EQ. 0 .AND. IREP .NE. 0) THEN 
                 SOM = ZERO
                 ZR(IVALE +K -1) = SOM 
                 K = K + 1                                    
              ELSE    
                 DO 120 L = 1 , ZI(ID2 +I -1)
                    L1 = ZI(IMCOL + ICUM + L -1)
                    IF((ZR(IDA+(L1-1)*NEQ+(J-1))).EQ.0) THEN
                      CONTINUE
                    ELSE                
                      SOM = SOM + ZR(IDA +(L1-1)*NEQ + (J-1))*
     +                            ZR(IDA +(L1-1)*NEQ + (I-1))
                      IREP = 1         
                    ENDIF
  120             CONTINUE         
                 IF(IREP .NE. 0) THEN
                    ZR(IVALE +K -1) = SOM 
                    K = K + 1
                 ENDIF   
              ENDIF              
  110       CONTINUE
        ENDIF
        ICUM = ICUM + ZI(ID2 +I -1)      
  100  CONTINUE 
C
C --- NBLC EST LE NOMBRE DE BLOCS DE LA MATRICE
C
C --- CREATION ET AFFECTATION DU TABLEAU .ABLO DES NUMEROS
C --- DES LIGNES DE DEBUT ET DE FIN DE BLOC :
C     ====================================
      KABLO = NUMDDL//'.SLCS.ABLO'
      CALL WKVECT(KABLO,BASE//' V I',NBLC+1,IDABLO)
C
      ZI(IDABLO) = 0
      IBLC = 1
      NTBLC = ZI(IHCOL)
C
      DO 130 IEQUA = 2,NEQ
        NTBLC = NTBLC + ZI(IHCOL+IEQUA-1)
        IF (NTBLC.GT.ITBLOC) THEN
          NTBLC = ZI(IHCOL+IEQUA-1)
          ZI(IDABLO+IBLC) = IEQUA - 1
          IBLC = IBLC + 1
        END IF
  130 CONTINUE
C
      ZI(IDABLO+NBLC) = NEQ
      IF (NBLC.EQ.1) THEN
        ITBLOC = NTBLC
      END IF
C
C --- CREATION ET AFFECTATION DU TABLEAU .IABL DONNANT POUR
C --- CHAQUE LIGNE DE LA MATRICE LE BLOC AUQUEL ELLE APPARTIENT :
C     =========================================================
      KIABL = NUMDDL//'.SLCS.IABL'
C
      CALL WKVECT(KIABL,BASE//' V I',NEQ,IDIABL)
C
      IEQ = 0
      DO 140 I = 1,NBLC
        NBLIGN = ZI(IDABLO+I) - ZI(IDABLO+I-1)
        DO 150 J = 1,NBLIGN
          IEQ = IEQ + 1
          ZI(IDIABL+IEQ-1) = I
  150   CONTINUE
  140 CONTINUE

C --- CREATION ET AFFECTATION DU TABLEAU .DESC DE DESCRIPTION
C --- DE LA MATRICE :
C     =============
      KDESC = NUMDDL//'.SLCS.DESC'
C
      CALL WKVECT(KDESC,BASE//' V I',6,IDDESC)
C
      ZI(IDDESC+1-1) = NEQ
      ZI(IDDESC+2-1) = ITBLOC
      ZI(IDDESC+3-1) = NBLC
C
C --- CREATION ET AFFECTATION DU TABLEAU .REFA DES REFERENCES
C --- DE LA MATRICE :
C     =============
      KREFA = ATA//'.REFA'
C
      CALL WKVECT(KREFA,BASE//' V K24',4,IDREFA)
C
      CALL DISMOI('F','NOM_MAILLA',NUMEDD,'NUME_DDL',IBID,MA,IER)
      ZK24(IDREFA-1+1) = MA
      ZK24(IDREFA-1+2) = NUMDDL//'.NUME'
      ZK24(IDREFA-1+3) = NUMDDL//'.SLCS'
C
C --- ON AFFECTE L'ETAT 'ASSEMBLE' A LA MATRICE :
C     -----------------------------------------
      CALL JEECRA(KREFA,'DOCU',IBID,'ASSE')
C
C --- CREATION ET AFFECTATION DE L'OBJET .REFE REFERENCANT LE
C --- NUME_DDL DE LA MATRICE :
C     ======================
      KREFE = NUMDDL//'.SLCS.REFE'
C
      CALL WKVECT(KREFE,BASE//' V K24',1,IDREFE)
C
      ZK24(IDREFE+1-1) (1:14) = NUMDDL
C
C --- ON AFFECTE LE STOCKAGE EN LIGNE DE CIEL A LA MATRICE :
C     ----------------------------------------------------
      CALL JEECRA(KREFE,'DOCU',IBID,'SLCS')
C
C --- CREATION ET AFFECTATION DU VECTEUR .CONL DE CONDITIONNEMENT
C --- DES INCONNUES DE LAGRANGE :
C     =========================
      KCONL = ATA//'.CONL'
C
      CALL WKVECT(KCONL,BASE//' V R',NEQ,IDCONL)
C
      DO 160 IEQ = 1,NEQ
        ZR(IDCONL+IEQ-1) = UN
  160 CONTINUE
C
C --- CREATION ET AFFECTATION DE LA COLLECTION .VALE
C --- DES BLOCS DE LA MATRICE :
C     =========================
      KVALE = ATA//'.VALE'
C
      CALL JECREC(KVALE,BASE//' V R','NU','DISPERSE','CONSTANT',NBLC)
      CALL JEECRA(KVALE,'LONMAX',ITBLOC,' ')
      CALL JEECRA(KVALE,'DOCU',IBID,'MS')
C 
C
      COMPT  = 0
      COMPT2 = 0
      DO 170 I = 1,NBLC
        CALL JECROC(JEXNUM(KVALE,I))
        CALL JEVEUO(JEXNUM(KVALE,I),'E',IDVALE)
        IL1 = ZI(IDABLO+I-1) + 1
        IL2 = ZI(IDABLO+I)
        DO 111 J = IL1,IL2
          COMPT = COMPT+ZI(IHCOL+J-1)
  111   CONTINUE
        DO 180 K = 1,COMPT
           ZR(IDVALE +K -1) = ZR(IVALE + COMPT2 +K -1)          
  180   CONTINUE
        COMPT2 = COMPT2 + COMPT
        COMPT  = 0
        CALL JELIBE(JEXNUM(KVALE,I))
  170 CONTINUE
C
      CALL JEDETR('&&ATALC2.ICOL')      
      CALL JEDETR('&&ATALC2.A')
      CALL JEDETR('&&ATALC2.COL')      
      CALL JEDETR('&&ATALC2.TRV2')
      CALL JEDETR('&&ATALC2.VALE')      
      CALL JEDEMA()
C
      END
