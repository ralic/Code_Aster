      SUBROUTINE ATASMO(AZ,NUMEDZ,RTBLOC,ATAZ,BASEZ,NBLIG)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 17/07/2002   AUTEUR ADBHHPM P.MASSIN 
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

C     ATASMO  --  LE BUT DE CETTE ROUTINE EST DE CREER LA MATR_ASSE
C                 DE NOM ATA.
C                 LE .VALE DE CETTE MATR_ASSE VA CONTENIR LES TERMES
C                 DU PRODUIT DE MATRICES AT*A.
C                 A EST 'CONCEPTUELLEMENT' UNE MATRICE RECTANGLE
C                 DE 'HAUTEUR' NBLIG ET DE LARGEUR NEQ.
C                 A EST 'INFORMATIQUEMENT' UNE COLLECTION NUMEROTEE
C                 COMPORTANT NBLIG OBJETS QUI SONT DES VECTEURS DE REELS
C                 DE LONGUEUR NEQ.
C                 CHACUN DE CES VECTEURS EST UNE LIGNE DE LA MATRICE A.
C                 LA MATR_ASSE ATA VA DONC ETRE SYMETRIQUE ET A
C                 VALEURS REELLES. D'AUTRE PART ON VA LUI AFFECTER
C                 UN PROFIL MORSE.

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
      INTEGER     IND,I,J,K
      CHARACTER*1  K1BID,BASE
      CHARACTER*14 NUMDDL,NUMEDD
      CHARACTER*19 PFCHNO,ATA
      CHARACTER*24 KHCOL,KADIA,KABLO,KIABL,KDESC,KREFA,KCONL,KVALE,
     +             KREFE,A      
C
C.========================= DEBUT DU CODE EXECUTABLE ==================
      CALL JEMARQ()
C      
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
C
      ZERO = 0.0D0
      UN = 1.0D0
C
C --- COPIE DE LA METHODE DE NUMEROTATION DE LA MATRICE A
C --- DANS LE NUMEDDL DE LA MATRICE ATA :
C     ---------------------------------
      CALL JEDUPO(NUMEDD//'.MLTF.RENU',BASE,
     +            NUMDDL//'.MLTF.RENU',.FALSE.)
C
C --- RECUPERATION DU NOMBRE D'OBJETS DE LA COLLECTION A (I.E.
C --- RECUPERATION DU NOMBRE DE VECTEURS LIGNES DE LA MATRICE
C --- RECTANGULAIRE A) :
C     ----------------
C      CALL JELIRA(A,'NMAXOC',NBLIG,K1BID)
C
C --- RECUPERATION DE LA LONGUEUR D'UN VECTEUR DE LA COLLECTION A
C --- (I.E. DU NOMBRE DE LIGNES DE LA MATRICE RECTANGULAIRE A) :
C     -------------------------------------------------------
      CALL JELIRA(A,'LONMAX',NEQ,K1BID)
C
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
C
C
C --- STOCKAGE DE LA COLLECTION A DANS UN VECTEUR DE TRAVAIL
C     (STOCKAGE EN LIGNE)
C     ---------------------------------------------------
C
      CALL WKVECT('&&ATASMO.A','V V R',NEQ*NBLIG,IDA)
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
C     ICUM EST LE NOMBRE DE TERMES NON NULS DE A
C
      CALL WKVECT('&&ATASMO.COL','V V R',NEQ*NBLIG,IDCOL)
      CALL WKVECT('&&ATASMO.TRV2','V V I',NEQ*NBLIG,IMCOL)      
      CALL WKVECT('&&ATASMO.ICOL','V V I',NEQ,ID2)      
      K = 1
      ICUM = 0 
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
        ZI(ID2+I-1) = INDCOL
        ICUM = ICUM + INDCOL      
   30 CONTINUE                              
C
C
C      
C
C --- CREATION ET AFFECTATION DU TABLEAU .HCOL DIMENSIONNE
C     AU MAJORANT ITAIL DE TERMES A STOCKER 
C --- CREATION ET AFFECTATION DE LA COLLECTION .VALE
C     ==============================================
C
      ISOM = 0
      DO 50 I = 1,NEQ  
         ISOM = ISOM + ZI(ID2+I-1)*ICUM
  50  CONTINUE
      ITAIL = ISOM + NEQ
C
      KHCOL = NUMDDL//'.SMOS.HCOL'
      CALL WKVECT(KHCOL,BASE//' V I',ITAIL,IHCOL)
C
      KVALE = ATA//'.VALE'
      NBLC = 1
      CALL JECREC(KVALE,BASE//' V R','NU','DISPERSE','CONSTANT',NBLC)
      CALL JEECRA(KVALE,'LONMAX',ITAIL,' ')
      CALL JEECRA(KVALE,'DOCU',IBID,'MS')  
      CALL JECROC(JEXNUM(KVALE,NBLC))
      CALL JEVEUO(JEXNUM(KVALE,NBLC),'E',IDVALE)
C 
C
C     TRAITEMENT DE LA PREMIERE COLONNE DE A
      K = 1     
      SOM = ZERO 
      IF (ZI(ID2 +1 -1) .EQ. 0) THEN
         SOM = ZERO
      ELSE           
         DO 60 L =1,ZI(ID2 +1 -1) 
            SOM = SOM + ZR(IDCOL +L -1)*ZR(IDCOL +L -1) 
  60     CONTINUE
      ENDIF
      ZR(IDVALE +K -1) = SOM
      ZI(IHCOL +K -1) = 1
      K = K + 1
C
C     INDIC EST LE NOMBRE DE TERMES STOCKES PAR LIGNE DE ATA    
      CALL WKVECT('&&ATASMO.INDIC','V V I',NEQ,ID3)
      ICUM = 0
      K1 = 2
      DO 70 I = 2 , NEQ
        INDIC = 0
        IF(ZI(ID2 +I -1).EQ. 0) THEN
           ZR(IDVALE +K -1) = ZERO
           ZI(IHCOL +K -1) = I
           K = K + 1
           ZI(ID3 +K1 -1) = 1
           K1 = K1 + 1
        ELSE           
           DO 80 J = 2 , I 
              SOM = ZERO
              IBORJ = ZI(ID2 +J -1)
              IF(IBORJ.EQ.0) THEN
                 CONTINUE
              ELSE  
                 DO 90 L = 1 , ZI(ID2 +I -1)        
                    L1 = ZI(IMCOL + ICUM + L - 1)
                    SOM =SOM + ZR(IDA +(L1-1)*NEQ + (J-1))*
     +                         ZR(IDA +(L1-1)*NEQ + (I-1))
  90             CONTINUE 
                 ZR(IDVALE +K -1) = SOM
                 ZI(IHCOL +K -1) = J
                 K = K + 1
                 INDIC = INDIC + 1 
              ENDIF 
  80       CONTINUE
           ZI(ID3 +K1 -1) = INDIC          
           K1 = K1 + 1  
        ENDIF
        ICUM = ICUM + ZI(ID2 +I -1)
  70  CONTINUE               
C       
      CALL JELIBE(JEXNUM(KVALE,NBLC))  
C 
C --- CREATION ET AFFECTATION DU TABLEAU .ADIA DES POSITIONS
C --- DES TERMES DIAGONAUX DANS LE BLOC :
C     ==================================
      KADIA = NUMDDL//'.SMOS.ADIA'
      CALL WKVECT(KADIA,BASE//' V I',NEQ,IADIA)
      ZI(IADIA +1 -1) = 1
      DO 100 I = 2,NEQ
         ZI(IADIA +I -1) = ZI(IADIA +I-1 -1) + ZI(ID3 +I -1)
  100 CONTINUE                   
C
C
C --- CREATION ET AFFECTATION DU TABLEAU .ABLO DES NUMEROS
C --- DES LIGNES DE DEBUT ET DE FIN DU BLOC :
C     ====================================
C
      KABLO = NUMDDL//'.SMOS.ABLO'
      CALL WKVECT(KABLO,BASE//' V I',2,IDABLO)
C
      ZI(IDABLO) = 0
      ZI(IDABLO+1) = NEQ
C
C
C --- TAILLE DU BLOC :
C     ==============
      ITBLOC = NINT(RTBLOC)*1024
C      
C      
C --- CREATION ET AFFECTATION DU TABLEAU .IABL DONNANT POUR
C --- CHAQUE LIGNE DE LA MATRICE LE BLOC AUQUEL ELLE APPARTIENT :
C     =========================================================
      KIABL = NUMDDL//'.SMOS.IABL'
      CALL WKVECT(KIABL,BASE//' V I',NEQ,IDIABL)
      DO 110 I = 1,NEQ
        ZI(IDIABL+I-1) = 1
 110  CONTINUE
C
C
C --- CREATION ET AFFECTATION DU TABLEAU .DESC DE DESCRIPTION
C --- DE LA MATRICE :
C     =============
      KDESC = NUMDDL//'.SMOS.DESC'
C
      CALL WKVECT(KDESC,BASE//' V I',6,IDDESC)
C
      ZI(IDDESC+1-1) = NEQ
      ZI(IDDESC+2-1) = ITBLOC
      ZI(IDDESC+3-1) = 1
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
      ZK24(IDREFA-1+3) = NUMDDL//'.SMOS'
C
C --- ON AFFECTE L'ETAT 'ASSEMBLE' A LA MATRICE :
C     -----------------------------------------
      CALL JEECRA(KREFA,'DOCU',IBID,'ASSE')
C
C --- CREATION ET AFFECTATION DE L'OBJET .REFE REFERENCANT LE
C --- NUME_DDL DE LA MATRICE :
C     ======================
      KREFE = NUMDDL//'.SMOS.REFE'

      CALL WKVECT(KREFE,BASE//' V K24',1,IDREFE)

      ZK24(IDREFE+1-1) (1:14) = NUMDDL

C --- ON AFFECTE LE STOCKAGE MORSE A LA MATRICE :
C     ----------------------------------------------------
      CALL JEECRA(KREFE,'DOCU',IBID,'SMOS')

C --- CREATION ET AFFECTATION DU VECTEUR .CONL DE CONDITIONNEMENT
C --- DES INCONNUES DE LAGRANGE :
C     =========================
      KCONL = ATA//'.CONL'
C
      CALL WKVECT(KCONL,BASE//' V R',NEQ,IDCONL)
C
      DO 120 IEQ = 1,NEQ
        ZR(IDCONL+IEQ-1) = UN
 120  CONTINUE
C          
      CALL JEDETR('&&ATASMO.COL')
      CALL JEDETR('&&ATASMO.TRV2')
      CALL JEDETR('&&ATASMO.ICOL')      
      CALL JEDETR('&&ATASMO.A')
      CALL JEDETR('&&ATASMO.INDIC')      
      CALL JEDEMA()
C
      END
