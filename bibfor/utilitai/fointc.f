      SUBROUTINE FOINTC ( NOMF, NBPU, NOMPU, VALPU, RESU, IER )
      IMPLICIT  NONE
      INTEGER                   NBPU,                     IER
      REAL*8                                 VALPU(*), RESU(*)
      CHARACTER*(*)       NOMF,       NOMPU(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 17/12/2002   AUTEUR CIBHHGB G.BERTRAND 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     INTERPOLATION POUR CALCULER RESU = F(X,Y,Z,...)
C
C     CETTE ROUTINE EST DUPLIQUEE,AVEC QUELQUES LIGNES EN MOINS
C     DANS FITABU. VEILLER A GARDER LA CONCORDANCE EN CAS DE
C     MODIFICATION.
C     ------------------------------------------------------------------
C IN  NOMF  : NOM DE LA FONCTION OU DE LA NAPPE
C IN  NBPU  : NOMBRE DE PARAMETRES DANS NOMPU ET VALPU
C IN  NOMPU : NOMS DES PARAMETRES "UTILISATEUR"
C IN  VALPU : VALEURS DES PARAMETRES "UTILISATEUR"
C OUT RESU  : R : RESULTAT DE L'INTERPOLATION
C OUT IER   : CODE RETOUR
C     ------------------------------------------------------------------
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       ZI
      COMMON/IVARJE/ZI(1)
      REAL*8        ZR
      COMMON/RVARJE/ZR(1)
      COMPLEX*16    ZC
      COMMON/CVARJE/ZC(1)
      LOGICAL       ZL
      COMMON/LVARJE/ZL(1)
      CHARACTER*8   ZK8
      CHARACTER*16         ZK16
      CHARACTER*24                 ZK24
      CHARACTER*32                         ZK32
      CHARACTER*80                                 ZK80
      COMMON/KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ------------------------------------------------------------------
      INTEGER      LPROL, NUPAR, MXPARA, NBPF, I1
      CHARACTER*1  CBID, BL
      CHARACTER*19 NOMFON
      CHARACTER*24 CHPROL
C     ------------------------------------------------------------------
      INTEGER      IPAR(10)
      CHARACTER*16 NOMP(10)
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      NOMFON = NOMF
C
      RESU(1) = 0.D0
      RESU(2) = 0.D0
C
      CHPROL = NOMFON//'.PROL'
      CALL JEVEUO ( CHPROL, 'L', LPROL )
C
C --- CALCUL DE LA FONCTION INTERPRETEE ---
C
      IF ( ZK16(LPROL) .EQ. 'INTERPRE' ) THEN
         CALL FONBPA ( NOMFON, ZK16(LPROL), CBID, MXPARA, NBPF, NOMP )
         DO 70 I1 = 1,NBPF
            IPAR(I1) = 0
            DO 72 NUPAR = 1,NBPU
               IF (NOMPU(NUPAR).EQ.NOMP(I1)) THEN
                  IF (IPAR(I1).EQ.0) THEN
                     IPAR(I1) = NUPAR
                  ELSE
                     IER = 120
                   CALL UTDEBM('F','FOINTC','ERREUR A L''INTERPOLATION')
                     CALL UTIMPK('S',' FONCTION ',1,NOMFON)
                     CALL UTIMPK('L',' PARAMETRE ',NBPU,NOMPU)
                     CALL UTIMPK('S',' EN DOUBLE',0,BL)
                     CALL UTFINM()
                     GOTO 9999
                  ENDIF
               ENDIF
 72         CONTINUE
            IF (IPAR(I1).EQ.0) THEN
               IER = 130
               CALL UTDEBM('F','FOINTC','ERREUR A L''INTERPOLATION')
               CALL UTIMPK('S',' FONCTION ',1,NOMFON)
               CALL UTIMPK('L',' PARAMETRES ATTENDUS ',NBPF,NOMP)
               CALL UTIMPK('L',' PARAMETRES RECUS    ',NBPU,NOMPU)
               CALL UTFINM()
               GOTO 9999
            ENDIF
 70      CONTINUE
         CALL FIINTC ( 'F', NOMF, NBPF, IPAR, VALPU, RESU, IER )
C
      ELSE
C
         CALL UTMESS('F','FOINTC','"'//ZK16(LPROL)//
     +                            '" TYPE DE FONCTION NON TRAITE')
      ENDIF
C
9999  CONTINUE
      CALL JEDEMA()
      END
