      SUBROUTINE MDFEXT (NVECT,BASEMO,TYPBAS,NEQGEN,
     &                                          TINIT,NBPAS,DT,FEXT,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            NVECT,              NEQGEN,  NBPAS,        IER
      REAL*8                                    TINIT, DT,FEXT(NEQGEN,*)
      CHARACTER*8              BASEMO
      CHARACTER*16                    TYPBAS
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/12/2002   AUTEUR CIBHHGB G.BERTRAND 
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
C     CALCULE LES FORCES EXTERIEURES A CHAQUE PAS DE TEMPS
C     ------------------------------------------------------------------
C IN  : NVECT  : NOMBRE DE CHARGEMENTS
C IN  : BASEMO : NOM DU CONCEPT BASE MODALE
C IN  : TYPBAS : TYPE DE LA BASE ('MODE_MECA' 'BASE_MODA' 'MODELE_GENE')
C IN  : NEQGEN : NOMBRE D'EQUATIONS GENERALISEES
C IN  : TINIT  : TEMPS INITIAL
C IN  : NBPAS  : NOMBRE DE PAS DE CALCUL
C IN  : DT     : PAS DE TEMPS
C OUT : FEXT   : TABLEAU DES FORCES EXTERIEURES A CHAQUE PAS DE TEMPS
C OUT : IER    : CODE RETOUR
C ----------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
C
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
C
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      PARAMETER     ( MXPARA = 10 )
      INTEGER       IPAR(MXPARA), I, J, K, IPT, NBPF, NBPT
      REAL*8        UN, T, ALPHA
      CHARACTER*1   COLI
      CHARACTER*8   K8B
      CHARACTER*16  INTERP, PROLGD
      CHARACTER*16  NOMCMD, NOMP(MXPARA)
      CHARACTER*19  CHANNO, FONCT
      LOGICAL       LFORC
      CHARACTER*1 K1BID
C
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      IER = 0
      EPSI = SQRT( R8PREM() )
      LFORC = .FALSE.
      NOMCMD = 'DYNA_TRAN_MODAL'
      CALL GETVIS('EXCIT','NUME_MODE',1,1,0,IBID,NF)
      LFORC = NF .NE. 0
C
C LE TEST SUIVANT EST REALISE DANS MDGENE POUR LA SOUS-STRUCTURATION
C
      IF (.NOT.LFORC.AND.TYPBAS.NE.'MODELE_GENE     ') THEN
        DO 10 I = 1, NVECT
          CALL GETVID('EXCIT','VECT_GENE',I,1,1,CHANNO,L)
          CALL JEVEUO(CHANNO//'.REFE','L',JREF1)
          IF (ZK24(JREF1)(1:8).NE.BASEMO) THEN
            IER = IER + 1
            CALL UTMESS('E',NOMCMD,'LES BASES UTILISEES POUR LA '//
     +                             'PROJECTION SONT DIFFERENTES.')
          ENDIF
          CALL JEVEUO(CHANNO//'.DESC','L',JDES1)
          IF (ZI(JDES1+1).NE.NEQGEN) THEN
            IER = IER + 1
            CALL UTMESS('E',NOMCMD,'LES BASES UTILISEES N''ONT PAS '//
     +                             'LE MEME NOMBRE DE VECTEURS.')
          ENDIF
  10    CONTINUE
      ENDIF
C
      IF (IER.NE.0) GOTO 9999
C
      UN = 1.D0
      DO 20 I = 1,NVECT
         T = TINIT
         IF (LFORC) THEN
           CALL GETVID('EXCIT','FONC_MULT',I,1,1,FONCT,N1)
           CALL GETVIS('EXCIT','NUME_MODE',I,1,1,NUMOR,N2)
         ELSE
           CALL GETVID('EXCIT','VECT_GENE',I,1,1,CHANNO,N1)
           CALL JEVEUO(CHANNO//'.VALE','L',JVALE)
C
           CALL GETVID('EXCIT','FONC_MULT',I,1,1,FONCT ,N1)
         ENDIF
         IF (N1.NE.0) THEN
            CALL JEVEUO(FONCT//'.PROL','L',LPROL)
            CALL FONBPA(FONCT,ZK16(LPROL),K8B,MXPARA,NBPF,NOMP)
            IPAR(1) = 0
            DO 22 I1 = 1,NBPF
               IF (NOMP(I1).EQ.'INST') THEN
                  IF (IPAR(1).EQ.0) THEN
                     IPAR(1) = 1
                  ELSE
                     IER = IER + 1
                   CALL UTDEBM('E','MDFEXT','ERREUR A L''INTERPOLATION')
                     CALL UTIMPK('S',' FONCTION: ',1,FONCT)
                     CALL UTIMPK('L',' PARAMETRE: ',1,'INST')
                     CALL UTIMPK('S',' EN DOUBLE.',0,' ')
                     CALL UTFINM()
                     GOTO 9999
                  ENDIF
               ENDIF
 22         CONTINUE
            IF (IPAR(1).EQ.0) THEN
               IER = IER + 1
               CALL UTDEBM('E','MDFEXT','ERREUR A L''INTERPOLATION')
               CALL UTIMPK('S',' FONCTION: ',1,FONCT)
               CALL UTIMPK('L',' PARAMETRES ATTENDUS:',1,'INST')
               CALL UTIMPK('L',' PARAMETRES RECUS   :',NBPF,NOMP)
               CALL UTFINM()
               GOTO 9999
            ENDIF
C
C           --- FONCTION FORMULE ---
            IF (ZK16(LPROL).EQ.'INTERPRE') THEN
               DO 30 K = 1,NBPAS
                  CALL FIINTE('F',FONCT,NBPF,IPAR,T,ALPHA,IER)
                  IF (LFORC) THEN
                    FEXT(NUMOR,K) = ALPHA
                  ELSE
                    DO 32 J = 1,NEQGEN
                      FEXT(J,K) = FEXT(J,K) + ALPHA * ZR(JVALE+J-1)
 32                 CONTINUE
                  ENDIF
                  T = TINIT + ( K * DT )
 30            CONTINUE
C
C           --- FONCTION CLASSIQUE ---
            ELSEIF (ZK16(LPROL).EQ.'FONCTION') THEN
               CALL JEVEUO(FONCT//'.VALE','L',LVAR)
               CALL JELIRA(FONCT//'.VALE','LONUTI',NBPT,K1BID)
               NBPT   = NBPT / 2
               LFON   = LVAR + NBPT
               INTERP = ZK16(LPROL+1)
               PROLGD = ZK16(LPROL+4)
               IPT = 1
               DO 40 K = 1,NBPAS
                  CALL FOLOCX(ZR(LVAR),NBPT,T,PROLGD,IPT,EPSI,COLI,IRET)
                  IF (IRET.NE.0) THEN
                     IER = IER + 1
                     CALL UTDEBM('E','MDFEXT',
     +                           'PROBLEME RENCONTRE DANS FOLOCX')
                     CALL UTIMPK('L',' POUR LA FONCTION:',1,FONCT)
                     CALL UTFINM()
                     GOTO 9999
                  ENDIF
                  CALL FOCOLI ( IPT,COLI,INTERP,ZR(LVAR),ZR(LFON),
     +                                          T , ALPHA , IER ,
     +                   FONCT,ZK16(LPROL),MXPARA,NOMP,IPAR,'INST',1,T )
                  IF (IER.NE.0) GOTO 9999
                  IF (LFORC) THEN
                     FEXT(NUMOR,K) = ALPHA
                  ELSE
                     DO 42 J = 1,NEQGEN
                        FEXT(J,K) = FEXT(J,K) + ALPHA * ZR(JVALE+J-1)
 42                  CONTINUE
                  ENDIF
                  T = TINIT + ( K * DT )
 40            CONTINUE
C
            ELSE
               IER = IER + 1
               CALL UTMESS('E','MDFEXT',
     +                     'FONCTION '//FONCT//' NON TRAITEE.')
               GOTO 9999
            ENDIF
C
         ELSE
            ALPHA = UN
            CALL GETVR8('EXCIT','COEF_MULT',I,1,1,ALPHA,L)
            DO 50 J = 1,NEQGEN
               DO 52 K = 1,NBPAS
                  FEXT(J,K) = FEXT(J,K) + ALPHA * ZR(JVALE+J-1)
 52            CONTINUE
 50         CONTINUE
         ENDIF
 20   CONTINUE
 9999 CONTINUE
C
      CALL JEDEMA()
      END
