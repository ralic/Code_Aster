      SUBROUTINE AVCRIT( NBVEC, NBORDR, VECTN, VWORK, TDISP, KWORK,
     &                   SOMMW, TSPAQ,I, VALA, COEFPA, NCYCL, VMIN,
     &                   VMAX, OMIN, OMAX, NOMCRI, NOMMAT, NOMFOR,GDREQ)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 26/09/2011   AUTEUR TRAN V-X.TRAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRS_1404
C RESPONSABLE F1BHHAJ J.ANGLES
      IMPLICIT      NONE
      INTEGER       NBVEC, NBORDR, NCYCL(NBVEC)
      INTEGER       OMIN(NBVEC*(NBORDR+2)), OMAX(NBVEC*(NBORDR+2))
      INTEGER       TDISP, KWORK, SOMMW, TSPAQ, I
      REAL*8        VECTN(3*NBVEC)
      REAL*8        VWORK(TDISP)
      REAL*8        VALA, COEFPA
      REAL*8        VMIN(NBVEC*(NBORDR+2)), VMAX(NBVEC*(NBORDR+2))
      REAL*8        GDREQ(NBVEC*NBORDR)
      CHARACTER*16  NOMCRI, NOMFOR
      CHARACTER*8   NOMMAT
C ----------------------------------------------------------------------
C BUT: CALCULER LA CONTRAINTE EQUIVALENTE POUR TOUS LES VECTEURS NORMAUX
C      A TOUS LES NUMEROS D'ORDRE.
C ----------------------------------------------------------------------
C ARGUMENTS :
C  NBVEC    IN   I  : NOMBRE DE VECTEURS NORMAUX.
C  NBORDR   IN   I  : NOMBRE DE NUMEROS D'ORDRE.
C  VALA     IN   R  : VALEUR DU PARAMETRE a ASSOCIE AU CRITERE.
C  COEFPA   IN   R  : COEFFICIENT DE PASSAGE CISAILLEMENT - UNIAXIAL.
C  NCYCL    IN   I  : NOMBRE DE CYCLES ELEMENTAIRES POUR TOUS LES
C                     VECTEURS NORMAUX.
C  VMIN     IN   R  : VALEURS MIN DES CYCLES ELEMENTAIRES POUR TOUS LES
C                     VECTEURS NORMAUX.
C  VMAX     IN   R  : VALEURS MAX DES CYCLES ELEMENTAIRES POUR TOUS LES
C                     VECTEURS NORMAUX.
C  OMIN     IN   I  : NUMEROS D'ORDRE ASSOCIES AUX VALEURS MIN DES
C                     CYCLES ELEMENTAIRES POUR TOUS LES VECTEURS
C                     NORMAUX.
C  OMAX     IN   I  : NUMEROS D'ORDRE ASSOCIES AUX VALEURS MAX DES
C                     CYCLES ELEMENTAIRES POUR TOUS LES VECTEURS
C                     NORMAUX.
C  GDREQ    OUT  R  : VECTEUR CONTENANT LES VALEURS DE LA GRANDEUR
C                     EQUIVALENTE, POUR TOUS LES NUMEROS D'ORDRE
C                     DE CHAQUE VECTEUR NORMAL.
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     ------------------------------------------------------------------
      INTEGER      IVECT, AD0, AD1, AD2, ICYCL, NVAL, IPAR, J, NP
      INTEGER      IBID, NPARMA, JPROF, PARACT(30), IARG
      REAL*8       COEPRE, VALPAR(30), VALPU(30) 
      REAL*8       VSIGN(NBVEC*NBORDR), VPHYDR(NBORDR)
      REAL*8       VSIPR(NBORDR), VEPSN(NBORDR)
      REAL*8       VETPR(NBORDR), VSITN(NBORDR)
      REAL*8       VEPPR(NBORDR), VSIPN(NBORDR)
      REAL*8       VSIEQ(NBORDR), VETEQ(NBORDR)
      LOGICAL      FORDEF, PLACRI
      CHARACTER*8  NOMPF(30), NOMPAR(30)
      CHARACTER*16 TYPCHA
      CHARACTER*24 CHNOM, CBID
C  VSIGN    IN   R  : VECTEUR CONTENANT LES VALEURS DE LA CONTRAINTE
C                     NORMALE, POUR TOUS LES NUMEROS D'ORDRE
C                     DE CHAQUE VECTEUR NORMAL, ON UTILISE
C                     VSIGN UNIQUEMENT DANS LE CRITERE MATAKE_MODI_AV.
C  VPHYDR   IN   R  : VECTEUR CONTENANT LA PRESSION HYDROSTATIQUE A
C                     TOUS LES INSTANTS, ON UTILISE VPHYDR
C                     UNIQUEMENT DANS LE CRITERE DE DANG VAN.
C     ------------------------------------------------------------------
      DATA  NOMPAR /  'TAUPR_1','TAUPR_2','SIGN_1',  'SIGN_2',
     &                 'PHYDR_1','PHYDR_2','EPSPR_1', 'EPSPR_2', 
     &                 'SIPR1_1','SIPR1_2','EPSN1_1', 'EPSN1_2',
     &                 'ETPR1_1','ETPR1_2','SITN1_1', 'SITN1_2',
     &                 'EPPR1_1','EPPR1_2','SIPN1_1', 'SIPN1_2',
     &                 'SIGEQ_1','SIGEQ_2', 'ETEQ_1', 'ETEQ_2',
     &                 'EPEQ_1', 'EPEQ_2',  'INVJ2_1','INVJ2_2',
     &                 'SITRE_1', 'SITRE_2'     /
C-----------------------------------------------------------------------
C
C234567                                                              012

      CALL JEMARQ()

C 
C RECUPERER LA LISTE DE GRANDEURS ACTIVES
         
      TYPCHA = 'NON_PERIODIQUE'

      CALL ANACRI( NOMCRI,NOMFOR,TYPCHA,'NON', PARACT, FORDEF)
      
C VOIR SI LE CRITERE DU TYPE DE PLANE CRITIQUE
C SI OUI, ON TOURNE NVVECT, SI NON ON NVEC=1
     
      PLACRI = .FALSE.
      DO 30 J = 1, 8
         IF (PARACT(J) .EQ. 1) THEN 
            PLACRI = .TRUE.
            GOTO 31
         ENDIF

30    CONTINUE

31    CONTINUE   
  
C-----------------------------------------------------------------------
C CALCULER LES GRANDEURS
C----------------------------------------------------------------------
C 1.1 CALCUL DE LA CONTRAINTE NORMALE
      IF ((PARACT(3) .EQ. 1 ) .OR. (PARACT(4) .EQ. 1 )) THEN
         CALL AVSIGN(NBVEC, NBORDR, VECTN,
     &         VWORK, TDISP, KWORK,
     &         SOMMW, TSPAQ, I,VSIGN)
         
      ENDIF

C 1.2 CALCUL DE LA PRESSION HYDROSTATIQUE
      IF ((PARACT(5) .EQ. 1 ) .OR. (PARACT(6) .EQ. 1 )) THEN

          CALL AVPHYD(NBORDR, VWORK, TDISP, KWORK, SOMMW,
     &                  TSPAQ, I, VPHYDR)
     
      ENDIF

      IF ( (PARACT(9) .EQ. 1 ) .OR. (PARACT(10) .EQ. 1 ) .OR.
     &     (PARACT(11) .EQ. 1 ) .OR. (PARACT(12) .EQ. 1 ) ) THEN

          CALL AVSIPR(NBORDR, VWORK, TDISP, KWORK, SOMMW,
     &                  TSPAQ, I, VSIPR, VEPSN)
     
      ENDIF     


      IF ( (PARACT(13) .EQ. 1 ) .OR. (PARACT(14) .EQ. 1 ) .OR.
     &     (PARACT(15) .EQ. 1 ) .OR. (PARACT(16) .EQ. 1 ) ) THEN

          CALL AVETPR(NBORDR, VWORK, TDISP, KWORK, SOMMW,
     &                  TSPAQ, I, VETPR, VSITN)
     
      ENDIF     

      IF ( (PARACT(17) .EQ. 1 ) .OR. (PARACT(18) .EQ. 1 ) .OR.
     &     (PARACT(19) .EQ. 1 ) .OR. (PARACT(20) .EQ. 1 ) ) THEN

          CALL AVEPPR(NBORDR, VWORK, TDISP, KWORK, SOMMW,
     &                  TSPAQ, I, NOMMAT, VEPPR, VSIPN)
     
      ENDIF     
      
      IF ((PARACT(21) .EQ. 1 ) .OR. (PARACT(22) .EQ. 1 )) THEN

         CALL AVSIEQ( NBORDR, VWORK, TDISP, KWORK, SOMMW, TSPAQ, I,
     &                   VSIEQ )

      ENDIF
      
      IF ((PARACT(23) .EQ. 1 ) .OR. (PARACT(24) .EQ. 1 )) THEN

         CALL AVETEQ( NBORDR, VWORK, TDISP, KWORK, SOMMW, TSPAQ, I,
     &                   VETEQ )

      ENDIF                      
C----------------------------------------------------------------------
C EVALUER LES GRANDEURS
C---------------------------------------------------------------------- 
C 1. CRITERE DE DANG_VAN MODIFIE (AMPLITUDE VARIABLE)  
      CALL GETVR8(' ','COEF_PREECROU',1,IARG,1,COEPRE,NVAL)
      
      IF (NOMCRI(1:7) .EQ. 'FORMULE') THEN  
C NOMBRE DE PARAMETRES DISPONIBLES
         NPARMA = 30
C RECUPERER LES NOMS DE PARAMETRES FOURNIS PAR L'UTILISATEUR         
         CHNOM(20:24) = '.PROL'
         CHNOM(1:19) = NOMFOR
      
         CALL JEVEUO(CHNOM,'L',JPROF)
         CALL FONBPA ( NOMFOR, ZK24(JPROF), CBID, NPARMA, NP, NOMPF ) 
      ENDIF
       
      DO 10 IVECT=1, NBVEC
         AD0 = (IVECT-1)*NBORDR
         DO 20 ICYCL=1, NCYCL(IVECT)
            AD1 = (IVECT-1)*NBORDR + ICYCL
            AD2 = (IVECT-1)*(NBORDR+2) + ICYCL
            
            IF (NOMCRI(1:14) .EQ. 'MATAKE_MODI_AV') THEN
               GDREQ(AD1)= COEPRE*ABS((VMAX(AD2) - VMIN(AD2))/2.0D0) +
     &                    VALA*MAX(VSIGN(AD0+OMAX(AD2)),
     &                             VSIGN(AD0+OMIN(AD2)),0.0D0)
               GDREQ(AD1)= GDREQ(AD1)*COEFPA               
            ENDIF
            
            IF (NOMCRI(1:16) .EQ. 'DANG_VAN_MODI_AV') THEN
               GDREQ(AD1)= COEPRE*ABS((VMAX(AD2) - VMIN(AD2))/2.0D0) +
     &                       VALA*MAX(VPHYDR(OMAX(AD2)),
     &                                VPHYDR(OMIN(AD2)),0.0D0)
     
               GDREQ(AD1)= GDREQ(AD1)*COEFPA
            ENDIF 
            
            IF (NOMCRI(1:16) .EQ. 'FATESOCI_MODI_AV') THEN
                GDREQ(AD1)=
     &            COEPRE*ABS((VMAX(AD2) - VMIN(AD2))/2.0D0)*
     &                   (1.0D0 + VALA*MAX(VSIGN(AD0+OMAX(AD2)),
     &                                     VSIGN(AD0+OMIN(AD2)),0.0D0))
               GDREQ(AD1)= GDREQ(AD1)*COEFPA
            ENDIF
            
            IF (NOMCRI(1:7) .EQ. 'FORMULE') THEN 
               VALPAR(1) = VMAX(AD2)
               VALPAR(2) = VMIN(AD2)
               VALPAR(3) = VSIGN(AD0+OMAX(AD2))
               VALPAR(4) = VSIGN(AD0+OMIN(AD2))
               VALPAR(5) = VPHYDR(OMAX(AD2))
               VALPAR(6) = VPHYDR(OMIN(AD2))             
               VALPAR(7) = VMAX(AD2)
               VALPAR(8) = VMIN(AD2)
               VALPAR(9) = VSIPR(OMAX(AD2))
               VALPAR(10) = VSIPR(OMIN(AD2))
               VALPAR(11) = VEPSN(OMAX(AD2))
               VALPAR(12) = VEPSN(OMIN(AD2))
               VALPAR(13) = VETPR(OMAX(AD2))
               VALPAR(14) = VETPR(OMIN(AD2))
               VALPAR(15) = VSITN(OMAX(AD2))
               VALPAR(16) = VSITN(OMIN(AD2))
               VALPAR(17) = VEPPR(OMAX(AD2))
               VALPAR(18) = VEPPR(OMIN(AD2))
               VALPAR(19) = VSIPN(OMAX(AD2))
               VALPAR(20) = VSIPN(OMIN(AD2))   
               VALPAR(21) = VSIEQ(OMAX(AD2))
               VALPAR(22) = VSIEQ(OMIN(AD2))
               VALPAR(23) = VETEQ(OMAX(AD2))
               VALPAR(24) = VETEQ(OMIN(AD2))
               VALPAR(25) = 0.D0
               VALPAR(26) = 0.D0
               VALPAR(27) = 0.D0
               VALPAR(28) = 0.D0
               VALPAR(29) = 0.D0
               VALPAR(30) = 0.D0  
                          
               DO 75 J = 1, NP
                  DO 65 IPAR = 1, NPARMA
                     IF (NOMPF(J).EQ.NOMPAR(IPAR)) THEN
                        VALPU(J) =  VALPAR(IPAR) 
                        GOTO 75            
                     ENDIF
65                CONTINUE             
75             CONTINUE          
 
               CALL FOINTE('F',NOMFOR, NP,NOMPF,VALPU,GDREQ(AD1),IBID)
               
            ENDIF
                         
 20      CONTINUE
         
         IF ( .NOT. PLACRI) THEN
            GOTO 99
         ENDIF
         
 10   CONTINUE
 
 99   CONTINUE
 
      CALL JEDEMA()
C
      END
