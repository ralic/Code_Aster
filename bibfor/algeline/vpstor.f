      SUBROUTINE VPSTOR (INEG, TYPE, MODES, NBMODE, NEQ, VECPR8, VECPC8,
     +                   MXRESF, NBPARI, NBPARR, NBPARK, NOPARA,
     +                   RESUFI, RESUFR, RESUFK, IPREC )
      IMPLICIT   NONE
      INTEGER           INEG, NBMODE, NEQ, MXRESF, NBPARI, NBPARR,NBPARK
      INTEGER           IPREC, RESUFI(MXRESF,*)
      CHARACTER*(*)     TYPE, MODES, RESUFK(MXRESF,*), NOPARA(*)
      REAL*8            VECPR8(NEQ,*), RESUFR(MXRESF,*)
      COMPLEX*16        VECPC8(NEQ,*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 16/09/2003   AUTEUR JMBHH01 J.M.PROIX 
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
C     STOCKAGE DES VALEURS PROPRES
C
C     REMARQUE:
C        DANS NOPARA, ON A LES NOMS DE PARAMETRES DE TYPE ENTIER
C                     ENSUITE LES NOMS DE PARAMETRES DE TYPE CHARACTER
C                     ENSUITE LES NOMS DE PARAMETRES DE TYPE REEL
C     ------------------------------------------------------------------
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     ------------------------------------------------------------------
      INTEGER       JREFE, IMODE, JMODE, IER, NMIN, IMIN, NMAX, IMAX,IEQ
      INTEGER       NMIN1, KMODE, NORDR, IBID, I, J, LADPA, LMODE, LVALE
      CHARACTER*8   RES ,RAIDE, K8B
      CHARACTER*16  TYPCON, NOMCMD, NOSY
      CHARACTER*19  CHAMNO
      CHARACTER*24  REFE
      LOGICAL       LREFE
C     ------------------------------------------------------------------
      DATA  REFE  /'                   .REFE'/
C     ------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
      CALL GETRES (RES, TYPCON, NOMCMD )

C     POUR POUVOIR UTILISER VPSTOR DANS STAT_NON_LINE VIA NMOP45
      IF ( TYPCON .EQ. 'EVOL_NOLI' ) THEN
        TYPCON = 'MODE_FLAMB'
      ENDIF
      
      IF ( TYPCON .EQ. 'MODE_ACOU' ) THEN
        NOSY = 'PRES'
      ELSE
        NOSY = 'DEPL'
      ENDIF
      LREFE = .TRUE.
      REFE(1:8) = MODES
      CALL JEEXIN(REFE,IER)
      IF (IER.EQ.0) THEN
         LREFE = .FALSE.
      ELSE
         CALL JEVEUO (REFE, 'L', JREFE )
         RAIDE = ZK24(JREFE+2)
      ENDIF
C
C
C     --- CONTROLE PREALABLE ---
      DO 10 IMODE = 1, NBMODE
         JMODE = RESUFI(IMODE,1)
         IF ( JMODE .LT. 1 .AND. INEG .GT. 0 ) THEN
            CALL UTMESS('A',NOMCMD//'.VPSTOR',
     +                  'LA POSITION MODALE D''UNE DES FREQUENCES'//
     +                  ' EST NEGATIVE OU NULLE, VOTRE SYSTEME '//
     +                  'MATRICIEL EST SUREMENT FORTEMENT '//
     +                  'SINGULIER (CECI CORRESPOND GENERALEMENT '//
     +                  'A UN PROBLEME DANS LA MODELISATION).' )
         ENDIF
 10   CONTINUE
C
C     --- STOCKAGE DES MODES ---
      CALL RSEXIS ( MODES , IER )
      IF ( IER .EQ. 0 ) THEN
         CALL UTMESS('F','VPSTOR','MODE A CREER AVANT APPEL A VPSTOR')
      ENDIF
C
      NMIN = RESUFI(1,1)
      IMIN = 1
      NMAX = RESUFI(1,1)
      IMAX = 1
      DO 80 IMODE = 2, NBMODE
         IF ( RESUFI(IMODE,1) .LT. NMIN ) THEN
            NMIN = RESUFI(IMODE,1)
            IMIN = IMODE
         ENDIF
         IF ( RESUFI(IMODE,1) .GT. NMAX ) THEN
            NMAX = RESUFI(IMODE,1)
            IMAX = IMODE
         ENDIF
 80   CONTINUE
      NMIN1 = NMAX
C
      DO 100 IMODE = 1, NBMODE
C
C       STOCKAGE DES FREQUENCES PAR ORDRE CROISSANT DE NUMERO
        IF (IMODE.EQ.1) THEN
           KMODE = IMIN
        ELSEIF (IMODE.EQ.NBMODE) THEN
           KMODE = IMAX
        ELSE
           DO 101 LMODE = 1, NBMODE
              IF ( RESUFI(LMODE,1) .GT. NMIN  .AND.
     +             RESUFI(LMODE,1) .LT. NMIN1 ) THEN
                 NMIN1 = RESUFI(LMODE,1)
                 KMODE = LMODE
              ENDIF
 101       CONTINUE
           NMIN  = NMIN1
           NMIN1 = NMAX
        ENDIF
C
        JMODE = RESUFI(KMODE,1)
        NORDR = IPREC + IMODE
C
C        --- VECTEUR PROPRE ---
        CALL RSEXCH (MODES, NOSY, NORDR, CHAMNO, IER )
        IF     ( IER .EQ. 0   ) THEN
        ELSEIF ( IER .EQ. 100 .AND. LREFE ) THEN
          CALL VTCREM (CHAMNO, RAIDE, 'G', TYPE(1:1) )
        ELSE
          CALL UTDEBM('F',NOMCMD//'.VPSTOR','APPEL ERRONE')
          CALL UTIMPI('L','MODE NUMERO',1,KMODE)
          CALL UTIMPI('L','POSITION MODALE',1,JMODE)
          CALL UTIMPI('L','CODE RETOUR DE RSEXCH :',1,IER)
          CALL UTIMPK('L','PB CHAM_NO',1,CHAMNO)
          CALL UTFINM()
        ENDIF
        IF (TYPCON.EQ.'MODE_GENE' .OR. TYPCON.EQ.'HARM_GENE') THEN
           CALL JEECRA (CHAMNO//'.DESC','DOCU',IBID,'VGEN')
        ENDIF
        CALL JEVEUO (CHAMNO//'.VALE', 'E', LVALE )
        IF (TYPE(1:1) .EQ. 'R' ) THEN
           DO 110 IEQ = 1, NEQ
              ZR(LVALE+IEQ-1) = VECPR8(IEQ,KMODE)
 110       CONTINUE
        ELSEIF (TYPE(1:1) .EQ. 'C' ) THEN
           DO 120 IEQ = 1, NEQ
              ZC(LVALE+IEQ-1) = VECPC8(IEQ,KMODE)
 120       CONTINUE
        ENDIF
        CALL RSNOCH (MODES, NOSY, NORDR, ' ' )
C
C        --- VARIABLES ET PARAMETRES ---
C
        IF ( TYPCON.EQ.'MODE_FLAMB'  .AND. NOMCMD.NE.'NORM_MODE') THEN
           CALL RSADPA(MODES,'E',1,NOPARA(1),NORDR,0,LADPA,K8B)
           ZI(LADPA) = RESUFI(KMODE,1)
           CALL RSADPA(MODES,'E',1,NOPARA(NBPARI+1),NORDR,0,LADPA,K8B)
           ZK24(LADPA) = RESUFK(KMODE,1)
           J = NBPARI + NBPARK
           CALL RSADPA(MODES,'E',1,'CHAR_CRIT',NORDR,0,LADPA,K8B)
           ZR(LADPA) = RESUFR(KMODE,2)
           CALL RSADPA(MODES,'E',1,'ERREUR',NORDR,0,LADPA,K8B)
           ZR(LADPA) = RESUFR(KMODE,4)
        ELSE
           DO 200 I = 1 , NBPARI
              CALL RSADPA(MODES,'E',1,NOPARA(I),NORDR,0,LADPA,K8B)
              ZI(LADPA) = RESUFI(KMODE,I)
 200       CONTINUE
           DO 210 I = 1 , NBPARK
              CALL RSADPA(MODES,'E',1,NOPARA(NBPARI+I),NORDR,
     &        0,LADPA,K8B)
              ZK24(LADPA) = RESUFK(KMODE,I)
 210       CONTINUE
           J = NBPARI + NBPARK
           DO 220 I = 1 , NBPARR
              CALL RSADPA(MODES,'E',1,NOPARA(J+I),NORDR,0,LADPA,K8B)
              ZR(LADPA) = RESUFR(KMODE,I)
 220       CONTINUE
        ENDIF
C
 100  CONTINUE
C
      CALL JEDEMA ( )
      END
