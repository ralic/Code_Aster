      SUBROUTINE JEIMPO ( UNIT , NOMLU , PARM , MESS )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
C MODIF JEVEUX  DATE 13/11/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C IMPRIME LE CONTENU D'UN OBJET JEVEUX
C
C IN  UNIT  : NUMERO D'UNITE LOGIQUE ASSOCIE AU FICHIER D'IMPRESSION
C IN  NOMLU : NOM DE L'OBJET JEVEUX A IMPRIMER
C IN  PARM  : ARGUMENT NON UTILISE (CHAINE DE CARACTERES)
C IN  MESS  : MESSAGE D'INFORMATION IMPRIME 
C ----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'jeveux_private.h'
      INTEGER             UNIT
      CHARACTER *(*)      NOMLU , PARM , MESS
C     ------------------------------------------------------------------
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON 
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
C     -----------------------------------------------------------------
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
C     ------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER IADMEX ,IADMI ,IBDESO ,IBIADD ,IBIADM ,IBLONO ,IDECI 
      INTEGER INAT ,IPGCEX ,IXIADM ,IXLONO ,JCARA ,JDATE ,JDOCU 
      INTEGER JGENR ,JHCOD ,JIADD ,JIADM ,JLONG ,JLONO ,JLTYP 
      INTEGER JLUTI ,JMARQ ,JORIG ,JRNOM ,JTYPE ,K ,N 
      INTEGER NBMAX 
C-----------------------------------------------------------------------
      PARAMETER  ( N = 5 )
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     &                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
C
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
C
      INTEGER          LBIS , LOIS , LOLS , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOR8 , LOC8
      INTEGER          IPGC,KDESMA(2),LGD,LGDUTI,KPOSMA(2),LGP,LGPUTI
      COMMON /IADMJE/  IPGC,KDESMA,   LGD,LGDUTI,KPOSMA,   LGP,LGPUTI
      INTEGER          NUMEC
      COMMON /INUMJE/  NUMEC
C     ------------------------------------------------------------------
      CHARACTER *32   NOML32
      CHARACTER *1    GENRI , TYPEI
      INTEGER         ICRE , IRET , JCTAB , LTYPI , LONOI , IADDI(2)
      INTEGER         IBACOL , IXIADD , IXDESO
      LOGICAL         LCONST , LCOL
      REAL*8          RB
C     ------------------------------------------------------------------
      INTEGER        IVNMAX     , IDDESO     , IDIADD     , IDIADM     ,
     &                              IDLONG     ,
     &               IDLONO          
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 , IDIADD = 2 , IDIADM = 3 ,
     &                              IDLONG = 7 ,
     &               IDLONO = 8   )
C DEB ------------------------------------------------------------------
      IPGCEX = IPGC
      IPGC   = -2
      NOML32 = NOMLU
C
      LCOL = .FALSE.
      ICRE = 0
      CALL JJVERN ( NOML32 , ICRE , IRET )
C
      IF ( IRET .EQ. 0 ) THEN
        CALL U2MESK('A','JEVEUX_26',1,NOML32(1:24))
        GOTO 9999
      ELSE IF ( IRET .EQ. 1 ) THEN
C
C ----  CAS D'UN OBJET SIMPLE
C
        INAT = 1
        IADMI  = IADM ( JIADM(ICLAOS) + 2*IDATOS-1 )
        IADMEX = IADMI
        GENRI  = GENR ( JGENR(ICLAOS) + IDATOS )
        TYPEI  = TYPE ( JTYPE(ICLAOS) + IDATOS )
        LTYPI  = LTYP ( JLTYP(ICLAOS) + IDATOS )
        LONOI  = LONO ( JLONO(ICLAOS) + IDATOS ) * LTYPI
        IF ( IADMEX .EQ. 0 ) THEN
          IADDI(1) = IADD ( JIADD(ICLAOS) + 2*IDATOS-1 )
          IADDI(2) = IADD ( JIADD(ICLAOS) + 2*IDATOS   )
          IF ( IADDI(1) .EQ. 0 ) THEN
            CALL U2MESK('A','JEVEUX_27',1,NOML32(1:24))
            GOTO 9999
          ENDIF
          CALL JJALTY (TYPEI , LTYPI , 'L' , 1 , JCTAB)
          IADMI = IADM ( JIADM(ICLAOS) + 2*IDATOS-1 )
        ENDIF
        IDECI = 0
        CALL JJIMPO ( UNIT,IADMI, IDECI, 0, GENRI, TYPEI, LTYPI, LONOI,
     &                MESS , PARM)
        IF ( IADMEX .EQ. 0 ) THEN
          CALL JJLIDE ( 'JEIMPO' , NOML32 , INAT )
        ENDIF
      ELSE IF ( IRET .EQ. 2 ) THEN
C
C ----- CAS D'UNE COLLECTION
C
        LCOL = .TRUE.
        CALL JJALLC ( ICLACO , IDATCO , 'L' , IBACOL )
        IF ( NOML32(25:32) .EQ. '        ') THEN
          INAT = 2
        ELSE
          CALL JJCROC ( NOML32(25:32) , ICRE )
          INAT = 3
        ENDIF
      ENDIF
      IF ( INAT .EQ. 2 ) THEN
C
C ----- CAS D'UNE COLLECTION ENTIERE
C
        IXIADD = ISZON ( JISZON + IBACOL + IDIADD )
        IXIADM = ISZON ( JISZON + IBACOL + IDIADM )
        IXDESO = ISZON ( JISZON + IBACOL + IDDESO )
        GENRI  = GENR( JGENR(ICLACO) + IXDESO )
        TYPEI  = TYPE( JTYPE(ICLACO) + IXDESO )
        LTYPI  = LTYP( JLTYP(ICLACO) + IXDESO )
        IF ( IXIADD .EQ. 0 ) THEN
C
C ------- COLLECTION CONTIGUE
C
          IADMI  = IADM ( JIADM(ICLACO) + 2*IXDESO-1 )
          IADDI(1) = IADD ( JIADD(ICLACO) + 2*IXDESO-1 )
          IADDI(2) = IADD ( JIADD(ICLACO) + 2*IXDESO   )
          IADMEX = IADMI
          IF ( IADMEX .EQ. 0 ) THEN
            IF ( IADDI(1) .EQ. 0 ) THEN
              CALL U2MESK('A','JEVEUX_28',1,NOML32(1:24))
              GOTO 9999
            ENDIF
            CALL JJALTY (TYPEI , LTYPI , 'L' , 2 , JCTAB)
            IADMI = IADM ( JIADM(ICLACO) + 2*IXDESO-1 )
          ENDIF
          LONOI  = LONO( JLONO(ICLACO) + IXDESO ) * LTYPI
          IDECI  = 0
          CALL JJIMPO ( UNIT,IADMI, IDECI, -1, GENRI,TYPEI,LTYPI,LONOI,
     &                  MESS , PARM)
          IF ( IADMEX .EQ. 0 ) THEN
            CALL JJLIDE ( 'JEIMPO' , NOML32 , INAT )
          ENDIF
        ELSE
C
C ------- COLLECTION DISPERSEE
C
          NBMAX  = ISZON ( JISZON + IBACOL + IVNMAX )
          IBIADM = IADM ( JIADM(ICLACO) + 2*IXIADM-1 )
          IBIADD = IADM ( JIADM(ICLACO) + 2*IXIADD-1 )
          IDECI  = 0
          DO 10 K = 1,NBMAX
            IADMI = ISZON(JISZON + IBIADM - 1 + 2*K-1 )
            IF ( IADMI .EQ. 0 ) THEN
              IADDI(1) = ISZON(JISZON + IBIADD - 1 + 2*K-1 )
              IADDI(2) = ISZON(JISZON + IBIADD - 1 + 2*K   )
              IF ( IADDI(1) .EQ. 0 ) GOTO 10
              CALL JJALTY (TYPEI , LTYPI , 'L' , 3 , JCTAB)
              IADMI  = ISZON(JISZON + IBIADM - 1 + 2*K-1 )
            ENDIF
            IXLONO = ISZON ( JISZON + IBACOL + IDLONO )
            IF ( IXLONO .EQ. 0 ) THEN
              LONOI = LONO ( JLONO(ICLACO) + IXDESO ) * LTYPI
            ELSE
              IBLONO = IADM ( JIADM(ICLACO) + 2*IXLONO-1 )
              LONOI  = ISZON ( JISZON + IBLONO - 1 + K ) * LTYPI
            ENDIF
            CALL JJIMPO(UNIT,IADMI,IDECI,K,GENRI,TYPEI,LTYPI,
     &                  LONOI,MESS,PARM)
            NUMEC = K
            CALL JJLIDE ('JEIMPO' , NOML32//'$$XNUM  ' , 2)
 10       CONTINUE
        ENDIF
        CALL JJLIDE ( 'JEIMPO' , NOML32 , INAT )
      ELSE IF ( INAT .EQ. 3 ) THEN
C       ------ CAS D'UN OBJET DE COLLECTION  ------
         IXIADD = ISZON ( JISZON + IBACOL + IDIADD )
         IXIADM = ISZON ( JISZON + IBACOL + IDIADM )
         IXDESO = ISZON ( JISZON + IBACOL + IDDESO )
         IXLONO = ISZON ( JISZON + IBACOL + IDLONO )
         GENRI = GENR( JGENR(ICLACO) + IXDESO )
         TYPEI = TYPE( JTYPE(ICLACO) + IXDESO )
         LTYPI = LTYP( JLTYP(ICLACO) + IXDESO )
         IF ( IXIADD .EQ. 0 ) THEN
C           ----------- COLLECTION CONTIGUE
           LCONST = ( ISZON ( JISZON + IBACOL + IDLONG ) .EQ. 0 )
           IBDESO = IADM ( JIADM(ICLACO) + 2*IXDESO-1 )
           IADDI(1)  = IADD ( JIADD(ICLACO) + 2*IXDESO-1 )
           IADDI(2)  = IADD ( JIADD(ICLACO) + 2*IXDESO   )
           IADMEX = IBDESO
           IF ( IADMEX .EQ. 0 ) THEN
             IF ( IADDI(1) .EQ. 0 ) THEN
               CALL U2MESG('A','JEVEUX_29',1,NOML32(1:24),1,IDATOC,0,RB)
               GOTO 9999
             ENDIF
             CALL JJALTY (TYPEI , LTYPI , 'L' , 2 , JCTAB)
             IBDESO = IADM ( JIADM(ICLACO) + 2*IXDESO-1 )
           ENDIF
           IF ( LCONST ) THEN
             LONOI = LONO ( JLONO(ICLACO) + IXDESO ) * LTYPI
             LONOI = LONOI / ISZON ( JISZON + IBACOL + IVNMAX )
             IADMI = IBDESO
             IDECI = ( IDATOC - 1 ) * LONOI
           ELSE
             IBLONO = IADM ( JIADM(ICLACO) + 2*IXLONO-1 )
             LONOI = LTYPI * ( ISZON(JISZON+IBLONO-1+IDATOC+1) -
     &                         ISZON(JISZON+IBLONO-1+IDATOC ) )
             IADMI = IBDESO
             IDECI = (LTYPI*(ISZON(JISZON+IBLONO-1+IDATOC)-1))
           ENDIF
           CALL JJIMPO(UNIT,IADMI,IDECI,IDATOC,GENRI,TYPEI,LTYPI,LONOI,
     &                   MESS , PARM)
           IF ( IADMEX .EQ. 0 ) THEN
              CALL JJLIDE ( 'JEIMPO' , NOML32 , INAT )
           ENDIF
         ELSE
C
C -------- COLLECTION DISPERSEE
C
           IBIADM = IADM ( JIADM(ICLACO) + 2*IXIADM-1 )
           IBIADD = IADM ( JIADM(ICLACO) + 2*IXIADD-1 )
           IADMI  = ISZON(JISZON + IBIADM - 1 + 2*IDATOC-1 )
           IADMEX = IADMI
           IDECI  = 0
           IF ( IADMEX .EQ. 0 ) THEN
             IADDI(1) = ISZON(JISZON + IBIADD - 1 + 2*IDATOC-1 )
             IADDI(2) = ISZON(JISZON + IBIADD - 1 + 2*IDATOC   )
             IF ( IADDI(1) .EQ. 0 ) THEN
               CALL U2MESG('A','JEVEUX_29',1,NOML32(1:24),1,IDATOC,0,RB)
               GOTO 9999
             ENDIF
             CALL JJALTY (TYPEI , LTYPI , 'L' , INAT , JCTAB)
             IADMI  = ISZON(JISZON + IBIADM - 1 + 2*IDATOC-1 )
           ENDIF
           IXLONO = ISZON ( JISZON + IBACOL + IDLONO )
           IF ( IXLONO .EQ. 0 ) THEN
             LONOI = LONO( JLONO(ICLACO) + IXDESO ) * LTYPI
           ELSE
             IBLONO = IADM ( JIADM(ICLACO) + 2*IXLONO-1 )
             LONOI  = ISZON ( JISZON + IBLONO + IDATOC - 1 ) * LTYPI
           ENDIF
           CALL JJIMPO(UNIT,IADMI,IDECI,IDATOC,GENRI,TYPEI,LTYPI,LONOI,
     &                   MESS , PARM)
           IF ( IADMEX .EQ. 0 ) THEN
             CALL JJLIDE ( 'JEIMPO' , NOML32 , INAT )
           ENDIF
         ENDIF
      ENDIF
 9999 CONTINUE
      IF ( LCOL ) THEN
        CALL JJLIDE ( 'JEIMPO' , NOML32(1:24) , 2 )
      ENDIF
      IPGC = IPGCEX
C FIN ------------------------------------------------------------------
      END
