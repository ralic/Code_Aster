      SUBROUTINE JERAZO ( NOMLU , NI , I1 )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INCLUDE 'jeveux_private.h'
      CHARACTER *(*)      NOMLU
      INTEGER             NI , I1
C ----------------------------------------------------------------------
C     REMISE A "ZERO" DU SEGMENT DE VALEURS ASSOCIE A UN OBJET JEVEUX
C IN  NI    : NOMBRE DE VALEURS A REINITIALISER
C IN  I1    : INDICE DE LA PREMIERE VALEUR
C IN  NOMLU : NOM DE L'OBJET JEVEUX
C ----------------------------------------------------------------------
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON 
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
C-----------------------------------------------------------------------
      INTEGER I ,IBACOL ,IBLONO ,ICRE ,INAT ,INATB ,IRET 
      INTEGER IXDESO ,IXIADD ,IXLONO ,J1 ,J2 ,JCARA ,JCTAB 
      INTEGER JDATE ,JDOCU ,JGENR ,JHCOD ,JIADD ,JIADM ,JINI 
      INTEGER JLONG ,JLONO ,JLTYP ,JLUTI ,JMARQ ,JORIG ,JRNOM 
      INTEGER JTYPE ,LONOI ,LTYPI ,N 
C-----------------------------------------------------------------------
      PARAMETER  ( N = 5 )
C ----------------------------------------------------------------------
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
C ----------------------------------------------------------------------
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     &                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
C
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
C
      INTEGER          NUMATR
      COMMON /IDATJE/  NUMATR
C -------------------------------------------------
      CHARACTER *32    NOML32
      CHARACTER *8     NOML8
      CHARACTER *1     TYPEI , GENRI
C ----------------------------------------------------------------------
      INTEGER         IDDESO     ,IDIADD          ,
     &               IDLONO          
      PARAMETER    (  IDDESO = 1 ,IDIADD = 2  ,
     &               IDLONO = 8   )
C ----------------------------------------------------------------------
      NOML32 = NOMLU
      NOML8  = NOML32(25:32)
C
      ICRE = 0
      CALL JJVERN ( NOML32 , ICRE , IRET )
      INAT  = IRET
      INATB = IRET
      GOTO ( 1010 , 1020 , 1030 ) ,IRET+1
C ----     IRET = 0
 1010    CONTINUE
         CALL U2MESK('F','JEVEUX_26',1,NOML32(1:24))
         GOTO 100
C ----     IRET = 1
 1020    CONTINUE
         GENRI =  GENR( JGENR(ICLAOS) + IDATOS )
         TYPEI =  TYPE( JTYPE(ICLAOS) + IDATOS )
         LTYPI =  LTYP( JLTYP(ICLAOS) + IDATOS )
         IF ( GENRI .EQ. 'N' ) THEN
            CALL U2MESK('F','JEVEUX1_20',1,NOML32)
         ENDIF
         GOTO 100
C ----     IRET = 2
 1030    CONTINUE
         CALL JJALLC ( ICLACO , IDATCO , 'E' , IBACOL )
         IXIADD = ISZON ( JISZON + IBACOL + IDIADD )
         IXDESO = ISZON ( JISZON + IBACOL + IDDESO )
         IF ( NOML8 .EQ. '$$XATR  ') THEN
            IXLONO = NUMATR
            IBLONO = IADM ( JIADM(ICLACO) + 2*IXLONO-1 )
            GENRI  = GENR ( JGENR(ICLACO) + IXLONO )
            LTYPI  = LTYP ( JLTYP(ICLACO) + IXLONO )
            LONOI  = LONO ( JLONO(ICLACO) + IXLONO ) * LTYPI
            CALL JXLOCS ( ZI, GENRI, LTYPI, LONOI, IBLONO, .FALSE.,
     &                    JCTAB)
            GOTO 1000
         ELSE
           IF ( NOML8 .NE. '        ') THEN
             INAT  = 3
             CALL JJCROC ( NOML8 , ICRE )
C            ------ CAS D'UN OBJET DE COLLECTION  ------
             IF ( IXIADD .NE. 0 ) INATB = 3
           ELSE
             IF ( IXIADD .NE. 0 ) THEN
C            ----------- COLLECTION DISPERSEE
                CALL U2MESK('F','JEVEUX1_21',1,NOML32)
             ENDIF
           ENDIF
           GENRI =  GENR( JGENR(ICLACO) + IXDESO )
           TYPEI =  TYPE( JTYPE(ICLACO) + IXDESO )
           LTYPI =  LTYP( JLTYP(ICLACO) + IXDESO )
         ENDIF
 100  CONTINUE
      CALL JJALTY ( TYPEI , LTYPI , 'E' , INATB , JCTAB )
      IF ( INAT .EQ. 3 .AND. IXIADD .EQ. 0 ) THEN
        IXLONO = ISZON ( JISZON + IBACOL + IDLONO )
        IF ( IXLONO .GT. 0 ) THEN
          IBLONO = IADM  ( JIADM(ICLACO) + 2*IXLONO-1 )
          LONOI  = ISZON(JISZON+IBLONO-1+IDATOC+1) -
     &                      ISZON(JISZON+IBLONO-1+IDATOC  )
          IF ( LONOI .GT. 0 ) THEN
            JCTAB  = JCTAB +  (ISZON(JISZON+IBLONO-1+IDATOC) - 1)
          ELSE
            CALL U2MESK('F','JEVEUX1_22',1,NOML32)
          ENDIF
        ELSE
          JCTAB = JCTAB + LONG(JLONG(ICLACO)+IXDESO) * (IDATOC-1)
        ENDIF
      ENDIF
 1000 CONTINUE
C
      JINI = JCTAB + I1 - 1
      J1 = 0
      J2 = NI - 1
      IF ( TYPEI .EQ. 'I' ) THEN
         DO 20 I = J1 , J2
            ZI(JINI+I) = 0
   20    CONTINUE
      ELSE IF ( TYPEI .EQ. 'R' ) THEN
         DO 30 I = J1 , J2
            ZR(JINI+I) = 0.D0
   30    CONTINUE
      ELSE IF ( TYPEI .EQ. 'C' ) THEN
         DO 40 I = J1 , J2
            ZC(JINI+I) = (0.D0,0.D0)
   40    CONTINUE
      ELSE IF ( TYPEI .EQ. 'L' ) THEN
         DO 50 I = J1 , J2
            ZL(JINI+I) = .FALSE.
   50    CONTINUE
      ELSE IF ( TYPEI .EQ. 'K' ) THEN
         IF ( LTYPI .EQ. 8 ) THEN
           DO 60 I = J1 , J2
             ZK8(JINI+I) = ' '
   60      CONTINUE
         ELSE IF ( LTYPI .EQ. 16 ) THEN
           DO 61 I = J1 , J2
             ZK16(JINI+I) = ' '
   61      CONTINUE
         ELSE IF ( LTYPI .EQ. 24 ) THEN
           DO 62 I = J1 , J2
             ZK24(JINI+I) = ' '
   62      CONTINUE
         ELSE IF ( LTYPI .EQ. 32 ) THEN
           DO 63 I = J1 , J2
             ZK32(JINI+I) = ' '
   63      CONTINUE
         ELSE IF ( LTYPI .EQ. 80 ) THEN
           DO 64 I = J1 , J2
             ZK80(JINI+I) = ' '
   64      CONTINUE
         ENDIF
      ENDIF
C
      END
