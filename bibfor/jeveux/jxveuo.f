      SUBROUTINE JXVEUO (CEL , ITAB , INAT , JITAB)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 19/12/2012   AUTEUR PELLET J.PELLET 
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
      INCLUDE 'jeveux_private.h'
      INTEGER                  ITAB(*),INAT , JITAB
      CHARACTER*(*)      CEL
C ----------------------------------------------------------------------
C MISE EN MEMOIRE D'UN SEGMENT DE VALEUR
C ROUTINE AVEC ADHERENCE SYSTEME CRAY : SHIFTR AND
C
C IN  CEL    : ACCES : 'L' OU 'E'
C IN  ITAB   : TABLEAU PAR RAPPORT AUQUEL L'ADRESSE EST CALCULEE
C IN  INAT   : TYPE D'OBJET 1:OS, 2:CO, 3:OC
C OUT JITAB  : ADRESSE PAR RAPPORT A ITAB
C ----------------------------------------------------------------------
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
C ----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER IADYN ,IBACOL ,IBIADD ,IBIADM ,IBLONG ,IBLONO ,IBLUTI
      INTEGER IBMARQ ,IC ,IDCO ,IDOS ,ISTA ,IUSA ,IXDESO
      INTEGER IXIADD ,IXIADM ,IXLONG ,IXLONO ,IXLUTI ,IXMARQ ,JCARA
      INTEGER JDATE ,JDOCU ,JGENR ,JHCOD ,JIADD ,JIADM ,JLONG
      INTEGER JLONO ,JLTYP ,JLUTI ,JMARQ ,JORIG ,JRNOM ,JTYPE
      INTEGER K ,LONGJ ,LONOJ ,LONOK ,LONTI ,LUTILO ,N,JJPREM

C-----------------------------------------------------------------------
      PARAMETER  ( N = 5 )
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     &                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
C
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
C ----------------------------------------------------------------------
      INTEGER            IDEHC
      PARAMETER      ( IDEHC=6)
C ----------------------------------------------------------------------
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      INTEGER          ISTAT
      COMMON /ISTAJE/  ISTAT(4)
      INTEGER          LBIS , LOIS , LOLS , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOR8 , LOC8
C ----------------------------------------------------------------------
      CHARACTER*1      TYPEI , GENRI
      INTEGER          LTYPI , IADDI(2) , IADMI ,  LONOI , IRT
      LOGICAL          LDEPS  , LCONST
C ----------------------------------------------------------------------
      INTEGER        IVNMAX     , IDDESO     , IDIADD     , IDIADM     ,
     &               IDMARQ           ,              IDLONG     ,
     &               IDLONO     , IDLUTI
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 , IDIADD = 2 , IDIADM = 3 ,
     &               IDMARQ = 4  ,              IDLONG = 7 ,
     &               IDLONO = 8 , IDLUTI = 9  )
C DEB ------------------------------------------------------------------
      JITAB = 0
      IRT = 0
      GOTO ( 10 , 20 , 30 ) , INAT
C
C ----- INAT =  1 : OBJET SIMPLE
C
 10   CONTINUE
        IC     = ICLAOS
        IDOS   = IDATOS
        IDCO   = 0
        IXDESO = IDATOS
        GENRI  = GENR ( JGENR(IC) + IDOS )
        TYPEI  = TYPE ( JTYPE(IC) + IDOS )
        LTYPI  = LTYP ( JLTYP(IC) + IDOS )
        LONOI  = LONO ( JLONO(IC) + IDOS ) * LTYPI
        IADMI  = IADM ( JIADM(IC) + 2*IDOS-1 )
        IADYN  = IADM ( JIADM(IC) + 2*IDOS   )
        IADDI(1) = IADD ( JIADD(IC) + 2*IDOS-1 )
        IADDI(2) = IADD ( JIADD(IC) + 2*IDOS   )
      GOTO 99
C
C ----- INAT = 2 : COLLECTION
C
 20   CONTINUE
        IC     = ICLACO
        IBACOL = IADM  ( JIADM(IC) + 2*IDATCO-1 )
        IXDESO = ISZON ( JISZON + IBACOL + IDDESO )
        IDOS   = IXDESO
        IDCO   = 0
        GENRI  = GENR ( JGENR(IC) + IXDESO )
        TYPEI  = TYPE ( JTYPE(IC) + IXDESO )
        LTYPI  = LTYP ( JLTYP(IC) + IXDESO )
        IXLONG = ISZON ( JISZON + IBACOL + IDLONG )
        LCONST = ( IXLONG .EQ. 0 )
        IXLONO = ISZON ( JISZON + IBACOL + IDLONO )
        IXLUTI = ISZON ( JISZON + IBACOL + IDLUTI )
        IF ( LCONST ) THEN
          IF ( LONG(JLONG(IC)+IXDESO) .NE. 0 ) THEN
            LONOI  = LONO ( JLONO(IC) + IXDESO ) * LTYPI
          ELSE
            CALL U2MESS('F','JEVEUX1_62')
          ENDIF
        ELSE
          IBLONO = IADM ( JIADM(IC) + 2*IXLONO-1 )
          IBLONG = IADM ( JIADM(IC) + 2*IXLONG-1 )
          LONTI  = LONO ( JLONO(IC) + IXDESO )
          LUTILO = LUTI ( JLUTI(IC) + IXLONO )
          IF ( LUTILO .EQ. 0 ) THEN
            K = 1
            ISZON(JISZON+IBLONO-1+K) = 1
 5          CONTINUE
            IF ( K .LE. ISZON(JISZON + IBACOL + IVNMAX) ) THEN
              LONGJ = ISZON(JISZON+IBLONG-1+K)
              IF ( LONGJ .GT. 0 ) THEN
                IF ( GENRI .EQ. 'V' ) THEN
                  LONOJ = LONGJ
                ELSE IF ( GENRI .EQ. 'N' ) THEN
                  LONOK = (IDEHC+JJPREM(LONGJ,IRT))*LOIS+(LONGJ+1)*LTYPI
                  IF ( MOD(LONOK,LTYPI) .GT. 0 ) THEN
                    LONOK = (LONOK/LTYPI + 1 )
                  ELSE
                    LONOK = LONOK/LTYPI
                  ENDIF
                  LONOJ = LONOK
                  IBLUTI = IADM ( JIADM(IC) + 2*IXLUTI-1 )
                  ISZON(JISZON+IBLUTI-1+K) = 0
                ENDIF
              ELSE
                LONOJ = 0
              ENDIF
              ISZON(JISZON+IBLONO-1+K+1)=LONOJ+ISZON(JISZON+IBLONO-1+K)
              K = K + 1
              LUTILO = LUTILO + 1
              GOTO 5
            ENDIF
            LUTI ( JLUTI(IC) + IXLONO ) = LUTILO
          ENDIF
          IF ( LONTI .NE. 0 ) THEN
            LONOI = LONTI * LTYPI
          ELSE
            LONTI = ISZON(JISZON+ IBLONO -1 + LUTILO + 1 ) - 1
            LONOI = LTYPI * LONTI
            LONO ( JLONO(IC) + IXDESO ) = LONOI / LTYPI
            LUTI ( JLUTI(IC) + IXDESO ) = LUTILO
          ENDIF
        ENDIF
        IADMI   = IADM ( JIADM(IC) + 2*IXDESO-1 )
        IADYN   = IADM ( JIADM(IC) + 2*IXDESO   )
        IADDI(1)  = IADD  ( JIADD(IC) + 2*IXDESO-1 )
        IADDI(2)  = IADD  ( JIADD(IC) + 2*IXDESO   )
        IF ( IADMI .EQ. 0 ) THEN
          IF ( IADDI(1) .NE. 0 ) THEN
            CALL JJALLS ( LONOI, IC, GENRI, TYPEI, LTYPI,
     &                   'NOINIT', ITAB, JITAB, IADMI, IADYN)
            CALL JXLIRO ( IC , IADMI , IADDI , LONOI )
          ELSE
            CALL JJALLS ( LONOI, IC, GENRI, TYPEI, LTYPI,
     &                   'INIT  ', ITAB, JITAB, IADMI, IADYN)
          ENDIF
          IADM( JIADM(IC) + 2*IXDESO-1 ) = IADMI
          IADM( JIADM(IC) + 2*IXDESO   ) = IADYN
          CALL JJECRS (IADMI,IC,IXDESO,0,CEL,
     &           IMARQ(JMARQ(IC)+2*IXDESO-1))
        ENDIF
      GOTO 99
C
C ----- INAT = 3 : OBJET DE COLLECTION
C
 30   CONTINUE
        IC     = ICLACO
        IDCO   = IDATCO
        IDOS   = IDATOC
        IBACOL = IADM ( JIADM(IC) + 2*IDATCO-1 )
        IXDESO = ISZON ( JISZON + IBACOL + IDDESO )
        GENRI = GENR(JGENR(IC)+IXDESO)
        TYPEI = TYPE(JTYPE(IC)+IXDESO)
        LTYPI = LTYP(JLTYP(IC)+IXDESO)
        IXLONO = ISZON ( JISZON + IBACOL + IDLONO )
        IF ( IXLONO .EQ. 0 ) THEN
          LONOI = LONO ( JLONO(IC) + IXDESO ) * LTYPI
        ELSE
          IBLONO = IADM ( JIADM(IC) + 2*IXLONO-1 )
          LONOI  = ISZON ( JISZON + IBLONO - 1 + IDATOC ) * LTYPI
        ENDIF
        IXIADM = ISZON ( JISZON + IBACOL + IDIADM )
        IXIADD = ISZON ( JISZON + IBACOL + IDIADD )
        IXMARQ = ISZON ( JISZON + IBACOL + IDMARQ )
        IBIADM = IADM  ( JIADM(IC) + 2*IXIADM-1 )
        IBIADD = IADM  ( JIADM(IC) + 2*IXIADD-1 )
        IBMARQ = IADM  ( JIADM(IC) + 2*IXMARQ-1 )
        IADMI  = ISZON ( JISZON + IBIADM - 1 + 2*IDATOC-1 )
        IADYN  = ISZON ( JISZON + IBIADM - 1 + 2*IDATOC   )
        IADDI(1)  = ISZON ( JISZON + IBIADD - 1 + 2*IDATOC-1 )
        IADDI(2)  = ISZON ( JISZON + IBIADD - 1 + 2*IDATOC   )
 99   CONTINUE
C
      IF ( IADMI .EQ. 0 ) THEN
C
C ----- PAS DE SEGMENT EN MEMOIRE
C
        IF ( IADDI(1) .EQ. 0 ) THEN
C
C ------- PAS D'IMAGE DISQUE
C
          IF ( CEL .EQ. 'E' ) THEN
            CALL JJALLS( LONOI, IC, GENRI, TYPEI, LTYPI, 'INIT',
     &                   ITAB, JITAB, IADMI, IADYN)
          ELSE
            CALL U2MESS('F','JEVEUX1_61')
          ENDIF
        ELSE
C
C ------- AVEC  IMAGE DISQUE
C
          CALL JJALLS( LONOI, IC, GENRI, TYPEI, LTYPI, 'NOINIT',
     &                 ITAB, JITAB, IADMI, IADYN)
          CALL JXLIRO ( IC , IADMI , IADDI , LONOI )
        ENDIF
      ELSE
C
C ----- SEGMENT EN MEMOIRE
C
        CALL JJLIRS ( IADMI, IC, IDOS, IUSA, ISTA )
        LDEPS = .FALSE.
        IF ( IUSA .NE. ISTAT(2) ) LDEPS = .TRUE.
        CALL JXLOCS (ITAB,GENRI,LTYPI,LONOI,IADMI,LDEPS,JITAB)
      ENDIF
C
      IF ( INAT .EQ. 3 ) THEN
        ISZON ( JISZON + IBIADM - 1 + 2*IDATOC-1 ) = IADMI
        ISZON ( JISZON + IBIADM - 1 + 2*IDATOC   ) = IADYN
        ISZON ( JISZON + IBIADD - 1 + 2*IDATOC-1 ) = IADDI(1)
        ISZON ( JISZON + IBIADD - 1 + 2*IDATOC   ) = IADDI(2)
        CALL JJECRS (IADMI,IC, IDOS, IDCO, CEL,
     &           ISZON (JISZON+IBMARQ-1+2*IDATOC-1))
      ELSE
        IADM( JIADM(IC) + 2*IXDESO-1 ) = IADMI
        IADM( JIADM(IC) + 2*IXDESO   ) = IADYN
        IADD( JIADD(IC) + 2*IXDESO-1 ) = IADDI(1)
        IADD( JIADD(IC) + 2*IXDESO   ) = IADDI(2)
        CALL JJECRS(IADMI,IC,IDOS,0,CEL,IMARQ(JMARQ(IC)+2*IDOS-1))
      ENDIF
C FIN ------------------------------------------------------------------
      END
