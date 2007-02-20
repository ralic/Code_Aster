      FUNCTION JJCODN(ICRE , NOMREP , NOMEC , IREP, CREP , NMAX , NUTI )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 19/02/2007   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C TOLE CRS_512
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)          NOMREP , NOMEC ,       CREP(*)
      INTEGER         ICRE                  , IREP(*)    , NMAX , NUTI
C ----------------------------------------------------------------------
      INTEGER          ILOREP , IDENO , ILNOM , ILMAX , ILUTI , IDEHC
      PARAMETER      ( ILOREP=1,IDENO=2,ILNOM=3,ILMAX=4,ILUTI=5,IDEHC=6)
      INTEGER          IRET
      CHARACTER*32     CLE , NOM
      CHARACTER*75     CMESS
      CHARACTER*6      PGME
      PARAMETER      ( PGME = 'JJCODN' )
      LOGICAL          RINSER
C DEB ------------------------------------------------------------------
      JJCODN = 0
      RINSER = .FALSE.
      IRET   = 0
      LOREP  = IREP(ILOREP)
      IDENOM = IREP(IDENO )
      LNOM   = IREP(ILNOM)
      IDEHCO = IREP(IDEHC )
      LL     = MIN ( LNOM , LEN(NOMEC) )
      NOM    = NOMEC(1:LL)
      I      = JXHCOD (NOM,LOREP)
      NE     = 1
C
    5 CONTINUE
      IF ( IREP(IDEHCO+I) .EQ. 0 .AND. .NOT. RINSER ) THEN
         IF ( ICRE .EQ. 3  ) THEN
            IF ( NUTI .GE. NMAX ) THEN
               CMESS = ' LE REPERTOIRE '//NOMREP//' EST SATURE '
               CALL U2MESK('F','JEVEUX_01',1,CMESS)
            ELSE
               J = NUTI + 1
               DO 12 K = 1 , LL
                 CREP(IDENOM+LNOM*(J-1)+K) = NOMEC(K:K)
   12          CONTINUE
               NUTI = NUTI + 1
               IREP(ILUTI) = NUTI
               IREP(IDEHCO+I) = J
               IRET          = J
            ENDIF
         ELSE
            IF ( ICRE .EQ. 0 )   THEN
              IRET = 0
            ELSE
              CMESS =' LE NOM N''EXISTE PAS DANS '//NOMREP
              CALL U2MESK('F','JEVEUX_01',1,CMESS)
            END IF
         END IF
      ELSE
         J = IREP(IDEHCO+I)
         DO 15 K = 1 , LL
             CLE(K:K) = CREP(IDENOM+LNOM*(ABS(J)-1)+K)
   15    CONTINUE
         DO 16 K = LL+1 , 32
             CLE(K:K) = ' '
   16    CONTINUE
         IF ( CLE .EQ. NOM ) THEN
            IF ( ICRE .EQ. 3 ) THEN
              CMESS=' LE NOM DEMANDE EXISTE DEJA DANS LE REPERTOIRE '
     &                //NOMREP
              CALL U2MESK('F','JEVEUX_01',1,CMESS)
            ELSE IF ( ICRE .EQ. 0 ) THEN
              IRET  = J
            ELSE IF ( ICRE .EQ. -3 ) THEN
              IREP(IDEHCO+I) = -J
              CREP(IDENOM+LNOM*(J-1)+1) = '?'
              IRET  = -J
            END IF
         ELSE
            IF ( J .LT. 0 .AND. .NOT. RINSER ) THEN
               IF ( ICRE .EQ. 3 ) THEN
                  RINSER = .TRUE.
                  JIN = J
                  IIN = I
               ENDIF
            ENDIF
            IF ( NE .EQ. 1 ) IN = JXHCOD (NOM,LOREP-2)
            NE = NE + 1
            I = 1 + MOD (I+IN,LOREP)
            IF ( NE .LE. LOREP ) THEN
               J =  IREP ( IDEHCO + I )
               IF ( J .EQ. 0 .AND. RINSER ) GOTO 10
               GOTO 5
            ELSE
               IF ( ICRE .EQ. 3 ) THEN
                  CMESS = 'POUR LE REPERTOIRE '//NOMREP//' TROP '//
     &                    'DE COLLISION DANS LE HCODING.'
                  CALL U2MESK('F','JEVEUX_01',1,CMESS)
               ELSE IF ( ICRE .EQ. 0 )   THEN
                  IRET = 0
               ENDIF
            END IF
         END IF
      END IF
 10   CONTINUE
      IF ( RINSER ) THEN
         IREP(IDEHCO+IIN) = -JIN
         DO 25 K = 1 , LL
           CREP(IDENOM+LNOM*(-JIN-1)+K) = NOMEC(K:K)
 25      CONTINUE
         IRET  = -JIN
      ENDIF
      JJCODN = IRET
C FIN ------------------------------------------------------------------
      END
