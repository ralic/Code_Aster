      SUBROUTINE DXTEMA ( NNO, NBPAR, NOMPAR, VALPAR )
      IMPLICIT   NONE
      INTEGER             NNO, NBPAR
      REAL*8                                   VALPAR
      CHARACTER*8                     NOMPAR
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 06/03/2002   AUTEUR CIBHHLV L.VIVAN 
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
C     RECUPERATION DE LA TEMPERATURE POUR LE MATERIAU
C
C     ------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                     ZK24
      CHARACTER*32                              ZK32
      CHARACTER*80                                       ZK80
      COMMON /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER       I, IBID, ITEMPR, ITEMPF, IER ,ITAB(8),IRET
      REAL*8        TPG1, T, TINF, TSUP, VALPU(2)
      CHARACTER*8   NOMU, NOMPU(2)
      CHARACTER*16  CONCEP, NOMCMP
C     ------------------------------------------------------------------
C
      NBPAR = 0
      NOMPAR = ' '
      VALPAR = 0.D0
C
      CALL TECAC2 ( 'NNN', 'PTEMPER', 8, ITAB ,IRET)
      ITEMPR=ITAB(1)
      CALL TECACH(.FALSE.,.FALSE.,'PTEMPEF',1,ITEMPF)
C
C --- SI LA TEMPERATURE EST CONNUE AUX NOEUDS :
C
      IF ( ITEMPR .NE. 0 ) THEN
         NBPAR = 1
         NOMPAR = 'TEMP'
         TPG1 = 0.D0
         DO 20 I = 1,NNO
            CALL DXTPIF(ZR(ITEMPR+3*(I-1)),ZL(ITAB(8)+3*(I-1)))
            T    = ZR(ITEMPR  +3*(I-1))
            TINF = ZR(ITEMPR+1+3*(I-1))
            TSUP = ZR(ITEMPR+2+3*(I-1))

            IF ((T.NE.TINF).OR.(T.NE.TSUP).OR.(TINF.NE.TSUP)) THEN
               CALL GETRES ( NOMU, CONCEP, NOMCMP )
               IF ( NOMCMP.NE.'STAT_NON_LINE' .AND.
     &              NOMCMP.NE.'DYNA_NON_LINE' .AND.
     &              NOMCMP.NE.'POST_ELEM'     .AND.
     &              NOMCMP.NE.'CALC_NO'       .AND.
     &              NOMCMP.NE.'CALC_ELEM'     ) THEN
                  CALL UTMESS('A',NOMCMP,'LORSQU''IL Y A VARIATION '//
     &    'DE TEMPERATURE DANS L''EPAISSEUR, UTILISER "STAT_NON_LINE"')
               ENDIF
            ENDIF
            TPG1 = TPG1 + T + ( TSUP + TINF - 2*T ) / 6.D0
   20    CONTINUE
         VALPAR = TPG1 / NNO
      ENDIF
C
C --- SI LA TEMPERATURE EST UNE FONCTION DE 'INST' ET 'EPAIS' :
C
      IF ( ITEMPF .NE. 0 ) THEN
C
         CALL GETRES ( NOMU, CONCEP, NOMCMP )
         IF ( NOMCMP.NE.'STAT_NON_LINE' .AND.
     &        NOMCMP.NE.'DYNA_NON_LINE' .AND.
     &        NOMCMP.NE.'POST_ELEM'     .AND.
     &        NOMCMP.NE.'CALC_NO'       .AND.
     &        NOMCMP.NE.'CALC_ELEM'     ) THEN
            CALL UTMESS('A',NOMCMP,'LORSQU''IL Y A VARIATION '//
     &    'DE TEMPERATURE DANS L''EPAISSEUR, UTILISER "STAT_NON_LINE"')
         ENDIF
C
         NBPAR = 1
         NOMPAR = 'TEMP'
         NOMPU(1) = 'INST'
         NOMPU(2) = 'EPAIS'
         CALL JEVECH ( 'PTEMPSR', 'L', IBID )
         VALPU(1) = ZR(IBID)
         VALPU(2) = 0.D0
         CALL FOINTE ( 'FM', ZK8(ITEMPF), 2, NOMPU, VALPU, VALPAR, IER)
C
      ENDIF
C
      END
