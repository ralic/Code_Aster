      SUBROUTINE MOTUBN ( TABPUS, DINST, NBSECT )
      IMPLICIT  NONE
      INTEGER             NBSECT
      REAL*8              DINST
      CHARACTER*(*)       TABPUS
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C     OPERATEUR  "POST_USURE"
C     REMPLACEMENT DU TUBE PERCE PAR UN TUBE NEUF
C
C ----------------------------------------------------------------------
C     ---- DEBUT DES COMMUNS JEVEUX ------------------------------------
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
      CHARACTER*24 VALK
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ---- FIN DES COMMUNS JEVEUX --------------------------------------

      INTEGER       IBID, I, IRET, NUMELI
      INTEGER VALI
      REAL*8        ZERO, LPREC(2), ACCES(2)
      REAL*8 VALR
      COMPLEX*16    C16B
      CHARACTER*8   K8B, LCRIT(2)
      CHARACTER*19  NOMTA
      CHARACTER*16  VALEK(2)
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NOMTA = TABPUS
      LPREC(1) = 1.D-06
      LPREC(2) = 1.D-06
      LCRIT(1) = 'RELATIF '
      LCRIT(2) = 'RELATIF '
      ZERO = 0.0D0
C
C --- LES PARAMETRES A REMETTRE A 0.
C        POUR L'INSTANT TRAITE:
C            V_USUR_TUBE , P_USUR_TUBE , 
C        POUR L'INSTANT TRAITE ET PAR SECTEUR:
C            V_USUR_TUBE_SECT , P_USUR_TUBE_SECT , V_USUR_TUBE_CUMU
C
      VALEK(1) = 'INST'
      ACCES(1) = DINST
C
      VALEK(2) = 'V_USUR_TUBE'
C
C     VERIFICATION DES PARAMETRES DE LA TABLE
      CALL TBEXP2(NOMTA,'INST')
      CALL TBEXP2(NOMTA,'SECTEUR')
      CALL TBEXP2(NOMTA,'V_USUR_TUBE')
      CALL TBEXP2(NOMTA,'P_USUR_TUBE')
      CALL TBEXP2(NOMTA,'V_USUR_TUBE_SECT')
      CALL TBEXP2(NOMTA,'P_USUR_TUBE_SECT')
      CALL TBEXP2(NOMTA,'V_USUR_TUBE_CUMU')
C
      CALL TBLIVA ( NOMTA, 1,VALEK, IBID,ACCES(1),C16B,K8B,LCRIT(1),
     +              LPREC(1), VALEK(2),K8B,IBID,ACCES(2),C16B,K8B,IRET)
      IF (IRET.NE.0) THEN
            VALR = DINST
            VALK = VALEK(2)
         CALL U2MESG('F', 'PREPOST5_57',1,VALK,0,0,1,VALR)
      ENDIF
C
      CALL TBNULI ( NOMTA, 2, VALEK, IBID , ACCES, C16B, K8B,
     +                                         LPREC, LCRIT, NUMELI )
      IF (NUMELI.LE.0) THEN
            VALR = DINST
            VALK = VALEK(2)
         CALL U2MESG('F', 'PREPOST5_58',1,VALK,0,0,1,VALR)
      ENDIF
C
      CALL TBACCE ( NOMTA, NUMELI, VALEK(2),'E', IBID,ZERO,C16B,K8B)
C
      VALEK(2) = 'P_USUR_TUBE'
      CALL TBACCE ( NOMTA, NUMELI, VALEK(2),'E', IBID,ZERO,C16B,K8B)
C
      VALEK(2) = 'SECTEUR'
C
      DO 20 I = 1 , NBSECT
C
         CALL TBNULI ( NOMTA, 2, VALEK, I , ACCES(1), C16B, K8B,
     +                                     LPREC(1), LCRIT(1), NUMELI )
         IF (NUMELI.LE.0) THEN
            VALR = DINST
            VALI = I
            CALL U2MESG('F', 'PREPOST5_59',0,' ',1,VALI,1,VALR)
         ENDIF
C
         CALL TBACCE ( NOMTA, NUMELI, 'V_USUR_TUBE_SECT', 'E',
     +                                          IBID, ZERO, C16B, K8B )
C
         CALL TBACCE ( NOMTA, NUMELI, 'P_USUR_TUBE_SECT', 'E',
     +                                          IBID, ZERO, C16B, K8B )
C
         CALL TBACCE ( NOMTA, NUMELI, 'V_USUR_TUBE_CUMU', 'E',
     +                                          IBID, ZERO, C16B, K8B )
C        
 20   CONTINUE
C
      CALL JEDEMA()
      END
