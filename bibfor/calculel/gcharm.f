      SUBROUTINE GCHARM(FONC, CHARG, NOMFON, NOMF, TIME, IORD, CHARGM)
      IMPLICIT NONE
      CHARACTER*19    CHARG, CHARGM
      CHARACTER*24    NOMFON
      CHARACTER*8     NOMF
      INTEGER         IORD
      REAL*8          TIME
      LOGICAL         FONC
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 29/01/2007   AUTEUR REZETTE C.REZETTE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C     BUT : APPLIQUE LA FONCTION MULTIPLICATRICE SUR UNE CHARGE
C           (ROUTINE SPECIFIQUE A L'OPERATEUR CALC_G,
C            APPELEE PAR GCHARG)
C
C     IN :    FONC    :  = .TRUE. SI LE CHARGEMENT EST 'FONCTION'
C                        = .FALSE. SI LE CHARGEMENT EST 'SCALAIRE'
C             CHARG   : NOM DE LA CHARGE (ex: NOMCHA//'.CHME.F3D3D')
C             NOMFON  : NOM DE LA FONCTION MULTIPLICATRICE
C             TIME    : INSTANT (ABSCISSE DE NOMFON)
C             IORD    : NUMERO D'ORDRE CORRESPONDANT A TIME
C     IN/OUT  NOMF    : UTILISE UNIQUEMENT POUR UN CHARGEMENT 'FONCTION'
C                       PERMET DE DEFINIR LE NOM DES NOUVELLES FONCTIONS
C                       DE CHARGM (.VALE)                       
C             CHARGM  : CHARGE APRES LA PRISE EN COMPTE
C                       DE LA FONCTION MULTIPLICATRICE
C ======================================================================
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C ----------------------------------------------------------------------
C     ------------------------------------------------------------------
C
      INTEGER IVAL,JVAL,NBVALE,IRET,IN,K,I,NB,NPT,JVALF,IPROL,IBID
      REAL*8 CONST
      CHARACTER*8 K8B
      CHARACTER*19 NCH19
      
      CALL JEMARQ()

      CALL FOINTE('A',NOMFON, 1,'INST',TIME,CONST,IRET)

      CALL JEVEUO(CHARG//'.VALE','L',IVAL)
      CALL JEVEUO(CHARGM//'.VALE','E',JVAL)
      CALL JELIRA(CHARG//'.VALE' ,'LONMAX',NBVALE,K8B)
C
C --- 1. CHARGEMENT 'SCALAIRE'
C
      IF (.NOT.FONC)THEN
        DO 10 IN = 1,NBVALE
          ZR(JVAL+IN-1)  = CONST* ZR(IVAL +IN-1)
  10    CONTINUE
C
C --- 2. CHARGEMENT 'FONCTION'
C
      ELSE
        K=0
        CALL CODENT(IORD,'D0',NOMF(4:5))
        DO 20 IN = 1,NBVALE
          IF(ZK8(IVAL+IN-1)(1:7).NE.'&FOZERO' .AND.
     &       ZK8(IVAL+IN-1)(1:7).NE.'       ' .AND.
     &       ZK8(IVAL+IN-1)(1:6).NE.'GLOBAL' )THEN
            K=K+1
            CALL CODENT(K,'D0',NOMF(8:8))
            CALL COPISD('FONCTION','V',ZK8(IVAL+IN-1),NOMF)
            NCH19=NOMF
            CALL JEVEUO(NCH19//'.PROL','L',IPROL)
            IF(ZK16(IPROL)(1:8).NE.'INTERPRE')THEN
              CALL JEVEUO(NCH19//'.VALE','E',JVALF)
              CALL JELIRA(NCH19//'.VALE','LONMAX',NB,K8B)
              NPT=NB/2
              DO 30 I = 1,NPT
                ZR(JVALF+NPT+I-1)=CONST*ZR(JVALF+NPT+I-1)
  30          CONTINUE
              ZK8(JVAL+IN-1)=NOMF
            ELSE
              CALL U2MESK('A','CALCULEL5_56',1,CHARG(1:8))
              CALL JEDUPO(CHARG//'.VALE' ,'V', CHARGM//'.VALE' ,.FALSE.)
              GOTO 9999
            ENDIF
          ENDIF
  20    CONTINUE
      ENDIF

 9999 CONTINUE      
      CALL JEDEMA()
      
      END
