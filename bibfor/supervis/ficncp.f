      SUBROUTINE FICNCP(NOMCON,JER,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     NOMCON
      INTEGER                  JER,IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 11/04/97   AUTEUR VABHHTS J.PELLET 
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
C     ------------------------------------------------------------------
C OUT JER : I : ADRESSE DE NOMCON
C     ------------------------------------------------------------------
C
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
C
      INTEGER      IVAL
      REAL*8       RVAL(2)
      CHARACTER*8  NOM
      CHARACTER*16 CTYP
C     ------------------------------------------------------------------
      CALL JEMARQ()
      NOM = NOMCON
      JER = 0
      CALL JEEXIN(NOM,IRET)
      IF (IRET.EQ.0) THEN
         IER = IER + 1
         GOTO 9999
      ELSE
         CALL JELIRA(NOM,'LONMAX',ILONG,CTYP)
         IF (ILONG.GT.1) THEN
            IER = IER + 1
            CALL UTMESS('E','SUPERVISEUR.(ERREUR.FICNCP.02)',
     +                                 ' "'//NOM//'" TROP DE VALEURS')
         ELSE
            CALL GETTCO(NOM,CTYP)
            CALL JEVEUO(NOM,'L',LNOM)
            ITYPE = 0
            IF ( CTYP.EQ.'IS') THEN
               ITYPE   = 11
               IVAL    = ZI(LNOM)
               RVAL(1) = 0.D0
               RVAL(2) = 0.D0
            ELSEIF ( CTYP.EQ.'R8') THEN
               ITYPE   = 12
               RVAL(1) = ZR(LNOM)
               RVAL(2) = 0.D0
               IVAL    = 0
            ELSEIF ( CTYP.EQ.'C8') THEN
               ITYPE   = 15
               RVAL(1) = ZC(LNOM)
               RVAL(2) = DIMAG(ZC(LNOM))
               IVAL    = 0
            ELSEIF ( CTYP.EQ.'LS') THEN
               ITYPE   = 16
               RVAL(1) = 0.D0
               RVAL(2) = 0.D0
               IVAL    = 0
               IF ( ZL(LNOM) ) IVAL = 1
            ENDIF
            IF ( ITYPE.EQ.0 ) THEN
               IER = IER + 1
               CALL UTMESS('E','SUPERVISEUR.(ERREUR.FICNCP.03)',
     +                         ' "'//NOM//'" DE TYPE ILLICITE')
            ELSE
               CALL FIREMP(0,ITYPE,IVAL,RVAL,NOM,JER )
            ENDIF
         ENDIF
      ENDIF
 9999 CONTINUE
      CALL JEDEMA()
      END
