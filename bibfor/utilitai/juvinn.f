      SUBROUTINE JUVINN(OJB)
      IMPLICIT NONE
      CHARACTER*(*)     OJB
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 23/10/2007   AUTEUR SALMONA L.SALMONA 
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
C     BUT : REMPLACER DANS L'OBJET JEVEUX DE NOM OJB (DE TYPE 'R')
C           LES VALEURS R8VIDE() PAR R8NNEM()
C     ------------------------------------------------------------------
C IN  OJB  : K24 : NOM DE L'OBJET A MODIFIER
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
      CHARACTER*8  TYPE, CBID
      INTEGER N1,K,JOJB,IBID,IISNAN
      REAL*8 R8VIDE,R8NNEM,RVID,RNAN
      CHARACTER*32 VALK(2)
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL JELIRA(OJB,'TYPE  ',IBID  ,TYPE)
      CALL ASSERT(TYPE.EQ.'R')

      CALL JEVEUO(OJB,'L',JOJB)
      CALL JELIRA(OJB,'LONMAX',N1,CBID)

      RVID=R8VIDE()
      RNAN=R8NNEM()
      DO 1, K=1,N1
         IF (IISNAN(ZR(JOJB-1+K)).EQ.0) THEN
           IF (ZR(JOJB-1+K).EQ.RVID) ZR(JOJB-1+K)=RNAN
         ENDIF
 1    CONTINUE

      CALL JEDEMA()
      END
