      LOGICAL FUNCTION ZEROBJ(OBJ)
      IMPLICIT NONE
      CHARACTER*(*) OBJ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 16/12/2003   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C RESPONSABLE VABHHTS J.PELLET
C ----------------------------------------------------------------------
C  BUT : DETERMINER SI UN OBJET JEVEUX EST NUL (OU PAS)
C       OBJ     : NOM DE L'OBJET JEVEUX à TESTER
C
C     RESULTAT:
C       ZEROBJ : .TRUE.    SI LES VALEURS DE OBJ SONT TOUTES NULLES
C                .FALSE.   SINON
C ----------------------------------------------------------------------
C --------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR,JEXR8
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C ----------------------------------------------------------------------
      CHARACTER*1  TYPSCA ,XOUS, GENR, KBID
      CHARACTER*24 OBJ2
      INTEGER I, J, N, JVAL, LONG,IBID,IRET
C
C -DEB------------------------------------------------------------------
C
      CALL JEMARQ()
      ZEROBJ=.TRUE.
      OBJ2=OBJ

      CALL JELIRA(OBJ2,'TYPE',IBID,TYPSCA)
      CALL ASSERT(TYPSCA.EQ.'R' .OR. TYPSCA.EQ.'C')
      CALL JELIRA(OBJ2,'XOUS',IBID,XOUS)
      CALL JELIRA(OBJ2,'XOUS',IBID,XOUS)
      CALL JELIRA(OBJ2,'GENR',IBID,GENR)
      CALL ASSERT(GENR.EQ.'V')


C     1) CAS DES OBJETS SIMPLES :
C     --------------------------------
      IF (XOUS.EQ.'S') THEN
          CALL JEVEUO(OBJ2,'L',JVAL)
          CALL JELIRA(OBJ2,'LONMAX',LONG,KBID)

          IF (TYPSCA.EQ.'R') THEN
            DO 2, J=1,LONG
              IF (ZR(JVAL-1+J).NE.0.D0) GO TO 9998
2           CONTINUE
          ELSE
            DO 3, J=1,LONG
              IF (ZC(JVAL-1+J).NE.(0.D0,0.D0)) GO TO 9998
3           CONTINUE
          END IF
      END IF


C     2) CAS DES COLLECTIONS :
C     --------------------------------
      IF (XOUS.EQ.'X') THEN
        CALL JELIRA(OBJ2,'NMAXOC',N,KBID)

        DO 10,I=1,N
          CALL JEEXIN(JEXNUM(OBJ2,I),IRET)
          IF (IRET.EQ.0) GO TO 10
          CALL JEVEUO(JEXNUM(OBJ2,I),'L',JVAL)
          CALL JELIRA(JEXNUM(OBJ2,I),'LONMAX',LONG,KBID)

          IF (TYPSCA.EQ.'R') THEN
            DO 20, J=1,LONG
              IF (ZR(JVAL-1+J).NE.0.D0) GO TO 9998
20          CONTINUE
          ELSE
            DO 30, J=1,LONG
              IF (ZC(JVAL-1+J).NE.(0.D0,0.D0)) GO TO 9998
30          CONTINUE
          END IF
10      CONTINUE
      END IF



      GOTO 9999
 9998 CONTINUE
      ZEROBJ=.FALSE.


 9999 CONTINUE
      CALL JEDEMA()
      END
