      SUBROUTINE OPS004( ICMD , ICOND, IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            ICMD , ICOND, IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 13/02/2001   AUTEUR DURAND C.DURAND 
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
C     OPERATEUR "DEFI_VALEUR"
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
      CHARACTER*2  BASE
      CHARACTER*3  TYPE
      CHARACTER*8  NOMRES
      CHARACTER*16 NOMCMD,CONCEP
C     ------------------------------------------------------------------
      CALL JEMARQ()
      IF (ICOND.EQ.-1.OR.ICOND.EQ.0) THEN
         IER  = 1
         BASE = 'L'
         IF (ICOND.EQ.0) BASE = 'G'
         CALL GETRES(NOMRES,CONCEP,NOMCMD)
         CALL JEEXIN(NOMRES,IRET)
         IF (IRET .NE. 0) CALL JEDETR(NOMRES)
         ILG  = LXLGUT(CONCEP)
         IDEB = 1
         IF (ILG .EQ.3 ) IDEB = 2
         TYPE = CONCEP(IDEB:ILG)
         CALL GETVR8(' ','R8',1,1,0,    ZR(1),NBVAL)
         IF(NBVAL.NE. 0 ) THEN
            NBVAL = -NBVAL
            CALL WKVECT(NOMRES,BASE//' V R',NBVAL,LVAL)
            CALL GETVR8(' ','R8',1,1,NBVAL,ZR(LVAL),L)
            IER = 0
         ENDIF
         CALL GETVIS(' ','IS',1,1,0    ,ZI(1),NBVAL)
         IF(NBVAL.NE. 0 ) THEN
            NBVAL = -NBVAL
            CALL WKVECT(NOMRES,BASE//' V I',NBVAL,LVAL)
            CALL GETVIS(' ','IS',1,1,NBVAL,ZI(LVAL),L)
            IER = 0
         ENDIF
         CALL GETVC8(' ','C8',1,1,0    ,ZC(1),NBVAL)
         IF(NBVAL.NE. 0 ) THEN
            NBVAL = -NBVAL
            CALL WKVECT(NOMRES,BASE//' V C',NBVAL,LVAL)
            CALL GETVC8(' ','C8',1,1,NBVAL,ZC(LVAL),L)
            IER = 0
         ENDIF
         CALL GETVTX(' ','TX',1,1,0    ,ZK80(1),NBVAL)
         IF(NBVAL.NE. 0 ) THEN
            NBVAL = -NBVAL
            CALL WKVECT(NOMRES,BASE//' V K80',NBVAL,LVAL)
            CALL GETVTX(' ','TX',1,1,NBVAL,ZK80(LVAL),L)
            IER = 0
         ENDIF
         CALL GETVLS(' ','LS',1,1,0    ,ZI(1),NBVAL)
         IF(NBVAL.NE. 0 ) THEN
            NBVAL = -NBVAL
            CALL WKVECT(NOMRES,BASE//' V I  ',NBVAL,LVAL)
            CALL GETVLS(' ','LS',1,1,NBVAL,ZI(LVAL),L)
            IER = 0
         ENDIF
         IF (IER.NE.0)
     +        CALL UTMESS('E',NOMCMD,' "'//TYPE//'" TYPE INCONNU')
      ELSEIF ( ICOND .EQ. 1 ) THEN
         CALL GETRES(NOMRES,CONCEP,NOMCMD)
         CALL JEEXIN(NOMRES,IRET)
         IF (IRET .EQ. 0 ) THEN
C-DEL        CALL UTMESS('E',NOMCMD,' "'//NOMRES//'" OBJET INEXISTANT')
C-DEL        IER = 1
         ELSE
            CALL JEDETR(NOMRES)
         ENDIF
      ENDIF
      CALL JEDEMA()
      END
