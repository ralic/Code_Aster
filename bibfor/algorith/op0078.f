      SUBROUTINE OP0078()
      IMPLICIT NONE

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/03/2011   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     OPERATEUR REST_COND_TRAN
C
C ----------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ---------------------------
C
      CHARACTER*8  NOMRES,RESIN
      CHARACTER*16 NOMCMD,TYPRES,CHAMP(4)
      CHARACTER*19 PROFNO
      INTEGER      I,J
      INTEGER      IR,JREFN,NBCHAM
C
C     -----------------------------------------------------------------


      CALL JEMARQ()
      CALL INFMAJ()
C
C     -----------------------------------------------------------------
C
C
      CALL GETRES(NOMRES,TYPRES,NOMCMD)
C
C --- PHASE DE TEST SUR LES CHAMPS A RESTITUER
C
      CALL GETVTX(' ','NOM_CHAM',1,1,4,CHAMP,NBCHAM)
      IF (NBCHAM.LT.0) THEN
        CALL U2MESS('E','ALGORITH9_44')
      ELSE
        DO 20 I=1,NBCHAM
          DO 10 J=I+1,NBCHAM
            IF (CHAMP(I).EQ.CHAMP(J)) THEN
              CALL U2MESS('E','ALGORITH9_30')
            ENDIF
   10     CONTINUE   
   20   CONTINUE
      ENDIF
C

C --- CREATION DU .REFN DU PROFIL :
C     ---------------------------
      PROFNO=NOMRES//'.PROFC.NUME'
      CALL JEEXIN(PROFNO(1:19)//'.REFN',IR  )
      IF ( IR .GT. 0 ) THEN
        CALL JEVEUO(PROFNO(1:19)//'.REFN','L',JREFN )
      ELSE       
        CALL WKVECT(PROFNO(1:19)//'.REFN','G V K24',4,JREFN)
      END IF 
      ZK24(JREFN+1)='DEPL_R'
C
      CALL GETVID(' ','RESULTAT',1,1,1,RESIN,IR)
      CALL TRAN78(NOMRES,TYPRES,RESIN)
C 
      CALL JEDEMA()
      END
