      SUBROUTINE ACTCIN(FONACT,CNCINX,MATASS,SOLALG)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 14/06/2011   AUTEUR TARDIEU N.TARDIEU 
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
      IMPLICIT NONE
      INTEGER      FONACT(*)
      CHARACTER*19 CNCINX,MATASS,SOLALG
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE
C
C BUT : ACTUALISER LES CHARGES CINEMATIQUES A PRENDRE EN COMPTE 
C       A CHAQUE ITERATION DE NEWTON EN CORRECTION
C
C ----------------------------------------------------------------------
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C

      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER      JCCID,NEQ,JCINX,JDEPDE,I
      CHARACTER*19 KBID,DEPDEL
      LOGICAL      ISFONC,LCINE
C
C ----------------------------------------------------------------------
C

      CALL JEMARQ()

      LCINE  = ISFONC(FONACT,'DIRI_CINE')
      IF (LCINE) THEN
        CALL JELIRA(CNCINX(1:19)//'.VALE','LONMAX',NEQ,KBID)
        CALL NMPCIN(MATASS)
        CALL JEVEUO(MATASS(1:19)//'.CCID','L',JCCID)
        CALL JEVEUO(CNCINX(1:19)//'.VALE','E',JCINX )
        CALL NMCHEX(SOLALG,'SOLALG','DEPDEL',DEPDEL)
        CALL JEVEUO(DEPDEL(1:19)//'.VALE','L',JDEPDE)

        DO 10 I=1,NEQ
            IF (ZI(JCCID+I-1).EQ.1) THEN
                ZR(JCINX-1+I)=ZR(JCINX-1+I)-ZR(JDEPDE-1+I)
            ENDIF
  10    CONTINUE
      ENDIF

      CALL JEDEMA()
      END
