      SUBROUTINE CCCHUT(RESUIN,RESUOU,LISORD,NBORDR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
      INTEGER      NBORDR
      CHARACTER*8  RESUOU,RESUIN
      CHARACTER*19 LISORD
C RESPONSABLE COURTOIS M.COURTOIS
C ----------------------------------------------------------------------
C  CALC_CHAMP - TRAITEMENT DE CHAM_UTIL
C  -    -                     --   --
C ----------------------------------------------------------------------
C IN  :
C   RESUIN K8   NOM DE LA SD IN
C   RESUOU K8   NOM DE LA SD OUT
C   LISORD K19  NOM DE LA LISTE DES NUMEROS D'ORDRE
C   NBORDR I    NOMBRE DE NUMEROS D'ORDRE
C ----------------------------------------------------------------------
C   ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      CHARACTER*9  MCFACT
      PARAMETER   (MCFACT='CHAM_UTIL')
      CHARACTER*19 LFORM
      PARAMETER   (LFORM='&&CCHUT.FORMULE    ')
C
      INTEGER      IFM, NIV, IOC, NUTI, NF, NC, IBID
      INTEGER      JFORM, NCHOUT
      CHARACTER*8  K8B
      CHARACTER*16 CHIN, CRIT
      INTEGER      IARG
C     ----- FIN  DECLARATIONS ------------------------------------------
C
      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)
C
      CALL GETFAC(MCFACT, NUTI)
      IF (NUTI.EQ.0) THEN
        GOTO 9999
      ENDIF
C
C     BOUCLE SUR LES OCCURRENCES DE CHAM_UTIL
      DO 10 IOC=1, NUTI
        CALL GETVTX(MCFACT, 'NOM_CHAM', IOC,IARG, 1, CHIN, IBID)
        CALL GETVIS(MCFACT, 'NUME_CHAM_RESU', IOC,IARG, 1, NCHOUT, IBID)
        CALL ASSERT(NCHOUT.GE.1 .AND. NCHOUT.LE.20)
C       CRITERE OU FORMULE ?
        CRIT = ' '
        CALL GETVID(MCFACT, 'FORMULE', IOC,IARG, 0, K8B, NF)
        IF (NF.EQ.0) THEN
          CALL GETVTX(MCFACT, 'CRITERE', IOC,IARG, 1, CRIT, NC)
        ELSE
          NF = -NF
          CALL WKVECT(LFORM, 'V V K8', NF, JFORM)
          CALL GETVID(MCFACT, 'FORMULE', IOC,IARG, NF, ZK8(JFORM), IBID)
        ENDIF
        CALL CCCHUC(RESUIN, RESUOU, CHIN, NCHOUT, CRIT,
     &              NF, ZK8(JFORM), LISORD, NBORDR)
        CALL JEDETR(LFORM)
 10   CONTINUE
C
 9999 CONTINUE
      CALL JEDEMA()
C
      END
