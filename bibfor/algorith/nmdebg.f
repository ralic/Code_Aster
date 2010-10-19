       SUBROUTINE NMDEBG(TYPOBZ,NOMOBZ,IFM   )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/10/2010   AUTEUR DESOZA T.DESOZA 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*(*) NOMOBZ
      CHARACTER*(*) TYPOBZ
      INTEGER       IFM,NIV

C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (UTILITAIRE)
C
C IMPRIME LE CONTENU D'UN OBJET POUR DEBUG
C      
C ----------------------------------------------------------------------
C
C
C IN  TYPOBJ : TYPE DE L'OBJET (VECT/MATR)
C IN  NOMOBJ : NOM DE L'OBJET
C IN  IFM    : UNITE D'IMPRESSION
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      CHARACTER*24 NOMOBJ
      CHARACTER*4  TYPOBJ
      CHARACTER*3  TYPE
      REAL*8       SOMMR
      INTEGER      RESUME,SOMMI,LONMAX
      INTEGER      IRET,IBID
C      
C ----------------------------------------------------------------------
C      
      CALL JEMARQ()  
C      
C --- INITIALISATIONS 
C
      NOMOBJ = NOMOBZ
      TYPOBJ = TYPOBZ
C
      CALL INFDBG('MECA_NON_LINE',IBID,NIV) 
C
C --- IMPRESSION
C
      IF (TYPOBJ.EQ.'VECT') THEN
        CALL TSTOBJ(NOMOBJ(1:19)//'.VALE','OUI' ,RESUME,SOMMI ,SOMMR ,
     &              IBID                 ,LONMAX,TYPE  ,IRET  ,IBID)
        IF ((TYPE.EQ.'R').AND.(IRET.EQ.0)) THEN
          IF (NIV.GE.2) THEN
            WRITE (IFM,1003) NOMOBJ(1:19),LONMAX,SOMMR
          ENDIF
        ENDIF
      ELSEIF (TYPOBJ.EQ.'CHEL') THEN
        CALL TSTOBJ(NOMOBJ(1:19)//'.CELV','OUI' ,RESUME,SOMMI ,SOMMR ,
     &              IBID                 ,LONMAX,TYPE  ,IRET  ,IBID)
        IF ((TYPE.EQ.'R').AND.(IRET.EQ.0)) THEN
          IF (NIV.GE.2) THEN
            WRITE (IFM,1003) NOMOBJ(1:19),LONMAX,SOMMR
          ENDIF
        ENDIF 
      ELSEIF (TYPOBJ.EQ.'MATA') THEN
        CALL TSTOBJ(NOMOBJ(1:19)//'.VALM','OUI' ,RESUME,SOMMI ,SOMMR ,
     &              IBID                 ,LONMAX,TYPE  ,IRET  ,IBID)
        IF ((TYPE.EQ.'R').AND.(IRET.EQ.0)) THEN
          IF (NIV.GE.2) THEN
            WRITE (IFM,1003) NOMOBJ(1:19),LONMAX,SOMMR
          ENDIF
        ENDIF  
      ELSE
        CALL UTIMSD(IFM,-1,.TRUE.,.TRUE.,NOMOBJ(1:19),1,' ')         
      ENDIF
C      
 1003 FORMAT (' <MECANONLINE>        ',A19,' | LONMAX=',I12,
     &        ' | SOMMR=',E30.21)
C
      CALL JEDEMA()      
C
      END
