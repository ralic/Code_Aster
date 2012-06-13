      SUBROUTINE NMFETI(NUMEDD,IFM   ,LFETI ,NIVMPI,LFETIP)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INCLUDE 'jeveux.h'
      LOGICAL      LFETI,LFETIP
      INTEGER      NIVMPI
      INTEGER      IFM
      CHARACTER*24 NUMEDD
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (UTILITAIRE)
C
C PREPARATION FETI
C      
C ----------------------------------------------------------------------
C
C
C IN  IFM    : NUMERO DU FICHIER LOGIQUE MESSAGE
C IN  NUMEDD : NUME_DDL
C OUT NIVMPI : NIVEAU IMPRESSION MPI
C OUT NBPROC : NOMBRE DE PROCESSEURS
C OUT LFETI  : .TRUE. SI SOLVEUR FETI
C OUT LFETIP : .TRUE. SI SOLVEUR FETI PARALLELE
C
C
C
C
      INTEGER      JREFN,IINF,IBID,NBPROC
      REAL*8       R8BID
      CHARACTER*24 KMET,INFOFE,K24BID     
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C    
      NBPROC = 1
      LFETI  = .FALSE.
      LFETIP = .FALSE.
C      
      CALL JEVEUO(NUMEDD(1:14)//'.NUME.REFN','L',JREFN)
      KMET   = ZK24(JREFN+2)
      IF (KMET(1:4).EQ.'FETI') THEN
        LFETI  = .TRUE.
        CALL JEVEUO('&FETI.FINF','L',IINF)
        INFOFE = ZK24(IINF)
        IF (INFOFE(10:10).EQ.'T') THEN
          NIVMPI=2
        ELSE
          NIVMPI=1
        ENDIF
        CALL FETMPI(3     ,IBID  ,IFM   ,NIVMPI,IBID  ,
     &              NBPROC,K24BID,K24BID,K24BID,R8BID)
      ELSE
        LFETI = .FALSE.
      ENDIF
C
      IF (LFETI) THEN
        IF (NBPROC.GT.1) THEN
          LFETIP = .TRUE.
        ENDIF 
      ENDIF      
C
      CALL JEDEMA()
      END
