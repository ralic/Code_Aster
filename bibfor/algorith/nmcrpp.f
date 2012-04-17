      SUBROUTINE NMCRPP(MOTFAZ,IOCC  ,PREC  ,CRITER,TOLE  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT      NONE
      CHARACTER*(*) MOTFAZ
      INTEGER       IOCC    
      CHARACTER*8   CRITER
      REAL*8        PREC,TOLE
C
C ----------------------------------------------------------------------
C
C ROUTINE *_NON_LINE (UTILITAIRE - SELEC. INST.)
C
C LECTURE PRECISION/CRITERE 
C
C ----------------------------------------------------------------------
C
C NB: SI LE CRITERE EST RELATIF MAIS QUE _PRECISION_ N'EST PAS 
C     PRECISEE, ALORS PRECISION VAUT PREDEF
C
C IN  MOTFAC : MOT-FACTEUR POUR LIRE (LIST_INST/INST)
C IN  IOCC   : OCCURRENCE DU MOT-CLEF FACTEUR MOTFAC
C OUT PREC   : PRECISION DE RECHERCHE
C OUT CRITER : CRITERE DE SELECTION (RELATIF/ABSOLU)
C OUT TOLE   : TOLERANCE
C                +PREC POUR RELATIF
C                -PREC POUR ABSOLU
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
      INTEGER      N1,N2
      CHARACTER*16 MOTFAC
      REAL*8       PREDEF,R8PREM
      INTEGER      IARG
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      PREC   = 0.D0
      TOLE   = 0.D0
      CRITER = 'RELATIF'
      MOTFAC = MOTFAZ
      PREDEF = 1.D-6
C
C --- LECTURE
C           
      CALL GETVR8(MOTFAC,'PRECISION',IOCC,IARG,1,PREC  ,N1)
      CALL GETVTX(MOTFAC,'CRITERE'  ,IOCC,IARG,1,CRITER,N2)
      IF (CRITER.EQ.'ABSOLU') THEN
        IF (N1.EQ.0) CALL U2MESS('F','LISTINST_1')
      ELSEIF (CRITER.EQ.'RELATIF') THEN
        IF (N1.EQ.0) THEN
          PREC   = PREDEF
          CALL U2MESR('A','LISTINST_2',1,PREDEF)
        ENDIF
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      IF (PREC.LE.R8PREM()) THEN
        CALL U2MESS('F','LISTINST_3')
      ENDIF
C
C --- TOLERANCE      
C
      IF (CRITER.EQ.'RELATIF') THEN
        TOLE   =  PREC
      ELSEIF (CRITER.EQ.'ABSOLU') THEN
        TOLE   = -PREC
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      CALL JEDEMA()      
C
      END
