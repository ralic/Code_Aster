      SUBROUTINE CRSVSI(SOLVEU)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/04/2012   AUTEUR TARDIEU N.TARDIEU 
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
C
      IMPLICIT NONE
      CHARACTER*19 SOLVEU
C
C ----------------------------------------------------------------------
C
C ROUTINE *_NON_LINE (STRUCTURES DE DONNES)
C
C ACTIVATION DE STOP_SINGULIER = 'NON' EN CAS DE DECOUPE DU PAS DE TEMPS
C
C ----------------------------------------------------------------------
C
C 
C IN  SOLVEU  : NOM SD SOLVEUR
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      INTEGER      ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8       ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16   ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL      ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8  ZK8
      CHARACTER*16    ZK16
      CHARACTER*24        ZK24
      CHARACTER*32            ZK32
      CHARACTER*80                ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER      ISLVI,ISLVK
      INTEGER      NPREC
      CHARACTER*24 NOMSLV
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL JEVEUO(SOLVEU//'.SLVK','L',ISLVK)
      CALL JEVEUO(SOLVEU//'.SLVI','E',ISLVI)
      NOMSLV = ZK24(ISLVK-1+1)
      NPREC  = ZI(ISLVI-1+1)
C      
      IF ((NOMSLV.EQ.'MUMPS').OR.
     &    (NOMSLV.EQ.'MULT_FRONT').OR.
     &    (NOMSLV.EQ.'LDLT')) THEN   
        IF (NPREC.GT.0) THEN
          ZI(ISLVI-1+3) = 2
        ELSE
          CALL U2MESK('I','DISCRETISATION_40',1,NOMSLV)
        ENDIF  
      ELSE
        CALL U2MESK('I','DISCRETISATION_40',1,NOMSLV)
      ENDIF
C
      CALL JEDEMA()
      END
