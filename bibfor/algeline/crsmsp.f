      SUBROUTINE CRSMSP(SOLVBZ,MATASZ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 18/01/2010   AUTEUR TARDIEU N.TARDIEU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*(*) SOLVBZ,MATASZ
C-----------------------------------------------------------------------
C     CREATION D'UNE SD SOLVEUR MUMPS SIMPLE PRECISION UTILISEE COMME
C     PRECONDITIONNEUR
C-----------------------------------------------------------------------
C IN  K*  SOLVBZ    : NOM DE LA SD SOLVEUR MUMPS BIDON
C IN  K*  MATASZ    : MATRICE du systeme
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     COMMUNS   JEVEUX
C-----------------------------------------------------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C----------------------------------------------------------------------
C     VARIABLES LOCALES
C----------------------------------------------------------------------
      INTEGER ISLVK,ISLVR,ISLVI,IBID,IRET
      CHARACTER*19 MATASS,SOLVBD
      CHARACTER*8  SYMK
      CHARACTER*3  SYME
C----------------------------------------------------------------------
      CALL JEMARQ()

      SOLVBD = SOLVBZ
      MATASS = MATASZ
      
C     SI LA MATRICE EST NON SYMETRIQUE IL *FAUT* LA SYMETRISER
      CALL DISMOI('F','TYPE_MATRICE',MATASS,'MATR_ASSE',IBID,SYMK,IRET)
      IF (SYMK.EQ.'SYMETRI') THEN
        SYME='NON'
      ELSE IF (SYMK.EQ.'NON_SYM') THEN
        SYME='OUI'
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
      
      CALL WKVECT(SOLVBD//'.SLVK','V V K24',11,ISLVK)  
      CALL WKVECT(SOLVBD//'.SLVR','V V R',   4,ISLVR)  
      CALL WKVECT(SOLVBD//'.SLVI','V V I',   7,ISLVI)  
                                                       
      ZK24(ISLVK-1+1)  = 'MUMPS'                       
C     PRETRAITEMENTS                                   
      ZK24(ISLVK-1+2)  = 'AUTO'                        
C     TYPE_RESOL                                       
      ZK24(ISLVK-1+3)  = 'SYMGEN'                      
C     RENUM                                            
      ZK24(ISLVK-1+4)  = 'AUTO'                        
C     SYME                                             
      ZK24(ISLVK-1+5)  =  SYME                         
C     ELIM_LAGR2                                       
      ZK24(ISLVK-1+6)  = 'NON'                         
C     MIXER_PRECISION                                  
      ZK24(ISLVK-1+7)  = 'OUI'                         
C     PRECONDITIONNEUR                                 
      ZK24(ISLVK-1+8)  = 'OUI'                         
C     OUT_OF_CORE                                      
      ZK24(ISLVK-1+9)  = 'NON'                         
C     MATR_DISTRIBUEE                                  
      ZK24(ISLVK-1+10) = 'NON'                         
C     POSTTRAITEMENTS                                  
      ZK24(ISLVK-1+11) = 'SANS'                        

      ZR(ISLVR-1+1) = -1.D0                            
      ZR(ISLVR-1+2) = -1.D0                            
      ZR(ISLVR-1+3) = 0.D0                             
      ZR(ISLVR-1+4) = 0.D0                             

      ZI(ISLVI-1+1) = -1                               
      ZI(ISLVI-1+2) = 100                              
      ZI(ISLVI-1+3) = 0                                
      ZI(ISLVI-1+4) = -9999                            
      ZI(ISLVI-1+5) = -9999                            
      ZI(ISLVI-1+6) = -9999                            
      ZI(ISLVI-1+7) = -9999                            

      CALL JEDEMA()
      END
