      SUBROUTINE MFCOOE(FID, MAA, MDIM, COO, MODCOO,
     &                  N,TYPREP,
     &                  NOM,UNIT,CRET)
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF MFCOOE ECHANGE  DATE 08/03/2011   AUTEUR SELLENET N.SELLENET 
! RESPONSABLE
! ================================================================== 
! COPYRIGHT (C) 1991 - 2011  EDF R&D              WWW.CODE-ASTER.ORG 
!                                                                    
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR      
! MODIFY IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS     
! PUBLISHED BY THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE 
! LICENSE, OR (AT YOUR OPTION) ANY LATER VERSION.                    
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,    
! BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF     
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU   
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.                           
!                                                                    
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE  
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO : EDF R&D CODE_ASTER,    
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.     
! ================================================================== 
      IMPLICIT NONE
      CHARACTER*(*) MAA
      REAL*8 COO(*)
      INTEGER FID,TYPREP,MDIM
      INTEGER N,CRET,MODCOO
      CHARACTER*16 NOM(*), UNIT(*)
#ifdef _DISABLE_MED
      CALL U2MESS('F','FERMETUR_2')
#else
! BEGIN MED
#ifdef _USE_MED_SHORT_INT
      INTEGER*4 FID4, TYPRE4, MDIM4
      INTEGER*4 N4, CRET4, MODCO4
      FID4 = FID
      TYPRE4 = TYPREP
      MDIM4 = MDIM
      N4 = N
      MODCO4 = MODCOO
#ifdef _DEBUG_MED
      PRINT *,'=== MFCOOE ==='
      PRINT *,'FID=',FID4,' MAA=',MAA,' MDIM=',MDIM4
      PRINT *,'COO(1)=',COO(1),' N=',N4,' TYPREP=',TYPRE4
      PRINT *,'MODCOO=',MODCO4,' NOM=',NOM(1),' UNIT=',UNIT(1)
#endif
      CALL EFCOOE(FID4, MAA, MDIM4, COO, MODCO4,
     &                  N4,TYPRE4,
     &                  NOM,UNIT,CRET4)
      CRET = CRET4
#else
      CALL EFCOOE(FID, MAA, MDIM, COO, MODCOO,
     &                  N,TYPREP,
     &                  NOM,UNIT,CRET)
#endif
! END MED
#endif
      END
