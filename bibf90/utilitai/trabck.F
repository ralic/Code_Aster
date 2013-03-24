      SUBROUTINE TRABCK (CMESS,IEXIT)
! aslint: disable=W0104
! Dummy argument si _USE_INTEL_IFORT n'est pas defini
! ----------------------------------------------------------------------
! IMPRIME LA REMONTEE DES APPELS
!
! IN  CMESS  : MESSAGE D'INFORMATION
! IN  IEXIT  : CONDITION DE SORTIE DE LA ROUTINE DE TRACEBACK
!              AVEC LE COMPILO INTEL <0 ON REDONNE LA MAIN
! ----------------------------------------------------------------------
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF TRABCK UTILITAI  DATE 18/03/2013   AUTEUR COURTOIS M.COURTOIS 
! ================================================================== 
! COPYRIGHT (C) 1991 - 2013  EDF R&D              WWW.CODE-ASTER.ORG 
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
#ifdef _USE_INTEL_IFORT
      USE IFCORE
#endif 
      IMPLICIT NONE
      CHARACTER* (*)     CMESS
      INTEGER*4          IEXIT
#ifdef _USE_INTEL_IFORT
      CALL TRACEBACKQQ(STRING=CMESS,USER_EXIT_CODE=IEXIT)
#else
C     ON NE PEUT PAS APPELER U2MESS (RECURSIVITE)
      WRITE(6,*) 'Traceback is not provided by the compiler'
#endif 

      END
