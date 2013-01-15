      SUBROUTINE STSTAT( ISTAT )
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF STSTAT UTILITAI  DATE 14/01/2013   AUTEUR COURTOIS M.COURTOIS 
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
! RESPONSABLE COURTOIS M.COURTOIS
      IMPLICIT NONE
      INCLUDE 'aster_constant.h'
!     ARGUMENT IN
      INTEGER            ISTAT
!-----------------------------------------------------------------------
!     FONCTION "SeT STATus" : ACTIVE LE DRAPEAU D'ETAT ISTAT.
!
!     LA VALEUR DU STATUT GLOBAL IGLBST EST STOCKE DANS LE COMMON CGLBST
!-----------------------------------------------------------------------
      INTEGER            IGLBST
      COMMON  / CGLBST / IGLBST
!
      IF (ISTAT .EQ. ST_OK) THEN
        IGLBST = ST_OK
      ELSE
        IGLBST = IOR(ISTAT, IGLBST)
      ENDIF
      END

