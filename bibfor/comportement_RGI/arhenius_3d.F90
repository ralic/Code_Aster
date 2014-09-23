subroutine arhenius_3d(ar,ard,temp,temp0,asr,sr,srpal)
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
! person_in_charge: etienne grimal at edf.fr
!=====================================================================
!      provient de rsi_3d : 
!     activation thermique et hydrique
!=====================================================================
        implicit none
      real(kind=8) :: ar
      real(kind=8) :: ard
      real(kind=8) :: temp
      real(kind=8) :: temp0
      real(kind=8) :: asr
      real(kind=8) :: sr
      real(kind=8) :: srpal
      real(kind=8) :: sr0
      real(kind=8) :: ear
      real(kind=8) :: eadr,cg1
      ear=5000.d0
      eadr=5292.d0

 !     activation thermique (loi d'arrhénus)      
      ar=dexp(-ear*((1.d0/temp)-(1.d0/temp0)))
      ard=dexp(-eadr*((1.d0/temp)-(1.d0/temp0)))
      
 !     activation hydrique
 !     on impose une valeur petite non nulle pour ne pas
 !     avoir un temps caracteristique infini (cf calcul de khi)
 !     sellier nov 2012      
 !      asr=dmax1(sr-srpal,1.d-6)/(1.d0-srpal)
 !     modification marie test courbe fidy
      sr0=srpal
      if((sr0.lt.1.).and.(sr0.gt.0.)) then
         cg1 = -0.2d1 * log(0.10d2) / (sr0 - 1.d0)
         asr=dexp(cg1*(sr-1.d0))           
      else
         print*, 'sr palier incoherent dans rsi_3d'
         if (sr0.gt.1.)then
          asr=1.d0
         else
          asr=1.d-6
         end if
      end if
end subroutine
