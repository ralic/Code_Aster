subroutine b3d_bgpg(vg,biot0,poro0,xmg,vp0,bg,epsvt,&
     epsvpg,epsvpw,pg)
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
!********************************************************************** 
!     calcul de la pression de gel      
        implicit none
        real(kind=8) :: vg
        real(kind=8) :: biot0
        real(kind=8) :: poro0
        real(kind=8) :: xmg
        real(kind=8) :: vp0
        real(kind=8) :: bg
        real(kind=8) :: epsvt
        real(kind=8) :: epsvpg
        real(kind=8) :: epsvpw
        real(kind=8) :: pg,vptg
!     prise en compte du degre de saturation en gel de la porosite
!     dans le coeff de Biot  (bg)    
!c      bg=biot0*min((vg/poro0),1.d0)
!     Modif Paulo Regis et Multon  : retour a la version grimal mais avec biot reel
!     (on omet le degre de saturation : ie le gel accede a toute l augmentation de volume)
      bg=biot0
!     calcul du volume de decharge du gel      
!     prise en compte du volume connecte (vp0), de la contribution
!     de la defeormation viscoelastique du squelette (Bg*(...)
!     et du volume des fissures connectees au site reactifs (epsvpg)      
      vptg=vp0+bg*(epsvt-epsvpg-epsvpw)+epsvpg 
!c     prise en compte de la condition de surpression (pg>0)     
      if(vg.gt.vptg)then
       pg=xmg*(vg-vptg)
!c       print*,'pg fin', pg
      else
       pg=0.d0
      end if
end subroutine
