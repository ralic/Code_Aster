subroutine b3d_relax1(spl0, s0, s1, vsigma, eta1,&
                      e1, eta2, e0, dt, sref1,&
                      spl1, depst, sref, wref, w,&
                      li, xx1, dam, dpic)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!     calcul de la relaxation de la contrainte dans la fissure
!=====================================================================
    implicit none
!
    real(kind=8) :: spl0
    real(kind=8) :: s0
    real(kind=8) :: s1
    real(kind=8) :: vsigma
    real(kind=8) :: eta1
    real(kind=8) :: e1
    real(kind=8) :: eta2
    real(kind=8) :: e0
    real(kind=8) :: dt
    real(kind=8) :: sref1
    real(kind=8) :: spl1
    real(kind=8) :: depst
    real(kind=8) :: sref
    real(kind=8) :: wref
    real(kind=8) :: w
    real(kind=8) :: li
    real(kind=8) :: xx1
    real(kind=8) :: dam
    real(kind=8) :: dpic, sf11, tau1, sf10
!     vsigma est non nul que si on est en train d ouvrir ou de fermer
!     la fissure
    sf10=s1-spl0
!     le temps de refermeture augmente si la fissure est presque fermee
    tau1=(eta1/e1)*(min(sref1,sref)/sref)
!     on desactive pour autoriser la refermeture complete
!
!     le temps de refermeture augmente si la contrainte est inferieure
!     a la contrainte de refermeture
!     est grande par rapport  à la contrainte de refermeture
!      tau1=tau1*(min(abs(sf10),sref1)/sref)
    if (vsigma .gt. 0.) then
!        on ouvre la fissure
        if (sf10 .ge. 0.) then
!            traction interdite dans la fissure
            sf11=0.d0
            spl1=s1
        else
!            on reouvre la fissure mais il y a encore une contrainte
!            de compression a l interieur, la vitesse de
!            relaxation depend de cette contrainte
!             if(sref1.ne.0.) then
!                on desactive cette condition qui empeche la relaxationd
!                fissure ouverte
!                coeff2=abs(max(sf10,-sref1))/sref1
!                coeff2=1.d0
!             else
!                coeff2=1.d0
!             end if
!             print*,'reduction vitesse de relaxation dans relax1',coeff
!             spl1=spl0
            spl1=spl0*exp(-dt/tau1)
            spl1=max(spl1,0.d0)
            sf11=s1-spl1
!            compression autorisee mais on relaxe ce qui est dans la fis
!             vspl=-eta2/E0*spl0
!             spl1=spl0+vspl*dt
!             spl1=max(spl1,0.d0)
!             sf11=s1-spl1
        end if
    end if
    goto 20
!      if(vsigma.eq.0.) then
!c        la fissure est refermee
!         if(sf10.ge.0.) then
!          sf11=min(s1,0.d0)
!          spl1=max(s1,0.d0)
!         else
!c         relaxation de spl a vsigma nulle
!          if(spl0.gt.0.) then
!             print*,'relaxation de spl a vsig nulle ds b3d_ralax1'
!             read*
!c            la relaxation de la contrainte plastique n est pas fini
!c             t10 = exp(-(E1 * eta2 + E0 * eta1) * dt / eta1 / eta2)
!c             spl1 = s0 + vsigma * dt + t10 * (-s0 + spl0)
!c             spl1=max(spl1,0.d0)
!c             sf11=s1-spl1
!             vspl=-eta2/E0*spl0
!             spl1=spl0+vspl*dt
!             spl1=max(spl1,0.d0)
!             sf11=s1-spl1
!          else
!c            on a fini de refermer la fissure
!             spl1=0.d0
!             sf11=min(s1,0.d0)
!          end if
!         end if
!      end if
    20 if(vsigma.le.0.) then
!         modif realise avec Said Rahal le 27/03/2013
!         on referme la fissure en relaxant spl
!          vspl1=vsigma*min(exp(eta2/E0*vsigma),1.d0)
!          vspl2=(-eta2/E0*spl0)
!          tau2=eta2/E0
!          vspl=vspl2
    spl1=spl0*exp(-dt/tau1)
    spl1=max(spl1,0.d0)
    sf11=s1-spl1
end if
end subroutine
