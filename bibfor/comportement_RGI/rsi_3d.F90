subroutine rsi_3d(x0, x1, beton, dt)
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
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!      M.SALGUES 28 avril 2012, modif sellier / multon sept 2012
!      modif sellier micro diffusion novembre 2012
!      rajout passage param materiaux via tableau beton
!      modif traitement des sous iterations oct 2012 pour branchement
!      modele rgi_lin_3d
!=====================================================================
        implicit none
#include "asterf_types.h"
#include "asterfort/vsolal_3d.h"
#include "asterfort/vsols_3d.h"
#include "asterfort/vallib_3d.h"
#include "asterfort/vsf_3d.h"
#include "asterfort/vaft_3d.h"
#include "asterfort/vafm_3d.h"
#include "asterfort/xid_3d.h"
#include "asterfort/cash_3d.h"
#include "asterfort/csheff_3d.h"
#include "asterfort/khis_3d.h"
#include "asterfort/arhenius_3d.h"
#include "asterfort/dtrgi_3d.h"
#include "asterfort/csh_3d.h"
#include "asterfort/ionfixe_3d.h"
#include "asterfort/poyet_3d.h"
#include "asterfort/thermo_3d.h"
#include "asterfort/activite_3d.h"


    real(kind=8) :: phi, alc, sc, sic, tref
    real(kind=8) :: alpha, temp, dalpha, sr, alpha0
    real(kind=8) :: nasol, alsol, ssol, casol
    real(kind=8) ::  alf, sf, dallib
    real(kind=8) ::  dsf, dalsol, dssol, daft, dafm
    real(kind=8) :: aft, afm, csh, kaft, kafm
    real(kind=8) :: xid, cash, csheff, dcash, dcsheff
    real(kind=8) :: f, alpal
    real(kind=8) :: khi
    real(kind=8) :: dtmin, dtcal
    real(kind=8) :: t, dt
    real(kind=8) :: x0(16), x1(16), beton(19)
    real(kind=8) :: alpha1, id0, id1, id2
    integer :: niter, i, nitermax
    real(kind=8) :: srpal,tau1,temp0,temp1,dth0,coth, sr0
    real(kind=8) ::sr1,valini,vnasol,dtemp,vsr,dteff,gam1,gam2
    real(kind=8) ::ohsol,ar,ard,vxid,xidtot,alfeq,sfeq
    real(kind=8) ::nasol0,nasol1,asr,xidtot1
    aster_logical :: conv_forcee
!
    !     calcul de la vitesse de !     parametres fixes
!     nombre de sous pas maxi avant reduction de la vitesse de reaction
    nitermax=10000
!     coeff correcteur pour la cinetique de fixation
!     definitive des alu ds les csh
!     fraction limitant la variation des concentrations entre t et t+dt
    f=0.05d0
!
!     donnees d'entrée
!     temperature de référence
    tref=298.d0
!          print*,x0,,x1,beton,dt
!     composition du beton en mol/m3
    srpal=beton(1)
    alc=beton(2)
    sc=beton(3)
    sic=beton(4)
!     temps caracteristique de micro diffusion pour alpha=1 id=0
    tau1=beton(5)
    phi=beton(6)
    alpha0=beton(8)
    alpha1=beton(9)
    temp0=beton(10)
    temp1=beton(11)
    dth0=beton(12)
    coth=beton(13)
    nasol0=beton(14)
    nasol1=beton(15)
    sr0=beton(16)
    sr1=beton(7)
    id0=beton(17)
    id1=beton(18)
    id2=beton(19)
!     constante de fixation des alu
    alpal=0.00025d0
!     données initiales rangées dans le tableau x0
!     initi non nulle
    valini=0.0001d0
    do i = 1, 16
        x0(i)=max(x0(i),valini)
    end do
!
    alpha=x0(1)
    temp=x0(2)
    csh=x0(3)
    nasol=x0(4)
!     libre
!      naf=x0(5)
    alsol=x0(6)
    alf=x0(7)
    ssol=x0(8)
    sf=x0(9)
    aft=x0(10)
    afm=x0(11)
!     indice de destructuration de la matrice cimentaire / micro diffusi
    xid=x0(12)
    cash=x0(13)
    csheff=x0(14)
!     il faut recalculer le coeff cinetique pour eviter les pb au premie
!     khi=1.d0/x0(15)
    casol=x0(16)
!     calcul des vitesses d hydratation et devolution de temperature
    if (dt .ne. 0.) then
        dalpha=(alpha1-alpha0)/dt
        vnasol=(nasol1-nasol0)/dt
        dtemp=(temp1-temp0)/dt
        vsr=(sr1-sr0)/dt
    else
        dalpha=0.d0
        dtemp=0.d0
        vnasol=0.d0
        vsr=0.d0
    end if
!     intialisation compteur de sous iterations
    niter=0
!     initialisation boucle sous iterations en temps
    t=0.d0
    dtmin=0.d0
    dtcal=0.d0
!     sous iterations en convergance normale
    conv_forcee=.false.
!
!
 !     ************* boucle sur les sous pas de temps *******************

      do while ((t.lt.dt .or. niter.le.1).and.(niter.le.(nitermax+1)))
      
 !     traitement de la convergence forcee      
      if(conv_forcee) then
        t=t+dtcal
        dteff=dtcal
      else
        t=t+dtmin
        dteff=dtmin
      end if

 !     actualisation hydratation, temperature, alcalins, saturation     
      alpha=max(alpha0+dalpha*t,valini)
      temp=temp0+dtemp*t
      nasol=nasol0+vnasol*t
      nasol=max(nasol,1.d-4)
      sr=sr0+vsr*t
      
 !     print*, 'dans rsi_3d', t, alpha,temp
 !     calculs possibles avec les données initiales et les données d'entrée
 !     modif electroneutralite sellier nov 2012
 !     approximation sur les ions positifs en presence d alu sous forme 
 !     al(oh)4 - et s04 2-
      call poyet_3d(casol,temp,max(nasol-alsol-2.d0*ssol,0.d0))
      call thermo_3d(kaft,temp,kafm)
      call activite_3d(gam1,gam2,temp,casol,nasol,ohsol)
      call arhenius_3d(ar,ard,temp,tref,asr,sr,srpal)
 !     hydratation des csh
      call csh_3d(alpha,sic,csh)
 !     indice de destructuratin de la pate 
      call xid_3d(vxid,dalpha,ar)
 !     cinetique des reactions      
      call khis_3d(tau1,khi,xid,alpha,ar,asr,dth0,coth,xidtot) 
 !      print*,'khi', tau1,khi,xid,alpha,ar,asr,dth0,coth,xidtot
 !      read*      
      if  (conv_forcee) then
 !       on limite les vitesses de reactions pour atteindre
 !       la fin du pas sans sortir des criteres
        khi=khi*dtmin/dtcal
      end if  
 !     vitesse de fixation definitive des a ds les csh 
      call cash_3d(alf,dcash,khi,csheff,casol,alsol,nasol,cash) 
 !     csheff capable de fixer de façon reversible      
      call csheff_3d(dcash,dcsheff,dalpha,sic,csh,alsol,dalsol,csheff,&
     xidtot,xidtot1,nasol,vnasol,dteff,alpha,cash,alc,sc,id0,id1,id2)
 !     alu et sulfates fixes a l equilibre     
      call ionfixe_3d(alfeq,sfeq,csh,csheff,temp,nasol,ssol,alsol,&
     alpal,cash) 
 !     print*,'ionfixe',alfeq,sfeq,csh,csheff,temp,nasol,ssol,
 !    &alsol,alpal,cash
 !     read*
 !     vitesses des reactions        
      call vsf_3d(khi,dsf,sf,sfeq,csh)
 !      print*,'dsf',khi,dsf,sf,sfeq,csh
 !      read*
      call vaft_3d(khi,casol,alsol,ssol,ohsol,kaft,gam1,gam2,daft)
 !      print*,'vaft',khi,casol,alsol,ssol,ohsol,kaft,gam1,gam2,daft
 !      read*      
      call vafm_3d(khi,casol,alsol,ssol,ohsol,kafm,gam1,gam2,dafm)
 !     si les deux reactions sont possibles on privelie l afm     
      if((daft.gt.0.d0).and.(dafm.gt.0.d0)) then
       daft=dafm/1.d9
      end if
 !     vitesse de fixation des alu dans les csheff      
      call vallib_3d(khi,dallib,alf,alfeq,dafm,casol,nasol,alsol)   
 !      print*,'apres dvallib',khi,dallib,alf,alfeq,dafm,casol,nasol,alsol 
 !      read*      
 !     on en deduit les vitesses de variations des concentrations 
 !     en solution     
      call vsolal_3d(dalsol,dallib,daft,dafm,alc,phi,sr,dalpha,&
     vsr,alsol)
      call vsols_3d(dssol,dsf,daft,dafm,sc,phi,sr,dalpha,vsr,ssol)
 !      print*,'ssol',dssol,dsf,daft,dafm,sc,phi,sr,dalpha
 !      read*

 !    choix du pas de temps minimum dtmin
      if(conv_forcee) then
       dtmin=dtcal
      else
       dtcal=dt-t      
       call dtrgi_3d(f,dtmin,dalsol,dssol,daft,dafm,dsf,alsol,ssol&
      ,aft,afm,sf,alf,dtcal,phi,sr,dalpha,sc,alc,dallib&
      ,dcash,dcsheff,csheff,vsr)
      end if

 !     actualisation des variables
      alf=alf+dtmin*dallib
      sf=sf+dtmin*dsf
      aft=aft+dtmin*daft
      afm=afm+dtmin*dafm
      alsol=alsol+dtmin*dalsol
      ssol=ssol+dtmin*dssol
 !     l indice de destructuration n evolue que si la temperature
 !     est superieure a la temperature de ref (sellier nov 2012)      
      xid=xid+dtmin*max((vxid-dalpha),0.d0)
      cash=cash+dtmin*dcash
 !      xidtot=xidtot+dxidtot*dtmin/dteff
 !      csheff=csheff+dtmin*dcsheff    
      
 !     compteur d iterations     
      niter=niter+1    
 !     modfication du critere si nombre d iteration important
      if ( niter .gt. (nitermax/2) ) then 
 !       1 er relachement des variations max autorisees      
        f=0.1
      end if
      if ( niter .gt. (3*nitermax/4) ) then 
 !       2nd relachement des variation max autorisee      
        f=0.2
      end if
      if ( niter .eq. nitermax ) then
        print*,'niter max atteint dans rsi_3d',niter
 !       print*,'relachement momentane du critere a 0.1'
        conv_forcee=.true.
 !       on limite les vitesses de reactions pour atteindre
 !       la fin du pas sans sortir des criteres      
      end if 
      
      enddo
      
 !    ***************** fin de boucle sur les sous pas *******************
    
 !      print*,'niter dans rsi_3d',niter
 !    enregistrement des variables actualisées dans le tableau x1      

      x1(1)=alpha      
      x1(2)=temp
      x1(3)=csh 
      x1(4)=nasol
      x1(6)=alsol 
      x1(7)=alf
      x1(8)=ssol 
      x1(9)=sf
      x1(10)=aft
      x1(11)=afm
      x1(12)=xid
      x1(13)=cash
      x1(14)=csheff
      x1(15)=1.d0/khi
      x1(16)=casol
 !       print*,x1
end subroutine
