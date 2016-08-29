!NECS_EVOL= 77
subroutine lcidbg(fami, kpg, ksp, typmod,compor, crit, instam, instap,&
                  neps, epsm, deps, nsig, sigm, vim, option)
use calcul_module, only : ca_jvcnom_, ca_nbcvrc_
implicit none

#include "jeveux.h"
#include "asterfort/iunifi.h"
#include "asterfort/utmess.h"
#include "asterfort/tecael.h"
#include "asterfort/rcvarc.h"
#include "asterfort/lxlgut.h"
#include "asterfort/codree.h"
#include "asterfort/utlcal.h"
#include "asterfort/assert.h"

    integer :: kpg, ksp, neps, nsig, iv, nbvari, iadzi, iazk24
    integer :: nval,nimp,   ier, nbvrc, iref(10), ier2, nl1
    character(len=*) ::  fami
    character(len=8) ::  typmod(*),nomail,novrc,nomvrc(10)
    character(len=16)::  compor(*), option, algo
    character(len=64)::  file
    real(kind=8) :: deps(neps), epsm(neps),vim(*),sigm(nsig)
    real(kind=8) :: crit(*), lvalr(20), vref(10), vrcm(10),vrcp(10), valvrc
    real(kind=8) :: instam, instap,rac2
    data nimp / 0/
    save nimp
!
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! aslint: disable=W1306
! ======================================================================
!.......................................................................
!
!   but: impression d'un fichier de commande simu_point_mat en cas d'echec
!   l'ecriture de defi_materiau est laisse pour le moment a l'utilisateur
!
!       in      fami    famille de point de gauss (rigi,mass,...)
!       in      kpg,ksp numero du (sous)point de gauss
!       in      ndim    dimension de l espace (3d=3,2d=2,1d=1)
!               typmod  type de modelisation
!               imate    adresse du materiau code
!               compor    comportement de l element
!                     compor(1) = relation de comportement (chaboche...)
!                     compor(2) = nb de variables internes
!                     compor(3) = type de deformation (petit,jaumann...)
!               crit    criteres  locaux
!                       crit(1) = nombre d iterations maxi a convergence
!                                 (iter_inte_maxi == itecrel)
!                       crit(2) = type de jacobien a t+dt
!                                 (type_matr_comp == macomp)
!                                 0 = en vitesse     > symetrique
!                                 1 = en incremental > non-symetrique
!                                 9 = methode implex
!                       crit(3) = valeur de la tolerance de convergence
!                                 (resi_inte_rela == rescrel)
!                       crit(5) = nombre d'increments pour le
!                                 redecoupage local du pas de temps
!                                 (iter_inte_pas == itedec)
!                                 0 = pas de redecoupage
!                                 n = nombre de paliers
!               instam   instant t
!               instap   instant t+dt
!               epsm   deformation totale a t
!               deps   increment de deformation totale
!               sigm    contrainte a t
!               vim    variables internes a t    + indicateur etat t
!               option     option de calcul a faire
!                             'rigi_meca_tang'> dsidep(t)
!                             'full_meca'     > dsidep(t+dt) , sig(t+dt)
!                             'raph_meca'     > sig(t+dt)
!                             'rigi_meca_implex' > dsidep(t), sigextr
!               angmas
!       OUT     un fichier
!
    real(kind=8) :: epslm(12),eps(12),sig(6)

    nimp=nimp+1
    if (nimp>5) goto 999

    rac2=sqrt(2.0d0)
    if (compor(3) .eq. 'SIMO_MIEHE') goto 999
    if (option(1:4).eq. 'RIGI') goto 999

    nbvrc=0
    if (ca_nbcvrc_ .gt. 0) then
       do iv=1,ca_nbcvrc_
          novrc=zk8(ca_jvcnom_-1+iv)
          call rcvarc(' ', novrc, '-', fami, kpg,ksp, valvrc , ier)
          if (ier .eq. 0) then
              nbvrc=nbvrc+1
              ASSERT(nbvrc.le.10)
              nomvrc(nbvrc)=novrc
              vrcm(nbvrc)=valvrc
              call rcvarc(' ', novrc, '+',   fami, kpg, ksp, vrcp(nbvrc), ier)
              if ((novrc.eq.'TEMP').or.(novrc.eq.'SECH')) then
                 call rcvarc(' ', novrc, 'REF', fami, kpg, ksp, vref(nbvrc), ier2)
                 iref(nbvrc)=1
              else
                 iref(nbvrc)=0
              endif
          endif
       end do
    endif

    read (compor(2),'(I16)') nbvari
    if (fami.ne.'PMAT') then
       call tecael(iadzi, iazk24)
       nomail = zk24(iazk24-1+3) (1:8)
    else
       nomail='PMAT'
       kpg=0
    endif

    nval=0
    if (typmod(1).eq.'C_PLAN') then
       nval=1
    else if (typmod(1).eq.'D_PLAN') then
       nval=2
    else if (typmod(1).eq.'AXIS') then
       nval=2
    else if (typmod(1).eq.'3D') then
       nval=3
    endif

    do iv=1,neps
       eps(iv)=epsm(iv)+deps(iv)
    enddo
    do iv=1,nsig
       sig(iv)=sigm(iv)
    enddo
    do iv=4,neps
       epslm(iv)=epsm(iv)/rac2
       eps(iv)=eps(iv)/rac2
    enddo
    if (nval<3) then
       epslm(5)=0.d0
       epslm(6)=0.d0
       sig(5)=0.d0
       sig(6)=0.d0
    endif

    lvalr(1)=instam
    lvalr(2)=instap
    file='REPE_OUT/simu_point_mat_'//nomail
!   longueur de la chaine
    nl1 = lxlgut(file)
    file=file(1:nl1)//'_inst_'
    call codree(instap, 'E', file(nl1+7:64))

    call utmess('I','COMPOR2_70',sk=nomail,si=kpg, nr=2, valr=lvalr,fname=file)
    call utmess('I','COMPOR2_71',sk='EXX', nr=4, valr=[instam,epslm(1),instap,eps(1)],fname=file)
    call utmess('I','COMPOR2_71',sk='EYY', nr=4, valr=[instam,epslm(2),instap,eps(2)],fname=file)
    call utmess('I','COMPOR2_71',sk='EPZZ', nr=4, valr=[instam,epslm(3),instap,eps(3)],fname=file)
    call utmess('I','COMPOR2_71',sk='EXY', nr=4, valr=[instam,epslm(4),instap,eps(4)],fname=file)
    if (nval.eq.3) then
       call utmess('I','COMPOR2_71',sk='EPXZ', nr=4,valr=[instam,epslm(5),instap,eps(5)],fname=file)
       call utmess('I','COMPOR2_71',sk='EPYZ', nr=4,valr=[instam,epslm(6),instap,eps(6)],fname=file)
    endif

    if (nbvrc .gt. 0) then
       do iv=1,nbvrc
          call utmess('I','COMPOR2_81',sk=nomvrc(iv), nr=4, &
                        valr=[instam,vrcm(iv),instap,vrcp(iv)],fname=file)
       end do
    endif

    if (nval.eq.1) then
       call utmess('I','COMPOR2_72',fname=file)
       call utmess('I','COMPOR2_73',fname=file)
    endif
    if (nval.eq.2) then
       call utmess('I','COMPOR2_72',fname=file)
    endif
    if (nval.eq.3) then
       call utmess('I','COMPOR2_74',fname=file)
    endif

    call utmess('I','COMPOR2_75',nr=6, valr=epslm,fname=file)
    call utmess('I','COMPOR2_76',nr=6, valr=sig,fname=file)
    do iv=1,nbvari
       call utmess('I','COMPOR2_77',nr=1, valr=vim(iv),fname=file)
    enddo
    call utmess('I','COMPOR2_86',fname=file)

    call utmess('I','COMPOR2_78',sk=compor(1),fname=file)

    if ((compor(1).eq.'MONOCRISTAL').or.compor(1).eq.'POLYCRISTAL') then
        call utmess('I','COMPOR2_87',sk=compor(7),fname=file)
    endif

    call utlcal('VALE_NOM', algo, crit(6))

    call utmess('I','COMPOR2_88',si=int(crit(1)), sr=crit(3), sk=algo, fname=file)

    call utmess('I','COMPOR2_89',si=int(crit(5)), sr=crit(4), fname=file)

    call utmess('I','COMPOR2_90',sk=compor(3), fname=file)

    call utmess('I','COMPOR2_85',fname=file)

    if (nbvrc.gt.0) then
       call utmess('I','COMPOR2_82',fname=file)
       do iv=1,nbvrc
          if (iref(iv)==1) then
              call utmess('I','COMPOR2_83',nk=2,valk=[nomvrc(iv),nomvrc(iv)],sr=vref(iv),&
                          fname=file)
          else
              call utmess('I','COMPOR2_84',nk=2,valk=[nomvrc(iv),nomvrc(iv)],fname=file)
          endif
       end do
       call utmess('I','COMPOR2_85',fname=file)
    endif
    if (option(1:4).eq. 'FULL') then
       call utmess('I','COMPOR2_79',fname=file)
    else
       call utmess('I','COMPOR2_80',fname=file)
    endif

999 continue
end subroutine
