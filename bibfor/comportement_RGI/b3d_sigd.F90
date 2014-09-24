subroutine b3d_sigd(bg1, pg1, bw1, pw1, sfld,&
                    ssg6, e1, rt2, ept1, mfr,&
                    erreur, dg3, dw3, xnu0, sigaf6,&
                    dflu0, sigef6, xmg, sigat6, vplg33,&
                    vplg33t, vssw33, vssw33t, ssw6, dth0)
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
!     calcul des contrainte apres endommagements diffus de fluage
!     actualisation de l endommagement hydrique
!     actualisation de l endommagement de gel
!
!     dff contient l endommagement diffus effectif
!=====================================================================
    implicit none
#include "asterfort/b3d_partition.h"
#include "asterfort/b3d_sst.h"
#include "asterfort/b3d_sdif.h"
#include "asterfort/b3d_d66.h"
#include "asterfort/x33x6.h"
#include "asterfort/x6x33.h"
#include "asterfort/b3d_chrep.h"
#include "asterfort/b3d_sigapp.h"
        real(kind=8) :: bg1
        real(kind=8) :: pg1
        real(kind=8) :: bw1
        real(kind=8) :: pw1
        real(kind=8) :: sfld
        real(kind=8) :: ssg6(6)
        real(kind=8) :: e1
        real(kind=8) :: rt2
        real(kind=8) :: ept1
        integer :: mfr,i
        integer :: erreur
        real(kind=8) :: dg3(3)
        real(kind=8) :: dw3(3)
        real(kind=8) :: xnu0
        real(kind=8) :: sigaf6(6)
        real(kind=8) :: dflu0
        real(kind=8) :: sigef6(6)
        real(kind=8) :: xmg
        real(kind=8) :: sigat6(6)
        real(kind=8) :: vplg33(3, 3)
        real(kind=8) :: vplg33t(3, 3)
        real(kind=8) :: vssw33(3, 3)
        real(kind=8) :: vssw33t(3, 3)
        real(kind=8) :: ssw6(6)
        real(kind=8) :: dth0
    real(kind=8) :: sigaf3(3), vsigaf33(3, 3), vsigaf33t(3, 3)
    real(kind=8) :: sigaft6(6), sigafc6(6), sigafc3(3), sigaft3(3)
!
!      variables internes
    real(kind=8) :: sigeg3(3), st3(3), vss33(3, 3), vss33t(3, 3), ddw66(6, 6)
    real(kind=8) :: ddg66(6, 6), sw3(3), rapp3(3), sigew3(3)
    real(kind=8) ::  sigaw33(3, 3), x33(3, 3), sigpg6(6)
    real(kind=8) :: sigag6(6), x6(6), sigaf33(3, 3), sigag33(3, 3)
    real(kind=8) :: bw2,sigext2,sigext1,smax,sigpw0,sigpg0,sigm1,sigext0
!
!***********************************************************************
!     prise en compte de l'endo de fluage sur les contraintes effectives
!     et de l endo thermique dth0
!     print*,'sigef6',sigef6
!      print*,'ds b3d_sigd, dflu',dflu0,' dth',dth0
    do i = 1, 6
!       l endo de fluage n affecte plus les contraintes totales
!       depui juin 2013 il n affecte que l etage de Maxwell
!        sigaf6(i)=sigef6(i)*(1.d0-dflu0)*(1.d0-dth0)
!        cf calage S.Multon
        sigaf6(i)=sigef6(i)*(1.d0-dth0)
    end do
!
!**********************************************************************
!     diagonalisation des contraintes effectives initiales
    call b3d_partition(sigaf6, sigaf3, vsigaf33, vsigaf33t, sigaft6,&
                       sigafc6, sigafc3, sigaft3)
!
!**********************************************************************
!     actualisation des contraintes seuils pour l'endommagement du au
!     gel et a la pression intra poreuse
!     calcul des contraintes conduisant a la fissuration diffuse
!     (y compris partie positive de l'auto contrainte hydrique)
!      print*,'sfld,bw1,pw1 dans b3d_sfld',sfld,bw1,pw1
    do i = 1, 3
!        contrainte externe equivalente (la contrainte vraie n etant pas
        sigext0=sigaf3(i)-bg1*pg1-bw1*pw1
        if (sigext0 .lt. 0.d0) then
!         seules les contraintes negatives ext attenuent la fissuration
            sigm1=sigext0
        else
            sigm1=0.d0
            end if
!        print*,'b3d_sigd sigaf3(',i,')=',sigaf3(i)
!        print*,'b3d-sigd bg:',bg1,' pg:',pg1,' bgpg:',bg1*pg1
!        calcul des contraintes préjudiciables dans le squelette solide
!        i.e partie positive des autocontraintes + attenuation externe e
            sigeg3(i)=sigm1+bg1*abs(pg1)
            sigew3(i)=sigm1+bw1*abs(pw1)
!        remarque : il vaudrait mieux calculer la proba de fissuration d
!        à la fin de ce programme car à ce moment là on connait sigex
            end do
!     actualisation des contraintes seuils de fissuration diffuse
!     pour le pas suivant : les contraintes orthogonales de compression
!     sont envoyees avec un aleas nul
            call b3d_sst(ssg6, 0, vsigaf33, vsigaf33t, sigeg3)
            call b3d_sst(ssw6, 0, vsigaf33, vsigaf33t, sigew3)
!
!**********************************************************************
!     si on neglige l effet de l endo hydrique sur le comportement
            goto 10
!     si non : calcul des endommagements diffus hydriques principaux
!     et de la matrice de passage
!      print*,ssw6,E1,rt2,ept1,
!     #erreur,dw3,sw3,vssw33,vssw33t,rapp3
            call b3d_sdif(ssw6, e1, rt2, ept1, erreur,&
                          dw3, sw3, vssw33, vssw33t, rapp3)
!     prise en compte des endommagents diffus hydriques actuels sur
!     le squelette solide
!     remarque : comp=.true. induit une matrice diagonale dans la base p
            call b3d_d66(xnu0, sw3, ddw66, e1, .false.,&
                         .true.)
!      call affiche66(ddw66)
!      print*,'pw',pw1,'bw1',bw1
!      print*,'sigaf',sigaf6
!     prise en compte de lendo diffus hydrique sur la partie positive de
!     contraintes effectives
            call x6x33(sigaft6,x33)
            call b3d_chrep(sigaf33, x33, vssw33)
            call x33x6(sigaf33,x6)
            call b3d_sigapp(x6, ddw66, sigaf6, .true.)
!     retour en base fixe
            call x6x33(sigaf6,x33)
            call b3d_chrep(sigaf33, x33, vssw33t)
            call x33x6(sigaf33,sigaft6)
!     prise en compte des fissures fermees sur
!     les contraintes effectives
            do i = 1, 6
                sigaf6(i)=(sigaft6(i)+sigafc6(i))
            end do
!
!***********************************************************************
!     prise en compte de lendo diffus de gel sur la partie positive des
!     contraintes effectives

            10 if (xmg.ne.0.) then
            do i = 1, 3
                st3(i)=1.d0/(1.d0-min(dg3(i),0.999d0))
                dw3(i)=0.d0
            end do
            
!      matrice d endommagement diffus en base prin d endo
            call b3d_d66(xnu0, st3, ddg66, e1, .false.,&
                         .true.)
!      traitement des refermetures des micro-fissures dues au gel
            call b3d_partition(sigaf6, sigaf3, vsigaf33, vsigaf33t, sigaft6,&
                               sigafc6, sigafc3, sigaft3)
!      on applique l endo de gel sur les contraintes de traction
            call x6x33(sigaft6,x33)
            call b3d_chrep(sigaf33, x33, vplg33)
            call x33x6(sigaf33,x6)
            call b3d_sigapp(x6, ddg66, sigaf6, .true.)
!      retour en base fixe
            call x6x33(sigaf6,x33)
            call b3d_chrep(sigaf33, x33, vplg33t)
            call x33x6(sigaf33,sigaf6)
!      prise en compte des fissures fermees sur
!      les contraintes effectives
            do i = 1, 6
                sigaf6(i)=(sigaf6(i)+sigafc6(i))
            end do
!      *****************************************************************
!      modif pression de gel a combiner a la contrainte effectives
!      seul le gel pas encore ds les fissures contribue a la contrainte
!      le gel passé ds les fissures a recristallisé il est devenu de l
!      qui ne doit plus être comptabilisée dans la pression de gel
            sigpg0=-bg1*pg1
            do i = 1, 3
                sigpg6(i)=sigpg0
            end do
            do i = 4, 6
                sigpg6(i)=0.d0
            end do
!      le gel dans les fissures n a plus de pression significative une f
!      que la deformation d ouverture est devenue une deformation plasti
!      la contribution du gel dans la reprise des contraintes effectives
!      est donc sigag=-bg(1-dg)Pg
            call b3d_sigapp(sigpg6, ddg66, sigag6, .true.)
!      retour base fixe
            call x6x33(sigag6,x33)
            call b3d_chrep(sigag33, x33, vplg33t)
            call x33x6(sigag33,sigag6)
!      consequence :les contraintes effectives dans la matrice non fissu
!      calculee dans endo3d est surestimee de bg.dg.pg
!      *****************************************************************
!      contrainte totale integrant la pression de gel
            do i = 1, 6
                sigaf6(i)=sigaf6(i)+sigag6(i)
            end do

        else
!      pas de gel de rag ni de def car xmg==0, sigaf6  inchangé
            do i = 1, 6
                sigag6(i)=0.d0
            end do
            do i = 1, 3
                dw3(i)=0.d0
            end do
            end if
!
!***********************************************************************
!     prise en compte de l amplification de retrait par precontrainte
!     (effet Picket anisotrope)
!     externe (en lien avec lendommagement hydrique diffus en theorie)
            if (pw1 .lt. 0.d0) then
!      contribution a sigext de la pression non corrigee
                sigpw0=-bw1*pw1
!      on se place dans les directions principales de contrainte apparen
                call b3d_partition(sigaf6, sigaf3, vsigaf33, vsigaf33t, sigaft6,&
                                   sigafc6, sigafc3, sigaft3)
!      contrainte equivalente maxi pour cet effet
!       smax=min(rt2,0.99d0*sfld)
!      smax==0 implique qu il n'y ait pas de fluage de dessiccation en t
!      cf these Nary pour integrer un fluage de traction en dessiccation
                smax=0.d0
!      on teste  l opportunite de l amplification dans chaque direction
                do i = 1, 3
                    sigext1=sigaf3(i)+sigpw0
                    sigext2=sigext1/(1.d0+sigpw0/sfld)
                    if (sigext2 .gt. smax) then
!          on limite la minoration a smax
                        bw2=bw1*(1.d0-smax/sfld)
!          print*,'minoration limite de bw', bw2
                    else
!          la modification est possible avec sigext2
                        bw2=bw1*(1.d0-sigext2/sfld)
!          print*,'majoration de bw2',bw2
                        end if
                        sigaf3(i)=sigaf3(i)-bw2*pw1
                        x6(i)=sigaf3(i)
                        end do
                        do i = 4, 6
                            x6(i)=0.d0
                        end do
!      retour base fixe
                        call x6x33(x6,x33)
                        call b3d_chrep(sigaf33, x33, vsigaf33t)
                        call x33x6(sigaf33,sigaf6)
!      recuperation des contraintes effectives integrant
!      les pressions de disjonction
!       do i=1,6
!        sigef6(i)=sigaf6(i)/(1.d0-dflu0)
!       end if
                        end if
!
!***********************************************************************
!     calcul des contraintes suivant la formulation
!     (en non saturé : la contrainte non endo = la contrainte totale ca
!     la contrainte dans le solide peut localement etre = a la totale)
!       i.e pas de depression dans les fissures
                        do i = 1, 6
                            sigat6(i)=sigaf6(i)
                        end do
!      print*,'pw ds b3d sigd',pw1,' bw',bw1 ,' mfr',mfr
!     changement said ok
!     traitement particulier de la formulation poreuse
                        if (mfr .eq. 33) then
!       formulation poreuse
!       sigaf contient les depression d'eau et la pression de gel
!       seules les supressions sont a traiter en mode poreux
!       elles sont triatées en dehors du point d integration mecanique
!       on fournit donc les contraintes effectives
!       complement par rapport a modif said cas non sature
                            if (pw1 .lt. 0.d0) then
!        on enleve les contraintes hydriques car le code va les remettre
!        mais on les a deja considere ici en cas de depression
                                do i = 1, 3
                                    sigat6(i)=sigaf6(i)+bw1*pw1
                                end do
                                end if
                            else
!      formulation non poreuse
!      en saturé : la contrainte non endo = la contrainte effective
!      (la contrainte dans le solide est toujours purement effective)
!      il ne manque que les surpression hydriques que l on rajoute ici
                                if (pw1 .ge. 0.d0) then
!        print*,'surpression',bw1,pw1
!       il faut rajouter les supressions
                                    do i = 1, 6
                                        if (i .le. 3) then
                                            sigat6(i)=sigaf6(i)-bw1*pw1
                                        else
                                            sigat6(i)=sigaf6(i)
                                            end if
                                            end do
                                            end if
                                            end if
end subroutine
