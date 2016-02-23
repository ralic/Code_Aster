subroutine prjint(coores,nbnes,types,coorma,nbnma,typma,&
                  resu,poids,nbpint, itvois, tole, ndim)
    
!
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/lcodrm.h"
#include "asterfort/insema.h"
#include "asterfort/ptinma.h"
#include "asterfort/mmnewt.h"
#include "asterfort/mmtang.h"
#include "asterfort/mmnorm.h"
#include "asterfort/mmdonf.h"
#include "asterfort/apdist.h"
!
    integer :: nbnma, nbnes, nbpint, itvois(4), ndim
    real(kind=8) :: coorma(3,9), coores(3,9), poids, tole
    character(len=8) :: typma, types 
    real(kind=8) :: resu(ndim-1,16)
! ----------------------------------------------------------------------
!         Projection intersection d'une maille maitre et esclave 
!         dans l'espace paramétrique esclave
! ----------------------------------------------------------------------
! IN         COORMA      COORDONNÉE DES POINT DE LA MAILLE MAITRE
! IN         NBNMA       NOMBRE DE POINT CONSTITUANT LA MAILLE MAITRE
! IN         TYPMA       TYPE DE MAILLE MAITRE
! IN         COORES      COORDONNÉE DES POINT DE LA MAILLE ESCLAVE
! IN         NBNES       NOMBRE DE POINT CONSTITUANT LA MAILLE ESCLAVE
! IN         TYPES       TYPE DE MAILLE ESCLAVE
! IN/OUT     RESU        OBJET JEVEUX RESULTAT 
!                        Coordonnée des points d'intersection 
!                        dans l'espace paramétrique maitre
! OUT        POIDS       AIRE DE L'INTERSECTION
! OUT        NBPINT      NOMBRE DE POINT D'INTERSECTION
! IN         TOLE        TOLERANCE
! ----------------------------------------------------------------------
!
    real(kind=8) :: corint(ndim-1,16), coresp(ndim-1,4), cormap(ndim-1,4), tevapr
    real(kind=8) :: aux1,aux2, tau1(3), tau2(3), coorno(3), xpt, ypt, inut, d, auxv(3), auxvec(3,3)
    real(kind=8) :: xp1, yp1, xp2, yp2 ,auxinse(3,3), normma(3), normes(3), dff(2,9),sig, dist(3)
    integer :: ind, ind2, idim, niverr, test, indsu(16),nbints, nbnesp, indpr(4)
    integer :: nbnaux
    character(len=8) :: typaux
!
! --- Initialisation ---------------------------------------------------
!
    nbpint=0
    poids=0.d0
    test=0
    niverr=0
    resu(1:ndim-1,1:16)=0.d0
    if (typma .eq. 'TR3') then
        aux1=0.d0
        aux2=0.d0
    elseif (typma .eq. 'SE2') then
        aux1=0.d0 
    end if
    call mmdonf(ndim,nbnma,typma,aux1,aux2,&
                 dff)
    tau1(1:3)=0.d0
    tau2(1:3)=0.d0
    call mmtang(ndim,nbnma,coorma,dff,tau1,tau2)
    call mmnorm(ndim, tau1, tau2, normma, inut)
    normma(1:3)=-normma(1:3)
    tau1(1:3)=0.d0
    tau2(1:3)=0.d0
    if (types .eq. 'TR3'.or. types .eq. 'TR6') then
        aux1=1.d0/3.d0
        aux2=1.d0/3.d0
        typaux = 'TR3'
        nbnaux = 3 
    elseif (types .eq. 'QU4'.or. types .eq. 'QU8'.or. types .eq. 'QU9') then
        aux1=0.d0
        aux2=0.d0
        typaux = 'QU4'
        nbnaux = 4  
    elseif (types .eq. 'SE2'.or. types .eq. 'SE3') then
        aux1=0.d0
        typaux = 'SE2'
        nbnaux = 2   
    end if
    call mmdonf(ndim,nbnaux,typaux,aux1,aux2,&
                 dff)
    tau1(1:3)=0.d0
    tau2(1:3)=0.d0
    call mmtang(ndim,nbnaux,coores,dff,tau1,tau2)
    call mmnorm(ndim, tau1, tau2, normes, inut)
    normes(1:3)=-normes(1:3)
    tau1(1:3)=0.d0
    tau2(1:3)=0.d0
!  
! --- Verificatuion type de maille maitre
!
    if (typma .eq. 'TR6' .or. typma .eq. 'SE3') then
        ASSERT(.false.)
    else if (typma .eq. 'QU8' .or. typma .eq. 'QU9') then
        ASSERT(.false.)
    end if
!
! --- Projection des noeuds maitres dans l'espace paramétrique esclave -
!
    do ind=1, nbnma
        coorno(1:3)=0.d0
        do idim=1, ndim
            coorno(idim)=coorma(idim,ind)
        end do     
        call mmnewt(types,nbnes,ndim,coores,coorno,200,&
                    tole,aux1,aux2,tau1,tau2,niverr)
        if (niverr.eq.0) then
            cormap(1,ind) = aux1
            if ((ndim-1) .eq. 2) then
                cormap(2,ind) = aux2
            end if
        else
           write(*,*)"mmnewt failed"
           ASSERT(.false.)
        endif
! --- Test validité de la projection (premier)    à retravailler!   
        call apdist(types, coores, nbnes, aux1, aux2,&
                    coorno, d, auxv)
        !call mmnorm(ndim, tau1, tau2, normes, inut)
        !normes(1:3)=-normes(1:3)
        auxvec(ind,1:3)=auxv(1:3)            
        if (ndim .eq. 3) then
            sig=auxvec(ind,1)*normes(1)+auxvec(ind,2)*normes(2)+auxvec(ind,3)*normes(3)
        elseif (ndim .eq. 2) then
            sig=auxvec(ind,1)*normes(1)+auxvec(ind,2)*normes(2)
        end if       
        dist(ind)=-sign(d,sig)
        if (ndim .eq. 3) then
            tevapr=auxvec(ind,1)*normma(1)+auxvec(ind,2)*normma(2)+auxvec(ind,3)*normma(3)
        elseif (ndim.eq.2) then
            tevapr=auxvec(ind,1)*normma(1)+auxvec(ind,2)*normma(2)
        end if
        if (dist(ind) .lt. 0.d0-tole) then
            if (tevapr .gt. 0.d0-tole) then
                 !WRITE(*,*)'error'
                 go to 110
            end if
        elseif (dist(ind) .gt. 0.d0+tole) then
            if (tevapr .lt. 0.d0+tole) then
                !WRITE(*,*)'error'
                go to 110
            end if
        end if     
    end do
! --- Test validité de projection (deuxième)     
!    do ind=1,nbnma-1
!        write(*,*)'auxvec1',auxvec(ind,1:3)
!        write(*,*)'auxvec2',auxvec(ind+1,1:3)
!        if (ndim.eq.3) then
!            tevapr=auxvec(ind,1)*auxvec(ind+1,1)+&
!                   auxvec(ind,2)*auxvec(ind+1,2)+&
!                   auxvec(ind,3)*auxvec(ind+1,3)
!        elseif (ndim.eq.2) then
!            tevapr=auxvec(ind,1)*auxvec(ind+1,1)+&
!                   auxvec(ind,2)*auxvec(ind+1,2)
!        end if
!        if ((dist(ind) .lt. 0.d0-tole .and. dist(ind+1) .lt. 0.d0-tole) .or.&
!            &(dist(ind).gt. 0.d0+tole .and. dist(ind+1) .gt. 0.d0+tole)) then
!            if (tevapr .lt. 0.d0) then
!                write(*,*) "error4"
!                go to 110
!            end if
!        end if
!    end do
    
!
! --- Coordonnées paramétriques des noeuds esclaves ----------------------
!
! --- CAS 2D
    if (types .eq. 'SE2' .or. types .eq. 'SE3') then
        coresp(1,1) = -1.d0
        coresp(1,2) =  1.d0
        nbnesp = 2
! --- CAS 3D
    elseif (types .eq. 'TR3' .or. types .eq. 'TR6') then
        coresp(1,1)=0.d0
        coresp(2,1)=0.d0
        coresp(1,2)=1.d0
        coresp(2,2)=0.d0
        coresp(1,3)=0.d0
        coresp(2,3)=1.d0
        nbnesp = 3
    else if (types .eq. 'QU4' .or. types .eq. 'QU8' .or. types .eq. 'QU9') then
        coresp(1,1)=-1.d0
        coresp(2,1)=-1.d0
        coresp(1,2)=1.d0
        coresp(2,2)=-1.d0
        coresp(1,3)=1.d0
        coresp(2,3)=1.d0
        coresp(1,4)=-1.d0
        coresp(2,4)=1.d0
        nbnesp = 4
    else
        ASSERT(.false.)
    end if
    indpr(1)=nbnesp
    do ind=2,nbnesp
        indpr(ind)=ind-1
    end do

!
! --- Noeuds maitres appartenant à l'intersection ---------------------
!
! --- CAS 2D SURFACE 1D SEG   
    if (nbnesp .eq. 2) then
        do ind=1, nbnma
            xpt=cormap(1,ind)
            if (xpt .ge. (-1.d0-tole) .and. xpt .le. (1.d0+tole)) then       
                nbpint=nbpint+1
                corint(1,nbpint)=xpt
            endif
        end do
! --- CAS 3D SURFACE 2D TRIA
    elseif (nbnesp .eq. 3) then
        do ind=1, nbnma
            xpt=cormap(1,ind)
            ypt=cormap(2,ind)
            if (xpt.ge.-tole .and. ypt.ge.-tole .and. (ypt+xpt).le.(1.d0+tole)) then       
                nbpint=nbpint+1
                corint(1,nbpint)=xpt
                corint(2,nbpint)=ypt
            endif
        end do
! --- CAS 3D SURFACE 2D QUAD
    elseif (nbnesp .eq. 4) then
        do ind=1, nbnma
            xpt=cormap(1,ind)
            ypt=cormap(2,ind)
            if (xpt.ge. -1.d0-tole .and. ypt.ge. -1.d0-tole .and. ypt.le.(1.d0+tole) .and.&
                xpt.le.(1.d0+tole)) then       
                nbpint=nbpint+1
                corint(1,nbpint)=xpt
                corint(2,nbpint)=ypt
            endif
        end do
    else
        ASSERT(.false.)
    end if
    
!
! --- Noeuds esclaves appartenant à l'intersection ---------------------
!    
    do ind=1, nbnesp
        xpt=coresp(1,ind)
        if ((ndim-1) .eq. 2) then    
            ypt=coresp(2,ind)
        elseif ((ndim-1).eq.1) then
            ypt =0.d0
        end if
        call ptinma(cormap,nbnma,typma,xpt,ypt,test,tole, ndim)
        if (test.eq.1) then    
            nbpint=nbpint+1
            corint(1,nbpint)=xpt
            if ((ndim-1) .eq. 2) then
                corint(2,nbpint)=ypt
            end if
! --- On renseigne le test intersection avec voisins -------------------
            if ((ndim-1) .eq. 2) then
                itvois(ind)=1
                itvois(indpr(ind))=1
            else if ((ndim-1) .eq. 1) then
                itvois(ind)=1
            endif
!-------- Erreur intersection dans ptinma----------------            
        else if(test.eq. -1) then
            !write(*,*),'error intersection'
            poids=0.d0
            nbpint=0
            itvois(1:4)=0
            resu(ndim-1,16)=0.d0
            go to 110   
        endif
    end do
!
! --- Intersection des arêtes (CAS 3D) ------------------------------------------------------------
!
    if (ndim .eq. 3 ) then
! --- Vecteur indice suivant
        do ind=2, nbnma
            indsu(ind-1)=ind
        end do
        indsu(nbnma)=1
! --- Boucle sur les segments de la maille maitre projetée -------------   
        do ind=1, nbnma
            xp1=cormap(1,ind)
            yp1=cormap(2,ind)
            xp2=cormap(1,indsu(ind))
            yp2=cormap(2,indsu(ind))
            call insema(coresp,nbnesp,xp1,yp1,xp2,yp2,auxinse,nbints,tole, itvois)
            if (nbints.gt.0) then
                do ind2=1,nbints
                    nbpint=nbpint+1
                    corint(1,nbpint)=auxinse(1,ind2)
                    corint(2,nbpint)=auxinse(2,ind2)
                end do
            endif
        end do
        ASSERT(nbpint.le.16)
    end if
    if ((nbpint.gt.2 .and. ndim .eq. 3 ).or. (nbpint .ge. 2 .and. ndim.eq.2) ) then
     
! ---- On ordonne dans le sens trigo et supprime les doublons ----------
        call lcodrm(corint,nbpint,tole,resu, ndim)
! ---- On calcule le poids ---------------------------------------------
        if ((ndim-1) .eq. 2) then
            do ind=2, nbpint
                indsu(ind-1)=ind
            end do
            indsu(nbpint)=1
            do ind=1,nbpint
                poids=poids+resu(1,ind)*resu(2,indsu(ind))-resu(1,indsu(ind))*resu(2,ind)
            end do
            poids=1.d0/2.d0*poids
            poids=sqrt(poids**2)
        else
            poids=sqrt((resu(1,2)-resu(1,1))**2)
        end if
    endif
110 continue 
!
end subroutine
