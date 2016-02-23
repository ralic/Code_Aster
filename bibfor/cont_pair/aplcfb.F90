subroutine aplcfb(mail, lima, nbmma, lies, nbmes, resoco,&
                  izone, lnewtg, tole, nbmact, vectap)
   
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
! aslint: disable=W1306
!
    implicit none
#include "jeveux.h"
#include "asterc/r8nnem.h"
#include "asterfort/jecrec.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/apcoor.h"
#include "asterfort/prjint.h"
#include "asterfort/gapint.h"
#include "asterfort/jecroc.h"
#include "asterfort/clpoma.h"
#include "asterfort/assert.h"
#include "asterfort/apdcma.h"
#include "asterfort/aprtpe.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "asterf_types.h"

!
    integer :: nbmes, nbmma, lima(nbmma), lies(nbmes), nbmact, izone
    character(len=8) :: mail
    character(len=24) :: resoco
    real(kind=8) :: tole
    aster_logical :: lnewtg
    integer, pointer :: vectap(:)
! ----------------------------------------------------------------------
!     ROUTINE APPARIEMENT MAITRE-ESCLAVE LAC
! ----------------------------------------------------------------------
!   IN        LIMA       LISTE DES MAILLES MAITRES
!   IN        NBMMA      NOMBRE DE MAILLES MAITRES
!   IN        LIES       LISTE DES MAILLES ESCLAVES
!   IN        NBMES      NOMBRE DE MAILLES ESCLAVES
!   IN        RESOCO     SD RESOLUTION DU CONTACT
!   IN        IZONE      ZONE DE CONTACT
!   IN        LNEWTG     LOGICAL FORMULATION NEWTON GENERALISE
!   IN        TOLE       TOLE APPARIEMENT   
!   OUT       VECTAP     VECTEUR APPARIEMENT 
!   OUT       NBMACT     NOMBRE DE MAILLES DE CONTACT      
! ----------------------------------------------------------------------
!
    integer ::tmp(nbmma), nbaptp, nbnes, nbnma, numaco, nuesco, ndim, idim
    integer :: nbpint, jgpint, jpatch, jcoein, jcmapa
    integer :: ind1, ind2, ind3, ind4, ind5, nupatch, idrfpa, nbpatch, idpatc
    character(len=8) :: typma, types
    character(len=24) ::  gpint, coeint
    real(kind=8) :: poids, gpin, pmi(nbmes), pmt(nbmes), poidin, aux, crsses(27)
    real(kind=8) :: crssma(27), poitt, corint(32), corinp(32), comama(27) , comaes(27)
    integer :: nbssma, lissma(8,4), nbnsma(8),inut(4)
    integer :: nbsses, lisses(8,4), nbnses(8)
    character(len=8) :: tpssma, tpsses 
    integer :: jtypma, jcoor
    character(len=19) ::newgeo
    integer, pointer :: vectmp(:) => null()     
! -----------------------------------------------------------------------------------------------
!
    call jemarq()
! 
! --- INITIALISATION ----------------------------------------------------------------------------
!

    gpint=resoco(1:14)//'.GAPINT'
    coeint=resoco(1:14)//'.COEINT'
    inut(1:4)=0
    pmi(1:nbmes)=0.d0
    pmt(1:nbmes)=0.d0
    call jeveuo(gpint,'E',jgpint)
    call jeveuo(coeint, 'E', jcoein) 
!
! --- INITIALISATION ADRESSE DU VARIABLE LIEES AU MAILLAGE ---------------------------------------
!
    call jeveuo(mail//'.TYPMAIL','L',jtypma)
    newgeo = resoco(1:14)//'.NEWG'
    call jeveuo(newgeo(1:19)//'.VALE', 'L', jcoor)
!
! --- Initialisation du patch
!
    call jeveuo(jexnum(mail//'.PATCH',1),'L',jpatch)
    nbpatch = zi(jpatch-1+(izone-1)*2+2) 
    idrfpa = zi(jpatch-1+(izone-1)*2+1)-1
    call jeveuo(mail//'.COMAPA','L',jcmapa)
!
! --- BOUCLE SUR LES MAILLES ESCLAVES ------------------------------------------------------------
!
    do ind1=1, nbmes
! --- Récuparation coordonnée maille esclave courante --------------------------------------------
        nuesco=lies(ind1)
        call apcoor(mail, jcoor, jtypma, nuesco, comaes,&
                    nbnes, types,ndim)
! --- Récupération du nouveau patch courant ------------------------------------------------------
        nupatch = zi(jcmapa-1+nuesco)
        idpatc = nupatch+1-idrfpa
! --- Poids maille esclave courante et contribution au poids du patch ----------------------------
        call clpoma(ndim,types,comaes,nbnes,aux)
        pmt(idpatc)=pmt(idpatc)+aux
! ---- Approximation geometrie esclave ------------------------------------------------------------
        call apdcma(types, lisses, nbnses, nbsses)
! -------- BOUCLE SUR LES MAILLES MAITRE ----------------------------------
! ------------ Initialisation ---------------------------------------------
            do ind3=1, nbmma
                tmp(ind3)=0
            end do
            nbaptp=0 
            do ind2=1, nbmma
            poitt=0.d0 
! ------------ Récuparation coordonnée maille maitre courante -------------
            numaco=lima(ind2)    
            call apcoor(mail, jcoor, jtypma, numaco, comama,&
                        nbnma, typma,ndim)
! ------------ Approximation geometrie maitre -----------------------------
            call apdcma(typma, lissma, nbnsma, nbssma)
! ------------ Boucle sur les sous-mailles --------------------------------
            do ind3=1, nbssma
                if (nbnsma(ind3) .eq. 2) then
                    tpssma=  'SE2' 
                elseif (nbnsma(ind3) .eq. 3) then
                    tpssma='TR3'
                elseif (nbnsma(ind3) .eq. 4) then
                    tpssma='QU4'
                else
                    ASSERT(.false.)
                end if 
                do ind4=1, nbnsma(ind3)
                    do idim=1,ndim
                         crssma((ind4-1)*3+idim) = comama((lissma(ind3,ind4)-1)*3+idim)
                    end do 
                end do 
                do ind5=1, nbsses
                    if (nbnses(ind5) .eq. 2) then
                        tpsses=  'SE2'
                    elseif (nbnses(ind5) .eq. 3) then
                        tpsses='TR3'
                    elseif (nbnses(ind5) .eq. 4) then
                        tpsses='QU4'
                    else
                        ASSERT(.false.)
                    endif                
                    do ind4=1, nbnses(ind5)
                        do idim=1, ndim
                            crsses((ind4-1)*3+idim) = comaes((lisses(ind5,ind4)-1)*3+idim)
                        end do
                    end do             
! ------------ Projection Intersection -----------------------------------------------------------
                    call prjint(crsses,nbnses(ind5),tpsses,crssma,nbnsma(ind3),tpssma,&
                                corint,poids,nbpint, inut, tole, ndim)
! ------------ Test du poids ---------------------------------------------------------------------
                    if (poids .gt. tole) then
                    poitt=poitt+poids 
! --------------- On calcul le gapint (Newtg) -----------------------------
                        if (lnewtg) then
                            call aprtpe(corint, nbpint, types,corinp,ndim, nudec=ind5)
                            call gapint(comaes, nbnes, comama, nbnma, corinp, nbpint,&
                                        types, typma, gpin, poidin, tole, ndim)
                            zr(jgpint+nupatch-1)=zr(jgpint+nupatch-1)+gpin
                            pmi(idpatc)=pmi(idpatc)+poidin
                        end if
                    end if
                end do
            end do
            if (poitt .gt. tole) then     
! --------------- On remplie le vecteur temporaire d'appariement ----------
                nbaptp=nbaptp+1
                tmp(nbaptp)=numaco
            end if 
                               
        end do
! ------- On remplie le vect VECTAP
        if (nbaptp.ne.0) then
            if (nbmact .eq. 0) then
                AS_DEALLOCATE(vi=vectap)
                AS_ALLOCATE(vi=vectap, size = 3*nbaptp)
                do ind2=1, nbaptp
                    vectap((ind2-1)*3+1) = nuesco    
                    vectap((ind2-1)*3+2) = tmp(ind2)
                    vectap((ind2-1)*3+3) = izone
                end do
            else
                AS_ALLOCATE(vi=vectmp, size = 3*nbmact)
                do ind2=1, 3*nbmact
                    vectmp(ind2) = vectap(ind2)    
                end do
                AS_DEALLOCATE(vi=vectap)
                AS_ALLOCATE(vi=vectap, size = 3*nbmact+3*nbaptp)
                do ind2=1, 3*nbmact
                    vectap(ind2) = vectmp(ind2)    
                end do
                do ind2=1, nbaptp
                    vectap(3*nbmact+(ind2-1)*3+1) = nuesco    
                    vectap(3*nbmact+(ind2-1)*3+2) = tmp(ind2)
                    vectap(3*nbmact+(ind2-1)*3+3) = izone
                end do
                AS_DEALLOCATE(vi=vectmp)
            end if 
        nbmact=nbmact+nbaptp           
        else
 
        end if
! ------- Menage ----------------------------------------------------------
    end do
! -----------------------------------------------------------------------------------------------
! ------- Gestion des Patchs non apparié --------------------------------------------------------
    do ind1=1, nbpatch
        if (pmi(ind1) .le. tole) then
            nupatch=ind1-1+idrfpa
            zr(jgpint+nupatch-1)=r8nnem()
            zr(jcoein+nupatch-1)=0.d0   
        end if
    end do
! ------- GAP moyen -----------------------------------------------------------------------------
    
    if (lnewtg) then
        do ind1=1, nbpatch
            nupatch=idrfpa-1+ind1
            if (.not.isnan(zr(jgpint+nupatch-1))) then
                zr(jgpint+nupatch-1)=zr(jgpint+nupatch-1)/pmi(ind1)
                zr(jcoein+nupatch-1)=pmi(ind1)/pmt(ind1)
            end if          
        end do
    end if
! -----------------------------------------------------------------------------------------------
    call jedema()
end subroutine    
        

        
