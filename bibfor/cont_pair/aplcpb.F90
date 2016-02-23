subroutine aplcpb(mail, lima, nbmma, lies, nbmes, resoco,&
                  izone, lnewtg, tole, nbmact, vectap, loptin)
!  
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
#include "asterfort/apdmae.h"
#include "asterfort/aprtpe.h"
#include "asterfort/cncinv.h"
#include "asterfort/wkvect.h"
#include "asterfort/cnvois.h"
#include "asterfort/codent.h"
#include "asterfort/testvois.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "asterf_types.h"
!
    integer, intent(in) :: nbmes
    integer, intent(in) :: nbmma
    integer, intent(in) :: lima(nbmma)
    integer, intent(in) :: lies(nbmes)
    integer, intent(in) :: izone
    character(len=8), intent(in) :: mail
    character(len=24),intent(in) :: resoco  
    real(kind=8), intent(in) :: tole
    aster_logical, intent(in) :: lnewtg
    aster_logical, intent(in) :: loptin
    integer, intent(out) :: nbmact
    integer, pointer :: vectap(:)
! ------------------------------------------------------------------------------------------------
!     ROUTINE APPARIEMENT MAITRE-ESCLAVE LAC (Methode pang avec remise "robuste")
! ------------------------------------------------------------------------------------------------
!   IN        MAIL       MAILLAGE
!   IN        LIMA       LISTE DES MAILLES MAITRES
!   IN        NBMMA      NOMBRE DE MAILLES MAITRES
!   IN        LIES       LISTE DES MAILLES ESCLAVES
!   IN        NBMES      NOMBRE DE MAILLES ESCLAVES
!   IN        RESOCO     SD RESOLUTION DU CONTACT
!   IN        DEFICO     SD DEFINITION DU CONTACT
!   IN        IZONE      ZONE DE CONTACT
!   IN        LNEWTG     LOGICAL FORMULATION NEWTON GENERALISE
!   IN        TOLE       TOLE APPARIEMENT   
!   OUT       VECTAP     COLLECTION APPARIEMENT (DISPERSEE)
!   OUT       NBMACT     NOMBRE DE MAILLES DE CONTACT      
! ------------------------------------------------------------------------------------------------
!
    integer ::tmp(nbmma), nbaptp, nbnes, nbnma, numaco, nuesco, ndim
    integer :: nbpint, jgpint, jpatch, jcoein, jpoint
    integer :: ind1, ind2, ind3,ind4, ind5, k, nupatch, nbpatch, cptpat, idrfpa, idim
    character(len=8) :: typma, types, knuzo
    character(len=24) ::   gpint, coeint, poiint
    real(kind=8) :: poids, gpin, pmi(nbmes), pmt(nbmes), poidin, aux, tole_appa
    real(kind=8) :: poitt, corint(32), corinp(32),comama(27),comaes(27)
    integer :: nbssma, lissma(8,4), nbnsma(8)
    integer :: nbsses, lisses(8,9), nbnses(8)
    real(kind=8) :: crssma(27), crsses(27)
    character(len=8) :: tpssma
    character(len=8) :: tpsses
    integer, pointer :: vectmp(:) => null()
!
! --- VARIABLES POUR LA RECHERCHE PANG -----------------------------------------------------------
    integer :: nblmes, nblmma, nblmdp
    integer :: limama(nbmma), limaes(nbmes), limadp(nbmes)
    integer :: idrfes, idrfma, idmxes, idmxma
    integer :: numadp
    aster_logical :: lrecup, debug
    integer, pointer :: flagma(:) => null()
    integer, pointer :: flgmad(:) => null()
    integer, pointer :: flages(:) => null()
!    
! --- VARIABLES CONNECTIVITE ET VOISINAGE --------------------------------------------------------
    character(len=24) :: cnivma, cnives, cnvoes, cnvoma
    integer :: jvoses, jvosma, nudpvs(4)
    integer :: nbvism, nbvise
    integer :: jcmapa
    integer :: itvois(4), itvaux(4), inut(4), nuvsco, ivois
    integer :: jtypma, jcoor
    real(kind=8) :: poidvs(4), potest, tolvis
    character(len=19) ::newgeo 
!
! ------------------------------------------------------------------------------------------------
! --- AFFICHAGE ----------------------------------------------------------------------------------
!
    !if (niv .ge. 2) then
        !write (ifm,*) '<APPARIEMENT LAC> PANG'
    !endif
!
    call jemarq()
    debug= .false.
! 
! --- INITIALISATION VARIABLE D'APPARIEMENT-------------------------------------------------------
!
    gpint=resoco(1:14)//'.GAPINT'
    coeint=resoco(1:14)//'.COEINT'
    poiint=resoco(1:14)//'.POIINT'
    inut(1:4)=0
    pmi(1:nbmes)=0.d0
    pmt(1:nbmes)=0.d0
    call jeveuo(gpint,'E',jgpint)
    call jeveuo(coeint, 'E', jcoein)
    call jeveuo(poiint, 'E', jpoint)
    nudpvs(1:4)=0
    poidvs(1:4)=0.d0
    tole_appa = 50.d0
!
! --- INITIALISATION ADRESSE DU VARIABLE LIEES AU MAILLAGE ---------------------------------------
!
    call jeveuo(mail//'.TYPMAIL','L',jtypma)
    newgeo = resoco(1:14)//'.NEWG'
    call jeveuo(newgeo(1:19)//'.VALE', 'L', jcoor)
!
! --- INITIALISATION VARIABLE LIEES AUX PACTH ----------------------------------------------------
!
    call jeveuo(jexnum(mail//'.PATCH',1),'L',jpatch)
    nbpatch=zi(jpatch+(izone-1)*2+2-1)
    idrfpa=zi(jpatch+(izone-1)*2+1-1)-1
    call jeveuo(mail//'.COMAPA','L', jcmapa)
!
! --- INITIALISATION LISTE DE RECHERCHE ---------------------------------------------------------- 
!   
    idmxma = maxval(lima)
    idmxes = maxval(lies)
    idrfma = minval(lima)
    idrfes = minval(lies)
!    
    AS_ALLOCATE(vi=flages, size= idmxes+1-idrfes)
    AS_ALLOCATE(vi=flagma, size= idmxma+1-idrfma)
    AS_ALLOCATE(vi=flgmad, size= idmxma+1-idrfma)
    flages(1:idmxes+1-idrfes) = 0
    flagma(1:idmxma+1-idrfma) = 0
    flgmad(1:idmxma+1-idrfma) = 0
!
    limama(1:nbmma) = 0
    limaes(1:nbmes) = 0
    limadp(1:nbmes) = 0
!
! --- INITIALISATION CONNECTIVITES INVERSES ------------------------------------------------------
!     
    call codent(izone, 'G', knuzo)
    cnvoma = resoco(1:14)//'.CNVOMA'//knuzo(1:1)
    cnvoes = resoco(1:14)//'.CNVOES'//knuzo(1:1)
    if (loptin) then
        cnivma = '&&aplcpg_cnivma'
        cnives = '&&aplcpg_cnives'
        call cncinv(mail,lies,nbmes,'V',cnives)
        call cncinv(mail,lima,nbmma,'V',cnivma)
        call jedetr(cnvoes)
        call jedetr(cnvoma) 
        call cnvois(mail  , lies  , nbmes  , idrfes, idmxes,&
                    cnives , jtypma, cnvoes)
        call cnvois(mail  , lima  , nbmma  , idrfma, idmxma,&
                    cnivma , jtypma, cnvoma)
        call jedetr(cnivma)
        call jedetr(cnives)     
    end if
! --- RECHERCHE DES MAILLES DE DEPART ------------------------------------------------------------
    120 continue
    if (debug) then 
        write(*,*)'Recherche mailles de départ'
    end if
!
    do ind1=1, nbmes
        nuesco=lies(ind1)
        if (flages(nuesco+1-idrfes).eq.1) then
            go to 130
        end if
        call apcoor(mail, jcoor, jtypma, nuesco, comaes,&
                    nbnes, types, ndim)
        call apdcma(types, lisses, nbnses, nbsses)
        do ind2=1, nbmma
            numaco=lima(ind2)
            if (flgmad(numaco+1-idrfma).eq.1) then
                go to 140
            end if
            call apcoor(mail, jcoor, jtypma, numaco, comama,&
                        nbnma, typma, ndim)
            call apdcma(typma, lissma, nbnsma, nbssma) 
            do ind3=1, nbssma
                if (nbnsma(ind3) .eq. 2) then
                    tpssma = 'SE2' 
                elseif (nbnsma(ind3) .eq. 3) then
                    tpssma = 'TR3'
                elseif (nbnsma(ind3) .eq. 4) then
                    tpssma = 'QU4'
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
                        tpsses = 'SE2'
                    elseif (nbnses(ind5) .eq. 3) then
                        tpsses = 'TR3'
                    elseif (nbnses(ind5) .eq. 4) then
                        tpsses = 'QU4'
                    else
                        ASSERT(.false.)
                    endif
!                      
                    do ind4=1, nbnses(ind5)
                        do idim=1, ndim
                            crsses((ind4-1)*3+idim) = comaes((lisses(ind5,ind4)-1)*3+idim) 
                        end do
                    end do             
                    call prjint(crsses,nbnses(ind5),tpsses,crssma,nbnsma(ind3),tpssma,&
                                corint,poids,nbpint, inut, tole, ndim)         
                    if (poids .gt. 100*tole) then
                        limadp(1)=numaco
                        nblmdp=1
                        limaes(1)=nuesco
                        nblmes=1
                        flages(nuesco+1-idrfes)=1
                        goto 100
                    end if
                 end do
            end do
            140 continue
        end do 
        130 continue       
    end do
    go to 110
100 continue
! --- APPARIEMENT RECHERCHE PANG -----------------------------------------------------------------
!
!
    if (debug) then 
        write(*,*)'Boucle appariement PANG'
    end if
    do while(nblmes .gt. 0) 
!    
! --- MAILLE ESCLAVE SUIVANTE -------------------------------------------------------------------- 
        nuesco=limaes(1)
        do k=1,nblmes-1
            limaes(k)=limaes(k+1)
        end do
        nblmes=nblmes-1
! --- Récuparation coordonnée maille esclave courante --------------------------------------------
        call apcoor(mail, jcoor, jtypma, nuesco, comaes,&
                    nbnes, types, ndim)
! ---- Approximation geometrie esclave cas quad4 et quad8 ----------------------------------------
        call apdmae(types, lisses, nbnses, nbsses)
! --- Récupération du patch associé 
       if (lnewtg) then
            nupatch=zi(jcmapa+nuesco-1)
            cptpat=nupatch+1-idrfpa
        end if
!        
! --- Poids de la maille esclave associée au patch nupacth ---------------------------------------
        call clpoma(ndim,types,comaes,nbnes,aux)
        pmt(cptpat)=pmt(cptpat)+aux
! --- Recherche des mailles voisines de nuesco ---------------------------------------------------
        if (ndim .eq. 2) then
            nbvise = 2
        elseif (types .eq. 'TR3' .or. types .eq. 'TR6') then
            nbvise = 3
        elseif  (types .eq. 'QU4' .or. types .eq. 'QU8' .or. types .eq. 'QU9') then
            nbvise = 4
        else
            ASSERT(.false.)
        endif
        call jeveuo(jexnum(cnvoes,nuesco+1-idrfes),'L', jvoses)
        nudpvs(1:nbvise)=0 
        poidvs(1:4)=0.d0     
! --- Première maille maitre à tester ------------------------------------------------------------
        numadp=limadp(1)
        limama(1)=numadp
        nblmma=1
        do k=1,nblmdp-1
            limadp(k)=limadp(k+1)
        end do
        nblmdp=nblmdp-1
        flagma(numadp+1-idrfma)=1
! --- Initialisation variable temporaire d'appariement -------------------------------------------
        do ind3=1, nbmma
            tmp(ind3)=0
        end do
        nbaptp=0
        lrecup= .true.
!
! ------------------------------------------------------------------------------------------------
       do while(nblmma .gt. 0)
            poitt=0.d0 
! ------------ Récuparation coordonnée maille maitre courante ------------------------------------
            numaco=limama(1)
            do k=1,nblmma-1
                limama(k)=limama(k+1)
            end do
            nblmma=nblmma-1
            call apcoor(mail, jcoor, jtypma, numaco, comama,&
                        nbnma, typma, ndim)          
! ------------ Approximation geometrie maitre ----------------------------------------------------
            call apdcma(typma, lissma, nbnsma, nbssma)
! ------------ Iitialiastion vecteur info inter voisin -------------------------------------------
            itvois(1:nbvise) = 0
!
! ------------ Boucle sur les sous-mailles --------------------------------
            do ind5=1, nbsses
                itvaux(1:nbvise) = 0
                if  (nbnses(ind5) .eq. 2 .and. ndim .eq. 2) then
                    tpsses = 'SE2'
                elseif  (nbnses(ind5) .eq. 3 .and. ndim .eq. 2) then
                    tpsses = 'SE3'        
                elseif (nbnses(ind5) .eq. 3 .and. ndim .eq. 3) then
                    tpsses = 'TR3'
                elseif (nbnses(ind5) .eq. 4 .and. ndim .eq. 3) then
                    tpsses = 'QU4'
                elseif (nbnses(ind5) .eq. 6 .and. ndim .eq. 3) then
                    tpsses = 'TR6'
                elseif (nbnses(ind5) .eq. 9 .and. ndim .eq. 3) then
                    tpsses = 'QU9'
                else 
                    ASSERT(.false.)
                end if
                do ind4=1, nbnses(ind5)
                    do idim=1,ndim
                        crsses((ind4-1)*3+idim) = comaes((lisses(ind5,ind4)-1)*3+idim) 
                    end do 
                end do
                do ind3=1, nbssma
                    if  (nbnsma(ind3) .eq. 2 .and. ndim .eq. 2) then
                        tpssma = 'SE2'
                    elseif (nbnsma(ind3) .eq. 3 .and. ndim .eq. 3) then
                        tpssma = 'TR3'
                    elseif (nbnsma(ind3) .eq. 4 .and. ndim .eq. 3) then
                        tpssma = 'QU4'
                    else
                        ASSERT(.false.)
                    endif
                    do ind4=1, nbnsma(ind3)
                        do idim=1,ndim
                            crssma((ind4-1)*3+idim) = comama((lissma(ind3,ind4)-1)*3+idim) 
                        end do 
                    end do
! ------------ Projection Intersection -----------------------------------------------------------
                    call prjint(crsses,nbnses(ind5),tpsses,crssma,nbnsma(ind3),tpssma,&
                                corint,poids,nbpint,itvaux, tole ,ndim)
!
! ------------ Test du poids --------------------------------------------------------------------- 
            if (poids .gt. tole) then
! ------------ Information intersection avec voisins ---------------------------------------------
                        if (tpsses.ne.types .and. types .eq. 'QU4' ) then
                            if (ind5.eq.1) then
                                itvois(1) = itvaux(1)
                                itvois(2) = itvaux(2)
                            elseif (ind5.eq.2) then 
                                itvois(3) = itvaux(1)
                                itvois(4) = itvaux(2)
                            end if
                        elseif (tpsses.ne.types .and. types .eq. 'QU8') then
                            if (ind5.eq.1) then
                                itvois(1) = itvaux(2)
                                itvois(4) = itvaux(3)
                            elseif (ind5.eq.2) then 
                                itvois(1) = itvaux(1)
                                itvois(2) = itvaux(2)
                            elseif (ind5.eq.3) then 
                                itvois(2) = itvaux(1)
                                itvois(3) = itvaux(2)
                            elseif (ind5.eq.4) then 
                                itvois(3) = itvaux(1)
                                itvois(4) = itvaux(2)
                            end if
                        else
                            do ivois=1,nbvise
                                if (itvaux(ivois).ne.0) then
                                    itvois(ivois) = itvaux(ivois)
                                endif
                            end do     
                        end if
                        poitt=poitt+poids 
! --------------- On calcul le gapint (Newtg) ----------------------------------------------------
                        if (lnewtg) then
                            if (types .ne. tpsses) then
                                call aprtpe(corint, nbpint, types,corinp, ndim, nudec=ind5 )
                            else
                                corinp(1:32)=corint(1:32)
                            endif
                            call gapint(crsses,nbnses(ind5), crssma, nbnsma(ind3), corint, nbpint,&
                                        tpsses, tpssma, gpin, poidin, tole, ndim)
                            zr(jgpint+nupatch-1)=zr(jgpint+nupatch-1)+gpin
                            pmi(cptpat)=pmi(cptpat)+poidin
                       end if
                    end if
                end do
            end do
! ------------ Menage ----------------------------------------------------------------------------
            if (poitt .gt. tole) then
! --------------- On remplie le vecteur temporaire d'appariement ---------------------------------
                nbaptp=nbaptp+1
                tmp(nbaptp)=numaco
                flgmad(numaco+1-idrfma) = 1
            end if
!           
! --------------- Mise a jour variable PANG ------------------------------------------------------
! --------------- Recherche des mailles voisines de numaco ---------------------------------------
            if (poitt .gt. tole .or. lrecup) then
                if (typma .eq. 'SE2' .or. typma .eq. 'SE3') then
                    nbvism = 2
                    tolvis = 0.2
                elseif (typma .eq. 'TR3' .or. typma .eq. 'TR6') then
                    nbvism = 3
                    tolvis = 0.05
                elseif  (typma .eq. 'QU4' .or. typma .eq. 'QU8' .or. typma .eq. 'QU9') then
                    nbvism = 4
                    tolvis = 0.4
                else
                    ASSERT(.false.)
                endif
!                
                call jeveuo(jexnum(cnvoma,numaco+1-idrfma),'L', jvosma)
                do ind2=1, nbvism
                    nuvsco=zi(jvosma-1+ind2)
                    if(nuvsco .ne. 0 .and. flagma(nuvsco+1-idrfma) .eq. 0 ) then
                        limama(nblmma+1)=nuvsco
                        nblmma=nblmma+1
                        flagma(nuvsco+1-idrfma)=1
                    endif
                end do
!
                do ind2=1,nbvise
                    nuvsco=zi(jvoses-1+ind2)
!                   
                    if ( nuvsco .ne. 0 .and. itvois(ind2) .eq. 1 .and.&
                         flages(nuvsco+1-idrfes) .eq. 0 .and.&
                         poidvs(ind2) .lt. tolvis) then
                        potest=0.d0
                        call testvois(mail,jcoor,jtypma,comama,typma,nuvsco,&
                                     tole,ndim,potest)
                        if (potest .gt. poidvs(ind2)) then
                            nudpvs(ind2)=numaco
                            poidvs(ind2)=potest
                        end if    
                    end if
                end do
!
                lrecup= .false.
!
            end if
        end do
!        
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
        end if
! ------- Mise a jour variable PANG --------------------------------------------------------------
        do ind2=1, nbvise
            nuvsco=zi(jvoses-1+ind2)
            if(nuvsco .ne. 0  .and. nudpvs(ind2).ne. 0 .and.&
                flages(nuvsco+1-idrfes) .eq. 0 ) then
                limaes(nblmes+1)=nuvsco
                nblmes=nblmes+1
                flages(nuvsco+1-idrfes)=1
                limadp(nblmdp+1)=nudpvs(ind2)
                nblmdp=nblmdp+1
            endif
        end do
! ------- Menage ---------------------------------------------------------------------------------
        flagma(1:idmxma+1-idrfma) = 0
    end do
    if (debug) then 
        write(*,*)'Fin boucle appariement PANG'
        write(*,*)'maille contact trouvee',nbmact
    end if
    go to 120
    110 continue
    if (debug) then 
        write(*,*)'Fin appariement PANG ROBUSTE'
    end if
! ------- Gestion des Patchs non apparié ---------------------------------------------------------
    if (lnewtg) then
        do ind1=1, nbpatch
            nupatch=ind1-1+idrfpa
            if (pmi(ind1) .le. tole) then
                zr(jgpint+nupatch-1)=r8nnem()
                zr(jcoein+nupatch-1)=0.d0
            end if
        end do
    end if
! ------- GAP moyen ------------------------------------------------------------------------------
    if (lnewtg) then
        do ind1=1, nbpatch
            nupatch=ind1-1+idrfpa
            if (.not.isnan(zr(jgpint+nupatch-1))) then
                zr(jgpint+nupatch-1)=zr(jgpint+nupatch-1)/pmi(ind1)
                zr(jcoein+nupatch-1)=pmi(ind1)/pmt(ind1)
                zr(jpoint+nupatch-1)=pmi(ind1)
            end if
        end do
    end if
    AS_DEALLOCATE(vi=flagma)
    AS_DEALLOCATE(vi=flages)
    AS_DEALLOCATE(vi=flgmad)
    call jedema()
!
end subroutine
