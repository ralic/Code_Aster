subroutine xprvir(fiss, covir, bavir, vitvir, angvir,&
                  numvir, numfon, nvit, nbeta, nbptff,&
                  radimp, radtor, damax, noma, locdom)
!
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    character(len=8) :: fiss, noma
!
    character(len=19) :: covir, bavir, vitvir, angvir, numvir
    character(len=24) :: nvit, nbeta
    integer :: numfon, nbptff
    real(kind=8) :: radimp, radtor, damax
    logical(kind=1) :: locdom
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: daniele.colombo at ifpen.fr
!
!
!      XPRVIR   : X-FEM PROPAGATION :CREATION DU FOND DE FISSURE VIRTUEL
!      ------     -     --                                  ---
!    CREATION D'UN FOND DE FISSURE VIRTUEL DANS LE CAS DE L'UTILISATION
!    DE LA METHODE UPWIND
!
!    ENTREE
!        NOMA    : NOM DU CONCEPT MAILLAGE
!        FISS    : NOM DU CONCEPT FISSURE X-FEM
!                  (FISSURE INITIALE DONT ON EXTRAIT LE FOND DE FISSURE)
!       NVIT     : VECTEUR DES VITESSES DE PROPAGATION POUR CHAQUE POINT
!                  DU FOND DE LA FISSURE (NOM DU CONCEPT)
!       NBETA    : VECTEUR DES ANGLES DE PROPAGATION POUR CHAQUE POINT
!                  DU FOND DE LA FISSURE (NOM DU CONCEPT)
!       NUMFON   : NOMBRE DE FONDS DE FISSURE
!       RADIMP   : RAYON DE LA ZONE DE REACTUALISATION DES LEVELS SETS
!       RADTOR   : RAYON DE LA ZONE DE REPROJECTION DES LEVELS SETS
!       DAMAX    : AVANCEMENT MAXIMUM DU FRONT DE FISSURE
!       LOCDOM   : LOCALISATION DES LEVELS SETS ACTIVEE
!       NBPTFF   : NOMBRE DE POINTS DU FOND DE FISSURE
!
!    SORTIE
!        NUMVIR  : VECTEUR CONTENANT LES POINTS DELIMITANT LES
!                  DIFFERENTS FRONTS DE FISSURE
!        COVIR   : VECTEUR CONTENANT LES COORDONNEES DES POINTS
!                  (REELS ET VIRTUELS) DU FOND
!        BAVIR   : VECTEUR CONTENANT LES BASES LOCALES DES POINTS
!                  (REELS ET VIRTUELS) DU FOND
!        VIRVIR  : VECTEUR CONTENANT LES VITESSES DES POINTS
!                  (REELS ET VIRTUELS) DU FOND
!        ANGVIR  : VECTEUR CONTENANT LES ANGLES DE PROPAGATION DES
!                  POINTS (REELS ET VIRTUELS) DU FOND
!     ------------------------------------------------------------------
!
!
    integer :: i, j, jcoor, nbno,  ifm, niv, jvit, jbeta, cfv, bfv
    integer :: vfv, afv, cfvpr, bfvpr, vfvpr, afvpr, nfv, npoin, nfvpr
    real(kind=8) :: v1, v2, ai, aj, ak, al, da1, da2, a1, a2
    integer ::  k
!
    real(kind=8) :: pi(3), pj(3), pk(3), pl(3), pij(3), pkl(3), p1(3), p2(3), vi
    real(kind=8) :: vj, vk, vl, dv1, dv2, normij, normkl, normj1, normk2, normjk
!
!     MULTIPLE CRACK FRONTS
    real(kind=8), pointer :: basefond(:) => null()
    integer, pointer :: fondmult(:) => null()
    real(kind=8), pointer :: fondfiss(:) => null()
!-----------------------------------------------------------------------
!     DEBUT
!-----------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
!     RECUPERATION DES CARACTERISTIQUES DU MAILLAGE
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbno)
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
!
!     RECUPERATION DU FOND DE FISSURE
    call jeveuo(fiss//'.FONDFISS', 'L', vr=fondfiss)
    call dismoi('NB_POINT_FOND', fiss, 'FISS_XFEM', repi=nbptff)
!
!     RETRIEVE THE DIFFERENT PIECES OF THE CRACK FRONT
    call jeveuo(fiss//'.FONDMULT', 'L', vi=fondmult)
!
!     RETRIEVE THE LOCAL REFERENCE SYSTEM FOR EACH NODE ON THE FRONT
    call jeveuo(fiss//'.BASEFOND', 'L', vr=basefond)
!
!     RETRIEVE THE CRACK'S SPEED AND PROPAGATION ANGLE FOR EACH NODE
!     ON THE FRONT
    call jeveuo(nvit, 'L', jvit)
    call jeveuo(nbeta, 'L', jbeta)
!
!     CREATION DES VECTEURS DE MISE A JOUR DES CARACTERISTIQUES DU FRONT
!     DE FISSURE APRES CREATION DU FRONT VIRTUEL
    call jeveuo(covir, 'E', cfv)
    call jeveuo(bavir, 'E', bfv)
    call jeveuo(vitvir, 'E', vfv)
    call jeveuo(angvir, 'E', afv)
    call jeveuo(numvir, 'E', nfv)
!
!     ON CREE DES VARIABLES DE GARAGE
    call wkvect('&&XPRVIR.CO_FON_VIRPR', 'V V R8', 4*(nbptff+2*numfon), cfvpr)
    call wkvect('&&XPRVIR.BA_FON_VIRPR', 'V V R8', 6*(nbptff+2*numfon), bfvpr)
    call wkvect('&&XPRVIR.VIT_FON_VIRPR', 'V V R8', (nbptff+2*numfon), vfvpr)
    call wkvect('&&XPRVIR.ANG_FON_VIRPR', 'V V R8', (nbptff+2*numfon), afvpr)
!
!     ON CREE UN VECTEUR OU SONT STOCKES LES DISTANCES
!     ENTRE LES DIFFERENTS FONDS DE FISSURE
    call wkvect('&&XPRVIR.NUM_FON_VIRPR', 'V V I', (2*numfon), nfvpr)
!
!     ON INITIALISE LES FONDS DE FISSURE ACTUEL
    do i = 1, (4*nbptff)
        zr(cfv+i-1)=fondfiss(i)
        zr(cfvpr+i-1)=fondfiss(i)
    end do
    do i = 1, 6*(nbptff)
        zr(bfv+i-1)=basefond(i)
        zr(bfvpr+i-1)=basefond(i)
    end do
    do i = 1, (nbptff)
        zr(vfv+i-1)=zr(jvit+i-1)
        zr(vfvpr+i-1)=zr(jvit+i-1)
    end do
    do i = 1, (nbptff)
        zr(afv+i-1)=zr(jbeta+i-1)
        zr(afvpr+i-1)=zr(jbeta+i-1)
    end do
    do i = 1, (2*numfon)
        zi(nfv+i-1)=fondmult(i)
        zi(nfvpr+i-1)=fondmult(i)
    end do
    if (niv .ge. 1) then
        write(ifm,*) ' '
        write(ifm,*) 'COORDONNEES DES POINTS VIRTUELS'
    endif
!
! --- PARTIE 1: CREATION DE POINTS VIRTUELS ENTRE LES DIFFERENTS
!     FRONTS DE FISSURE
!
    if (numfon .gt. 1) then
        if (niv .ge. 1) then
            write(ifm,*) ' '
            write(ifm,*) 'POINTS VIRTUELS INTERIEURS'
            write(ifm,*) '------------------------------------------'
        endif
!
        do k = 1, (numfon-1)
!         ON REPERE LE DERNIER POINT DU Kieme FOND
            npoin=zi(nfv-1+2*k)
!
!         COORDONNEES DES 2 DERNIERS POINTS DU FOND
!         ET DES 2 PREMIERS POINTS DU FOND SUIVANT
            do i = 1, 3
                pi(i)=zr(cfvpr-1+4*(npoin-2)+i)
                pj(i)=zr(cfvpr-1+4*(npoin-1)+i)
                pk(i)=zr(cfvpr-1+4*(npoin)+i)
                pl(i)=zr(cfvpr-1+4*(npoin+1)+i)
            end do
!
!         CALCUL DES VECTEURS IJ ET KL
            do i = 1, 3
                pij(i)=pj(i)-pi(i)
                pkl(i)=pl(i)-pk(i)
            end do
!
!         CALCUL DES NORMES DES VECTEURS IJ ET KL
            normij=sqrt(pij(1)**2+pij(2)**2+pij(3)**2)
            normkl=sqrt(pkl(1)**2+pkl(2)**2+pkl(3)**2)
!         ON NORME LES VECTEURS PIJ et PKL
            do i = 1, 3
                pij(i)=pij(i)/normij
                pkl(i)=pkl(i)/normkl
            end do
!
            if (locdom) then
!         LE RAYON D'ACTUALISATION DES LEVEL SETS A BIEN ETE DEFINI
                if (radimp .gt. 0.d0) then
                    do i = 1, 3
                        p1(i)=pj(i)+radimp*pij(i)
                        p2(i)=pk(i)-radimp*pkl(i)
                    end do
                else
                    do i = 1, 3
                        p1(i)=pj(i)+radtor*pij(i)
                        p2(i)=pk(i)-radtor*pkl(i)
                    end do
                endif
            else
!         SINON ON UTILISE L'AVANCEMENT MAXIMAL DE LA FISSURE
                do i = 1, 3
                    p1(i)=pj(i)+7*damax*pij(i)
                    p2(i)=pk(i)-7*damax*pkl(i)
                end do
            endif
!
!         DISTANCE ENTRE LES FRONTS DE FISSURES
            normjk=sqrt((pk(1)-pj(1))**2+(pk(2)-pj(2))**2+ (pk(3)-pj(&
            3))**2)
!
!         ON VERIFIE QUE LES 2 NOUVEAUX POINTS SONT BIEN ELOIGNES
            normj1=sqrt((p1(1)-pj(1))**2+(p1(2)-pj(2))**2+ (p1(3)-pj(&
            3))**2)
            normk2=sqrt((p2(1)-pk(1))**2+(p2(2)-pk(2))**2+ (p2(3)-pk(&
            3))**2)
!
!         SI LA DISTANCE EST TROP GRANDE ON RAPPROCHE LE POINT VIRTUEL
            if (normj1 .gt. (normjk/3.d0)) then
                normj1=normjk/3.d0
                do i = 1, 3
                    p1(i)=pj(i)+(normj1*pij(i))
                end do
            endif
            if (normk2 .gt. (normjk/3.d0)) then
                normk2=normjk/3.d0
                do i = 1, 3
                    p2(i)=pk(i)-(normk2*pkl(i))
                end do
            endif
!
!         ON S'ASSURE QUE LA VITESSE DES POINTS VIRTUELS EST POSITIVE
            vi=zr(vfvpr+npoin-2)
            vj=zr(vfvpr+npoin-1)
            vk=zr(vfvpr+npoin)
            vl=zr(vfvpr+npoin+1)
            dv1=(vj-vi)/normij
            dv2=(vl-vk)/normkl
            v1=vj+dv1*normj1
            v2=vk-dv2*normk2
            normj1=sqrt((p1(1)-pj(1))**2+(p1(2)-pj(2))**2+ (p1(3)-pj(&
            3))**2)
!
!         SI LES VITESSES SONT TROP FAIBLES, ON ELOIGNE OU ON
!         RAPPROCHE LES POINTS
            if (abs(v1) .lt. abs(2.d0*vj/3.d0) .or. (abs(v1).gt. abs(( 4.d0*vj/3.d0)))) then
                normj1=abs(vj/(3.d0*dv1))
                do i = 1, 3
                    p1(i)=pj(i)+(pij(i)*normj1)
                end do
                v1=vj+(dv1*normj1)
            endif
            if ((abs(v2).lt.abs(2.d0*vk/3.d0)) .or. (abs(v2).gt. abs(4.d0*vk/3.d0))) then
                normk2=abs(vk/(3.d0*dv2))
                do i = 1, 3
                    p2(i)=pk(i)-(pkl(i)*normk2)
                end do
                v2=vk-(dv2*normk2)
            endif
!
            ai=zr(afvpr+(npoin-2))
            aj=zr(afvpr+(npoin-1))
            ak=zr(afvpr+(npoin))
            al=zr(afvpr+(npoin+1))
            da1=(ai-aj)/normij
            da2=(al-ak)/normkl
            a1=aj+da1*normj1
            a2=ak-da2*normk2
!
!         SI L'ANGLE DE PROPAGATION SUR LE FRONT VIRTUEL
!         N'EST PAS COMPRIS ENTRE -90 ET 90 DEGRES, ON
!         RAPPROCHE LE POINT VIRTUEL DU FRONT PHYSIQUE
!         POUR LE POINT 1
            if ((a1.gt.(1.51d0)) .or. (a1.lt.(-1.51d0))) then
                normj1=(1.51d0-abs(ai))/abs(da1)
                do i = 1, 3
                    p1(i)=pj(i)+(pij(i)*normj1)
                end do
                v1=vj+(dv1*normj1)
                a1=aj+da1*normj1
            endif
!         POUR LE POINT 2
            if ((a2.gt.(1.51d0)) .or. (a2.lt.(-1.51d0))) then
                normk2=(1.51d0-abs(al))/abs(da2)
                do i = 1, 3
                    p2(i)=pk(i)-(pkl(i)*normk2)
                end do
                v2=vk-(dv2*normk2)
                a2=ak-da2*normk2
            endif
!         ON REMPLIT LE VECTEUR CONTENANT LES COORDONNEES DU FRONT
            do i = 1, 3
                zr(cfv-1+4*npoin+i)=p1(i)
                zr(cfv-1+4*(npoin+1)+i)=p2(i)
            end do
!
!         IMPRESSION DES COORDONNEES DES POINTS VIRTUELS
!         INTERIEURS EN INFO>0
            if (niv .ge. 1) then
                write(ifm,132) k,k+1
                write(ifm,131)(p1(j),j=1,3)
                write(ifm,131)(p2(j),j=1,3)
                132       format(1x,' ENTRE FOND DE FISSURE ',i2,1x,' ET ',i2)
                131       format(2x,3(e12.5,2x))
                write(ifm,*) '------------------------------------------'
            endif
!
!         ON ALLOUE UNE ABSCISSE CURVILIGNE
            zr(cfv-1+4*npoin+4)=(zr(cfvpr-1+4*(npoin-2)+4)+ zr(cfvpr-&
            1+4*(npoin-1)+4))/2.d0
            zr(cfv-1+4*(npoin+1)+4)=(zr(cfvpr-1+4*(npoin)+4)+ zr(&
            cfvpr-1+4*(npoin+1)+4))/2.d0
!
            do i = (npoin+1), nbptff
                do j = 1, 4
                    zr(cfv-1+4*(i+1)+j)=zr(cfvpr-1+4*(i-1)+j)
                end do
            end do
!
!         ON ECRIT LES VITESSES DANS LES EMPLACEMENTS RESERVES
            zr(vfv+npoin)=v1
            zr(vfv+npoin+1)=v2
            do i = (npoin+1), nbptff
                zr(vfv+(i+1))=zr(vfvpr+(i-1))
            end do
!
!         ON ECRIE LES VECTEURS DE LA BASE LOCALE DANS LES
!         EMPLACEMENTS RESERVES
            do j = 1, 6
                zr(bfv-1+6*npoin+j)=zr(bfvpr-1+6*(npoin-1)+j)
                zr(bfv-1+6*(npoin+1)+j)=zr(bfvpr-1+6*(npoin)+j)
                do i = (npoin+1), nbptff
                    zr(bfv+6*(i+1)+j)=zr(bfvpr+6*(i-1)+j)
                end do
            end do
!
!         ON ECRIT LES ANGLES DE PROPAGATION DANS EMPLACEMENTS
!         RESERVES
            do i = (npoin+1), nbptff
                zr(afv+(i+1))=zr(afvpr+(i-1))
            end do
!
!         REACTUALISATION DE LA FISSURE PRECEDENTE
!         REACTUALISATION DES COORDONNEES
            do i = 1, 4*(nbptff+2)
                zr(cfvpr+i-1)=zr(cfv+i-1)
            end do
!         REACTUALISATION DE LA BASE LOCALE
            do i = 1, 6*(nbptff+2)
                zr(bfvpr+i-1)=zr(bfv+i-1)
            end do
!         REACTUALISATION DE LA VITESSE DE PROPAGATION
            do i = 1, (nbptff+2)
                zr(vfvpr+i-1)=zr(vfv+i-1)
            end do
!         REACTUALISATION DES ANGLES DE PROPAGATION
            do i = 1, (nbptff+2)
                zr(afvpr+i-1)=zr(afv+i-1)
            end do
!         REACTUALISATION DU NOMBRE DE POINTS DU FOND
            nbptff=nbptff+2
!         REACTUALISATION DU VECTEUR FONDMULT
            zi(nfv+2*k-1)=zi(nfvpr+2*k-1)+1
            zi(nfv+2*k)=zi(nfvpr+2*k)+1
            do i = (2*(k+1)), (2*numfon)
                zi(nfv+i-1)=zi(nfvpr+i-1)+2
            end do
            do i = 1, (2*numfon)
                zi(nfvpr+i-1)=zi(nfv+i-1)
            end do
!
        end do
!
    endif
!
! --- PARTIE 2: CREATION DE POINTS VIRTUELS AUX EXTREMITES DU
!               FRONTS DE FISSURE
!
!     COORDONNEES DES 2 PREMIERS ET DES
!     2 DERNIERS POINTS DU FOND DE FISSURE
    do i = 1, 3
        pi(i)=zr(cfvpr-1+i)
        pj(i)=zr(cfvpr-1+4+i)
        pk(i)=zr(cfvpr-1+4*(nbptff-2)+i)
        pl(i)=zr(cfvpr-1+4*(nbptff-1)+i)
    end do
!
!     CALCUL DES VECTEURS IJ ET KL
    do i = 1, 3
        pij(i)=pj(i)-pi(i)
        pkl(i)=pl(i)-pk(i)
    end do
!
!     CALCUL DES NORMES DES VECTEURS IJ ET KL
    normij=sqrt(pij(1)**2+pij(2)**2+pij(3)**2)
    normkl=sqrt(pkl(1)**2+pkl(2)**2+pkl(3)**2)
!
!     ON NORME LES VECTEURS PIJ ET PKL
    do i = 1, 3
        pij(i)=pij(i)/normij
        pkl(i)=pkl(i)/normkl
    end do
!
!     CALCUL DES POINTS VIRTUELS A GAUCHE ET A DROITE
    if (locdom) then
!       LE RAYON D'ACTUALISATION DES LEVEL SETS A BIEN ETE DEFINI
        if (radimp .gt. 0.d0) then
            do i = 1, 3
                p1(i) = pi(i)-radimp*pij(i)
                p2(i) = pl(i)+radimp*pkl(i)
            end do
        else
            do i = 1, 3
                p1(i) = pi(i)-radtor*pij(i)
                p2(i) = pl(i)+radtor*pkl(i)
            end do
        endif
    else
!     SINON ON UTILISE L'AVANCEMENT MAXIMAL DE LA FISSURE
        do i = 1, 3
            p1(i) = pi(i)-7*damax*pij(i)
            p2(i) = pl(i)+7*damax*pkl(i)
        end do
    endif
!
!     DISTANCE ENTRE LE PREMIER ET LE DERNIER FOND DE FISSURE
    normjk = sqrt((pi(1)-pl(1))**2+(pi(2)-pl(2))**2+ (pi(3)-pl(3))**2)
!
!     ON VERIFIE QUE LES 2 NOUVEAUX POINTS SONT BIEN ELOIGNES
!     DISTANCE ENTRE LES POINTS 1 ET J
    normj1=sqrt((p1(1)-pi(1))**2+(p1(2)-pi(2))**2+&
     &           (p1(3)-pi(3))**2)
!     SI DISTANCE EST TROP GRANDE ON RAPPROCHE LE POINT VIRTUEL
    if (normj1 .gt. (normjk/3.d0)) then
        normj1=normjk/3.d0
        do i = 1, 3
            p1(i)=pi(i)-(normj1*pij(i))
        end do
    endif
!
!     DISTANCE ENTRE LES POINTS 2 ET K
    normk2 = sqrt((p2(1)-pl(1))**2+(p2(2)-pl(2))**2+ (p2(3)-pl(3))**2)
!     SI DISTANCE EST TROP GRANDE ON RAPPROCHE LE POINT VIRTUEL
    if (normk2 .gt. (normjk/3.d0)) then
        normk2=normjk/3.d0
        do i = 1, 3
            p2(i)=pl(i)+(normk2*pkl(i))
        end do
    endif
!
!     CALCUL DE LA VITESSE AUX POINTS VIRTUELS
    vi=zr(vfvpr)
    vj=zr(vfvpr+1)
    vk=zr(vfvpr+(nbptff-2))
    vl=zr(vfvpr+(nbptff-1))
    dv1=(vj-vi)/normij
    dv2=(vl-vk)/normkl
    v1=vi-dv1*normj1
    v2=vl+dv2*normk2
!
!     ON VERIFIE QUE LA VITESSE AUX POINTS VIRTUELS N'EST
!     NI TROP GRANDE NI TROP PETITE
    if ((abs(v1).lt.abs(1.d0*vi/3.d0)) .or. (abs(v1).gt.abs( 5.d0*vi/3.d0))) then
        normj1=abs(2.d0*vi/(3.d0*dv1))
        do i = 1, 3
            p1(i)=pi(i)-(pij(i)*normj1)
        end do
        v1=vi-(dv1*normj1)
    endif
    if ((abs(v2).lt.abs(1.d0*vl/3.d0)) .or. (abs(v2).gt.abs(5.d0 *vl/3.d0))) then
        normk2=abs(2.d0*vl/(3.d0*dv2))
        do i = 1, 3
            p2(i)=pl(i)+(pkl(i)*normk2)
        end do
        v2=vl+(dv2*normk2)
    endif
    ai=zr(afvpr)
    aj=zr(afvpr+1)
    ak=zr(afvpr+nbptff-2)
    al=zr(afvpr+nbptff-1)
    da1=(aj-ai)/normij
    da2=(al-ak)/normkl
    a1=ai-da1*normj1
    a2=al+da2*normk2
!
!     ON VERIFIE QUE L'ANGLE DE PROPAGATION AU FRONT VIRTUEL
!     EST COMPRIS ENTRE -90 et 90 DEGRE SINON ON RAPPROCHE
!     LE POINT VIRTUEL DU FRONT PHYSIQUE
    if ((a1.gt.(1.51d0)) .or. (a1.lt.(-1.51d0))) then
        normj1=(1.51d0-abs(ai))/abs(da1)
        do i = 1, 3
            p1(i)=pi(i)-(pij(i)*normj1)
        end do
        v1=vi-(dv1*normj1)
        a1=ai-da1*normj1
    endif
    if ((a2.gt.(1.51d0)) .or. (a2.lt.(-1.51d0))) then
        normk2=(1.51d0-abs(al))/abs(da2)
        do i = 1, 3
            p2(i)=pl(i)+(pkl(i)*normk2)
        end do
        v2=vl+(dv2*normk2)
        a2=al+da2*normk2
    endif
!
!     IMPRESSION DES COORDONNEES DES POINTS VIRTUELS
!     EXTERIEURS EN INFO=2
    if (niv .ge. 1) then
        write(ifm,*) ' '
        write(ifm,*) 'POINTS VIRTUELS EXTERIEURS'
        write(ifm,381)(p1(j),j=1,3)
        write(ifm,381)(p2(j),j=1,3)
        381   format(2x,3(e12.5,2x))
        write(ifm,*) '------------------------------------------'
    endif
!
!     ON RENTRE LES NOUVEAUX POINTS VIRTUELS
    do i = 1, 3
        zr(cfv-1+i)=p1(i)
    end do
    zr(cfv-1+4)=-100.d0
    do i = 1, 3
        zr(cfv-1+4*(1+nbptff)+i)=p2(i)
    end do
    zr(cfv-1+4*(1+nbptff)+4)=100.d0
!
!     ON RENTRE LES VITESSES DE LA FISSURE AU POINT VIRTUEL
    zr(vfv)=v1
    zr(vfv+1+nbptff)=v2
!     ON RENTRE LA BASE LOCALE ET L'ANGLE DE PROPAGATION
    do j = 1, 6
        zr(bfv-1+j)=zr(bfvpr-1+j)
        zr(bfv-1+6*(1+nbptff)+j)=zr(bfvpr-1+6*(nbptff-1)+j)
    end do
    zr(afv)=ai-da1*normj1
    zr(afv+nbptff+1)=al+da2*normk2
!     On copie les coordonnes,  du front physique dans
!     l emplacememt dans les emplacements reserves au
!     front virtuel
    do i = 1, nbptff
        do j = 1, 4
            zr(cfv-1+4*i+j)=zr(cfvpr-1+4*(i-1)+j)
        end do
        do j = 1, 6
            zr(bfv-1+6*i+j)=zr(bfvpr-1+6*(i-1)+j)
        end do
        zr(vfv+i)=zr(vfvpr+i-1)
        zr(afv+i)=zr(afvpr+i-1)
    end do
    do i = 1, (2*numfon-1)
        zi(nfv+i)=1+zi(nfv+i)
    end do
    zi(nfv+2*(numfon-1)+1)=1+zi(nfv+2*(numfon-1)+1)
    nbptff=nbptff+2
!
!
    call jedetr('&&XPRVIR.NUM_FON_VIRPR')
    call jedetr('&&XPRVIR.CO_FON_VIRPR')
    call jedetr('&&XPRVIR.BA_FON_VIRPR')
    call jedetr('&&XPRVIR.VIT_FON_VIRPR')
    call jedetr('&&XPRVIR.ANG_FON_VIRPR')
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
    call jedema()
end subroutine
