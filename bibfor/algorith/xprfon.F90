subroutine xprfon(noma, fiss, numfon, nvit, nbeta)
!
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/dismoi.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/vecini.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    character(len=8) :: fiss, noma
!
    character(len=24) :: nvit, nbeta
    integer :: numfon
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
!       XPRFON   : X-FEM PROPAGATION :RENUMEROTATION DU FRONT DE FISSURE
!       ------     -     --                                  ---
!    RENUMEROTATION DU FRONT DE FISSURE DANS LE CAS DE L'UTILISATION DE
!    LA METHODE UPWIND AVEC PLUSIEURS FOND DE FISSURE
!
!    ENTREE
!        NOMA    : NOM DU CONCEPT MAILLAGE
!        FISS    : NOM DU CONCEPT FISSURE X-FEM
!                  (FISSURE INITIALE DONT ON EXTRAIT LE FOND DE FISSURE)
!        NVIT    : VECTEUR DES VITESSES DE PROPAGATION POUR CHAQUE POINT
!                  DU FOND DE LA FISSURE (NOM DU CONCEPT)
!        NBETA   : VECTEUR DES ANGLES DE PROPAGATION POUR CHAQUE POINT
!                  DU FOND DE LA FISSURE (NOM DU CONCEPT)
!        NUMFON  : NOMBRE DE FONDS DE FISSURE
!
!     ------------------------------------------------------------------
    integer :: i, j, k, ifm, niv, nbnol
    integer ::    jbeta, jvit
    integer :: long, nptfg, nbptff, ivalue, nval
    integer :: npoin, npoinp, npoino, nponop
    real(kind=8) :: a1(4), b1(4), m1(3), a2(4), b2(4), m2(3)
    real(kind=8) :: prosca, normab, coeffk, mem(5), memo(5)
    real(kind=8) :: vect1, vect2
    real(kind=8), pointer :: vjbaso(:) => null()
    real(kind=8), pointer :: vjbetao(:) => null()
    integer, pointer :: vjfmulo(:) => null()
    real(kind=8), pointer :: vjfono(:) => null()
    real(kind=8), pointer :: vjvito(:) => null()
    real(kind=8), pointer :: vmemo(:) => null()
    real(kind=8), pointer :: fondfisg(:) => null()
    real(kind=8), pointer :: fondfiss(:) => null()
    integer, pointer :: fondmult(:) => null()
    real(kind=8), pointer :: basefond(:) => null()
!-----------------------------------------------------------------------
!     DEBUT
!-----------------------------------------------------------------------
    call jemarq()
!
    call infmaj()
    call infniv(ifm, niv)
    call vecini(4, 0.d0, mem)
!
!     RECUPERATION DES CARACTERISTIQUES DU FOND SUR LA GRILLE
!     NPTFG : NBRE DE POINTS DU FOND SUR LA GRILLE
!     (UTILE POUR REORIENTER LES FONDS SUR LE MAILLAGE REEL, cf DOC???)
    call jeveuo(fiss//'.FONDFISG', 'L', vr=fondfisg)
    call jelira(fiss//'.FONDFISG', 'LONMAX', long)
    nptfg=long/4
!
!     RECUPERATION DU FOND DE FISSURE
    call jeveuo(fiss//'.FONDFISS', 'E', vr=fondfiss)
    call dismoi('NB_POINT_FOND', fiss, 'FISS_XFEM', repi=nbptff)
!
!     RETRIEVE THE DIFFERENT PIECES OF THE CRACK FRONT
    call jeveuo(fiss//'.FONDMULT', 'E', vi=fondmult)
    call dismoi('NB_FOND', fiss, 'FISS_XFEM', repi=numfon)
!
!     RETRIEVE THE LOCAL REFERENCE SYSTEM FOR EACH NODE ON THE FRONT
    call jeveuo(fiss//'.BASEFOND', 'E', vr=basefond)
!
!     RETRIEVE THE CRACK'S SPEED AND PROPAGATION ANGLE FOR EACH NODE ON
!     THE FRONT
    call jeveuo(nvit, 'E', jvit)
    call jeveuo(nbeta, 'E', jbeta)
!
!     CREATION DE VECTEUR DE PASSAGE
    AS_ALLOCATE(vr=vmemo, size=5*numfon)
    AS_ALLOCATE(vr=vjfono, size=4*nbptff)
    AS_ALLOCATE(vi=vjfmulo, size=2*numfon)
    AS_ALLOCATE(vr=vjbaso, size=6*nbptff)
    AS_ALLOCATE(vr=vjvito, size=nbptff)
    AS_ALLOCATE(vr=vjbetao, size=nbptff)
    do j = 1, nbptff
        do i = 1, 4
            vjfono(4*(j-1)+i)=fondfiss(4*(j-1)+i)
        end do
        do i = 1, 6
            vjbaso(6*(j-1)+i)=basefond(6*(j-1)+i)
        end do
        vjvito((j-1)+1)=zr(jvit-1+(j-1)+1)
        vjbetao((j-1)+1)=zr(jbeta-1+(j-1)+1)
    end do
    do i = 1, numfon
        vjfmulo(2*(i-1)+1)=fondmult(2*(i-1)+1)
        vjfmulo(2*(i-1)+2)=fondmult(2*(i-1)+2)
    end do
    mem(1)=1.d0/r8prem()
!
! VERIFICATION DU BON ORDONNANCEMENT DES DIFFERENTS FONDS
    do i = 1, numfon
        do j = 1, nptfg-1
            do k = 1, 4
                a1(k)=fondfisg(4*(j-1)+k)
                b1(k)=fondfisg(4*(j+1-1)+k)
            end do
            normab=(b1(1)-a1(1))**2+(b1(2)-a1(2))**2+ (b1(3)-a1(3))**&
            2
!   ON EXTRAIT LES COORDONNEES DU PREMIER POINT DU FOND DE FISSURE
            if (i .eq. 1) then
                npoin=vjfmulo(1-1)
                m1(1)=fondfiss(4*(npoin)+1)
                m1(2)=fondfiss(4*(npoin)+2)
                m1(3)=fondfiss(4*(npoin)+3)
            else
                npoin=vjfmulo(2*(i-1))
                m1(1)=fondfiss(4*(npoin)+1)
                m1(2)=fondfiss(4*(npoin)+2)
                m1(3)=fondfiss(4*(npoin)+3)
            endif
            coeffk=((b1(1)-a1(1))*(m1(1)-a1(1))+(b1(2)-a1(2))*&
            (m1(2)-a1(2))+(b1(3)-a1(3))*(m1(3)-a1(3)))/normab
!
            if (abs(coeffk) .gt. 1.d0) then
                if (abs(coeffk) .lt. mem(1)) then
                    mem(1)=abs(coeffk)
                    do k = 1, 4
                        mem(k+1)=a1(k)
                    end do
                endif
                goto 505
            else
                call vecini(4, 0.d0, mem)
                mem(1)=1.d0/r8prem()
                goto 66
            endif
505         continue
        end do
        do k = 1, 4
            a1(k)=mem(k+1)
        end do
        call vecini(4, 0.d0, mem)
        mem(1)=1.d0/r8prem()
 66     continue
        do j = 1, nptfg-1
            do k = 1, 4
                a2(k)=fondfisg(4*(j-1)+k)
                b2(k)=fondfisg(4*(j+1-1)+k)
            end do
            normab=(b1(1)-a1(1))**2+(b1(2)-a1(2))**2+ (b1(3)-a1(3))**&
            2
!  ON EXTRAIT LES COORDONNEES DU DERNIER POINT DU FOND DE FISSURE
            if (i .eq. 1) then
                npoin=vjfmulo(1+1)
                m2(1)=fondfiss(4*(npoin-1)+1)
                m2(2)=fondfiss(4*(npoin-1)+2)
                m2(3)=fondfiss(4*(npoin-1)+3)
            else if (i.eq.numfon) then
                m2(1)=fondfiss(4*(nbptff-1)+1)
                m2(2)=fondfiss(4*(nbptff-1)+2)
                m2(3)=fondfiss(4*(nbptff-1)+3)
            else
                npoin=vjfmulo(2*i)
                m2(1)=fondfiss(4*(npoin-1)+1)
                m2(2)=fondfiss(4*(npoin-1)+2)
                m2(3)=fondfiss(4*(npoin-1)+3)
            endif
            coeffk=((b2(1)-a2(1))*(m2(1)-a2(1))+(b2(2)-a2(2))*&
            (m2(2)-a2(2))+(b2(3)-a2(3))*(m2(3)-a2(3)))/normab
!
            if (abs(coeffk) .gt. 1.d0) then
                if (abs(coeffk) .lt. mem(1)) then
                    mem(1)=abs(coeffk)
                    do k = 1, 4
                        mem(k+1)=b2(k)
                    end do
                endif
                goto 506
            else
                goto 67
            endif
506         continue
        end do
!
        do k = 1, 4
            b2(k)=mem(k+1)
        end do
 67     continue
        call vecini(4, 0.d0, mem)
        mem(1)=1.d0/r8prem()
        vmemo(5*(i-1)+1)= i
        vmemo(5*(i-1)+2)= a1(4)
        vmemo(5*(i-1)+3)= b2(4)
        vmemo(5*(i-1)+4)= dble(vjfmulo((2*i-1)))
        vmemo(5*(i-1)+5)= dble(vjfmulo((2*i)))
    end do
!
! ON TRIE LE VECTEUR JMEMO
! (DE L'ABSCISSE CURV. LA PLUS PETITE A LA PLUS GRANDE)
    do i = 1, numfon-1
        do j = i+1, numfon
            if (vmemo(5*(j-1)+2) .lt. vmemo(5*(i-1)+2)) then
                do k = 1, 5
                    memo(k) = vmemo(5*(i-1)+k)
                end do
                do k = 1, 5
                    vmemo(5*(i-1)+k) = vmemo(5*(j-1)+k)
                end do
                do k = 1, 5
                    vmemo(5*(j-1)+k) = memo(k)
                end do
            endif
        end do
    end do
!
    do i = 1, numfon
        vjfmulo(2*(i-1)+1)=nint(vmemo(5*(i-1)+4))
        vjfmulo(2*(i-1)+2)=nint(vmemo(5*(i-1)+5))
    end do
!
    ivalue=0
!
    do i = 1, numfon
        npoino=vjfmulo(2*i)
        nponop=vjfmulo(2*i-1)
!
        if (fondmult(2*(i-1)+2) .ne. vjfmulo(2*(i-1)+2)) then
            vjfmulo(2*(i-1)+1)=ivalue+1
            vjfmulo(2*(i-1)+2)=ivalue+1+(npoino-nponop)
            npoinp=fondmult(2*(nint(vmemo(5*(i-1)+1)))-1)
            nval=vjfmulo(2*(i-1)+1)
            do j = 1, (npoino-nponop)+1
                do k = 1, 4
                    vjfono(4*(nval+j-2)+k)= fondfiss(4*(npoinp+&
                    j-2)+k)
                end do
                do k = 1, 6
                    vjbaso(6*(nval+j-2)+k)= basefond(6*(npoinp+&
                    j-2)+k)
                end do
                vjvito((nval+j-2)+1)=zr(jvit-1+(npoinp+j-2)+1)
                vjbetao((nval+j-2)+1)=zr(jbeta-1+(npoinp+j-2)+1)
            end do
        endif
        npoino=vjfmulo(2*i)
        ivalue=npoino
    end do
!
    do i = 1, numfon
        fondmult(2*(i-1)+1)=vjfmulo(2*(i-1)+1)
        fondmult(2*(i-1)+2)=vjfmulo(2*(i-1)+2)
    end do
    do j = 1, nbptff
        do i = 1, 4
            fondfiss(4*(j-1)+i)=vjfono(4*(j-1)+i)
        end do
        do i = 1, 6
            basefond(6*(j-1)+i)=vjbaso(6*(j-1)+i)
        end do
        zr(jvit-1+(j-1)+1)=vjvito((j-1)+1)
        zr(jbeta-1+(j-1)+1)=vjbetao((j-1)+1)
    end do
!
!     ON VERIFIE QUE LA LECTURE DES POINTS DES FRONTS SE FAIT DANS
!     LE MEME SENS. POUR CELA ON VERIFIE LA COHERENCE AVEC LE SENS
!     DE PARCOURS DU FRONT DE FISSURE SUR LA GRILLE
!
    vect1=vmemo(3)-vmemo(2)
!
    do i = 2, numfon
        vect2=vmemo(5*(i-1)+3)-vmemo(5*(i-1)+2)
        prosca=vect1*vect2
!
        if (prosca .lt. 0.d0) then
!     ON DOIT CHANGER LE SENS DE LECTURE
            nbnol = fondmult(1+2*i-1)-fondmult(1+2*i-2)
            npoin=fondmult(2*(i-1))
            do j = 1, nbnol+1
                do k = 1, 4
                    vjfono(4*(j-1)+k) = fondfiss(4*(npoin+j-1)+ k)
                end do
                do k = 1, 6
                    vjbaso(6*(j-1)+k) = basefond(6*(npoin+j-1)+ k)
                end do
                vjvito((j-1)+1)=zr(jvit-1+(npoin+j-1)+1)
                vjbetao((j-1)+1)=zr(jbeta-1+(npoin+j-1)+1)
            end do
!
            do j = 1, nbnol+1
                do k = 1, 4
                    fondfiss(4*(npoin+j-1)+k)= vjfono(4*(nbnol-&
                    j+1)+k)
                end do
                do k = 1, 6
                    basefond(6*(npoin+j-1)+k)= vjbaso(6*(nbnol-&
                    j+1)+k)
                end do
                zr(jvit-1+(npoin+j-1)+1)=vjvito((nbnol-j+1)+1)
                zr(jbeta-1+(npoin-j-1)+1)=vjbetao((nbnol-j+1)+1)
            end do
        endif
    end do
!
    AS_DEALLOCATE(vr=vmemo)
    AS_DEALLOCATE(vr=vjfono)
    AS_DEALLOCATE(vi=vjfmulo)
    AS_DEALLOCATE(vr=vjbaso)
    AS_DEALLOCATE(vr=vjvito)
    AS_DEALLOCATE(vr=vjbetao)
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
    call jedema()
end subroutine
