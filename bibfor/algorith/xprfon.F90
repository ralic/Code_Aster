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
    integer :: jbasef, jfmult, jfonf, jbeta, jvit, jmemo
    integer :: jbaso, jfmulo, jfono, jbetao, jvito, jfong
    integer :: long, nptfg, nbptff, ivalue, nval
    integer :: npoin, npoinp, npoino, nponop
    real(kind=8) :: a1(4), b1(4), m1(3), a2(4), b2(4), m2(3)
    real(kind=8) :: prosca, normab, coeffk, mem(5), memo(5)
    real(kind=8) :: vect1, vect2
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
    call jeveuo(fiss//'.FONDFISG', 'L', jfong)
    call jelira(fiss//'.FONDFISG', 'LONMAX', long)
    nptfg=long/4
!
!     RECUPERATION DU FOND DE FISSURE
    call jeveuo(fiss//'.FONDFISS', 'E', jfonf)
    call dismoi('NB_POINT_FOND', fiss, 'FISS_XFEM', repi=nbptff)
!
!     RETRIEVE THE DIFFERENT PIECES OF THE CRACK FRONT
    call jeveuo(fiss//'.FONDMULT', 'E', jfmult)
    call dismoi('NB_FOND', fiss, 'FISS_XFEM', repi=numfon)
!
!     RETRIEVE THE LOCAL REFERENCE SYSTEM FOR EACH NODE ON THE FRONT
    call jeveuo(fiss//'.BASEFOND', 'E', jbasef)
!
!     RETRIEVE THE CRACK'S SPEED AND PROPAGATION ANGLE FOR EACH NODE ON
!     THE FRONT
    call jeveuo(nvit, 'E', jvit)
    call jeveuo(nbeta, 'E', jbeta)
!
!     CREATION DE VECTEUR DE PASSAGE
    call wkvect('&&XPRFON.MEMO', 'V V R8', 5*numfon, jmemo)
    call wkvect('&&XPRFON.JFONO', 'V V R8', 4*nbptff, jfono)
    call wkvect('&&XPRFON.JFMULO', 'V V I', 2*numfon, jfmulo)
    call wkvect('&&XPRFON.JBASO', 'V V R8', 6*nbptff, jbaso)
    call wkvect('&&XPRFON.JVITO', 'V V R8', nbptff, jvito)
    call wkvect('&&XPRFON.JBETAO', 'V V R8', nbptff, jbetao)
    do j = 1, nbptff
        do i = 1, 4
            zr(jfono-1+4*(j-1)+i)=zr(jfonf-1+4*(j-1)+i)
        end do
        do i = 1, 6
            zr(jbaso-1+6*(j-1)+i)=zr(jbasef-1+6*(j-1)+i)
        end do
        zr(jvito-1+(j-1)+1)=zr(jvit-1+(j-1)+1)
        zr(jbetao-1+(j-1)+1)=zr(jbeta-1+(j-1)+1)
    end do
    do i = 1, numfon
        zi(jfmulo-1+2*(i-1)+1)=zi(jfmult-1+2*(i-1)+1)
        zi(jfmulo-1+2*(i-1)+2)=zi(jfmult-1+2*(i-1)+2)
    end do
    mem(1)=1.d0/r8prem()
!
! VERIFICATION DU BON ORDONNANCEMENT DES DIFFERENTS FONDS
    do i = 1, numfon
        do j = 1, nptfg-1
            do k = 1, 4
                a1(k)=zr(jfong-1+4*(j-1)+k)
                b1(k)=zr(jfong-1+4*(j+1-1)+k)
            end do
            normab=(b1(1)-a1(1))**2+(b1(2)-a1(2))**2+ (b1(3)-a1(3))**&
            2
!   ON EXTRAIT LES COORDONNEES DU PREMIER POINT DU FOND DE FISSURE
            if (i .eq. 1) then
                npoin=zi(jfmulo-1)
                m1(1)=zr(jfonf-1+4*(npoin)+1)
                m1(2)=zr(jfonf-1+4*(npoin)+2)
                m1(3)=zr(jfonf-1+4*(npoin)+3)
            else
                npoin=zi(jfmulo-1+2*(i-1))
                m1(1)=zr(jfonf-1+4*(npoin)+1)
                m1(2)=zr(jfonf-1+4*(npoin)+2)
                m1(3)=zr(jfonf-1+4*(npoin)+3)
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
                a2(k)=zr(jfong-1+4*(j-1)+k)
                b2(k)=zr(jfong-1+4*(j+1-1)+k)
            end do
            normab=(b1(1)-a1(1))**2+(b1(2)-a1(2))**2+ (b1(3)-a1(3))**&
            2
!  ON EXTRAIT LES COORDONNEES DU DERNIER POINT DU FOND DE FISSURE
            if (i .eq. 1) then
                npoin=zi(jfmulo+1)
                m2(1)=zr(jfonf-1+4*(npoin-1)+1)
                m2(2)=zr(jfonf-1+4*(npoin-1)+2)
                m2(3)=zr(jfonf-1+4*(npoin-1)+3)
            else if (i.eq.numfon) then
                m2(1)=zr(jfonf-1+4*(nbptff-1)+1)
                m2(2)=zr(jfonf-1+4*(nbptff-1)+2)
                m2(3)=zr(jfonf-1+4*(nbptff-1)+3)
            else
                npoin=zi(jfmulo-1+2*i)
                m2(1)=zr(jfonf-1+4*(npoin-1)+1)
                m2(2)=zr(jfonf-1+4*(npoin-1)+2)
                m2(3)=zr(jfonf-1+4*(npoin-1)+3)
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
        zr(jmemo-1+5*(i-1)+1)= i
        zr(jmemo-1+5*(i-1)+2)= a1(4)
        zr(jmemo-1+5*(i-1)+3)= b2(4)
        zr(jmemo-1+5*(i-1)+4)= dble(zi(jfmulo-1+(2*i-1)))
        zr(jmemo-1+5*(i-1)+5)= dble(zi(jfmulo-1+(2*i)))
    end do
!
! ON TRIE LE VECTEUR JMEMO
! (DE L'ABSCISSE CURV. LA PLUS PETITE A LA PLUS GRANDE)
    do i = 1, numfon-1
        do j = i+1, numfon
            if (zr(jmemo-1+5*(j-1)+2) .lt. zr(jmemo-1+5*(i-1)+2)) then
                do k = 1, 5
                    memo(k) = zr(jmemo-1+5*(i-1)+k)
                end do
                do k = 1, 5
                    zr(jmemo-1+5*(i-1)+k) = zr(jmemo-1+5*(j-1)+k)
                end do
                do k = 1, 5
                    zr(jmemo-1+5*(j-1)+k) = memo(k)
                end do
            endif
        end do
    end do
!
    do i = 1, numfon
        zi(jfmulo-1+2*(i-1)+1)=nint(zr(jmemo-1+5*(i-1)+4))
        zi(jfmulo-1+2*(i-1)+2)=nint(zr(jmemo-1+5*(i-1)+5))
    end do
!
    ivalue=0
!
    do i = 1, numfon
        npoino=zi(jfmulo-1+2*i)
        nponop=zi(jfmulo-1+2*i-1)
!
        if (zi(jfmult-1+2*(i-1)+2) .ne. zi(jfmulo-1+2*(i-1)+2)) then
            zi(jfmulo-1+2*(i-1)+1)=ivalue+1
            zi(jfmulo-1+2*(i-1)+2)=ivalue+1+(npoino-nponop)
            npoinp=zi(jfmult-1+2*(nint(zr(jmemo-1+5*(i-1)+1)))-1)
            nval=zi(jfmulo-1+2*(i-1)+1)
            do j = 1, (npoino-nponop)+1
                do k = 1, 4
                    zr(jfono-1+4*(nval+j-2)+k)= zr(jfonf-1+4*(npoinp+&
                    j-2)+k)
                end do
                do k = 1, 6
                    zr(jbaso-1+6*(nval+j-2)+k)= zr(jbasef-1+6*(npoinp+&
                    j-2)+k)
                end do
                zr(jvito-1+(nval+j-2)+1)=zr(jvit-1+(npoinp+j-2)+1)
                zr(jbetao-1+(nval+j-2)+1)=zr(jbeta-1+(npoinp+j-2)+1)
            end do
        endif
        npoino=zi(jfmulo-1+2*i)
        ivalue=npoino
    end do
!
    do i = 1, numfon
        zi(jfmult-1+2*(i-1)+1)=zi(jfmulo-1+2*(i-1)+1)
        zi(jfmult-1+2*(i-1)+2)=zi(jfmulo-1+2*(i-1)+2)
    end do
    do j = 1, nbptff
        do i = 1, 4
            zr(jfonf-1+4*(j-1)+i)=zr(jfono-1+4*(j-1)+i)
        end do
        do i = 1, 6
            zr(jbasef-1+6*(j-1)+i)=zr(jbaso-1+6*(j-1)+i)
        end do
        zr(jvit-1+(j-1)+1)=zr(jvito-1+(j-1)+1)
        zr(jbeta-1+(j-1)+1)=zr(jbetao-1+(j-1)+1)
    end do
!
!     ON VERIFIE QUE LA LECTURE DES POINTS DES FRONTS SE FAIT DANS
!     LE MEME SENS. POUR CELA ON VERIFIE LA COHERENCE AVEC LE SENS
!     DE PARCOURS DU FRONT DE FISSURE SUR LA GRILLE
!
    vect1=zr(jmemo-1+3)-zr(jmemo-1+2)
!
    do i = 2, numfon
        vect2=zr(jmemo-1+5*(i-1)+3)-zr(jmemo-1+5*(i-1)+2)
        prosca=vect1*vect2
!
        if (prosca .lt. 0.d0) then
!     ON DOIT CHANGER LE SENS DE LECTURE
            nbnol = zi(jfmult+2*i-1)-zi(jfmult+2*i-2)
            npoin=zi(jfmult-1+2*(i-1))
            do j = 1, nbnol+1
                do k = 1, 4
                    zr(jfono-1+4*(j-1)+k) = zr(jfonf-1+4*(npoin+j-1)+ k)
                end do
                do k = 1, 6
                    zr(jbaso-1+6*(j-1)+k) = zr(jbasef-1+6*(npoin+j-1)+ k)
                end do
                zr(jvito-1+(j-1)+1)=zr(jvit-1+(npoin+j-1)+1)
                zr(jbetao-1+(j-1)+1)=zr(jbeta-1+(npoin+j-1)+1)
            end do
!
            do j = 1, nbnol+1
                do k = 1, 4
                    zr(jfonf-1+4*(npoin+j-1)+k)= zr(jfono-1+4*(nbnol-&
                    j+1)+k)
                end do
                do k = 1, 6
                    zr(jbasef-1+6*(npoin+j-1)+k)= zr(jbaso-1+6*(nbnol-&
                    j+1)+k)
                end do
                zr(jvit-1+(npoin+j-1)+1)=zr(jvito-1+(nbnol-j+1)+1)
                zr(jbeta-1+(npoin-j-1)+1)=zr(jbetao-1+(nbnol-j+1)+1)
            end do
        endif
    end do
!
    call jedetr('&&XPRFON.MEMO')
    call jedetr('&&XPRFON.JFONO')
    call jedetr('&&XPRFON.JFMULO')
    call jedetr('&&XPRFON.JBASO')
    call jedetr('&&XPRFON.JVITO')
    call jedetr('&&XPRFON.JBETAO')
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
    call jedema()
end subroutine
