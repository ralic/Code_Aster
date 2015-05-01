subroutine pacouc(typflu, vecr1, vecr2, vite, vecr3,&
                  masg, freq, amor, nbno, indic,&
                  nbpv, w, veci1, vecr4, vecr5,&
                  ier)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/pacou0.h"
#include "asterfort/wkvect.h"
    character(len=8) :: typflu
    integer :: nbno, indic, nbpv, veci1(*), ier, jtrav1, jtrav2
    real(kind=8) :: vecr1(*), vecr2(*), vite(*), vecr3(*), masg(*), freq(*)
    real(kind=8) :: amor(*), w(*), vecr4(*), vecr5(*)
    character(len=24) :: nom1, nom2
!
    aster_logical :: check, veriu0
    real(kind=8) :: ksi0, kcaj, vgap
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, i1, i2, itypfl, j, jcompt, jextr
    integer :: k, k1, k10, k11, k12, k2
    integer :: k3, k4, k5, k6, k7, k8, k9
    integer :: l1, l2, l3, nb, nt
    integer :: nzone
    real(kind=8) :: bmax, bmin, delta, hmoy, pi, pulsam
    real(kind=8) :: visc
    integer, pointer :: fsic(:) => null()
    real(kind=8), pointer :: fsvr(:) => null()
    integer, pointer :: tempo(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
!
    nom1 = '&&COEFMO.COMPT'
    nom2 = '&&COEFMO.EXTR'
!
    veriu0 = .false.
    kcaj = 0.d0
    call jeveuo(typflu//'           .FSIC', 'L', vi=fsic)
    itypfl = fsic(1)
    if (itypfl .eq. 4) then
        if (indic .eq. 1) then
            veriu0 = .true.
            call jeveuo(typflu//'           .FSVR', 'L', vr=fsvr)
            visc = fsvr(2)
            hmoy = vecr4(1)
            kcaj = 12.d0*visc/(hmoy*hmoy)
        endif
    endif
!
    pi = r8pi()
    nt = 2
    k1 = 1 + nt
    k2 = k1 + nt
    k3 = k2 + nt*nt
    k4 = k3 + nt*nt
    k5 = k4 + nt
    k6 = k5 + nt
    k7 = k6 + nt
    k8 = k7 + nt
    k9 = k8 + nt
    k10 = k9 + nt
    k11 = k10 + nt
    k12 = k11 + nt
!
    if (itypfl .eq. 1) then
        call jeveuo('&&MDCONF.TEMPO', 'L', vi=tempo)
        nzone = tempo(1)
        nb = nbno*nbpv*nzone
        call wkvect('&&PACOUC.TRAV1', 'V V R', 2*nb, jtrav1)
        call wkvect('&&PACOUC.TRAV2', 'V V I', 3*nb, jtrav2)
    endif
!
    do 10 i = 1, nbpv
        vgap = vite(i)
        do 20 j = 1, nbno
            if (veriu0 .and. dble(abs(vgap)) .lt. 1.d-5) then
                ksi0 = ( amor(j) + kcaj*vecr1(j)) / (masg(j)*4.d0*pi* amor(nbno+j) )
            else
                ksi0 = amor(j) / (masg(j)*4.d0*pi*amor(nbno+j))
            endif
            delta = -2.d0*pi*amor(nbno+j)*ksi0
            pulsam = 2.d0*pi*amor(nbno+j)*sqrt(1.d0-ksi0*ksi0)
            w(1) = delta
            w(2) = pulsam
!
            call pacou0(w(1), w(k1), w(k2), w(k3), w(k4),&
                        w(k5), w(k6), w(k7), w(k8), w(k9),&
                        w(k10), w(k11), w(k12), w(13), check,&
                        vecr1, vecr2, typflu, vecr3, amor,&
                        masg, vecr4, vecr5, veci1, vgap,&
                        indic, nbno, j, nt)
!
            if (check) then
                w(1) = -delta
                w(2) = pulsam
                call pacou0(w(1), w(k1), w(k2), w(k3), w(k4),&
                            w(k5), w(k6), w(k7), w(k8), w(k9),&
                            w(k10), w(k11), w(k12), w(13), check,&
                            vecr1, vecr2, typflu, vecr3, amor,&
                            masg, vecr4, vecr5, veci1, vgap,&
                            indic, nbno, j, nt)
            endif
!
            if (check) then
                w(1) = 0.d0
                w(2) = pulsam
                call pacou0(w(1), w(k1), w(k2), w(k3), w(k4),&
                            w(k5), w(k6), w(k7), w(k8), w(k9),&
                            w(k10), w(k11), w(k12), w(13), check,&
                            vecr1, vecr2, typflu, vecr3, amor,&
                            masg, vecr4, vecr5, veci1, vgap,&
                            indic, nbno, j, nt)
            endif
!
            if (check) then
                w(1) = 0.5d0*delta
                w(2) = pulsam
                call pacou0(w(1), w(k1), w(k2), w(k3), w(k4),&
                            w(k5), w(k6), w(k7), w(k8), w(k9),&
                            w(k10), w(k11), w(k12), w(13), check,&
                            vecr1, vecr2, typflu, vecr3, amor,&
                            masg, vecr4, vecr5, veci1, vgap,&
                            indic, nbno, j, nt)
            endif
!
            if (check) then
                w(1) = 2.d0*delta
                w(2) = pulsam
                call pacou0(w(1), w(k1), w(k2), w(k3), w(k4),&
                            w(k5), w(k6), w(k7), w(k8), w(k9),&
                            w(k10), w(k11), w(k12), w(13), check,&
                            vecr1, vecr2, typflu, vecr3, amor,&
                            masg, vecr4, vecr5, veci1, vgap,&
                            indic, nbno, j, nt)
            endif
!
            if (check) then
                w(1) = 5.d0*delta
                w(2) = pulsam
                call pacou0(w(1), w(k1), w(k2), w(k3), w(k4),&
                            w(k5), w(k6), w(k7), w(k8), w(k9),&
                            w(k10), w(k11), w(k12), w(13), check,&
                            vecr1, vecr2, typflu, vecr3, amor,&
                            masg, vecr4, vecr5, veci1, vgap,&
                            indic, nbno, j, nt)
            endif
!
            if (check) then
                w(1) = 10.d0*delta
                w(2) = pulsam
                call pacou0(w(1), w(k1), w(k2), w(k3), w(k4),&
                            w(k5), w(k6), w(k7), w(k8), w(k9),&
                            w(k10), w(k11), w(k12), w(13), check,&
                            vecr1, vecr2, typflu, vecr3, amor,&
                            masg, vecr4, vecr5, veci1, vgap,&
                            indic, nbno, j, nt)
            endif
!
            if (check) then
                w(1) = 20.d0*delta
                w(2) = pulsam
                call pacou0(w(1), w(k1), w(k2), w(k3), w(k4),&
                            w(k5), w(k6), w(k7), w(k8), w(k9),&
                            w(k10), w(k11), w(k12), w(13), check,&
                            vecr1, vecr2, typflu, vecr3, amor,&
                            masg, vecr4, vecr5, veci1, vgap,&
                            indic, nbno, j, nt)
            endif
!
            if (check) then
                i1 = (i-1)*2*nbno + (j-1)*2 + 1
                i2 = (i-1)*2*nbno + (j-1)*2 + 2
                freq(i1) = -1.d0
                freq(i2) = -1.d0
            else
                i1 = (i-1)*2*nbno + (j-1)*2 + 1
                i2 = (i-1)*2*nbno + (j-1)*2 + 2
                freq(i1) = sqrt(w(1)*w(1)+w(2)*w(2))/ (2.d0*pi)
                freq(i2) = -w(1)/ (2.d0*pi*freq(i1))
!
!           ON STOCKE EN FIN DE BOUCLE ET POUR CHAQUE ZONE LES VALEURS
!           DE VITESSES REDUITES MIN ET MAX QUI SORTENT DE LA PLAGE
!           EXPERIMENTALE
                if (itypfl .eq. 1) then
                    call jeveuo(nom1, 'L', jcompt)
                    call jeveuo(nom2, 'L', jextr)
                    do 30 k = 1, nzone
                        l1 = zi(jcompt+3*(k-1))
                        l2 = zi(jcompt+3*(k-1)+1)
                        l3 = zi(jcompt+3*(k-1)+2)
                        bmin = zr(jextr+2*(k-1))
                        bmax = zr(jextr+2*(k-1)+1)
                        zr(jtrav1 + 2*nzone*nbpv*(j-1) + 2*(i-1)*&
                        nzone + 2*(k-1)) = bmin
                        zr(jtrav1 + 2*nzone*nbpv*(j-1) + 2*(i-1)*&
                        nzone + 2*(k-1) + 1) = bmax
                        zi(jtrav2 + 3*nzone*nbpv*(j-1) + 3*(i-1)*&
                        nzone + 3*(k-1)) = l1
                        zi(jtrav2 + 3*nzone*nbpv*(j-1) + 3*(i-1)*&
                        nzone + 3*(k-1) + 1) = l2
                        zi(jtrav2 + 3*nzone*nbpv*(j-1) + 3*(i-1)*&
                        nzone + 3*(k-1) + 2) = l3
 30                 continue
                endif
            endif
 20     continue
 10 end do
!
    if (.not.check) ier = 0
!
    call jedema()
end subroutine
